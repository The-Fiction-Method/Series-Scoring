--	create tables for each Series, create an _Order_Series, create RUN view, and run the code it generates

CREATE TABLE "@series" (
	"Episode-Production"	INTEGER,
	"Episode-Air"	TEXT,
	"Title"	TEXT,
	"hostA"	NUMERIC,
	"hostB"	NUMERIC,
	"hostC"	NUMERIC,
	...
	"Link"	TEXT,
	"IMDB_Rating"	NUMERIC
)

CREATE TABLE "_Order_Series" (
	"sort"	INTEGER UNIQUE,
	"name"	TEXT UNIQUE,
	"abbr"	TEXT,
	PRIMARY KEY("sort" AUTOINCREMENT)
)

CREATE TABLE "Stream_Notes" (
	"Stream_Date"	TEXT,
	"Stream_Link"	TEXT UNIQUE,
	"Notes"	TEXT,
	...
);

DROP VIEW IF EXISTS "_Stream_Notes_Update";
CREATE VIEW  "_Stream_Notes_Update" AS
WITH RECURSIVE TABS AS (
	SELECT name FROM sqlite_master WHERE type IS 'table'
)
SELECT
	_Order_Series.name AS "Series",
	'INSERT INTO Stream_Notes (Stream_Link, Stream_Date) SELECT LINK, date() FROM "' || TABS.name || '" WHERE Link IS NOT NULL ON CONFLICT (Stream_Link) DO NOTHING;' AS 'INSERT_Commands'
FROM TABS
	JOIN _Order_Series ON ltrim(REPLACE(TABS.name, '_', ' '), '@') = _Order_Series.name
ORDER BY _Order_Series.sort;


DROP VIEW IF EXISTS '__RUN';
CREATE VIEW '__RUN' AS
WITH Hosts AS (
WITH RECURSIVE TABS AS (
	SELECT name FROM sqlite_master WHERE type IS 'table'
)
SELECT
	TABS.name AS 'name',
	COLS.name AS 'host',
	_Order_Series.name AS 'readable',
	_Order_Series.abbr,
	_Order_Series.sort,
	row_number() OVER (PARTITION BY COLS.name ORDER BY _Order_Series.sort) as rowNUM
FROM TABS
	JOIN pragma_table_info(TABS.name) COLS
	JOIN _Order_Series ON ltrim(REPLACE(TABS.name, '_', ' '), '@') = _Order_Series.name
WHERE COLS.type IS 'NUMERIC' AND COLS.name IS NOT 'IMDB_Rating'
ORDER BY _Order_Series.sort
), hostsAVG AS (
SELECT
name, sort,
group_concat(char(9) || 'CASE WHEN ' || host || ' NOT NULL THEN 1 ELSE 0 END', ' +' || char(10)) AS hostCOUNT,
group_concat('ifnull(' || abbr || '.' || host || ', 0)', ' + ') || ' + 0.0)/present_' || abbr || '.Present' AS hostAverage
FROM Hosts
WHERE host NOT LIKE '%IMDB%'
GROUP BY name
), hostsSTDEV AS (
SELECT
Hosts.name, Hosts.sort,
'round(pow((' || char(10) ||
group_concat('ifnull(pow(' || abbr || '.' || Hosts.host || ' - (' || hostsAVG.hostAverage || ', 2), 0)', ' + ' || char(10)) ||
') / (present_' || abbr || '.Present - 1), 0.5), 3)'
AS hostStDev
FROM Hosts
	LEFT JOIN hostsAVG ON Hosts.name = hostsAVG.name
GROUP BY Hosts.name
), HostsCALC AS (
SELECT
Hosts.*,
hostsAVG.hostCount,
hostsAVG.hostAverage,
hostsSTDEV.hostStDev
FROM Hosts
	LEFT JOIN hostsAVG ON Hosts.name = hostsAVG.name
	LEFT JOIN hostsSTDEV ON Hosts.name = hostsSTDEV.name
ORDER BY sort
), hostPresents AS (
SELECT
 group_concat(
'present_' || Hosts.abbr || ' AS (' || char(10) || char(9) ||
'SELECT' || char(10) || char(9) ||
'"' || Hosts.readable || '" AS Series,' || char(10) || char(9) ||
'"Episode-Air",' || char(10) ||
hostCount || char(10) || char(9) ||
'AS present' || char(10) || char(9) || 'FROM "' || Hosts.name || '"' || char(10) || ')'
, ', ' || char(10))
AS OUT, 1 AS 'part', Hosts.sort
FROM (SELECT * FROM Hosts GROUP BY name ORDER BY sort) Hosts
	LEFT JOIN (SELECT * FROM HostsCALC GROUP BY name) HostsCALC ON Hosts.name = HostsCALC.name
GROUP BY Hosts.name
), hostSummary AS(
SELECT
'SELECT' || char(10) ||
'''' || Hosts.readable || ''' AS Series,' || char(10) ||
'Title, ' || Hosts.abbr || '."Episode-Air",' || char(10) ||
'round(nullif((' || hostAverage || ', 0), 3) AS Average,' || char(10) ||
hostStDev || ' AS StDev,' || char(10) ||
'nullif(present_' || Hosts.abbr || '.Present, 0) AS "Hosts",' || char(10) ||
'IMDB_Rating,' || char(10) ||
'round(IMDB_Rating - round(' || hostAverage || ', 3) AS IMDB_Diff' || char(10) ||
'FROM "present_' || Hosts.abbr || '"' || char(10) ||
char(9) || 'LEFT JOIN "' || Hosts.name || '" ' || Hosts.abbr || ' ON ' || Hosts.abbr || '."Episode-Air" = present_' || Hosts.abbr || '."Episode-Air"'
AS OUT,
2 as part, Hosts.sort
FROM (SELECT * FROM Hosts GROUP BY name) Hosts
	LEFT JOIN HostsCALC ON Hosts.name = HostsCALC.name

GROUP BY Hosts.name
ORDER BY Hosts.sort
), rankSUMM AS (
SELECT
name,
'SELECT ''' || readable || ''' AS Series, ' || char(10) ||
rtrim(group_concat(
'NULL AS Score,' || char(10) ||
'count(' || host || ') AS ' || host || '_Count,' || char(10) ||
'round(sum(' || host || ')/count(' || host || '), 3) || '' - Average'' AS ' || host || '_Episodes,'
, char(10)), ',') || char(10) ||
'FROM "' || name || '"' || char(10) ||
char(9) || 'WHERE "Episode-Air" LIKE ''S%%''' || char(10) ||
char(9) || 'GROUP BY 1'
AS OUT, sort, abbr
FROM Hosts
WHERE host NOT LIKE '%IMDB%'
GROUP BY name
ORDER BY sort
), rankEPIS AS (
--	ranked
SELECT
name,
'SELECT' || char(10) ||
'''' || readable || ''' AS Series,' || char(10) ||
rtrim(group_concat(
' Score,' || char(10) ||
'scoreS' || host || '.Counts AS ' || host || '_Count,' || char(10) ||
'scoreS' || host || '.Title AS ' || host || '_Episodes,'
, char(10)), ',') || char(10) ||
'FROM (' || group_concat(
'SELECT ifnull(' || host || ', ''Missing'') AS Score FROM "' || name || '" WHERE "Episode-Air" LIKE ''S%%'''
, ' UNION ') || char(10) ||
')' || char(10) ||
group_concat(char(9) || 'LEFT JOIN (SELECT group_concat(''"'' || Title || ''"'', '', '') AS Title, count(Title) AS Counts, ifnull(' || host || ', ''Missing'') AS scores FROM "' || name || '" WHERE "Episode-Air" LIKE ''S%%'' AND Link IS NOT NULL GROUP BY scores) scoreS' || host || ' ON scoreS' || host || '.scores = Score', char(10)) || char(10) ||
'WHERE Score IS NOT NULL' || char(10) ||
'GROUP BY Score' || char(10) ||
'ORDER BY CASE WHEN Score IS ''Missing'' THEN -10 ELSE Score END DESC'
AS OUT, sort, abbr
FROM Hosts
WHERE host NOT LIKE '%IMDB%'
GROUP BY name ORDER BY sort
)

--	Score-Averages
SELECT 'Score-Averages' AS 'Creates', group_concat(OUT, char(10) || char(10)) AS OUT FROM (
SELECT
'DROP VIEW IF EXISTS "Score-Averages";' || char(10) || 'CREATE VIEW "Score-Averages" AS ' || char(10) ||
'WITH ' || group_concat(OUT, ', ') AS OUT, part, -1 AS sort
FROM hostPresents
GROUP BY part

UNION ALL

SELECT
group_concat(OUT || char(10) || 'WHERE Average IS NOT NULL', char(10) || char(10) || 'UNION ALL' || char(10) || char(10)) || ';' AS OUT, part, -1 AS sort
FROM hostSummary
) GROUP BY sort

UNION ALL
--	Summary
SELECT '@_Summary' AS 'Creates', 
'DROP VIEW IF EXISTS ''@_Summary'';' || char(10) ||
'CREATE VIEW ''@_Summary'' AS ' || char(10) ||
'SELECT ' || char(10) ||
'Series, Title, Average || '' average, '' || StDev || '' standard deviation'' as Summary' || char(10) ||
'FROM "Score-Averages"' || char(10) || char(9) ||
	'LEFT JOIN _Order_Series ON _Order_Series.name = "Score-Averages".series' || char(10) ||
'ORDER BY _Order_Series.sort DESC, row_number() over () DESC;' AS OUT

UNION ALL

SELECT 'Ranks per Series' AS 'Creates',  * FROM ( SELECT
'DROP VIEW IF EXISTS ''Ranks-' || abbr ||''';' || char(10) ||
'CREATE VIEW ''Ranks-' || abbr ||''' AS' || char(10) ||
group_concat(OUT, char(10) || char(10) || 'UNION ALL' || char(10) || char(10)) || ';' || char(10) AS OUT
FROM (
	SELECT name, sort, abbr, OUT FROM rankSUMM
	UNION ALL
	SELECT name, sort, abbr, 'SELECT * FROM (' || OUT || ')' AS OUT FROM rankEPIS
)
GROUP BY name
ORDER BY sort );

--	gives example code for per-season rank information to help construct a combined View
DROP VIEW IF EXISTS '__RUN_Ranks-Season';
CREATE VIEW '__RUN_Ranks-Season' AS
SELECT
Creates,
rtrim(Out, ';' || char(10)) AS Series,
rtrim(replace(replace(replace(replace(replace(OUT, 'scoreS', 'scoreS01'), 'S%%', 'S01%%'), ' Score', ' Score_S01'), '_Count', '_Count_S01'), '_Episodes', '_Episodes_S01'), ';' || char(10)) as S01,
rtrim(replace(replace(replace(replace(replace(OUT, 'scoreS', 'scoreS02'), 'S%%', 'S02%%'), ' Score', ' Score_S02'), '_Count', '_Count_S02'), '_Episodes', '_Episodes_S02'), ';' || char(10)) as S02,
rtrim(replace(replace(replace(replace(replace(OUT, 'scoreS', 'scoreS03'), 'S%%', 'S03%%'), ' Score', ' Score_S03'), '_Count', '_Count_S03'), '_Episodes', '_Episodes_S03'), ';' || char(10)) as S03
FROM __RUN
	WHERE Creates like '%Ranks%';

--	gives SELECT code, per series, for each column, so hosts and season is possible, but meant for easy turning-off of columns
DROP VIEW IF EXISTS '_Ranks-Specific';
CREATE VIEW '_Ranks-Specific' AS
WITH rankings AS(
WITH RECURSIVE VIEWS AS (
	SELECT name FROM sqlite_master WHERE type IS 'view' AND name LIKE 'Rank%%'
)
SELECT
VIEWS.name,
COLS.name AS host,
_Order_Series.sort
FROM VIEWS
	JOIN pragma_table_info(VIEWS.name) COLS
	JOIN _Order_Series ON substr(VIEWS.name, -3) = _Order_Series.abbr
WHERE COLS.name LIKE '%_Count%'
ORDER BY _Order_Series.sort
)

SELECT 
name,
'SELECT' || char(10) ||
'Score,' || char(10) ||
rtrim(group_concat(
char(9) || '"' || host || '",' || char(10) ||
char(9) || replace(host, '_Count', '_Episodes')
, ',' || char(10)), ',') || char(10) ||
'FROM "' || name || '"'
AS OUT
FROM rankings
GROUP BY name
ORDER BY sort;
