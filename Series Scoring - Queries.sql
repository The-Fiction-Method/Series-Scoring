--	create tables for each Series, create an _Order_Series, create __RUN view, and run the code it generates

CREATE TABLE "@series" (
	"Episode-Production"	INTEGER,
	"Episode-Air"	TEXT,
	"Title"	TEXT,
	"hostA"	NUMERIC,
	"hostB"	NUMERIC,
	"hostC"	NUMERIC,
	...
	"Link"	TEXT,
	"score_Rating"	NUMERIC
)

CREATE TABLE "_Order_Series" (
	"sort"	INTEGER UNIQUE,
	"name"	TEXT UNIQUE,
	"abbr"	TEXT,
	PRIMARY KEY("sort" AUTOINCREMENT)
)


--	__RUN
DROP VIEW IF EXISTS '__RUN';
CREATE VIEW '__RUN' AS
WITH INFO AS (
WITH RECURSIVE TABS AS (
	SELECT name FROM sqlite_master WHERE type IS 'table'
)
SELECT
	TABS.name AS 'tab',
	COLS.name AS 'host',
	_Order_Series.name AS 'readable',
	_Order_Series.abbr,
	_Order_Series.sort,
	row_number() OVER (PARTITION BY COLS.name ORDER BY _Order_Series.sort) as rowNUM
FROM TABS
	JOIN pragma_table_info(TABS.name) COLS
	JOIN _Order_Series ON ltrim(REPLACE(TABS.name, '_', ' '), '@') = _Order_Series.name
WHERE (COLS.type IS 'NUMERIC' AND COLS.name NOT LIKE '%Rating%') OR COLS.name = 'Link'
ORDER BY _Order_Series.sort
), createTRIGS AS (
	SELECT
	'Triggers' AS 'Creates',
	'DROP TRIGGER IF EXISTS Notes_'||abbr||';'||char(10)||
	'CREATE TRIGGER Notes_'||abbr||char(10)||
	char(9)||'AFTER UPDATE OF Link ON "'||tab||'"'||char(10)||
	char(9)||'WHEN (old.Link IS NULL AND new.Link LIKE ''http%'')' ||char(10)||
	'BEGIN'||char(10)||
	char(9)||'INSERT INTO "Stream_Notes" ("Series", "Stream_Date", "Stream_Link")'||char(10)||
	char(9)||'VALUES ("'||readable||'", date(''now'', ''localtime''), new.Link);'||char(10)||
	'END;' AS OUT
	FROM (SELECT * FROM INFO WHERE (tab NOT LIKE '%Original%' AND host = 'Link') GROUP BY tab ORDER BY rowNUM) INFO
), tabNOTES AS (
	SELECT
	-- 'Tables' AS 'Creates',
	'CREATE TABLE IF NOT EXISTS "Stream_Notes" ('||char(10)||
	char(9)||'"Series" TEXT,'||char(10)||
	char(9)||'"Stream_Date"	TEXT,'||char(10)||
	char(9)||'"Stream_Link"	TEXT,'||char(10)||
	char(9)||'"Notes"	TEXT'||char(10)||
	')'||char(10) AS OUT
), hostsAVG AS (
SELECT
tab, sort,
group_concat(char(9) || 'CASE WHEN ' || host || ' NOT NULL THEN 1 ELSE 0 END', ' +' || char(10)) AS hostCOUNT,
group_concat('ifnull(' || abbr || '.' || host || ', 0)', ' + ') || ' + 0.0)/present_' || abbr || '.Present' AS hostAverage
FROM (SELECT * FROM INFO WHERE (tab NOT LIKE '%Original%' AND host <> 'Link')) INFO
WHERE host NOT LIKE '%Rating%'
GROUP BY tab
), hostsSTDEV AS (
SELECT
INFO.tab, INFO.sort,
'round(pow((' || char(10) ||
group_concat('ifnull(pow(' || abbr || '.' || INFO.host || ' - (' || hostsAVG.hostAverage || ', 2), 0)', ' + ' || char(10)) ||
') / (present_' || abbr || '.Present - 1), 0.5), 3)'
AS hostStDev
FROM (SELECT * FROM INFO WHERE (tab NOT LIKE '%Original%' AND host <> 'Link')) INFO
	LEFT JOIN hostsAVG ON INFO.tab = hostsAVG.tab
GROUP BY INFO.tab
), HostsCALC AS (
SELECT
INFO.*,
hostsAVG.hostCount,
hostsAVG.hostAverage,
hostsSTDEV.hostStDev
FROM INFO
	LEFT JOIN hostsAVG ON INFO.tab = hostsAVG.tab
	LEFT JOIN hostsSTDEV ON INFO.tab = hostsSTDEV.tab
ORDER BY sort
), hostPresents AS (
SELECT
 group_concat(
'present_' || INFO.abbr || ' AS (' || char(10) || char(9) ||
'SELECT' || char(10) || char(9) ||
'"' || INFO.readable || '" AS Series,' || char(10) || char(9) ||
'"Title",' || char(10) ||
hostCount || char(10) || char(9) ||
'AS present' || char(10) || char(9) || 'FROM "' || INFO.tab || '"' || char(10) || ')'
, ', ' || char(10))
AS OUT, 1 AS 'part', INFO.sort
FROM (SELECT * FROM INFO WHERE (tab NOT LIKE '%Original%' AND host <> 'Link') GROUP BY tab ORDER BY rowNUM) INFO
	LEFT JOIN (SELECT * FROM HostsCALC GROUP BY tab) HostsCALC ON INFO.tab = HostsCALC.tab
GROUP BY INFO.tab
ORDER BY INFO.sort
), hostSummary AS(
SELECT
'SELECT' || char(10) ||
'''' || INFO.readable || ''' AS Series,' || char(10) ||
INFO.abbr || '."Title",Notes.Stream_Date,' || char(10) ||
'round(nullif((' || hostAverage || ', 0), 3) AS Average,' || char(10) ||
hostStDev || ' AS StDev,' || char(10) ||
'nullif(present_' || INFO.abbr || '.Present, 0) AS "Hosts"' || char(10) ||
'FROM "present_' || INFO.abbr || '"' || char(10) ||
char(9) || 'LEFT JOIN "' || INFO.tab || '" ' || INFO.abbr || ' ON ' || INFO.abbr || '."Title" = present_' || INFO.abbr || '."Title"' || char(10) ||
char(9) || 'LEFT JOIN "Stream_Notes" Notes ON ' || INFO.abbr || '.Link = Notes.Stream_Link'
AS OUT,
2 as part, INFO.sort
FROM (SELECT * FROM INFO WHERE (tab NOT LIKE '%Original%' AND host <> 'Link') ORDER BY rowNUM) INFO
	LEFT JOIN HostsCALC ON INFO.tab = HostsCALC.tab

GROUP BY INFO.tab
ORDER BY INFO.sort
)

--	Tables
SELECT 'Stream_Notes' AS Creates, * FROM tabNOTES
UNION ALL
--	Triggers
SELECT
Creates,
group_concat(OUT, char(10)) AS OUT
FROM createTRIGS
GROUP BY "Creates"
UNION ALL
--	Views
--		Score-Averages
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
--		Summary
SELECT '@_Summary' AS 'Creates', 
'DROP VIEW IF EXISTS ''@_Summary'';' || char(10) ||
'CREATE VIEW ''@_Summary'' AS ' || char(10) ||
'SELECT ' || char(10) ||
'Series, Title, Average || '' average, '' || StDev || '' standard deviation'' as Summary' || char(10) ||
'FROM "Score-Averages"' || char(10) || char(9) ||
	'LEFT JOIN _Order_Series ON _Order_Series.name = "Score-Averages".series' || char(10) ||
'WHERE Series NOT LIKE ''%Original%''' || char(10)||
'ORDER BY Stream_Date DESC;' AS OUT
UNION ALL
--		Franchise
SELECT '_Franchise' AS 'Creates', 
'DROP VIEW IF EXISTS ''_Franchise'';' || char(10) ||
'CREATE VIEW ''_Franchise'' AS ' || char(10) ||
'WITH FRAN AS ('||char(10)||
group_concat(char(9)||'SELECT '''||abbr||''' AS Series, "Title", "Air Date" FROM "'||tab||'"', char(10)||char(9)||'UNION ALL'||char(10))||
')'||char(10)||
'SELECT ORD.name, FRAN.* FROM FRAN'||char(10)||
char(9)||'LEFT JOIN "_Order_Series" ORD ON ORD.abbr = FRAN.Series' AS OUT
FROM (SELECT * FROM INFO GROUP BY tab HAVING tab NOT LIKE '%Original%' ORDER BY rowNUM);


--	should not be necessary as Triggers handle this now
/*
DROP VIEW IF EXISTS "_Stream_Notes_Update";
CREATE VIEW "_Stream_Notes_Update" AS
WITH RECURSIVE TABS AS (
	SELECT name FROM sqlite_master WHERE type IS 'table'
)
SELECT
	_Order_Series.name AS "Series",
	'INSERT INTO Stream_Notes (Stream_Link, Stream_Date) SELECT LINK, date() FROM "' || TABS.name || '" WHERE Link IS NOT NULL ON CONFLICT (Stream_Link) DO NOTHING;' AS 'INSERT_Commands'
FROM TABS
	JOIN _Order_Series ON ltrim(REPLACE(TABS.name, '_', ' '), '@') = _Order_Series.name
ORDER BY _Order_Series.sort
*/