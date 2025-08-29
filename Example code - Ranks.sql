--	Example of how to combine the series rankings with the individual season rankings

DROP VIEW IF EXISTS 'Ranks-SG1';
CREATE VIEW 'Ranks-SG1' AS

WITH SG1 AS (
SELECT 'Stargate SG1' AS Series,
NULL AS Score,
count(Richard) AS Richard_Count,
round(sum(Richard)/count(Richard), 3) || ' - Average' AS Richard_Episodes,
count(Michael) AS Michael_Count,
round(sum(Michael)/count(Michael), 3) || ' - Average' AS Michael_Episodes,
count(Amanda) AS Amanda_Count,
round(sum(Amanda)/count(Amanda), 3) || ' - Average' AS Amanda_Episodes,
count(Chris) AS Chris_Count,
round(sum(Chris)/count(Chris), 3) || ' - Average' AS Chris_Episodes
FROM "@Stargate_SG1"
	WHERE "Episode-Air" LIKE 'S%%'
	GROUP BY 1

UNION ALL

SELECT * FROM (SELECT
'Stargate SG1' AS Series,
Score,
scoreSRichard.Counts AS Richard_Count,
scoreSRichard.Title AS Richard_Episodes,
scoreSMichael.Counts AS Michael_Count,
scoreSMichael.Title AS Michael_Episodes,
scoreSAmanda.Counts AS Amanda_Count,
scoreSAmanda.Title AS Amanda_Episodes,
scoreSChris.Counts AS Chris_Count,
scoreSChris.Title AS Chris_Episodes
FROM (SELECT ifnull(Richard, 'Missing') AS Score FROM "@Stargate_SG1" UNION SELECT ifnull(Michael, 'Missing') AS Score FROM "@Stargate_SG1" UNION SELECT ifnull(Amanda, 'Missing') AS Score FROM "@Stargate_SG1" UNION SELECT ifnull(Chris, 'Missing') AS Score FROM "@Stargate_SG1"
)
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Richard, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S%%' AND Link IS NOT NULL GROUP BY scores) scoreSRichard ON scoreSRichard.scores = Score
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Michael, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S%%' AND Link IS NOT NULL GROUP BY scores) scoreSMichael ON scoreSMichael.scores = Score
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Amanda, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S%%' AND Link IS NOT NULL GROUP BY scores) scoreSAmanda ON scoreSAmanda.scores = Score
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Chris, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S%%' AND Link IS NOT NULL GROUP BY scores) scoreSChris ON scoreSChris.scores = Score
WHERE Score IS NOT NULL
GROUP BY Score
ORDER BY CASE WHEN Score IS 'Missing' THEN -10 ELSE Score END DESC)
), SG1_S01 AS (
SELECT 'Stargate SG1' AS Series_S01,
NULL AS Score_S01,
count(Richard) AS Richard_Count_S01,
round(sum(Richard)/count(Richard), 3) || ' - Average' AS Richard_Episodes_S01,
count(Michael) AS Michael_Count_S01,
round(sum(Michael)/count(Michael), 3) || ' - Average' AS Michael_Episodes_S01,
count(Amanda) AS Amanda_Count_S01,
round(sum(Amanda)/count(Amanda), 3) || ' - Average' AS Amanda_Episodes_S01,
count(Chris) AS Chris_Count_S01,
round(sum(Chris)/count(Chris), 3) || ' - Average' AS Chris_Episodes_S01
FROM "@Stargate_SG1"
	WHERE "Episode-Air" LIKE 'S01%%'
	GROUP BY 1

UNION ALL

SELECT * FROM (SELECT
'Stargate SG1' AS Series_S01,
Score_S01,
scoreS01Richard.Counts AS Richard_Count_S01,
scoreS01Richard.Title AS Richard_Episodes_S01,
scoreS01Michael.Counts AS Michael_Count_S01,
scoreS01Michael.Title AS Michael_Episodes_S01,
scoreS01Amanda.Counts AS Amanda_Count_S01,
scoreS01Amanda.Title AS Amanda_Episodes_S01,
scoreS01Chris.Counts AS Chris_Count_S01,
scoreS01Chris.Title AS Chris_Episodes_S01
FROM (SELECT ifnull(Richard, 'Missing') AS Score_S01 FROM "@Stargate_SG1" UNION SELECT ifnull(Michael, 'Missing') AS Score_S01 FROM "@Stargate_SG1" UNION SELECT ifnull(Amanda, 'Missing') AS Score_S01 FROM "@Stargate_SG1" UNION SELECT ifnull(Chris, 'Missing') AS Score_S01 FROM "@Stargate_SG1"
)
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Richard, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S01%%' AND Link IS NOT NULL GROUP BY scores) scoreS01Richard ON scoreS01Richard.scores = Score_S01
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Michael, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S01%%' AND Link IS NOT NULL GROUP BY scores) scoreS01Michael ON scoreS01Michael.scores = Score_S01
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Amanda, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S01%%' AND Link IS NOT NULL GROUP BY scores) scoreS01Amanda ON scoreS01Amanda.scores = Score_S01
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Chris, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S01%%' AND Link IS NOT NULL GROUP BY scores) scoreS01Chris ON scoreS01Chris.scores = Score_S01
WHERE Score_S01 IS NOT NULL
GROUP BY Score_S01
ORDER BY CASE WHEN Score_S01 IS 'Missing' THEN -10 ELSE Score_S01 END DESC)
), SG1_S02 AS (
SELECT 'Stargate SG1' AS Series_S02,
NULL AS Score_S02,
count(Richard) AS Richard_Count_S02,
round(sum(Richard)/count(Richard), 3) || ' - Average' AS Richard_Episodes_S02,
count(Michael) AS Michael_Count_S02,
round(sum(Michael)/count(Michael), 3) || ' - Average' AS Michael_Episodes_S02,
count(Amanda) AS Amanda_Count_S02,
round(sum(Amanda)/count(Amanda), 3) || ' - Average' AS Amanda_Episodes_S02,
count(Chris) AS Chris_Count_S02,
round(sum(Chris)/count(Chris), 3) || ' - Average' AS Chris_Episodes_S02
FROM "@Stargate_SG1"
	WHERE "Episode-Air" LIKE 'S02%%'
	GROUP BY 1

UNION ALL

SELECT * FROM (SELECT
'Stargate SG1' AS Series_S02,
Score_S02,
scoreS02Richard.Counts AS Richard_Count_S02,
scoreS02Richard.Title AS Richard_Episodes_S02,
scoreS02Michael.Counts AS Michael_Count_S02,
scoreS02Michael.Title AS Michael_Episodes_S02,
scoreS02Amanda.Counts AS Amanda_Count_S02,
scoreS02Amanda.Title AS Amanda_Episodes_S02,
scoreS02Chris.Counts AS Chris_Count_S02,
scoreS02Chris.Title AS Chris_Episodes_S02
FROM (SELECT ifnull(Richard, 'Missing') AS Score_S02 FROM "@Stargate_SG1" UNION SELECT ifnull(Michael, 'Missing') AS Score_S02 FROM "@Stargate_SG1" UNION SELECT ifnull(Amanda, 'Missing') AS Score_S02 FROM "@Stargate_SG1" UNION SELECT ifnull(Chris, 'Missing') AS Score_S02 FROM "@Stargate_SG1"
)
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Richard, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S02%%' AND Link IS NOT NULL GROUP BY scores) scoreS02Richard ON scoreS02Richard.scores = Score_S02
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Michael, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S02%%' AND Link IS NOT NULL GROUP BY scores) scoreS02Michael ON scoreS02Michael.scores = Score_S02
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Amanda, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S02%%' AND Link IS NOT NULL GROUP BY scores) scoreS02Amanda ON scoreS02Amanda.scores = Score_S02
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Chris, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S02%%' AND Link IS NOT NULL GROUP BY scores) scoreS02Chris ON scoreS02Chris.scores = Score_S02
WHERE Score_S02 IS NOT NULL
GROUP BY Score_S02
ORDER BY CASE WHEN Score_S02 IS 'Missing' THEN -10 ELSE Score_S02 END DESC)
), SG1_S03 AS (
SELECT 'Stargate SG1' AS Series_S03,
NULL AS Score_S03,
count(Richard) AS Richard_Count_S03,
round(sum(Richard)/count(Richard), 3) || ' - Average' AS Richard_Episodes_S03,
count(Michael) AS Michael_Count_S03,
round(sum(Michael)/count(Michael), 3) || ' - Average' AS Michael_Episodes_S03,
count(Amanda) AS Amanda_Count_S03,
round(sum(Amanda)/count(Amanda), 3) || ' - Average' AS Amanda_Episodes_S03,
count(Chris) AS Chris_Count_S03,
round(sum(Chris)/count(Chris), 3) || ' - Average' AS Chris_Episodes_S03
FROM "@Stargate_SG1"
	WHERE "Episode-Air" LIKE 'S03%%'
	GROUP BY 1

UNION ALL

SELECT * FROM (SELECT
'Stargate SG1' AS Series_S03,
Score_S03,
scoreS03Richard.Counts AS Richard_Count_S03,
scoreS03Richard.Title AS Richard_Episodes_S03,
scoreS03Michael.Counts AS Michael_Count_S03,
scoreS03Michael.Title AS Michael_Episodes_S03,
scoreS03Amanda.Counts AS Amanda_Count_S03,
scoreS03Amanda.Title AS Amanda_Episodes_S03,
scoreS03Chris.Counts AS Chris_Count_S03,
scoreS03Chris.Title AS Chris_Episodes_S03
FROM (SELECT ifnull(Richard, 'Missing') AS Score_S03 FROM "@Stargate_SG1" UNION SELECT ifnull(Michael, 'Missing') AS Score_S03 FROM "@Stargate_SG1" UNION SELECT ifnull(Amanda, 'Missing') AS Score_S03 FROM "@Stargate_SG1" UNION SELECT ifnull(Chris, 'Missing') AS Score_S03 FROM "@Stargate_SG1"
)
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Richard, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S03%%' AND Link IS NOT NULL GROUP BY scores) scoreS03Richard ON scoreS03Richard.scores = Score_S03
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Michael, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S03%%' AND Link IS NOT NULL GROUP BY scores) scoreS03Michael ON scoreS03Michael.scores = Score_S03
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Amanda, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S03%%' AND Link IS NOT NULL GROUP BY scores) scoreS03Amanda ON scoreS03Amanda.scores = Score_S03
	LEFT JOIN (SELECT group_concat('"' || Title || '"', ', ') AS Title, count(Title) AS Counts, ifnull(Chris, 'Missing') AS scores FROM "@Stargate_SG1" WHERE "Episode-Air" LIKE 'S03%%' AND Link IS NOT NULL GROUP BY scores) scoreS03Chris ON scoreS03Chris.scores = Score_S03
WHERE Score_S03 IS NOT NULL
GROUP BY Score_S03
ORDER BY CASE WHEN Score_S03 IS 'Missing' THEN -10 ELSE Score_S03 END DESC)
)

SELECT
SG1.*,
SG1_S01.*,
SG1_S02.*,
SG1_S03.*
FROM SG1
	LEFT JOIN SG1_S01 ON ifnull(SG1_S01.Score_S01, 'avg') = ifnull(SG1.Score, 'avg')
	LEFT JOIN SG1_S02 ON ifnull(SG1_S02.Score_S02, 'avg') = ifnull(SG1.Score, 'avg')
	LEFT JOIN SG1_S03 ON ifnull(SG1_S03.Score_S03, 'avg') = ifnull(SG1.Score, 'avg')