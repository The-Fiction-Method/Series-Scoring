CREATE TABLE "@Stargate_SG1" (
	"Episode-Air"	TEXT,
	"Title"	TEXT,
	"Richard"	NUMERIC,
	"Michael"	NUMERIC,
	"Amanda"	NUMERIC,
	"Chris"	NUMERIC,
	"Link"	TEXT,
	"IMDB_rating" NUMERIC
);


CREATE TABLE "@Stargate_Atlantis" (
	"Episode-Air"	TEXT,
	"Title"	TEXT,
	"Michael"	NUMERIC,
	"Amanda"	NUMERIC,
	"Joe"	NUMERIC,
	"David"	NUMERIC,
	"Link"	TEXT,
	"IMDB_rating" NUMERIC
);

CREATE TABLE "@Stargate_Universe" (
	"Episode-Air"	TEXT,
	"Title"	TEXT,
	"Amanda"	NUMERIC,
	"David"	NUMERIC,
	"Link"	TEXT,
	"IMDB_rating" NUMERIC
);

CREATE TABLE "_Order_Series" (
	"sort"	INTEGER UNIQUE,
	"name"	TEXT UNIQUE,
	"abbr"	TEXT,
	PRIMARY KEY("sort" AUTOINCREMENT)
);

--

INSERT INTO "_Order_Series" ("name", "abbr") VALUES
('Stargate SG1', 'SG1'),
('Stargate Atlantis', 'SGA'),
('Stargate Universe', 'SGU')
;

INSERT INTO "@Stargate_SG1" VALUES
('S01E01-E02', 'Children of the Gods', '8', '7.5', '6', '8', NULL, '7.8'),
('S01E03', 'The Enemy Within', '8', '6.5', '7.5', '9', NULL, '7.6'),
('S01E04', 'Emancipation', '8', '7', '9', '7', NULL, '5.9')
;

INSERT INTO "@Stargate_Atlantis" VALUES
('S01E01-E02', 'Rising', '9', '8', '9', '10', NULL, '8.3'),
('S01E03', 'Hide and Seek', '6', '7', '9', '10', NULL, '7.4'),
('S01E04', 'Thirty-Eight Minutes', '6', '6.5', '10', '8', NULL, '7.6')
;
INSERT INTO "@Stargate_Universe" VALUES
('S01E01-E03', 'Air', '8', '6', NULL, '7.4'),
('S01E04', 'Darkness', '6', '5.5', NULL, '7.2'),
('S01E05', 'Light', '8.5', '8', NULL, '7.8')
;