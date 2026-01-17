CREATE TABLE "@Stargate_SG1" (
	"Episode Air"	TEXT,
	"Title"	TEXT,
	"Air Date" TEXT,
	"Link"	TEXT,
	"Richard"	NUMERIC,
	"Michael"	NUMERIC,
	"Amanda"	NUMERIC,
	"Chris"	NUMERIC,
	"IMDB_rating" NUMERIC
);

CREATE TABLE "@Stargate_Atlantis" (
	"Episode Air"	TEXT,
	"Title"	TEXT,
	"Air Date" TEXT,
	"Link"	TEXT,
	"Michael"	NUMERIC,
	"Amanda"	NUMERIC,
	"Joe"	NUMERIC,
	"David"	NUMERIC,
	"IMDB_rating" NUMERIC
);

CREATE TABLE "@Stargate_Universe" (
	"Episode Air"	TEXT,
	"Title"	TEXT,
	"Air Date" TEXT,
	"Link"	TEXT,
	"Amanda"	NUMERIC,
	"David"	NUMERIC,
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
('S01E01-E02', 'Children of the Gods', '1997-07-27', NULL, '8', '7.5', '6', '8', '7.8'),
('S01E03', 'The Enemy Within', '1997-08-01', NULL, '8', '6.5', '7.5', '9', '7.6'),
('S01E04', 'Emancipation', '1997-08-08', NULL, '8', '7', '9', '7', '5.9')
;

INSERT INTO "@Stargate_Atlantis" VALUES
('S01E01-E02', 'Rising', '2004-07-16', NULL, '9', '8', '9', '10', '8.3'),
('S01E03', 'Hide and Seek', '2004-07-23', NULL, '6', '7', '9', '10', '7.4'),
('S01E04', 'Thirty-Eight Minutes', '2004-07-30', NULL, '6', '6.5', '10', '8', '7.6')
;

INSERT INTO "@Stargate_Universe" VALUES
('S01E01-E03', 'Air', '2009-10-02', NULL '8', '6', '7.4'),
('S01E04', 'Darkness', '2009-10-16', NULL '6', '5.5', '7.2'),
('S01E05', 'Light', '2009-10-23', NULL '8.5', '8', '7.8')
;