#	Series Scoring SQLite Database and R-Shiny Applet

##	Introduction

Some time ago I learned of [The Landing Party](https://www.youtube.com/@gnolan12/streams), a weekly stream on Graham Nolan's YouTube channel, where he and co-hosts go through *Star Trek* in its production order and provide scores for the episodes at the end.
At the time I decided to build a database to track these scores, for the fun of tracking that data and to develop my SQL skills.
The project has grown since then.

The database now features various Views and Triggers to automate some of the processes and calculationes, and I have developed an **R-Shiny** applet to handle more advanced processing.
**R** is a statistics software that is able to interface with SQL databases while **Shiny** is a library for creating convenient UIs that run in browsers for interactivity.

If anyone else wants to track episode-by-episode scores for some show or franchise, I hope what I have built can help them.
I have done my best to make these tools as generalized as possible, so it should not be necessary to do more than build the initial tables in the database that track the scores.
Some knowledge of SQL(ite) will help with that, but no knowledge of R or Shiny should be necessary; it should "just work" when you run the script.

For my own ease, I am going to assume some level of SQL(lite) understanding as I go over the contents of the repository and the set up process.

I must mention there is a second, "SQL-Pivots" branch to this repository that I do not recommend using, though it may still prove interesting to some.
SQL does not have innate support for what are called pivot tables, which are used to group data according to their values across columns in a table.
I was able to build SQL queries to create pivot tables, and while they do function, the code itself is quite cumbersome.
R, however, can build these very easily and in a far more abstract way, so I have removed the SQL-pivot code from this branch.

### Software used:
- [DB Browser for SQLite](https://sqlitebrowser.org/)
- [R](https://www.r-project.org/)
	- [Shiny](https://shiny.posit.co/r/getstarted/)
	- [DBI](https://dbi.r-dbi.org/)
	- [stringr](https://stringr.tidyverse.org/)
	- [dplyr](https://dplyr.tidyverse.org/)
	- [tidyr](https://tidyr.tidyverse.org/)
	- [ggplot2](https://ggplot2.tidyverse.org/)
	- [purrr](https://purrr.tidyverse.org/)
	- [tibble](https://tibble.tidyverse.org/)

Both **DB Browser for SQLite** and **R** are free and open source, and this is true of R's packages also listed.

R does not come with these packages already installed, but the applet will check for their presence at the start and install them, if necessary.
The installation process may require some manual input, such as to select the specific repository to download the packages from.
For this reason I would recommend first running "app_Series-Scoring.r" by copying its contents into the R GUI.
Once the packages are installed, you should be able to run the R script directly, which opens the script in a console window.
When a Shiny applet is launched, it will open a tab in your system's default browser, regardless of it running from the GUI or the console window.

---

## Step 1: Initial Database Configuration

Preparing the database is the first and most important step, and everything else has been constructed around the tables in the database being formatted correctly.
I might be bias, but I do believe the formats are logical, so the only difficulties should be the creation of the tables and then filling them in.
To that end, I have prepared **Example Code.sql** with references to the Stargate franchise.
Why Stargate instead of Star Trek, like what The Landing Party is going through?
Because I already built that database and Stargate is also a multi-series franchise.

There are a couple ways to construct a table in **DB Browser for SQLite**; the "Create Table" button and running the appropriate SQL command in the "Execute SQL" tab.
I am going to explain the latter because the information from it is easily interpreted for the "Create Table" button.

Here is an example of the code that might be run to create a table for *Stargate SG-1*:

```
CREATE TABLE "@Stargate_SG1" (
	"Episode Air"	TEXT,
	"Episode DVD"	TEXT,
	"Title"	TEXT,
	"Air Date" TEXT,
	"Link"	TEXT,
	"Richard"	NUMERIC,
	"Michael"	NUMERIC,
	"Amanda"	NUMERIC,
	"Chris"	NUMERIC,
	"IMDB_rating" NUMERIC
);
```

Starting with the first line, it tells SQL to create a table named "@Stargate_SG1."
The `@` character is used to mark those SQL objects containing series information in them, and it is important for the table names to not contain spaces, which is why the underscore (`_`) is used.
The name you use here is stored elsewhere, so what you choose here is important, but I will explain that a little later.

Between the parantheses are the specifications for the columns to be created, which start with their name, in quotes, and their type.
The column type determines what kind of data it can hold, so these "TEXT" columns can only hold text while the "NUMERIC" columns can hold any number.
Occasionally an "INTEGER" column might also be used, which I shall explain shortly.

The first two columns in this example are "Episode Air" and "Episode DVD" and both should use `SXXEYY` formatting, with "XX" and "YY" being integers.
If you wish a separator between the `SXX` and `EYY` portion, you may place one, but the general formatting should remain the same.
Some series have different episode orders between how they aired and how they appear on the DVD or other release.
While this is not always the case, the cause of the re-ordering can be to address continuity issues, such as if episode 10 makes a reference to episode 9 but was aired first because it was ready earlier.

If you can find the production order, it can be stored in an "Episode Production" column and here the "INTEGER" column type makes sense, as it will just be a sequence of integers counting up.

You may have noticed these column names include spaces, which is discouraged as a rule, but the R-Shiny applet is able to work with them just the same.
If you wish to replace the spaces you may as the R script checks for "Episode" being in the name, so as long as that is present and some other text, such as "Air" or "DVD," it will work.
I find [TheTVDB](https://thetvdb.com/) to be a good source for a series aired order and, usually, the DVD order.
Production order is harder to find.

One episode order column is necessary, whatever the specific name is.

The next column specification is "Title," which I doubt needs any special explanation, other than to say it is necessary.

The "Air Date" column is optional, but easy enough to find that I have made it a regular column.
If you wish to skip it, do so as the R-Shiny applet recognizes it as just extra information you can toggle on and off freely.

The "Link" column is not optional as the R script uses it to check if scores have been provided.
The script will be happy if it contains anything, not just a valid URL, but having it start with "http" will activate a SQL Trigger I will explain later.

After the "Link" column are five "NUMERIC" columns, each intended to hold a score.
The first four are host scores, and for this example I am using the names of actors from *Stargate SG1* but any names will do.
The fifth column is different, as it is not to hold the score from a host, but another source; IMDB in this case.
To distinguish host and non-host scores, place `_Rating` at the end of the name.
The R script will track these columns differently so they do not impact any calculations made, but are still available to be shown.

To create this same table with DB Browser's "Create Table" GUI, hit the button, enter the intended table name at the top, and add columns with the "Add" button.
Give them the appropriate name and type, and hit the "OK" button when done.

Below is a more generalized form of the SQL command I gave above.
Change the names and add or remove columns as you wish, but remember you will need columns for "Episode order" ("order" can be some other word", "Title", "Link", and at least one "NUMERIC" column for host scores.
Only "Air Date" and "other_Rating" columns are completely optional for the R script.

```
CREATE TABLE "@series" (
	"Episode order"	TEXT,
	"Title"	TEXT,
	"Air Date"	TEXT,
	"Link"	TEXT,
	"hostA"	NUMERIC,
	"hostB"	NUMERIC,
	"hostC"	NUMERIC,
	"other_Rating"	NUMERIC
)
```

I think it is important to mention it is quite easy to add columns after the table is created.
This means you can build up the table with just the episode information and add the hosts later.
For fun and practice, I have built a number of databases like this.

There are two ways to add a column to a table, with the first being to modify the table within DB Browser.
You just need to right-click on it in the list of tables under the "Database Structure" tab and select "Modify Table," but this does not always work.
DB Browser will throw an error if the table is connected to some other object, even though the addition of a column should not disrupt this connection.
The second means to add a column does not run into this problem though.

```
ALTER TABLE "@Stargate_SG1" ADD COLUMN "hostD" NUMERIC;
```

Use whatever the appropriate column name is, in place of "hostD," and this should add the column without issue.
(At least I have yet to see DB Browser complain when I add a column this way.)

This is not the only table you must create, but this next one is more constrained and so is a little easier to explain.

The "_Order_Series" table is necessary even when the database is for a single series, because it is used to check against.
You can just run the code below to create it, rather than going through the "Create Table" UI.

```
CREATE TABLE "_Order_Series" (
	"sort"	INTEGER UNIQUE,
	"name"	TEXT UNIQUE,
	"abbr"	TEXT,
	PRIMARY KEY("sort" AUTOINCREMENT)
);
```

Exactly what this table must contain I will explain in the next chapter.

There is one more table that can be added, "Stream_Notes," but it is optional and relates to something something else I will discuss later.

##	Step 2: Data Entry

For data entry you have options, depending on your technical experience and comfort with SQL.

The first option is to use the button in DB Browser to insert a new row; it looks like a table with a green bar across it.
This will add a single row to the current table that you can then write into like a spreadsheet.

The second option is to construct SQL commands to insert the data into the table.
For those comfortable writing out commands in a text editor, this may prove more efficient.
An "INSERT" command looks like this:

```
INSERT INTO "@Stargate_SG1" ("Episode Air", "Episode Date", "Title", "Air Date") VALUES
("S01E01-02", "S01E01", "Rising," "2004-07-16"),
("S01E03", "S01E02", "Hide and Seek," "2004-07-23"),
("S01E04", "S01E03", "Thirty-Eight Minutes," "2004-07-30")
```

After declaring the table, a list of the columns you wish to insert the data into is provided, then "VALUES", and lastly the data itself.
As these are all TEXT columns and to ensure the complete text inserted into the appropriate places, I have quoted the entries.
Each row is then within its own parantheses.

A third option is to build the table in a spreadsheet first, save it to a CSV file, and then import that.
Databases have definite advantages over spreadsheets, but moving cell contents around in a spreadsheet can be easier, and you will not need to constantly add rows.
The catch is you must make sure the spreadsheet is formatted correctly, according to what I described above.

However you enter the data, editing cells here is as easy as editing them in a spreadsheet.

With the "how" covered, we can get to the "what" for the "_Order_Series" table.
Because of how small this table will be, just using the "Insert Row" button is the most appropriate way to add data to it.

This table exists for a couple purposes, with the first being to provide the correct order to display the series in, as they are not always alphabetical.
The "sort" column was set to automatically increment itself as you add rows, so it is doubtful that will need to be changed, but you can edit it, if you wish.
Just be aware, it expects every value to be unique if you find you need to edit the values.

The "name" and "abbr" (for abbreviation) columns must be entered by hand and both are necessary.
The "abbr" value is not very visible while the "name" column is used by R-Shiny when identifying the tables.
For this reason, it cannot be just anything but must relate to the name given to the tables made earlier.

Specifically, the "name" column is to be the table name with the `@` removed and underscores replaced with spaces.
The R script does these two things, and removes any colons, to then connect these rows in "_Order_Series" with the tables.
If the names do not match after this conversion, the table will not be listed by the R-Shiny applet.

---
Work in progress
