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
Still in progress
