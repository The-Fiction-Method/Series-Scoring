if (!require(shiny))	install.packages("shiny");		library(shiny)

if (!require(DBI))		install.packages("DBI");		library(DBI)
if (!require(stringr))	install.packages("stringr");	library(stringr)
if (!require(dplyr))	install.packages("dplyr");		library(dplyr)
if (!require(dplyr))	install.packages("dplyr");		library(dplyr)
if (!require(tidyr))	install.packages("tidyr");		library(tidyr)
if (!require(ggplot2))	install.packages("ggplot2");	library(ggplot2)

if (interactive())	{
	# setwd("CURRENT_DIR")
	#	set the directory this script and the database are within
}

DATA	<-	new.env()
TABLES	<-	new.env()
GRAPH	<-	new.env()
FILES	<-	list.files(pattern = "*.db")
DATA$Default	<-	"Series Scoring.db"
DATA$TABS	<-	NULL
DATA$TABLE	<-	NULL
GRAPH$graphHEIGHT	<-	900
GRAPH$graphRES		<-	90

dataLOADtabs	=	function(name = DATA$Default)	{
	con	<-	dbConnect(
		drv		=	RSQLite::SQLite(),
		# host	=	"127.0.0.1",
		# port	=	3306,
		dbname	=	name
	)
	DATA$ORDER	<-	dbReadTable(con, "_Order_Series")	|>	mutate(	TABS	=	paste0('@', str_replace_all(name, ' ', '_')))
	TABS	<-	dbListTables(con)[dbListTables(con)	|>	str_starts('@')]
	DATA$TABS	<-	(data.frame(TABS) |> inner_join(DATA$ORDER))	|>	arrange(sort)

	dbDisconnect(con)
}

dataLOAD	=	function(name = DATA$Default, TAB)	{
	con	<-	dbConnect(
		drv		=	RSQLite::SQLite(),
		# host	=	"127.0.0.1",
		# port	=	3306,
		dbname	=	name
	)
	hold	<-	dbReadTable(con, TAB)	|>	tibble()
	dbDisconnect(con)
	hold	<-	hold	|>	mutate(	Episode.Air	=	as.factor(Episode.Air)	)
	if ( hold |> names() |> str_detect("Production") |> any() )	{
		hold	<-	hold	|>	mutate(Episode.Production	=	as.ordered(Episode.Production))
		DATA$exiPRD	<-	TRUE
	}	else	{
		hold	<-	hold	|>	bind_cols(Episode.Production	=	ordered(NA))
		DATA$exiPRD	<-	FALSE
	}

	DATA$TABLE	<-	hold

	#	Hosts
	DATA$colHOST	<-	hold	|>	select(where(is.numeric) & !contains("Rating"))	|>	names()
	#	Episode Info
	DATA$colEPIN	<-	hold	|>	select(where(is.factor) | matches("Title"))	|>	names()
	#	Extra Columns
	DATA$colEXTR	<-	hold	|>	select((where(is.character) | contains("Rating")) & !(matches("Title") | matches("Link")))	|>	names()

	COUNTepisodes	<-	sum(!is.na(hold$Link))
	DATA$COUNThosts	<-	(hold	|>	summarize(across(where(is.numeric), ~ sum(!is.na(.))))	>=	COUNTepisodes/10)	|>	data.frame()
	DATA$COUNThosts$IMDB_Rating	<-	FALSE
	GRAPH$hostPAL	<-	scales:::pal_hue()(length(DATA$COUNThosts))

	DATA$SEASONS	<-	hold$Episode.Air	|>	str_sub(1, 3)	|>	unique()
}

tableSTATS	<-	function(IN, SEL)	{
	DATA$colSTAT	<-	c("Mean", "StDev", "Median", "MAD")
	IN	|>	rowwise()	|>	mutate(
		Mean	=	mean(	c_across(SEL),	na.rm = TRUE),
		StDev	=	sd(		c_across(SEL),	na.rm = TRUE),
		Median	=	median(	c_across(SEL),	na.rm = TRUE),
		MAD		=	mad(	c_across(SEL),	na.rm = TRUE)
	)
}

tableFORM	<-	function(IN, selORDER = NULL)	{
	out	<-	IN	|>
		# relocate(where(is.factor), Title, where(is.numeric), everything())	|>
		mutate(across(DATA$colHOST, function(x) {paste0(x) |> str_remove('NA')}))	|>
		mutate(across(DATA$colEXTR, function(x) {paste0(x) |> str_remove('NA')}))
		# mutate(across(contains("IMDB"), function(x) {paste0(x) |> str_remove('NA')}))	|>
		#	alternative that only applies to IMDB columns, but using colEXTR should be more reliable

	if (isTruthy(selORDER))	out	<-	out	|>	arrange(.data[[selORDER]])	|>
		rename(Production = Episode.Production)
	out	|>	rename(Air = Episode.Air)
}

tableLONG	<-	function(IN)	{
	IN	|>	filter(!is.na(Link)) |>
		select(!where(is.character) | Title)	|>
		pivot_longer(names(select(IN, where(is.numeric))), names_to = "Host", values_to = "Score") |>
		mutate(
			Host	=	factor(Host, levels = names(select(IN, where(is.numeric)))),
			Season	=	str_sub(Episode.Air, 1, 3)
		)
}

ranksHOST	<-	function(IN, COL = ', ')	{
	IN	|>
		group_by(Score, Host, .add = TRUE)	|>
		reframe(
			Count	=	n(),
			Titles	=	paste(paste0('"', Title, '"'), collapse = COL)
			)	|>
		arrange(desc(Score))
}

ranksSUMM	<-	function(IN, ROUND = 2)	{
	bind_rows(
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	mean(Score, na.rm = T)	|>	round(ROUND),
			Titles = 'Mean')	|>	relocate(Score, .before = Count),
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	sd(Score, na.rm = T)	|>	round(ROUND),
			Titles = 'StDev')	|>	relocate(Score, .before = Count),
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	median(Score, na.rm = T)	|>	round(ROUND),
			Titles = 'Median')	|>	relocate(Score, .before = Count),
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	mad(Score, na.rm = T)	|>	round(ROUND),
			Titles = 'MAD')	|>	relocate(Score, .before = Count),
		IN	|>	ranksHOST()	|>	select(!Host)
	)
}

graphHIST	<-	function(IN)	{
	ggplot(IN) +
		stat_bin(aes(y = Score, fill = Host), color = "black", breaks = (0:20)/2) +
		scale_y_continuous(limits = c(0, 10),	expand = c(0.02, 0),	minor_breaks = NULL,
			name	=	"Score",
			breaks	=	(0:20) / 2 - 0.25,
			labels	=	(0:20) / 2
		) +
		scale_x_continuous(limits = c(0, NA), expand = c(0.02, 0),
			name	=	"Count",
			breaks	=	(1:200) * 2
		) +
		facet_wrap(vars(Host), axes = "all") +
		scale_fill_discrete(breaks = DATA$HOSTS, palette = GRAPH$hostPAL) +
		theme(legend.position = "none", plot.title.position = "plot")
}

graphPLOT	<-	function(IN)	{
	SUMM	<-	IN	|>	group_by(Host, Season = str_sub(Episode.Air, 1, 3))	|>	summarize(Mean = mean(Score, na.rm = T), SD = sd(Score, na.rm = T), MAD = mad(Score, na.rm = T))

	ggplot(IN, aes(x = Order, group = 1)) +
		geom_line(aes(y = Score, color = Host), linewidth = 1, na.rm = T) +
		facet_grid(rows = vars(Host), cols = vars(Season),	switch = "y",	scales = "free_x") +
		scale_y_continuous(limits = c(0, 10),	expand = c(0.02, 0),	minor_breaks	=	NULL,
			name	=	"Score",
			breaks	=	(0:20) / 2,
			labels	=	(0:20) / 2
		) +
		scale_x_discrete(minor_breaks	=	NULL,
			labels = function(breaks){paste0(rep(c("", "\n"), length.out = length(breaks)), str_remove(breaks, "S\\d+:"))}
		) +
		scale_color_discrete(breaks = DATA$HOSTS, palette = GRAPH$hostPAL) +
		theme(legend.position = "none", plot.title.position = "plot") +
		geom_smooth(aes(y = Score, group = str_sub(Episode.Air, 1, 3))) +
		geom_segment(data = SUMM, aes(y = Mean, group = Season, color = Host, x = -Inf, xend = Inf), linewidth = 1) +
		geom_label(data = SUMM, aes(x = -Inf, y = -Inf, label = paste0('Mean: ', round(Mean, 2)), group = Host, hjust = 0, vjust = 0, color = Host))
}



server <- function(input, output, session) {
	# output$Title	=	renderUI({	titlePanel("Series Scoring")	})

	observeEvent(input$dataDBload,	{
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"tables", target = seas)	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"plots", target = seas)	})
		lapply(DATA$colHOST, function(host)	{	removeTab(inputId	=	"ranks", target = host)	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"histograms", target = seas)	})

		dataLOADtabs(input$dataDB)
		updateSelectInput(inputId = "dataTAB",	choices	=	setNames(DATA$TABS$TABS,	DATA$TABS$name)	)
	},	priority	=	10)

	observeEvent(input$dataTABload,	{
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"tables",		target = seas)	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"plots",		target = seas)	})
		lapply(DATA$colHOST, function(host)	{	removeTab(inputId	=	"ranks",		target = host)	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"histograms",	target = seas)	})

		dataLOAD(input$dataDB, input$dataTAB)

		output$ordCON	<-	renderUI({
			if (DATA$exiPRD)	{
				radioButtons(inputId	=	"tableORDER",	label	=	"Ordering",	inline	=	TRUE,
					choiceNames	=	c("Production", "Air"),	choiceValues	=	c("Episode.Production", "Episode.Air"),
					selected	=	"Episode.Production"
				)
			}	else	{	NULL	}
		})

		if (!DATA$exiPRD)	{
			updateRadioButtons(inputId = "tableORDER", label = '', choices = '', selected = '')
		}

		updateCheckboxGroupInput(inputId = "dataHOSTS",		choices	= DATA$colHOST,	selected = names(DATA$COUNThosts)[unlist(DATA$COUNThosts)]	)

		updateCheckboxGroupInput(inputId = "dataEXTRAS",		inline = TRUE,	choices	= DATA$colEXTR	)
		updateCheckboxGroupInput(inputId = "dataEXTRASplot",	inline = TRUE,	choices	= DATA$colEXTR[str_detect(DATA$colEXTR, "Rating")]	)
		if (is.null(DATA$colEXTR))	{
			updateCheckboxGroupInput(inputId = "dataEXTRAS",		label = '')
			updateCheckboxGroupInput(inputId = "dataEXTRASplot",	label = '')
		}

		# output$clickHISTverb	<-	renderPrint({	input$clickHIST$y + 0.25	})
	},	priority	=	10)

	observeEvent(list(input$dataHOSTS, input$dataTABload), {
		DATA$tabFORM	<-	DATA$TABLE	|>	tableSTATS(input$dataHOSTS)
		DATA$tabLONG	<-	DATA$TABLE	|>	tableLONG()
	},	priority = 5,	ignoreInit = TRUE)

	#	this will set a default to input$tableORDER so databases without Production order still work
	TABLES$tableORD	<-	reactive({
		if (isTruthy(input$tableORDER))	{return(input$tableORDER)}	else	{return("Episode.Air")}
	})	|>	bindEvent(input$dataTABload, input$tableORDER)

#	Tables
	TABLES$tableORG	<-	reactive({
		DATA$tabFORM	|>	select(!Link)	|>	tableFORM(selORDER = TABLES$tableORD())
	})	|>	bindEvent(input$dataTABload, input$dataHOSTS, TABLES$tableORD(), ignoreInit = TRUE)

	output$seriesTable	=	renderTable({
		TABLES$tableORG()	|>

			select(!any_of(DATA$colHOST) | any_of(input$dataHOSTS))	|>
			select(!any_of(DATA$colSTAT) | any_of(input$dataSTATS))	|>
			select(!any_of(DATA$colEXTR) | any_of(input$dataEXTRAS))	|>
			relocate(where(is.factor), Title, !where(is.numeric), where(is.numeric))
	},	digits = reactive(input$roundTerm), striped = TRUE, na='')

	observe({
		lapply(DATA$SEASONS, function(seas)	{appendTab(inputId = "tables", tab = tabPanel(seas	|>	str_replace('S', 'Season '),
			tagList(
				renderTable({
					TABLES$tableORG()	|>	filter(str_starts(Air, seas))	|>

					select(!any_of(DATA$colHOST) | any_of(input$dataHOSTS))	|>
					select(!any_of(DATA$colSTAT) | any_of(input$dataSTATS))	|>
					select(!any_of(DATA$colEXTR) | any_of(input$dataEXTRAS))	|>
					relocate(where(is.factor), Title, !where(is.numeric), where(is.numeric))
				},	digits = reactive(input$roundTerm), striped = TRUE, na='')
			),	value	=	seas	)
		)}	)
	})	|>	bindEvent(input$dataTABload, ignoreInit = TRUE)


#	Plots
	output$hostPLOT	<-	renderPlot({
		hold	<-	DATA$tabLONG	|>
				filter(Host %in% input$dataHOSTS | Host %in% input$dataEXTRASplot)
		if (DATA$exiPRD)	hold	<-	hold	|>	mutate(Order = .data[[TABLES$tableORD()]])	|>	arrange(Order)
		hold	|>	graphPLOT()
	},	height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES)

	observe({
		lapply(DATA$SEASONS, function(seas)	{appendTab(inputId = "plots", tab = tabPanel(seas	|>	str_replace('S', 'Season '),
			tagList(
				renderPlot({
					hold	<-	DATA$tabLONG	|>	filter(Season == seas)	|>
							filter(Host %in% input$dataHOSTS | Host %in% input$dataEXTRASplot)
					if (DATA$exiPRD)	hold	<-	hold	|>	mutate(Order = .data[[TABLES$tableORD()]])	|>	arrange(Order)
					hold	|>	graphPLOT()
				},	height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES),
			),	value	=	seas	)
		)}	)
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)

	observe({
		output$clickPLOT	<-	renderTable({
			hold	<-	DATA$tabLONG	|>	filter(!is.na(Score))

			if (DATA$exiPRD)	hold	<-	hold	|>	mutate(Order = .data[[TABLES$tableORD()]])	|>	arrange(Order) |>
				rename(Air = Episode.Air)
			if (DATA$exiPRD)	hold	<-	hold	|>	rename(Production = Episode.Production)

			hold	|>	nearPoints(input$clickPLOT, threshold = 10)	|>
				select(any_of(c("Host", "Score", "Production", "Air", "Title")))	|>	mutate(Score = paste0(Score))
		})
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)

#	Host Ranks
	observe({
		lapply(DATA$colHOST, function(host)	{appendTab(inputId = "ranks", tab = tabPanel(host,
			tagList(
				h4("All Seasons"),
				renderTable({
					DATA$tabLONG	|>	filter(Host == host)	|>
						ranksSUMM(input$roundTerm)	|>	mutate(Score = paste0(Score))	|>
					filter(!(Titles %in% c("Mean", "StDev", "Median", "MAD")) | (Titles %in% input$dataSTATS))	|>	mutate(Titles = str_replace(Titles, "StDev", "Standard Deviation"))

				}, striped = TRUE, na=''),
				lapply(DATA$SEASONS, function(seas)	{tagList(
					hr(style = "border-color: #333"),
					h4(str_replace(seas, 'S', 'Season ')),
					renderTable({
						out	<-	DATA$tabLONG	|>	filter(Host == host)	|>	filter(Season == seas)	|>
							ranksSUMM(input$roundTerm)

						if (!input$rankNA)	{out	<-	out	|>	filter(!is.na(Score))}
						out	|>	mutate(Score = paste0(Score))	|>
							filter(!(Titles %in% c("Mean", "StDev", "Median", "MAD")) | (Titles %in% input$dataSTATS))	|>	mutate(Titles = str_replace(Titles, "StDev", "Standard Deviation"))
				}, striped = TRUE, na='')
				)})
			)
		), select = DATA$colHOST[1] == host)}	)
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)


#	Histograms
	observe({
		output$hostHIST	<-	renderPlot({
			DATA$tabLONG	|>	filter(!is.na(Score))	|>
			filter(Host %in% input$dataHOSTS) |>
			graphHIST()
		},	height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES)
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)

	observe({
		lapply(DATA$SEASONS, function(seas)	{appendTab(inputId = "histograms", tab = tabPanel(seas	|>	str_replace('S', 'Season '),
			tagList(
				renderPlot({
					DATA$tabLONG	|>	filter(!is.na(Score))	|>	filter(Season == seas)	|>
					filter(Host %in% input$dataHOSTS) |>
					graphHIST()
				}, height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES)
			),	value	=	seas	)
		)}	)
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)

	observe({
		output$clickHIST	<-	renderTable({	req(input$clickHIST)
			roughHIST	<-	(input$clickHIST$y + 0.25)	|>	round(1)

			DATA$tabLONG	|>	group_by(Season)	|>
				ranksHOST()	|>
				filter(
					Score	|>	between(roughHIST - 0.25, roughHIST + 0.25),
					# Score	|>	near(input$clickHIST$y + 0.25, tol = 0.25),
					#	near does work, but it doesn't match the bins so best to not use
					Host	==	input$clickHIST$panelvar1,
					)	|>
				mutate(	Season	=	str_replace(Season, 'S', 'Season '),	Score	=	paste0(Score)	)	|>
				select(Host, Season, Score, Count, Titles)
		})
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)
}

tableORGui	<-	function(name, season = NULL)	{
	if (is.null(season))	season	<-	name	|>	str_replace('S', 'Season ')

	tabPanel(season,	tableOutput("seriesTable")	)
}

ui <- function(request)	{fluidPage(
	titlePanel("Series Scoring"),
	sidebarLayout(
		sidebarPanel(
			selectInput(inputId	=	"dataDB",	label	=	"Database to Load",	selectize	=	FALSE,
				choices	=	setNames(FILES,	gsub(".db", "", FILES)),	selected	=	DATA$Default
			),
			actionButton(inputId	=	"dataDBload",	label	=	"Load Selected Database"),
			selectInput(inputId	=	"dataTAB",	label	=	"Table to Load",	selectize	=	FALSE,
				choices	=	setNames(DATA$TABS$TABS,	DATA$TABS$name)
			),
			actionButton(inputId	=	"dataTABload",	label	=	"Load Selected Table"),
			checkboxGroupInput(inputId	=	"dataHOSTS",	label	=	"Hosts",
				choices	=	NULL
			),
			checkboxGroupInput(inputId	=	"dataSTATS",	label	=	"Statistics ",
				choiceNames	=	c("Mean", "Standard Deviation", "Median", "MAD"),	choiceValues	=	c("Mean", "StDev", "Median", "MAD"),
				selected	=	c("Mean", "StDev")
			),	helpText("MAD - Median Absolute Deviation"),
			numericInput(inputId = 'roundTerm',	label = "Round to",	value = 2),
			width	=	2
		),
		mainPanel(
			fluidRow(
				uiOutput('ordCON'),
				# radioButtons(inputId	=	"tableORDER",	label	=	"Ordering",	inline	=	TRUE,
					# choiceNames	=	c("Production", "Air"),	choiceValues	=	c("Episode.Production", "Episode.Air"),
					# selected	=	"Episode.Production"
				# )
			),
			tabsetPanel(
				tabPanel("Tables",
					tabsetPanel(id	=	"tables",
						tabPanel("All Seasons",	tableOutput("seriesTable"),	value = "all"),
						# tableORGui("all", "All Seasons"),
						# tableORGui('S01'),
						# tableORGui('S02'),
						# tableORGui('S03'),
						header	=	tagList(
							checkboxGroupInput(inputId	=	"dataEXTRAS",	label	=	"Extra Information",	inline = TRUE,
								choices	=	NULL
							),
						)
					)
				),
				tabPanel("Plots",
					tabsetPanel(id	=	"plots",
						tabPanel("All Seasons",
							plotOutput("hostPLOT",	height = GRAPH$graphHEIGHT, click = "clickPLOT"),
							tableOutput("clickPLOT"),
						),
						header	=	tagList(
							checkboxGroupInput(inputId = "dataEXTRASplot",	label = "Extra Scores",	inline = TRUE,	choices = NULL),
						),
					),
				),
				tabPanel("Host Ranks",
					tabsetPanel(id	=	"ranks",
						header	=	tagList(
							checkboxInput("rankNA",	label = "List Missing Episodes per Season")
						)
					)
				),
				tabPanel("Histograms",
					tabsetPanel(id	=	"histograms",
						tabPanel("All Seasons",
							plotOutput("hostHIST",	height = GRAPH$graphHEIGHT, click = "clickHIST"),
							tableOutput("clickHIST"),
						)
					),
				),
			),
			width	=	10
		)
	)
)	}


shinyApp(ui = ui, server = server)
