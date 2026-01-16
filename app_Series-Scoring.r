if (!require(shiny))	install.packages("shiny");		library(shiny)

if (!require(DBI))		install.packages("DBI");		library(DBI)
if (!require(stringr))	install.packages("stringr");	library(stringr)
if (!require(dplyr))	install.packages("dplyr");		library(dplyr)
if (!require(tidyr))	install.packages("tidyr");		library(tidyr)
if (!require(ggplot2))	install.packages("ggplot2");	library(ggplot2)
if (!require(purrr))	install.packages("purrr");		library(purrr)
if (!require(tibble))	install.packages("tibble");		library(tibble)

if (interactive())	{
	# setwd("CURRENT_DIR")
	#	place the path for the directory containing these files if working in the R GUI
}

DATA	<-	new.env()
TABLES	<-	new.env()
GRAPH	<-	new.env()
FILES	<-	list.files(pattern = "*.db")
DATA$Default	<-	"Series Scoring.db"
DBcontrol	<-	TRUE
countHOST	<-	7	#	based on this number, the host selection UI will move from nearer the top to the bottom of the UI
DATA$TABS	<-	NULL
DATA$TABLE	<-	NULL
GRAPH$graphHEIGHT	<-	900
GRAPH$graphRES		<-	90

str_SEAS	<-	function(IN)	str_extract(IN, "S\\d+")
str_SEASON	<-	function(IN)	paste0("Season ", str_extract(IN, "\\d+") |> as.numeric())

dataLOADtabs	<-	function(name = DATA$Default)	{
	con	<-	dbConnect(
		drv		=	RSQLite::SQLite(),
		# host	=	"127.0.0.1",
		# port	=	3306,
		dbname	=	name
	)
	DATA$ORDER	<-	dbReadTable(con, "_Order_Series")	|>	mutate(	TABS	=	paste0('@', str_remove_all(str_replace_all(name, ' ', '_'), ':')))
	TABS	<-	dbListTables(con)[dbListTables(con)	|>	str_starts('@')]
	DATA$TABS	<-	(data.frame(TABS) |> inner_join(DATA$ORDER))	|>	arrange(sort)

	dbDisconnect(con)
}
if (!DBcontrol)	dataLOADtabs(DATA$Default)

dataLOAD	<-	function(name = DATA$Default, TAB)	{
	con	<-	dbConnect(
		drv		=	RSQLite::SQLite(),
		# host	=	"127.0.0.1",
		# port	=	3306,
		dbname	=	name
	)
	hold	<-	dbReadTable(con, TAB, check.names = FALSE)	|>	tibble()
	dbDisconnect(con)
	hold	<-	hold	|> mutate(across(contains("Episode"), as.factor))

	DATA$TABLE	<-	hold

	#	Hosts
	DATA$colHOST	<-	hold	|>	select(where(is.numeric) & !contains("Rating"))	|>	names()
	#	Episode Info
	DATA$colEPIN	<-	hold	|>	select(where(is.factor) | matches("Title"))	|>	names()
	DATA$exiPRD		<-	length(DATA$colEPIN) > 2
	#	Extra Columns
	DATA$colEXTR	<-	hold	|>	select((where(is.character) | contains("Rating")) & !(matches("Title") | matches("Link")))	|>	names()

	COUNTepisodes	<-	sum(!is.na(hold$Link))
	DATA$COUNThosts	<-	(hold	|>	summarize(across(where(is.numeric), ~ sum(!is.na(.))))	>=	COUNTepisodes/10)	|>	data.frame(check.names = FALSE)
	DATA$COUNThosts$IMDB_Rating	<-	FALSE
	GRAPH$hostPAL	<-	scales:::pal_hue()(length(DATA$COUNThosts))

	DATA$SEASsel	<-	DATA$colEPIN[!(str_detect(DATA$colEPIN, "Production") | str_detect(DATA$colEPIN, "Title"))][1]
	DATA$SEASONS	<-	hold[[	DATA$SEASsel	]]	|>	str_SEAS()	|>	unique()
}


tableSTATS	<-	function(IN, SEL)	{
	DATA$colSTAT	<-	c("Mean", "StDev", "Median", "MAD")
	if (!isTruthy(SEL))	return(IN)
	IN	|>	rowwise()	|>	mutate(
		Mean	=	mean(	c_across(any_of(SEL)),	na.rm = TRUE),
		StDev	=	sd(		c_across(any_of(SEL)),	na.rm = TRUE),
		Median	=	median(	c_across(any_of(SEL)),	na.rm = TRUE),
		MAD		=	mad(	c_across(any_of(SEL)),	na.rm = TRUE)
	)
}

tableFORM	<-	function(IN, selORDER = NULL)	{
	out	<-	IN	|>
		# relocate(where(is.factor), Title, where(is.numeric), everything())	|>
		mutate(across(DATA$colHOST, function(x) {paste0(x) |> str_remove('NA')}))	|>
		mutate(across(DATA$colEXTR, function(x) {paste0(x) |> str_remove('NA')}))
		# mutate(across(contains("IMDB"), function(x) {paste0(x) |> str_remove('NA')}))	|>
		#	alternative that only applies to IMDB columns, but using colEXTR should be more reliable

	if (isTruthy(selORDER))	out	<-	out	|>	arrange(.data[[selORDER]])
	out	|>	rename_with(function(IN) str_remove(IN, "Episode."))
}

tableLONG	<-	function(IN)	{
	IN	|>	filter(!is.na(Link)) |>
		select(!where(is.character) | Title)	|>
		pivot_longer(names(select(IN, where(is.numeric))), names_to = "Host", values_to = "Score") |>
		mutate(
			Host	=	factor(Host, levels = names(select(IN, where(is.numeric)))),
			Season	=	str_SEAS(.data[[DATA$SEASsel]])
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

ranksSUMM	<-	function(IN)	{
	bind_rows(
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	mean(Score, na.rm = T),
			Titles	=	'Mean')		|>	relocate(Score, .before = Count),
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	sd(Score, na.rm = T),
			Titles	=	'StDev')	|>	relocate(Score, .before = Count),
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	median(Score, na.rm = T),
			Titles	=	'Median')	|>	relocate(Score, .before = Count),
		IN	|>	summarize(Count	=	sum(!is.na(Score)),
			Score	=	mad(Score, na.rm = T),
			Titles	=	'MAD')		|>	relocate(Score, .before = Count)
		# IN	|>	ranksHOST()	|>	select(!Host)
	)
}

tableDIFF	<-	function(IN)	{
	scoresALL	<-	IN	|>	select(where(is.numeric) |	contains("Rating"))	|>	names()
	pairs	<-	combn(scoresALL, 2)

	within(IN	|>	column_to_rownames("Title"),
		out	<-	map(1:ncol(pairs), \(x) IN[pairs[1, x]] - IN[pairs[2, x]])	|>
					as.data.frame()	|>	abs()	|>
					setNames(paste(pairs[1, ], pairs[2, ], sep="-"))
		)	|>	rownames_to_column("Title")	|>	select(DATA$colEPIN, out, Link)	|>	unnest(out)	|>
		mutate(Season	=	str_SEAS(.data[[DATA$SEASsel]]))
}

tableFORMdiff	<-	function(IN, selORDER = NULL)	{
	if (isTruthy(selORDER))	return(	IN	|>	arrange(.data[[selORDER]])	)
	return(IN)
}

tableDIFFstats	<-	function(IN, SEAS = FALSE)	{
	pairs	<-	IN	|>	select(where(is.numeric))	|>	names()

	hold	<-	IN
	if (SEAS)	{hold	<-	hold	|>	mutate(Season = "All")}

	hold	|>	group_by(Season)	|>
		reframe(	across(all_of(pairs),	list(
			"Mean"		=	\(x) mean(x, na.rm=T),
			"StDev"		=	\(x) sd(x, na.rm=T),
			"Median"	=	\(x) median(x, na.rm=T),
			"MAD"		=	\(x) mad(x, na.rm=T)
			)
		)	)	|>
		pivot_longer(cols = -1, names_to = c(".value", "Stat"), names_pattern = "(.*)_(Mean|StDev|Median|MAD)")	|>
		mutate(Season = ifelse(str_detect(Season, 'S\\d+'), str_SEAS(Season), Season))
}

ranksDIFF	<-	function(IN, SEAS = FALSE, COL = ', ')	{
	out	<-	IN	|>	filter(!is.na(Link)) |>
		select(!where(is.character) | Title)	|>
		pivot_longer(names(select(IN, where(is.numeric))), names_to = "Hosts", values_to = "Difference")	|>
		mutate(
				Host	=	factor(Hosts, levels = names(select(IN, where(is.numeric)))),
				Season	=	str_SEAS(.data[[DATA$SEASsel]])
			)	|>
		group_by(Difference, Hosts, .add = TRUE)

	if	(SEAS)	out	<- out	|>	group_by(Season, .add = TRUE)

	out	|>	reframe(	Count	=	n(),
						Titles	=	paste(paste0('"', Title, '"'), collapse = COL)	)
}


graphPLOT	<-	function(IN,	HOSTS	=	TRUE)	{
	SUMM	<-	IN	|>	group_by(Host, Season)	|>	summarize(Mean = mean(Score, na.rm = T))

	facetHOST	<-	list(
		facet_grid(rows = vars(Host), cols = vars(Season),	switch = "y",	scales = "free_x",	labeller = labeller(Season = function(IN) str_SEASON(IN))),
		geom_segment(data = SUMM, aes(y = Mean, group = Season, color = Host, x = -Inf, xend = Inf), linewidth = 1),
		geom_label(data = SUMM, aes(x = -Inf, y = -Inf, label = paste0('Mean: ', round(Mean, 2)), group = Host, hjust = 0, vjust = 0, color = Host))
	)

	out	<-	ggplot(IN, aes(x = Order, group = 1)) +
		geom_line(aes(y = Score, color = Host, group = Host), linewidth = 1, na.rm = T) +
		# facet_grid(rows = vars(Host), cols = vars(Season),	switch = "y",	scales = "free_x",
			# labeller	=	labeller(Season = function(IN) str_SEASON(IN))
		# ) +
		facet_grid(cols = vars(Season),	switch = "y",	scales = "free_x",
			labeller	=	labeller(Season = function(IN) str_SEASON(IN))
		) +
		scale_y_continuous(limits = DATA$scoreRANG,	expand = c(0.02, 0),	minor_breaks	=	NULL,
			name	=	"Score",
			breaks	=	seq(-10, 10, by = DATA$scoreSTEP),
			labels	=	seq(-10, 10, by = DATA$scoreSTEP)
		) +
		scale_x_discrete(minor_breaks	=	NULL,
			labels = function(breaks){paste0(rep(c("", "\n"), length.out = length(breaks)), str_remove(breaks, "S\\d+:"))}
		) +
		scale_color_discrete(breaks = DATA$HOSTS, palette = GRAPH$hostPAL) +
		theme(legend.position = "none", plot.title.position = "plot") +
		geom_smooth(aes(y = Score, group = str_SEAS(.data[[DATA$SEASsel]])))
		# geom_segment(data = SUMM, aes(y = Mean, group = Season, color = Host, x = -Inf, xend = Inf), linewidth = 1) +
		# geom_label(data = SUMM, aes(x = -Inf, y = -Inf, label = paste0('Mean: ', round(Mean, 2)), group = Host, hjust = 0, vjust = 0, color = Host))

	if (HOSTS)	return(out + facetHOST)
	out
}

graphHIST	<-	function(IN)	{
	ggplot(IN) +
		stat_bin(aes(y = Score, fill = Host), color = "black", breaks = seq(-10, 10, by = DATA$scoreSTEP)) +
		scale_y_continuous(limits = DATA$scoreRANG,	expand = c(0.02, 0),	minor_breaks = NULL,
			name	=	"Score",
			breaks	=	seq(-10, 10, by = DATA$scoreSTEP) - 0.25,
			labels	=	seq(-10, 10, by = DATA$scoreSTEP)
		) +
		scale_x_continuous(limits = c(0, NA), expand = c(0.02, 0),
			name	=	"Count",
			breaks	=	(1:200) * 2
		) +
		facet_wrap(vars(Host), axes = "all") +
		scale_fill_discrete(breaks = DATA$HOSTS, palette = GRAPH$hostPAL) +
		theme(legend.position = "none", plot.title.position = "plot")
}



server <- function(input, output, session) {
	output$Title	=	renderUI({	titlePanel("Series Scoring")	})

	DATA$dbinfo	<-	reactive({
		if (isTruthy(input$dataDB))	return(input$dataDB)
		DATA$Default
	})

	observeEvent(input$dataDBload,	{
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"tables",		target = seas)	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"plots",		target = seas)	})
		lapply(DATA$colHOST, function(host)	{	removeTab(inputId	=	"ranks",		target = host	|>	str_replace_all(fixed('.'), ' '))	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"histograms",	target = seas)	})
		removeTab(inputId	=	"tables",	target	=	"Links")

		dataLOADtabs(DATA$dbinfo())
		updateSelectInput(inputId = "dataTAB",	choices	=	setNames(DATA$TABS$TABS,	DATA$TABS$name)	)

		output$Title	=	renderUI({	titlePanel(paste0("Series Scoring - ", str_remove(DATA$dbinfo(), '.db')))	})
	},	priority	=	10)

	observeEvent(input$dataTABload,	{
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"tables",		target = seas)	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"plots",		target = seas)	})
		lapply(DATA$colHOST, function(host)	{	removeTab(inputId	=	"ranks",		target = host	|>	str_replace_all(fixed('.'), ' '))	})
		lapply(DATA$SEASONS, function(seas)	{	removeTab(inputId	=	"histograms",	target = seas)	})
		removeTab(inputId	=	"tables",	target	=	"Links")

		dataLOAD(DATA$dbinfo(), input$dataTAB)

		output$ordCON	<-	renderUI({
			if (DATA$exiPRD)	{
				radioButtons(inputId	=	"tableORDER",	label	=	"Ordering",	inline	=	TRUE,
					choiceNames		=	DATA$colEPIN[!(DATA$colEPIN %in% "Title")]	|>	str_remove("Episode."),
					choiceValues	=	DATA$colEPIN[!(DATA$colEPIN %in% "Title")],
					selected		=	DATA$colEPIN[!(DATA$colEPIN %in% "Title")][1]
				)
			}	else	{	NULL	}
		})

		if (!DATA$exiPRD)	{
			updateRadioButtons(inputId = "tableORDER", label = '', choices = '', selected = '')
		}

		updateCheckboxGroupInput(inputId = "dataHOSTS",		choices	= DATA$colHOST,	selected = names(DATA$COUNThosts)[unlist(DATA$COUNThosts)]	)
		output$hostSELtop	<-	renderUI({
			if (length(DATA$colHOST)	>	countHOST)	return(NULL)
			checkboxGroupInput(inputId	=	"dataHOSTS",	label	=	"Hosts",
				choices	= DATA$colHOST,	selected = names(DATA$COUNThosts)[unlist(DATA$COUNThosts)]	)
		})
		output$hostSELbot	<-	renderUI({
			if (length(DATA$colHOST)	<=	countHOST)	return(NULL)
			checkboxGroupInput(inputId	=	"dataHOSTS",	label	=	"Hosts",
				choices	= DATA$colHOST,	selected = names(DATA$COUNThosts)[unlist(DATA$COUNThosts)]	)
		})

		updateCheckboxGroupInput(inputId = "dataEXTRAS",	inline = TRUE,
			choiceValues	=	DATA$colEXTR,
			choiceNames		=	DATA$colEXTR	|>	str_replace_all(fixed('.'), ' ')
		)
		updateCheckboxGroupInput(inputId = "dataEXTRASplot",	inline = TRUE,	choices	= DATA$colEXTR[str_detect(DATA$colEXTR, "Rating")]	)
		if (is.null(DATA$colEXTR))	{
			updateCheckboxGroupInput(inputId = "dataEXTRAS",		label = '')
			updateCheckboxGroupInput(inputId = "dataEXTRASplot",	label = '')
		}

		updateCheckboxGroupInput(inputId = "diffSCORES",	inline = TRUE,
			choices		=	DATA$TABLE	|>	select(where(is.numeric) |	contains("Rating"))	|>	names(),
			selected	=	names(DATA$COUNThosts)[unlist(DATA$COUNThosts)]
		)
		updateCheckboxGroupInput(inputId = "diffSEASON",	inline = TRUE,
			choiceNames		=	DATA$SEASONS	|>	str_SEASON(),
			choiceValues	=	DATA$SEASONS	|>	str_SEAS(),
			selected	=	DATA$SEASONS	|>	str_SEAS()
		)
	},	priority	=	10)

	observeEvent(list(input$dataHOSTS, input$dataTABload), {
		DATA$tabFORM	<-	DATA$TABLE	|>	tableSTATS(input$dataHOSTS)
	},	priority = 5,	ignoreInit = TRUE)

	observeEvent(input$dataTABload, {
		DATA$tabLONG	<-	DATA$TABLE	|>	tableLONG()

		holdSCORE	<-	DATA$tabLONG	|>	filter(!str_detect(Host, "Rating"))	|>	select(Score)
		DATA$scoreSTEP	<-	ifelse(all(	holdSCORE == round(holdSCORE)	), 1, 0.5)
		updateSliderInput(inputId = "ranksRANGE",	step = DATA$scoreSTEP)

		DATA$scoreRANG	<-	holdSCORE	|>	rbind(0)	|>	range(na.rm = TRUE)#	|>	reactive()
		updateSliderInput(inputId = "ranksRANGE",	min = DATA$scoreRANG[1],	max = DATA$scoreRANG[2],	value = c(-Inf, Inf))

		updateSliderInput(inputId = "diffsRANGE",	step = DATA$scoreSTEP)
		updateSliderInput(inputId = "diffsRANGE",	max = DATA$scoreRANG[2],	value = c(-Inf, Inf))
	},	priority = 5,	ignoreInit = TRUE)

	#	this will set a default to input$tableORDER so databases without Production order still work
	TABLES$tableORD	<-	reactive({
		if (isTruthy(input$tableORDER))	{return(input$tableORDER)}	else	{return(DATA$colEPIN[!(DATA$colEPIN %in% "Title")][1])}
	})	|>	bindEvent(input$dataTABload, input$tableORDER)
	TABLES$tableORDseas	<-	reactive({
		if (isTruthy(input$tableORDER) & !str_detect(input$tableORDER, "Production"))	{return(input$tableORDER)}	else	{return(DATA$SEASsel)}
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
			relocate(where(is.factor), Title, !where(is.numeric), where(is.numeric))	|>
			rename_with(~ str_replace_all(.x, fixed('.'), ' '))
	},	digits = reactive(input$roundTerm), striped = TRUE, na='')

	observe({
		lapply(DATA$SEASONS, function(seas)	{appendTab(inputId = "tables", tab = tabPanel(seas	|>	str_SEASON(),
			tagList(
				renderTable({
					# TABLES$tableORG()	|>	filter(	str_starts(	.data[[	str_remove(TABLES$tableORD(), "Episode.")	]],	seas))	|>
					TABLES$tableORG()	|>	filter(	str_starts(	.data[[	str_remove(TABLES$tableORDseas(), "Episode.")	]],	seas))	|>

					select(!any_of(DATA$colHOST) | any_of(input$dataHOSTS))	|>
					select(!any_of(DATA$colSTAT) | any_of(input$dataSTATS))	|>
					select(!any_of(DATA$colEXTR) | any_of(input$dataEXTRAS))	|>
					relocate(where(is.factor), Title, !where(is.numeric), where(is.numeric))	|>
					rename_with(~ str_replace_all(.x, fixed('.'), ' '))
				},	digits = reactive(input$roundTerm), striped = TRUE, na='')
			),	value	=	seas	)
		)}	)

		appendTab(inputId = "tables", tab = tabPanel("Links",
			tagList(
				renderTable({
					DATA$tabFORM	|>	tableFORM(selORDER = TABLES$tableORD())	|>
					filter(!is.na(Link))	|>
					select(!any_of(DATA$colHOST))	|>	select(!any_of(DATA$colSTAT))	|>
					select(!any_of(DATA$colEXTR) | any_of(input$dataEXTRAS))	|>
					relocate(where(is.factor), Title, !where(is.numeric), where(is.numeric))	|>
					mutate(Link	=	paste0('<a href="', Link, '" target="_blank">', Link, '</a>'))
				},	sanitize.text.function = \(x) {x},	striped = TRUE, na='')
			)
		)	)
	})	|>	bindEvent(input$dataTABload, ignoreInit = TRUE)


#	Plots
	output$hostPLOT	<-	renderPlot({
		hold	<-	DATA$tabLONG	|>
				filter(Host %in% input$dataHOSTS | Host %in% input$dataEXTRASplot)
		if (DATA$exiPRD)	hold	<-	hold	|>	mutate(Order = .data[[TABLES$tableORD()]])	|>	arrange(Order)

		hold	|>	graphPLOT(input$dataHOSTSplot)
	},	height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES)	|>
	bindCache(input$dataTAB, input$dataHOSTS, input$dataEXTRASplot, TABLES$tableORD(), input$dataHOSTSplot)	|>
	bindEvent(input$dataTABload, input$dataHOSTS, input$dataEXTRASplot, TABLES$tableORD(), input$dataHOSTSplot)

	observe({
		lapply(DATA$SEASONS, function(seas)	{appendTab(inputId = "plots", tab = tabPanel(seas	|>	str_SEASON(),
			tagList(
				renderPlot({
					hold	<-	DATA$tabLONG	|>	filter(Season == seas)	|>
							filter(Host %in% input$dataHOSTS | Host %in% input$dataEXTRASplot)
					if (DATA$exiPRD)	hold	<-	hold	|>	mutate(Order = .data[[TABLES$tableORD()]])	|>	arrange(Order)
					hold	|>	graphPLOT(input$dataHOSTSplot)
				},	height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES),
			),	value	=	seas	)
		)}	)
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)

	observe({
		# output$clickPLOT	<-
		output$clickPLOTtall	<-	renderUI({	div(style = paste0("height: ", 35 * (length(DATA$colHOST) + 1), "px; overflow-y: auto;"),
			renderTable({
				hold	<-	DATA$tabLONG	|>	filter(!is.na(Score))

				if (DATA$exiPRD)	hold	<-	hold	|>	mutate(Order = .data[[TABLES$tableORD()]])	|>	arrange(Order)

				out	<-	hold	|>	nearPoints(input$clickPLOT, threshold = 10)
				if (nrow(out) == 0) return(NULL)

				out	|>	cbind("selected_" = TRUE)	|>	select(DATA$colEPIN, selected_)	|>
					right_join(hold)	|>	filter(selected_ & !str_detect(Host, "Rating"))	|>
					rename_with(function(IN) str_remove(IN, "Episode."))	|>
					select(any_of(c("Host", "Score")), (where(is.factor) & !any_of("Order")), Title)	|>	mutate(Score = paste0(Score))
				#	weird, but necessary to have it select across all hosts, not just the one clicked on

				# hold	|>	nearPoints(input$clickPLOT, threshold = 10)	|>	rename_with(function(IN) str_remove(IN, "Episode."))	|>
					# select(any_of(c("Host", "Score")), (where(is.factor) & !any_of("Order")), Title)	|>	mutate(Score = paste0(Score))
			})	)	})
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)

#	Host Ranks
	observe({	updateSliderInput(inputId = "ranksRANGE", value = c(-Inf, Inf))	})	|>	bindEvent(input$ranksRANGEres)

	observe({
		lapply(DATA$colHOST, function(host)	{appendTab(inputId = "ranks", tab = tabPanel(host	|>	str_replace_all(fixed('.'), ' '),
			tagList(
				h4("All Seasons"),
				renderTable({
					DATA$tabLONG	|>	filter(Host == host)	|>	ranksSUMM()	|>
						filter(Titles %in% input$dataSTATS)	|>	mutate(Titles = str_replace(Titles, "StDev", "Standard Deviation"))
				}, digits = reactive(input$roundTerm), striped = TRUE, na=''),
				renderTable({
					DATA$tabLONG	|>	filter(Host == host)	|>	ranksHOST()	|>	select(!Host)	|>	filter(between(Score, input$ranksRANGE[1], input$ranksRANGE[2]) | is.na(Score))	|>
						mutate(Score = paste0(Score))
				}, striped = TRUE, na=''),

				lapply(DATA$SEASONS, function(seas)	{tagList(
					hr(style = "border-color: #333"),
					h4(str_SEASON(seas)),
					renderTable({
						DATA$tabLONG	|>	filter(Host == host)	|>	filter(Season == seas)	|>
							ranksSUMM()	|>
							filter(Titles %in% input$dataSTATS)	|>	mutate(Titles = str_replace(Titles, "StDev", "Standard Deviation"))
					}, digits = reactive(input$roundTerm), striped = TRUE, na=''),
					renderTable({
						out	<-	DATA$tabLONG	|>	filter(Host == host)	|>	filter(Season == seas)	|>
							ranksHOST()	|>	select(!Host)	|>	filter(between(Score, input$ranksRANGE[1], input$ranksRANGE[2]) | is.na(Score))

						if (!input$rankNA)	{out	<-	out	|>	filter(!is.na(Score))}
						out	|>	mutate(Score = paste0(Score))
					}, striped = TRUE, na=''),
				)})
			)
		), select = DATA$colHOST[1] == host)}	)
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)


#	Histograms
	output$hostHIST	<-	renderPlot({
		DATA$tabLONG	|>	filter(!is.na(Score))	|>
		filter(Host %in% input$dataHOSTS) |>
		graphHIST()
	},	height = GRAPH$graphHEIGHT,	res = GRAPH$graphRES)	|>
	# bindCache(input$dataTAB, input$dataHOSTS)	|>
	bindEvent(input$dataTABload, input$dataHOSTS)

	observe({
		lapply(DATA$SEASONS, function(seas)	{appendTab(inputId = "histograms", tab = tabPanel(seas	|>	str_SEASON(),
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
		output$clickHISTtall	<-	renderUI({	div(style = paste0("height: ", 51 * (length(DATA$SEASONS) + 1), "px; overflow-y: auto;"),
			renderTable({	req(input$clickHIST)
				roughHIST	<-	(input$clickHIST$y + DATA$scoreSTEP/2)	|>	round(1)

				DATA$tabLONG	|>	group_by(Season)	|>
					ranksHOST()	|>
					filter(
						Score	|>	between(roughHIST - DATA$scoreSTEP/2, roughHIST + DATA$scoreSTEP/2),
						# Score	|>	near(input$clickHIST$y + 0.25, tol = 0.25),
						#	near does work, but it doesn't match the bins so best to not use
						Host	==	input$clickHIST$panelvar1,
						)	|>
					mutate(	Season	=	Season |>	str_SEASON(),	Score	=	paste0(Score)	)	|>
					select(Host, Season, Score, Count, Titles)
			})	)	})
	})	|>	bindEvent(input$dataTABload,	ignoreInit = TRUE)


#	Differences
	scoresSEL	<-	reactive({
		hold	<-	DATA$TABLE	|>	select(where(is.numeric) |	contains("Rating"))	|>	names()
		holdTF	<-	hold	%in%	input$diffSCORES
		list(
			hold[holdTF],
			hold[!holdTF]
		)
	})

	TABLES$tableDIFF	<-	reactive({
		req(DATA$TABLE)
		DATA$TABLE	|>	tableDIFF()	|>	tableFORMdiff(selORDER = TABLES$tableORD())
	})	|>	bindEvent(input$dataTABload, TABLES$tableORD())

	output$diffsTable	=	renderTable({
		req(TABLES$tableDIFF(), length(scoresSEL()[[1]]) > 1, length(input$diffSEASON) >= 1)
		TABLES$tableDIFF()		|>
			rename_with(function(IN) str_remove(IN, "Episode(.|-)"))	|>
			filter(Season %in% input$diffSEASON)	|>
			select(!Link & !Season)	|>
			select(where(is.factor) | Title | contains(scoresSEL()[[1]], ignore.case=FALSE))	|>	select(!contains(scoresSEL()[[2]], ignore.case=FALSE))
	},	digits = reactive(input$roundTerm), striped = TRUE, na='')

	TABLES$tableDIFFstatsAll	<-	reactive({
		req(TABLES$tableDIFF())

		TABLES$tableDIFF()	|>	tableDIFFstats(TRUE)
	})	|>	bindEvent(input$dataTABload, TABLES$tableORD())

	output$diffsStatsAll	=	renderTable({
		req(TABLES$tableDIFFstatsAll(), length(scoresSEL()[[1]]) > 1)

		TABLES$tableDIFFstatsAll()	|>
			rename_with(function(IN) str_remove(IN, "Episode(.|-)"))	|>
			select(Season | Stat | contains(scoresSEL()[[1]], ignore.case=FALSE))	|>	select(!contains(scoresSEL()[[2]], ignore.case=FALSE))	|>
			filter(Stat %in% input$dataSTATS)	|>	mutate(Stat = str_replace(Stat, "StDev", "Standard Deviation"))	|>
			mutate(Season = ifelse(str_detect(Season, 'S\\d+'), str_SEASON(Season), Season))
	},	digits = reactive(input$roundTerm), striped = TRUE, na='')

	TABLES$tableDIFFstats	<-	reactive({
		req(TABLES$tableDIFF())

		TABLES$tableDIFF()	|>	tableDIFFstats(FALSE)
	})	|>	bindEvent(input$dataTABload, TABLES$tableORD())

	output$diffsStats	=	renderTable({
		req(TABLES$tableDIFFstats(), length(scoresSEL()[[1]]) > 1)

		TABLES$tableDIFFstats()	|>
			rename_with(function(IN) str_remove(IN, "Episode(.|-)"))	|>
			select(Season | Stat | contains(scoresSEL()[[1]], ignore.case=FALSE))	|>	select(!contains(scoresSEL()[[2]], ignore.case=FALSE))	|>
			filter(Season %in% input$diffSEASON)		|>
			filter(Stat %in% input$dataSTATS)	|>	mutate(Stat = str_replace(Stat, "StDev", "Standard Deviation"))	|>
			mutate(Season = ifelse(str_detect(Season, 'S\\d+'), str_SEASON(Season), Season))
	},	digits = reactive(input$roundTerm), striped = TRUE, na='')

	observe({	updateSliderInput(inputId = "diffsRANGE", value = c(-Inf, Inf))	})	|>	bindEvent(input$diffsRANGEres)

	scoresSELcomb	<-	reactive({
		pairsSEL	<-	combn(scoresSEL()[[1]], 2)
		paste(paste(pairsSEL[1, ], pairsSEL[2, ], sep="-"), collapse = "|")
		#	because the order of hosts does not change at any time, we do not have to worry about the order when filtering
	})

	output$diffsRanks	=	renderTable({
		req(TABLES$tableDIFF(), length(scoresSEL()[[1]]) > 1, input$diffsORDER)

		out	<-	TABLES$tableDIFF()	|>	ranksDIFF(SEAS = input$diffsSEAS)	|>
			mutate(	Hosts = ordered(Hosts, levels = scoresSELcomb() |> str_split("\\|") |> unlist())	)	|>
			filter(	str_detect(Hosts, scoresSELcomb())	)	|>
			filter(	between(Difference, input$diffsRANGE[1], input$diffsRANGE[2])	)	|>
			arrange(Difference, Hosts)	|>	mutate(Difference	=	Difference	|>	round(2)	|>	paste0())

		if (input$diffsORDER)	out	<-	out	|>	arrange(desc(Difference), Hosts)
		if (input$diffsSEAS)	{
			out	<-	out	|>
			filter(Season %in% input$diffSEASON)	|>
			mutate(Season	=	Season	|>	str_SEASON())
		}

		out
	}, striped = TRUE, na='')

	output$diffRanksMax	=	renderTable({
		req(TABLES$tableDIFF(), length(scoresSEL()[[1]]) > 1)

		out	<-	TABLES$tableDIFF()	|>	ranksDIFF(SEAS = input$diffsSEAS)	|>	group_by(Hosts)	|>
			mutate(	Hosts = ordered(Hosts, levels = scoresSELcomb() |> str_split("\\|") |> unlist())	)	|>
			filter(	str_detect(Hosts, scoresSELcomb() )	)	|> arrange(Hosts)

		if (input$diffsSEAS)	{
			out	<-	out	|>	group_by(Season, .add = TRUE)	|>
				filter(Season %in% input$diffSEASON)	|>
				mutate(Season	=	Season	|>	str_SEASON())
		}

		out	|>	slice_max(Count, na_rm = TRUE)	|>	mutate(Difference	=	Difference	|>	round(2)	|>	paste0())	#|>	rename("Max Count" = Count)
	}, striped = TRUE, na='')

	output$diffRanksMin	=	renderTable({
		req(TABLES$tableDIFF(), length(scoresSEL()[[1]]) > 1)

		out	<-	TABLES$tableDIFF()	|>	ranksDIFF(SEAS = input$diffsSEAS)	|>	group_by(Hosts)	|>
			mutate(	Hosts = ordered(Hosts, levels = scoresSELcomb() |> str_split("\\|") |> unlist())	)	|>
			filter(	str_detect(Hosts, scoresSELcomb() )	)	|> arrange(Hosts)

		if (input$diffsSEAS)	{
			out	<-	out	|>	group_by(Season, .add = TRUE)	|>
				filter(Season %in% input$diffSEASON)	|>
				mutate(Season	=	Season	|>	str_SEASON())
		}

		out	|>	slice_min(Count, na_rm = TRUE)	|>	mutate(Difference	=	Difference	|>	round(2)	|>	paste0())	#|>	rename("Min Count" = Count)
	}, striped = TRUE, na='')
}


ui <- function(request)	{fluidPage(
	# titlePanel("Series Scoring"),
	titlePanel(uiOutput("Title"), windowTitle="Series Scoring"),
	sidebarLayout(
		sidebarPanel(
			if (DBcontrol)	tagList(
				selectInput(inputId	=	"dataDB",	label	=	"Database to Load",	selectize	=	FALSE,
					choices	=	setNames(FILES,	gsub(".db", "", FILES)),	selected	=	DATA$Default
				),
				actionButton(inputId	=	"dataDBload",	label	=	"Load Selected Database")
			),
			selectInput(inputId	=	"dataTAB",	label	=	"Table to Load",	selectize	=	FALSE,
				choices	=	setNames(DATA$TABS$TABS,	DATA$TABS$name)
			),
			actionButton(inputId	=	"dataTABload",	label	=	"Load Selected Table"),
			# checkboxGroupInput(inputId	=	"dataHOSTS",	label	=	"Hosts",	choices	=	NULL),
			uiOutput("hostSELtop"),
			checkboxGroupInput(inputId	=	"dataSTATS",	label	=	"Statistics ",
				choiceNames	=	c("Mean", "Standard Deviation", "Median", "MAD"),	choiceValues	=	c("Mean", "StDev", "Median", "MAD"),
				selected	=	c("Mean", "StDev")
			),	helpText("MAD - Median Absolute Deviation"),
			numericInput(inputId = 'roundTerm',	label = "Round to",	value = 2, min = 0, step = 1),
			uiOutput("hostSELbot"),
			width	=	2
		),
		mainPanel(
			if (DBcontrol)	conditionalPanel(condition	=	"input.dataDBload == 0 && input.dataTABload == 0",
				h3('Select a database from the "Database to Load" list on the left and click "Load Selected Database"'),
			),
			conditionalPanel(condition	=	"input.dataTABload == 0",
				h3('Select a table from the "Table to Load" list on the left and click "Load Selected Table" to get data'),
			),
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
							# div(	style = "height: 216px; overflow-y: auto;",
								# tableOutput("clickPLOT")
							# ),
							uiOutput("clickPLOTtall"),
						),
						header	=	tagList(
							fluidRow(
								column(4,	checkboxInput(inputId = "dataHOSTSplot",	label = "Separate Hosts",	value = TRUE	)	),
								column(2,	checkboxGroupInput(inputId = "dataEXTRASplot",	label = "Extra Scores",	inline = TRUE,	choices = NULL)),
							),
						),
					),
				),
				tabPanel("Host Ranks",
					tabsetPanel(id	=	"ranks",
						header	=	tagList(
							fluidRow(
								column(3,	checkboxInput("rankNA",	label = "List Missing Episodes per Season")),
								column(2,	strong("Score Range")),
								column(4,	sliderInput("ranksRANGE", label = NULL, min = 0, max = 10, value = c(-Inf, Inf), step = 1, width = "100%")),
								column(2,	actionButton("ranksRANGEres", label = "Reset Range")),
							)
						)
					)
				),
				tabPanel("Histograms",
					tabsetPanel(id	=	"histograms",
						tabPanel("All Seasons",
							plotOutput("hostHIST",	height = GRAPH$graphHEIGHT, click = "clickHIST"),
							# div(	style = "height: 300px; overflow-y: auto;",
								# tableOutput("clickHIST")
							# ),
							uiOutput("clickHISTtall"),
						)
					),
				),
				tabPanel("Differences",
					tabsetPanel(id	=	"diffs",
						tabPanel("Host A - Host B",
							tableOutput("diffsTable"),
						),
						tabPanel("Stats",
							wellPanel(	tableOutput("diffsStatsAll")	),
							tableOutput("diffsStats"),
						),
						tabPanel("Ranks",
							fluidRow(
								column(3,	radioButtons("diffsORDER", label = "Difference Sorting",
									choiceNames		=	c("Ascending",	"Descending"),
									choiceValues	=	c(FALSE,		TRUE),
									inline	=	TRUE)
								),
								column(2,
									checkboxInput("diffsSEAS",			label = "Include Seasons"),
									checkboxGroupInput("diffRanksMinMaxShow",	label	=	NULL,
										choiceNames		=	c("Show Count Maximums"),
										choiceValues	=	c("max"),
										#	minimum does not really serve a purpose
										# choiceNames		=	c("Show Count Maximums", "Show Count Minimums"),
										# choiceValues	=	c("max", "min"),
									),
								),
								column(4,	sliderInput("diffsRANGE", label = NULL, min = 0, max = 10, value = c(-Inf, Inf), step = 1, width = "100%")),
								column(1,	actionButton("diffsRANGEres", label = "Reset Range")),
							),
							conditionalPanel(condition	=	"input.diffRanksMinMaxShow.includes('max')",
								wellPanel(strong("Maximums"), tableOutput("diffRanksMax"))
							),
							conditionalPanel(condition	=	"input.diffRanksMinMaxShow.includes('min')",
								wellPanel(strong("Minimums"), tableOutput("diffRanksMin"))
							),
							tableOutput("diffsRanks"),
						),
						header	=	tagList(
							fluidRow(
								column(5,	checkboxGroupInput(inputId = "diffSCORES",	label = "Scores to Compare",	inline = TRUE,	choices = NULL)),
								column(6,	checkboxGroupInput(inputId = "diffSEASON",	label = "Seasons to Include",	inline = TRUE,	choices = NULL)),
							),
						),
					),
				),
			),
			width	=	10
		)
	)
)	}


shinyApp(ui = ui, server = server)
