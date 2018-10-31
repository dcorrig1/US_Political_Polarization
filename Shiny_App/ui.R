library(shiny)
library(shinydashboard)
library(ggplot2)



header = dashboardHeader(title="Political Polarization in the US", titleWidth = 350)

sidebar = dashboardSidebar(
	width=350,
	sidebarMenu(
		menuItem("Introduction", tabName = 'intro', icon = icon("info")),
		menuItem("Polarization Over Time", tabName = 'byyear', icon = icon("arrow-right")),
		menuItem("Maps!", tabName = 'mapstab', icon =icon("map")),
		menuItem("County by County", tabName = 'bycounty', icon = icon("map-pin")),
		menuItem("Demographic Analysis", tabName = 'demographics', icon = icon("users")),
		menuItem("County Percentiles", tabName = 'percentiles', icon=icon("map-pin"))
		)
	)

body = dashboardBody(
	tabItems(
		tabItem(tabName='intro',
			box(h1('Exploring the Rise in Political Polarization in the United States', align='center'), h2('David Corrigan, Shiny Project October 2018', align='center'), 
				background='light-blue', width=24),
			box(background='light-blue', width=24, p('A quick glance at U.S. presidential results over the last 20 years 
				illustrates some major changes that have taken place among the electorate in only a few short election cycles. 
				Red counties keep getting redder, and highly populated blue counties continue to become bluer. 
				I designed a Shiny App to explore this in more detail, using data from both uselectionatlas.org and the U.S. census. Just look at maps from 1996 and 2016 to see how polarized things have become!', style='font-size:20px')
				),
			box(background='purple', width=24, p("I scraped election data from uselectionatlas.org and joined this to demographic data from the U.S. 
				Census department. Using this app, you can visualize election data over time and see just how geographically polarized American voters 
				have become. Here's what you can do using these tabs:", style='font-size:20px'), p("POLARIZATION OVER TIME: Compare boxplots and actual U.S. county maps
				with any selection of years from 1980 to 2016, and view a corresponding barplot showing just how few counties (in green) are within 20 points of the national
				average in recent elections.", style='font-size:20px'), p("MAPS!: Here you can select any of the 50 states (except for Alaska, which doesn't really have counties) and see how that state's presidential preferences have evolved over 
				time.", style='font-size:20px'), p("COUNTY BY COUNTY: Here, you can select any county in the U.S. and see how it has evolved over the past 40 years in comparison to the National 
				Vote.", style='font-size:20px'), p("DEMOGRAPHIC ANALYSIS: In this section, you can view scatter plots of all 3,113 U.S. counties and how they have polarized from 2000 to 
				2016. You can also select subsets of counties based on demograpic data and view scatter plots of these.", style='font-size:20px'), p("COUNTY PERCENTILES: Finally, you can select a year, 
				and a percentile and see which county was in that percentile (0 being most Democratic, 100 the most Republican), and a table showing how that
				 value has changed for that county over time. Have some fun with this one!", style='font-size:20px'))
			),
		tabItem(tabName='byyear',
			fluidRow(
				column(3,
					checkboxGroupInput(inputId='byyear1', label='Pick Year(s)', choices=unique(CountyData$Year), selected=1980)
					),
				column(4,
					actionButton(inputId="GoButton1", label='Click to Display!')
					)
				),
			fluidRow(
				column(6,
					box(align='center', height=100, width=16, background='light-blue', h3("Boxplot of all U.S. counties normalized to the National Popular Vote"))),
				column(6,
					box(align='center', height=100, width=16, background='light-blue', h3("Barplot of all counties 20+ percent above average 
						(Democratic/Blue, Republican/Red), or within 20 points of the National Average (Green).")))
				),
			fluidRow(
				column(6,
					plotOutput('YearBoxplot')),
				column(6,
					plotOutput('YearBarplot'))
				),
			fluidRow(
				column(12,
					box(align='center', height=100, width=32, background='light-blue', h3("Map of relative vote 
						margins (compared to nat'l average) in selected years"))
					)
				),
			fluidRow(
				column(1,
					checkboxGroupInput(inputId='byyear2', label='Pick Year(s)', choices=unique(CountyData$Year), selected=1980)
					),
				column(2,
					actionButton(inputId="GoButton2", label='Click to Display!')
					),
				column(10,
					plotOutput('YearMaps', width='100%', height='800px'))
				)
			),
		tabItem(tabName='mapstab',
			fluidRow(
				column(12,
					box(align='center', height=100, width=32, background='light-blue', h3("For this page, pick a 
						state and see how its political polarization has evolved over time")
						)
					)
				),
			fluidRow(
				column(6,
					selectizeInput(inputId='ChosenState', label="Select a State", choices=sort(unique(CountyData %>% filter(State!='Alaska') %>% .$State)))
					)
				),
			fluidRow(
				column(12,
					plotOutput('StateByYear', width='100%', height='800px'))
				)
			),
		tabItem(tabName='bycounty',
			fluidRow(
				column(6,
					selectizeInput(inputId='FirstChoiceState', label="First, choose a state", choices=sort(unique(CountyData %>% filter(State!='Alaska') %>% .$State)))
					),
				column(6,
					selectizeInput(inputId='FinalCounty', label="Next, choose a county", choices=sort(unique(CountyData %>% filter(State!='Alaska') %>% .$County)))
					)
				),
			fluidRow(
				column(8,
					plotOutput('CountyByYear', width='100%', height='800px')
					),
				column(4,
					tableOutput('CountyTable')
					)
				)
			),
		tabItem(tabName='demographics',
			fluidRow(
				column(12,
					box(align='center', height=100, width=32, background='light-blue', h3("On this page, choose a demographic and percentile range
						of that demographic, and observe the D/R lean of these counties and if they have become polarized over time. Placing the range from 0 to 100
						will show all U.S. counties.")
						)
					)
				),
			fluidRow(
				column(6,
					selectizeInput(inputId='ChosenDemo', label='Choose Demographic of Interest', choices=c('Percent_White', 'Percent_Black', 
						'Percent_Hispanic', 'Percent_Asian', 'Percent_Foreign_Born', 'Percent_Urban', 'Percent_Below_Poverty_Line', 
						'Percent_Unemployment', 'Percent_With_College_Degree', 'Percent_Under18Years', 'Percent_Over65Years', 
						'Household_Income', 'Overall_Population'))
					),
				column(6,
					sliderInput(inputId='ChosenDemoRange', label='Next, modify this scatter plot to display only counties within 
						your chosen percentile range and demographic', min=0, max=100, value=c(0,100))
					)
				),
			fluidRow(
				column(12,
					plotOutput('DisplayDemoScatter', width='100%', height='800px')
					)
				)
			),
		tabItem(tabName="percentiles",
			fluidRow(
				column(12,
					box(align='center', height=100, width=32,background='light-blue', h3("Finally, let's have some fun with numbers in this 
						section. Choose a year and a percentile (0 = Most Democratic, 100 = Most Republican.) See which county matches these parameters
						 and how it has changed over time")
						)
					)
				),
			fluidRow(
				column(6,
					radioButtons(inputId='ChoiceYearX', label='Choose Target Year', choices=c(1980, 1984, 1988, 1992, 1996, 2000, 
						2004, 2008, 2012, 2016))
					),
				column(6,
					sliderInput(
						inputId='ChoicePercentileX', label='Now choose a percentile to find the county to match it', min=0,
						max=100, value=0
						)
					)
				),
			fluidRow(
				column(8,
					plotOutput('ShowMatchingCounty', width='100%', height='800px')
					),
				column(4,
					tableOutput('ShowMatchingTable')
					)
				)			
			)		
		)
	)

shinyUI(dashboardPage(header, sidebar, body))