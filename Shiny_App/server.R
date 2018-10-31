library(shiny)
library(shinydashboard)


shinyServer(function(input, output, session){
	
	#Write the introductory items
	

	randomVals1 = eventReactive(input$GoButton1, {
		input$byyear1
		})
	

	output$YearBoxplot = renderPlot({ShowBoxplotYears(randomVals1())})

	output$YearBarplot = renderPlot({ShowMarginPlotYears(randomVals1())})

	randomVals2 = eventReactive(input$GoButton2,{
		input$byyear2
		})

	output$YearMaps = renderPlot({PrintYearMargins(randomVals2())})

	ElectionorPopChoice = reactive({
		if(input$ElecOrPop == "Election Map"){
			PrintMarginsState(input$ChosenState)
		} else {
			PrintPopOverlayState(input$ChosenState)
		}
		})

	output$StateByYear = renderPlot(ElectionorPopChoice())

	observe({
		updateSelectizeInput(session, 'FinalCounty', 
		choices=unique(CountyData %>% filter(State==input$FirstChoiceState) %>% arrange(desc(Population)) %>% .$County))
		})


	output$CountyByYear = renderPlot(PrintMarginsCounty(input$FirstChoiceState, input$FinalCounty))

	output$CountyTable = renderTable(PrintCountyTable(input$FirstChoiceState, input$FinalCounty), striped=T, bordered=T, digits=1)

	output$DisplayDemoScatter = renderPlot(PlotCountiesScatter(input$ChosenDemo, input$ChosenDemoRange))

	output$ShowMatchingCounty = renderPlot(PrintTargetCounty(input$ChoicePercentileX, input$ChoiceYearX))

	output$ShowMatchingTable = renderTable(PrintTargetCountyTable(input$ChoicePercentileX, input$ChoiceYearX), striped=T, bordered=T, digits=1)

})