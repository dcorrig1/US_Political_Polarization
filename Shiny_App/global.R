library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(maptools)
library(ggplot2)
library(maps)
library(shiny)
library(shinydashboard)


CountyData = read.csv("CountyElectionDataF.csv")

#To this data, add D and R percent per county, round Population, and also revert the FIPS for Oglala Lakota (newly changed to 46102) back to 46113 (Shannon County) because this change hasn't been reflected yet in Choropleth
CountyData = CountyData %>% mutate(County_DPct = 50+County_DMargin/2, County_RPct=50-County_DMargin/2) %>% mutate(Population=ceiling(Population))
CountyData$FIPS = ifelse(CountyData$FIPS==46102, 46113, CountyData$FIPS)
CountyData = CountyData %>% mutate(MarginPercentile = round(CumCounty_StartPct/2 + CumCounty_FinishPct/2), digits=1)

#Also add D/R overall values to this dataset, and also a parameter dataset which assigns "M" to counties that are intermediate (between 60/40) either way, which could also be adjusted
CountyData$DR = ifelse(CountyData$County_Net_DVote>=0, "D", "R")
CountyData$RelDR = ifelse(CountyData$CountyRelativeMargin>=0, "D", "R")
CountyData$RelDRParam = case_when(CountyData$CountyRelativeMargin>20 ~ "D", CountyData$CountyRelativeMargin>-20 ~ "M", CountyData$CountyRelativeMargin<=-20 ~ "R")

#Also add a column to indicate change in relative vote margin within a county between 2000 and 2016
CountyData16 = CountyData %>% filter(Year==2016, FIPS!=8014) %>% arrange(FIPS)
CountyData00 = CountyData %>% filter(Year==2000) %>% arrange(FIPS)
CountyData16$Margin00 = CountyData00$CountyRelativeMargin
CountyData16$Change_00to16 = CountyData16$CountyRelativeMargin - CountyData16$Margin00
CountyChangeJoin = CountyData16 %>% select(FIPS, Change_00to16)
CountyData = full_join(CountyData, CountyChangeJoin, by="FIPS")

#Design some specific modifications to the CountyData dataset, which will help with visualization and display for users
CountyDataforYearsGraphs = CountyData %>% select(Year, County_RRank, CountyRelativeMargin, RelDRParam)
CountyDataforYearMaps = CountyData %>% select(Year, FIPS, CountyRelativeMargin)
CountyDataEasyDemoDisplays = CountyData %>% rename(Percent_White=WhitePct, Percent_Black=BlackPct, Percent_Hispanic=HispanicPct, Percent_Asian=AsianPct, 
  Percent_Foreign_Born=ForeignPct, Percent_Urban=UrbanPct, Percent_Below_Poverty_Line=PovertyPct, Percent_Unemployment=UnemploymentRate, 
  Percent_With_College_Degree=CollegeDegree, Percent_Under18Years=Under18Pct, Percent_Over65Years=Over65Pct, Household_Income=HouseholdIncome, 
  Overall_Population=Population)

#Load the mapping data from choroplethrMaps and remove Alaska from it, as well as adjusting the position of Hawaii for visualization purposes. Also rename 'region' to 'FIPS' to match the ElectionData DF
data(county.map)
county.mapNoAKModHI = county.map %>% filter(STATE != "02") %>% mutate(long = case_when(STATE=="15" ~ long+36, STATE!="15" ~ long), lat = case_when(STATE=="15" ~ lat+8, STATE != "15" ~ lat)) %>% rename(FIPS=region)


#Define functions to use to generate plots
ShowBoxplotYears = function(Years){
	ggplot(CountyDataforYearsGraphs %>% filter(Year %in% Years), aes(fill=as.factor(Year), y=CountyRelativeMargin)) + 
	geom_boxplot() + xlab("Election Year") + 
	ylab("Relative Margin per County") + labs(fill="Year") + theme(axis.text.x=element_blank(), text=element_text(size=20)) + 
	ggtitle("Relative Vote Margin, All U.S. Counties by Year")
}

ShowMarginPlotYears = function(Years){
	ggplot(CountyDataforYearsGraphs %>% filter(Year %in% Years), aes(x=County_RRank, y=CountyRelativeMargin, fill=RelDRParam)) + 
	geom_col(show.legend=F) + facet_wrap(~Year, nrow=2) + 
	scale_fill_manual(values=c("blue", "green", "red")) + xlab("County, Ranked from Most to Least Republican") + 
	ylab("Relative Vote Margin") + ggtitle("Relative Margin of All American Counties") + theme(text=element_text(size=20))
}

PrintYearMargins = function(Years){
  CensusPiece = CountyDataforYearMaps %>% filter(Year %in% Years)
  Joint = right_join(county.mapNoAKModHI, CensusPiece, by="FIPS") %>% arrange(order)
  TempMap = ggplot(Joint, aes(long, lat, group=group, fill=CountyRelativeMargin)) + geom_polygon() + 
  scale_fill_gradient2(name = "Relative Margin", labels=c("Republican", "Even", "Democratic"), breaks=c(-100, 0, 90),
  	low="red2", mid="white", high="blue2", limits=c(-100, 90.7)) + coord_map() + theme(text=element_text(size=20), axis.text.y=element_blank(), 
  	axis.text.x=element_blank()) + 
    facet_wrap(~Year, ncol=2)
  return(TempMap)
}


PrintMarginsState = function(Stateinput){
  CensusPiece = CountyData %>% filter(State==Stateinput)
  Joint = right_join(county.mapNoAKModHI, CensusPiece, by="FIPS") %>% arrange(order)
  TempMap = ggplot(Joint, aes(long, lat, group=group, fill=CountyRelativeMargin)) + geom_polygon() + geom_polygon(color='black', fill=NA) + 
  scale_fill_gradient2(name = "Relative Margin", labels=c("Republican", "Even", "Democratic"), breaks=c(-100, 0, 90),
  	low="red2", mid="white", high="blue2", limits=c(-100, 90.7), na.value='blue2') + coord_map() + theme_void() + facet_wrap(~Year, nrow=2) +
    theme(text=element_text(size=20), axis.text.y=element_blank(), 
  	axis.text.x=element_blank())
  return(TempMap)
}

PrintMarginsCounty = function(Stateinput, Countyinput){
  CensusPiece = CountyData %>% filter(State==Stateinput, County==Countyinput)
  Joint = right_join(county.mapNoAKModHI, CensusPiece, by="FIPS") %>% arrange(order)
  TempMap = ggplot(Joint, aes(long, lat, group=group, fill=CountyRelativeMargin)) + geom_polygon() + geom_polygon(color='black', fill=NA) + 
  scale_fill_gradient2(name = "Relative Margin", labels=c("Republican", "Even", "Democratic"), breaks=c(-100, 0, 90),
  	low="red2", mid="white", high="blue2", limits=c(-100, 90.7)) + coord_map() + theme(text=element_text(size=20), axis.text.y=element_blank(), 
  	axis.text.x=element_blank()) + facet_wrap(~Year, ncol=4) + 
    geom_text(size=10, data=Joint, aes(mean(long), mean(lat), label=round(CountyRelativeMargin, digits=1)))
   return(TempMap)
}

PlotCountiesScatter = function(Demoinput, RangeVector){
  CountiestoCutFromBottom = round((RangeVector[1]/100)*3113)
  CountiestoCutFromTop = round(((100-RangeVector[2])/100)*3113)

  NewCutDF = CountyDataEasyDemoDisplays %>% arrange_(Demoinput) %>% filter(Year==2016) %>%
    slice((CountiestoCutFromBottom+1):(3113-CountiestoCutFromTop))

    LinearModelX = lm(NewCutDF$Change_00to16 ~ NewCutDF$CountyRelativeMargin)
    LinearModelR2 = round(summary(LinearModelX)$r.squared, 3)
    LinearModelPval = round(summary(LinearModelX)$coefficients[,4][2], 6)

    TTest = t.test(NewCutDF$CountyRelativeMargin, CountyDataEasyDemoDisplays$CountyRelativeMargin)
    TTestPVal = round(TTest[['p.value']], 6)

    TextDF = data.frame(A=LinearModelR2, B=LinearModelPval, C=TTestPVal)

	TempPlot = ggplot() +  
	  geom_point(data=NewCutDF, mapping=aes(x=CountyRelativeMargin, y=Change_00to16, color=CountyRelativeMargin, size=Overall_Population)) + scale_color_gradient2(name = "Relative Margin", labels=c("Republican", "Even", "Democratic"), breaks=c(-100, 0, 90),
		low="red2", mid="palegoldenrod", high="blue2", limits=c(-100, 90.7)) + theme(text=element_text(size=16)) + geom_text(data=TextDF, size=8, 
		mapping=aes(x=55, y=-68, label=paste0("More Polarized?", "\n", "R^2: ", A, "\n", "P-val: ", B, 
			"\n", "\n", "National Average", "\n", "R^2 = 0.426"))) + 
	scale_size_continuous(range=c(1,12), guide=F) + xlab("Republican/Democratic Margin, 2016") + ylab("Change 2000 to 2016") + 
	geom_hline(yintercept=0, color="black", linetype="dashed") + geom_vline(xintercept=0, color="black", linetype="dashed") + 
	ggtitle("Change in County Margin, 2000 to 2016") + xlim(-100, 75) + ylim(-100, 60)

  return(TempPlot)
}

PrintCountyTable = function(Stateinput, Countyinput){
  CountyPiece = CountyData %>% filter(State==Stateinput, County==Countyinput) %>% mutate(Year=as.integer(Year))
  return(CountyPiece %>% group_by(Year) %>% mutate(Dem_Pct=round(County_DPct,1), Rep_Pct=round(County_RPct,1),  
  	Relative_Margin=round(CountyRelativeMargin,1)) %>% select(Year, Dem_Pct, Rep_Pct, Relative_Margin))
}

PrintCountyTableRange = function(Demoinput, Rangeinputlow, Ranginputhi){
  CountyPiece = CountyData %>% filter(Year==2016, Demoinput>=Rangeinputlow, Demoinput<=Rangeinputhi)
  renderTable(CountyPiece %>% group_by(Year) %>% mutate(Dem_Pct=round(County_DPct,1), Rep_Pct=round(County_RPct,1),  Relative_Margin=round(CountyRelativeMargin,1)) %>% select(Year, Dem_Pct, Rep_Pct, Relative_Margin))
}

PrintTargetCounty = function(TargetRange, Yearinput){
  CensusPiece = CountyData %>% filter(Year==Yearinput, CumCounty_StartPct<=TargetRange) %>% filter(CumCounty_FinishPct>=TargetRange) %>% select(Year, State, County, D_Cand, R_Cand, County_DVote, County_RVote, County_DPct, County_RPct, Population, FIPS, CountyRelativeMargin, MarginPercentile, DR, County_NetVote_AbsValue, County_DPct, County_RPct)
  Joint = right_join(county.mapNoAKModHI, CensusPiece, by="FIPS") %>% arrange(order)
  TempMap = ggplot(Joint, aes(long, lat, group=group, fill=CountyRelativeMargin)) + geom_polygon() + 
  scale_fill_gradient2(name = "Relative Margin", labels=c("Republican", "Even", "Democratic"), breaks=c(-100, 0, 90),
    low="red2", mid="white", high="blue2", limits=c(-100, 90.7)) + coord_map() + theme(text=element_text(size=20), axis.text.y=element_blank(), 
    axis.text.x=element_blank()) + facet_wrap(~Year, ncol=4) + geom_polygon(color='black', fill=NA)
    return(TempMap + geom_text(size=12, data=Joint, aes(mean(long), mean(lat), label=paste0(County, " County", "\n", State, "\n", DR, " + ", 
      County_NetVote_AbsValue, "\n", "Dem: ", round(County_DPct), " Rep: ", round(County_RPct)))))
}

PrintTargetCountyTable = function(TargetRange, Yearinput){
  CountyPiece = CountyData %>% filter(Year==Yearinput, CumCounty_StartPct<=TargetRange) %>% filter(CumCounty_FinishPct>=TargetRange)
  CountyPieceActual = CountyData %>% filter(FIPS==CountyPiece$FIPS[1]) %>% mutate(Year=as.integer(Year))
  return(CountyPieceActual %>% group_by(Year) %>% mutate(Dem_Pct=round(County_DPct,1), Rep_Pct=round(County_RPct,1),  
    Relative_Margin=round(CountyRelativeMargin,1)) %>% select(Year, Dem_Pct, Rep_Pct, Relative_Margin))
}

PrintPopOverlayState = function(Stateinput){
  CensusPiece = CountyData %>% filter(State==Stateinput)
  Joint = right_join(county.mapNoAKModHI, CensusPiece, by="FIPS") %>% arrange(order)
  TempMap = ggplot(Joint, aes(long, lat, group=group, fill=Population)) + geom_polygon() + geom_polygon(color='black', fill=NA) + 
  scale_fill_gradient(name = "Relative Margin",
  	low="white", high="#006600", na.value='blue2') + coord_map() + theme_void() + facet_wrap(~Year, nrow=2) +
    theme(text=element_text(size=20), axis.text.y=element_blank(), 
  	axis.text.x=element_blank())
  return(TempMap)
}