# US_Political_Polarization

This repository contains the code used to design my Shiny App, in which I created a tool to visualize the increasing political polarization in the United States. The process was a scrape of county election data using uselectionatlas.org, with election data then joines to downloaded Census data for demograpic information, and mapping in the Shiny App using Choropleth.

The app can be located here: https://dcorrig1.shinyapps.io/shiny_app/

The actual Shiny scripts can be found in the Shiny_App folder

DLAtl folder contains the scripts used to deploy the Scrapy spider, used to scrape uselectionatlas.org.

The DF_Cleanup folder contains code to clean the scraped dataset.

The Census_Join folder contains code used to create the joint election data/census information, in which the two data frames were joined using the county FIPS identifier created by the U.S. Census Bureau.

The EDA_Visualization folder contains some basic coding pre-work, and designing of visualization functions that were then used ultimately in the Shiny scripts.
