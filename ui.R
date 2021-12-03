#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Shiny app to assist with criteria air pollutant trend graphs 
# This app began development in 11/2021
# Author: Kevin Zolea 
### Download necessary packages ###
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("shiny","tidyverse","readxl","shinycssloaders")
###############################################################################
# load in packages
library(shiny)
library(tidyverse)
library(readxl)
library(shinycssloaders)

#read in pollutant data

no2<-read_xlsx("NO2_1990_2020.xlsx",sheet = "byYear")%>%
  select(Year:value)%>%
  mutate(pollutant = "no2")
so2<-read_xlsx("SO2_1990_2020.xlsx",sheet = "byYear")%>%
  mutate(pollutant = "so2")
ozone<-read_xlsx("ozone_1990_2020.xlsx",sheet = "byYear")%>%
  mutate(pollutant = "ozone")
co<-read_xlsx("CO_1990_2020.xlsx",sheet = "byYear")%>%
  mutate(pollutant = "co")
pm10<-read_xlsx("PM10_1990_2020.xlsx",sheet = "byYear")%>%
  mutate(pollutant = "PM10",value=as.numeric(value))
#combine all datasets into one dataframe
criteriap<-bind_rows(no2,so2,ozone,co,pm10)
###########################################################################################
## theme for plots ####
graph_theme<- theme_linedraw()+
  theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        plot.margin = unit(c(1.5,2,4,2), "lines"), 
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        #legend.title = element_text(face="bold"),
        legend.title = element_blank(),
        legend.text=element_text(size=10, face="bold"))
###########################################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("Criteria Air Pollutant Trends"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("pollutant",label =em("Select Pollutant:",style="color:Navy;font-weight: bold;"),
                  choices = unique(criteriap$pollutant)),
      uiOutput("county"),
      uiOutput("station"),
      downloadButton('downloadPlot','Download Plot')
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1")%>%
        withSpinner(type = 5, color = "blue")
    )
  )
)