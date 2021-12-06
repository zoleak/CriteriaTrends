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

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  ### Create reactive dataframe based on pollutant info ###
  datasub<-reactive({
    foo <- subset(criteriap, pollutant == input$pollutant)
    return(foo)
  })
    
  output$county<-renderUI({
    selectizeInput("county_input",label = strong("Select County:",style = "color:Navy;font-weight: bold;"),
                   choices = unique(datasub()$County),
                   selected = unique(datasub()$County[1]))})
  
  
  datasub2<-reactive({
    foo<-subset(datasub(),County == input$county_input)
  })
  
  
    output$station<-renderUI({
        selectizeInput("station_input",multiple = TRUE,label = strong("Select Station:",style = "color:Navy;font-weight: bold;"),
                       choices = unique(datasub2()$Station_Name),
                       selected = unique(datasub2()$Station_Name[1]))})
    
    
    datasub3<-reactive({
      foo<-subset(datasub2(),Station_Name %in% input$station_input)
      return(foo)
      
    })
  # This creates the plot 
   output$plot1 <- renderPlot({
     req(input$pollutant)
     req(input$station_input)
     if(input$pollutant == "ozone"){
        ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
         geom_line(size = 1.3)+
         ggtitle(paste0(datasub3()$Station_Name," Ozone Trend\n 4th-Highest Daily Maximum 8-Hour Concentration (ppm)",sep = "")) +
         #ggtitle("Ocean County Ozone Trend\n4th-Highest Daily Maximum 8-Hour Concentration (ppm)")+
         ylab("Concentration, Parts per Million (ppm)") +
         scale_y_continuous(expand = c(0,0),limits = c(0, 0.130),
                            labels = scales::number_format(accuracy = 0.001,
                                                           decimal.mark = "."))+
         geom_segment(aes(x=1997,xend=2008,y=0.08,yend=0.08),color="red",size =1.3,linetype = "dashed")+
         geom_segment(aes(x=2008,xend=2016,y=0.075,yend=0.075),color="red",size =1.3,linetype = "dashed")+
         geom_segment(aes(x=2016,xend=2018,y=0.070,yend=0.070),color="red",size =1.3,linetype = "dashed")+
        scale_x_continuous(breaks=seq(1990,2020,by=1))+
         annotate("text",
                  x = c(2002, 2011, 2017),
                  y = c(0.078, 0.059, 0.055),
                  label = c("1997 8-Hour NAAQS = 0.08 ppm",
                            "2008 8-Hour NAAQS = 0.075 ppm" , "2016 8-Hour\nNAAQS = 0.070 ppm"),
                  family = "", fontface = 3, size=4) + 
         graph_theme
     }
     else if(input$pollutant == "no2"){
       
       ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
         geom_line(size = 1.3)+
         ggtitle(expression(bold(atop("Nitrogen Dioxide (NO"[2]*") Trend","98th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)")))) +
          ylab("Concentration, Parts per Billion (ppb)") +
          scale_y_continuous(expand = c(0,0),limits = c(0, 150))+
          geom_segment(aes(x=2010,xend=2018,y=100 ,yend=100 ),color="red",size =1.3,linetype = "dashed")+
          scale_x_continuous(breaks=seq(1990,2020,by=1))+
         annotate("text",
                  x = c(2014),
                  y = c(90),
                  label = c("2010 1-Hour NAAQS = 100  ppb"),
                  family = "", fontface = 3, size=4) + 
          graph_theme
     }
     
     else if(input$pollutant == "co"){
       
       ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
         geom_line(size = 1.3)+
         ggtitle(paste0(datasub3()$Station_Name," Carbon Monoxide (CO) Trend\n2nd Highest 8-Hour Average Concentration (ppm)",sep = "")) +
         ylab("Concentration, Parts per Million (ppm)") +
         scale_y_continuous(expand = c(0,0),limits = c(0, 11.5))+
         scale_x_continuous(breaks=seq(1990,2020,by=1)) +
         geom_segment(aes(x=1990,xend=2018,y=9,yend=9),color="red",size =1.3,linetype = "dashed")+
         annotate("text",
                  x = c(2010),
                  y = c(8.5),
                  label = c("8 Hour NAAQS = 9 ppm"),
                  family = "", fontface = 3, size=4) + 
         graph_theme
     }
     
     else if(input$pollutant == "so2"){
       ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
         geom_line(size = 1.3)+
         #ggplot(df,aes(Year,Percentile_99th))+geom_line(colour="#109f45",size=1.3)+
         scale_y_continuous(expand = c(0,0),limits = c(0, 200))+
         scale_x_continuous(breaks=seq(1990,2020,by=1)) +
         ggtitle(paste0(datasub2()$Station_Name," Sulfur Dioxide (SO2) Trend\n99th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)",sep = "")) +
         #ggtitle(expression(bold(atop("Essex County Sulfur Dioxide (SO"[2]*") Trend","99th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)")))) +
         ylab("Concentration, Parts per Billion (ppb)") +
         geom_segment(aes(x=2010,xend=2018,y=75,yend=75),color="red",size =1.3,linetype = "dashed")+
         annotate("text",
                  x = c(2012),
                  y = c(60),
                  label = c("2010 1-Hour NAAQS = 75 ppb"),
                  family = "", fontface = 3, size=4) + 
         graph_theme
     }
     
     else if(input$pollutant == "PM10"){
       ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
         geom_line(size = 1.3)+
         scale_y_continuous(expand = c(0,0),limits = c(0, 160))+
         scale_x_continuous(breaks=seq(1990,2020,by=1)) +
         ggtitle(paste0(datasub3()$Station_Name," Particulate Matter(PM10) Trend\n2nd Highest 24-Hour Average Concentration (µg/m3)",sep = "")) +
         #ggtitle(expression(paste(bold(atop("Hudson County Particulate Matter (PM"[10]*") Trend"," 2nd-Highest 24-Hour Average Concentration (µg/m"^3*")"))))) +
         ylab(expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")"))) +
         geom_segment(aes(x=1990,xend=2018,y=150,yend=150),color="red",size =1.3,linetype = "dashed")+
         annotate("text",
                  x = c(2010),
                  y = c(145),
                  label = c(expression("24-Hour NAAQS = 150 µg/m"^3)),
                  family = "", fontface = 3, size=4) + 
         graph_theme
     }
     
     else{
       ggplot(data = datasub2(),aes(x=Year,y=value))+
         geom_line(color="red")+
         scale_x_continuous(breaks=seq(1990,2020,by=1)) 
     }
            
   })
   
   # Download button to save plots from app
   output$downloadPlot <- downloadHandler(
     filename = "plot_download.pdf",
     content = function(file){
       ggsave(file,device = "pdf",width = 11.5,height = 8)
     }
   )
   
}

# Run the application 
shinyApp(ui = ui, server = server)