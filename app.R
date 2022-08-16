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
###############################################################################
###############################################################################
###############################################################################
### Download necessary packages ###
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("shiny","tidyverse","readxl","shinycssloaders","shinymanager")
###############################################################################
# load in packages
library(shiny)
library(tidyverse)
library(readxl)
library(shinycssloaders)
#library(shinymanager)
###############################################################################
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

pm2.5<-read_xlsx("PM2.5_1999_2020.xlsx", sheet = "byYear",skip = 8)%>%
  select(1:5)%>%
  mutate(pollutant = "PM2.5")

pm2.5_annual<-read_xlsx("PM2.5_1999_2020_annual_weighted_mean.xlsx")%>%
  pivot_longer('1999':'2020', names_to = "Year", values_to = "value")%>%
  mutate(pollutant = "PM2.5 Annual Average",
         State = "NJ",
         Year = as.numeric(Year))%>%
  plyr::rename(c("Station_name" = "Station_Name"))
###############################################################################
#combine all datasets into one dataframe
criteriap<-bind_rows(no2,so2,ozone,co,pm10,pm2.5,pm2.5_annual)
###########################################################################################
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
## dataframe that holds usernames, passwords and other user data
#credentials <- data.frame(
#  user = c("njdep", "shinymanager"), # mandatory
#  password = c("airmon", "12345"), # mandatory
#  start = c("2019-04-15"), # optinal (all others)
#  expire = c(NA, "2019-12-31"),
#  admin = c(FALSE, TRUE),
#  comment = "Simple and secure authentification mechanism 
#  for single ‘Shiny’ applications.",
#  stringsAsFactors = FALSE
#)
###############################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Criteria Air Pollutant Trends"),

    # Sidebar with a drop down menus to filter data
    sidebarLayout(
        sidebarPanel(
            selectInput("pollutant",label =em("Select Pollutant:",style="color:Navy;font-weight: bold;"),
                        choices = unique(criteriap$pollutant)),
            uiOutput("county"),
            uiOutput("station"),
            downloadButton('downloadPlot','Download Plot')
            
        ),
    # Main panel 
        mainPanel(
           plotOutput("plot1")%>%
             withSpinner(type = 5, color = "blue")
        )
    )
)
###############################################################################
# Wrap your UI with secure_app
#ui <- secure_app(ui,background  = "linear-gradient(rgba(0, 0, 255, 0.5), 
#                    rgba(255, 255, 0, 0.5)),
#                    url('https://wetlandsinstitute.org/wp-content/uploads/2018/09/NJDEP-logo.jpg')no-repeat bottom fixed;")
###############################################################################
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials)
  #)
  
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
    if(input$pollutant == "ozone" & length(input$station_input) == 1){
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
    else if(input$pollutant == "ozone" && length(input$station_input>1)){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        ggtitle(paste0(datasub3()$County," County Ozone Trend\n 4th-Highest Daily Maximum 8-Hour Concentration (ppm)",sep = ""))+
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
    
    else if(input$pollutant == "no2"& length(input$station_input) == 1){
      req(input$pollutant)
      req(input$station_input)
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
    
    else if(input$pollutant == "no2" && length(input$station_input>1)){
      req(input$pollutant)
      req(input$station_input)
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        ggtitle(paste0(datasub3()$County," County Nitrogen Dioxide (NO2) Trend\n98th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)")) +
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
    
    else if(input$pollutant == "co"& length(input$station_input) == 1){
      req(input$pollutant)
      req(input$station_input)
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
    
  else if(input$pollutant == "co"& length(input$station_input) > 1){
    req(input$pollutant)
    req(input$station_input)
    ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
      geom_line(size = 1.3)+
      ggtitle(paste0(datasub3()$County," County Carbon Monoxide (CO) Trend\n2nd Highest 8-Hour Average Concentration (ppm)",sep = "")) +
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
  
  
    else if(input$pollutant == "so2" & length(input$station_input) == 1){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        #ggplot(df,aes(Year,Percentile_99th))+geom_line(colour="#109f45",size=1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 200))+
        scale_x_continuous(breaks=seq(1990,2020,by=1)) +
        ggtitle(paste0(datasub3()$Station_Name," Sulfur Dioxide (SO2) Trend\n99th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)",sep = "")) +
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
    
    else if(input$pollutant == "so2" && length(input$station_input>1)){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        #ggplot(df,aes(Year,Percentile_99th))+geom_line(colour="#109f45",size=1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 200))+
        scale_x_continuous(breaks=seq(1990,2020,by=1)) +
        ggtitle(paste0(datasub3()$County," County Sulfur Dioxide (SO2) Trend\n99th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)",sep = "")) +
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
    
    else if(input$pollutant == "PM10"& length(input$station_input) == 1){
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
    
    else if(input$pollutant == "PM10"& length(input$station_input) > 1){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 160))+
        scale_x_continuous(breaks=seq(1990,2020,by=1)) +
        ggtitle(paste0(datasub3()$County," County Particulate Matter(PM10) Trend\n2nd Highest 24-Hour Average Concentration (µg/m3)",sep = "")) +
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
    
    else if(input$pollutant == "PM2.5" & length(input$station_input) == 1){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 75))+
        scale_x_continuous(breaks=seq(1999,2020,by=1)) +
        ggtitle(expression(paste(bold(atop("Particulate Matter (PM"[2.5]*") Trend"," of the 98th Percentile 24-Hour Average Concentration (µg/m"^3*")"))))) +
        ylab(expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")"))) +
        geom_segment(aes(x=1999,xend=2006,y=65,yend=65),color="red",size =1.3,linetype = "dashed")+
        geom_segment(aes(x=2006,xend=2018,y=35,yend=35),color="red",size =1.3,linetype = "dashed")+
        annotate("text",
                 x = c(2002,2015),
                 y = c(63,32),
                 label = c(expression("24-Hour NAAQS = 65 µg/m"^3),
                           expression("24-Hour NAAQS = 35 µg/m"^3)),
                 family = "", fontface = 3, size=4) + 
        graph_theme
    }
    
    else if(input$pollutant == "PM2.5" && length(input$station_input>1)){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 75))+
        scale_x_continuous(breaks=seq(1999,2020,by=1)) +
        ggtitle(paste0(datasub3()$County," County Particulate Matter (PM2.5) Trend\nof the 98th Percentile 24-Hour Average Concentration (µg/m3)")) +
        ylab(expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")"))) +
        geom_segment(aes(x=1999,xend=2006,y=65,yend=65),color="red",size =1.3,linetype = "dashed")+
        geom_segment(aes(x=2006,xend=2018,y=35,yend=35),color="red",size =1.3,linetype = "dashed")+
        annotate("text",
                 x = c(2002,2015),
                 y = c(63,32),
                 label = c(expression("24-Hour NAAQS = 65 µg/m"^3),
                           expression("24-Hour NAAQS = 35 µg/m"^3)),
                 family = "", fontface = 3, size=4) + 
        graph_theme
    }
    
    else if(input$pollutant == "PM2.5 Annual Average" & length(input$station_input) == 1){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 20))+
        scale_x_continuous(breaks=seq(1999,2020,by=1)) +
        ggtitle(expression(paste(bold(atop("Particulate Matter (PM"[2.5]*") Trend"," of the Annual Average Concentration (µg/m"^3*")"))))) +
        ylab(expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")"))) +
        geom_segment(aes(x=1999,xend=2013,y=15,yend=15),color="red",size =1.3,linetype = "dashed")+
        geom_segment(aes(x=2013,xend=2018,y=12,yend=12),color="red",size =1.3,linetype = "dashed")+
        annotate("text",
                 x = c(2003,2015),
                 y = c(16,11),
                 label = c(expression("Annual NAAQS = 15 µg/m"^3),
                           expression("Annual NAAQS = 12 µg/m"^3)),
                 family = "", fontface = 3, size=4) + 
        graph_theme
    }
    
    else if(input$pollutant == "PM2.5 Annual Average"&& length(input$station_input>1)){
      ggplot(data = datasub3(),aes(x=Year,y=value,color = datasub3()$Station_Name))+
        geom_line(size = 1.3)+
        scale_y_continuous(expand = c(0,0),limits = c(0, 20))+
        scale_x_continuous(breaks=seq(1999,2020,by=1)) +
        ggtitle(paste0(datasub3()$County," County Particulate Matter (PM2.5) Trend\nof the Annual Average Concentration (µg/m3)"))+
        ylab(expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")"))) +
        geom_segment(aes(x=1999,xend=2013,y=15,yend=15),color="red",size =1.3,linetype = "dashed")+
        geom_segment(aes(x=2013,xend=2018,y=12,yend=12),color="red",size =1.3,linetype = "dashed")+
        annotate("text",
                 x = c(2003,2015),
                 y = c(16,11),
                 label = c(expression("Annual NAAQS = 15 µg/m"^3),
                           expression("Annual NAAQS = 12 µg/m"^3)),
                 family = "", fontface = 3, size=4) + 
        graph_theme
    }
    
    
    
    
    
  })
  ###############################################################################
  # Download button to save plots from app
  output$downloadPlot <- downloadHandler(
    filename = "plot_download.pdf",
    content = function(file){
      ggsave(file,device = "pdf",width = 11.5,height = 8)
    }
  )
  
  
}
###############################################################################
# Run the application 
shinyApp(ui = ui, server = server)
