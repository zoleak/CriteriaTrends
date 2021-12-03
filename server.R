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
