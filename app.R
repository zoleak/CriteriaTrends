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

pacman::p_load("shiny","tidyverse","readxl","shinycssloaders",
               "shinymanager","bslib","bsicons","shinyWidgets")
###############################################################################
# load in necessary packages
library(shiny)
library(tidyverse)
library(readxl)
library(shinycssloaders)
library(bslib)
library(bsicons)
library(shinyWidgets)
#library(shinymanager)
###############################################################################
#read in pollutant data
no2<-read_xlsx("data/NO2_1990_2022.xlsx",sheet = "byYear")%>%
    select(Year:value)%>%
    mutate(pollutant = "no2")

so2<-read_xlsx("data/SO2_1990_2022.xlsx",sheet = "byYear")%>%
    mutate(pollutant = "so2")

ozone<-read_xlsx("data/ozone_1990_2022.xlsx",sheet = "byYear")%>%
    mutate(pollutant = "ozone")

co<-read_xlsx("data/CO_1990_2022.xlsx",sheet = "byYear")%>%
    mutate(pollutant = "co")

pm10<-read_xlsx("data/PM10_1990_2022.xlsx",sheet = "byYear")%>%
  mutate(pollutant = "PM10",value=as.numeric(value))

pm2.5<-read_xlsx("data/PM2.5_1999_2022.xlsx")%>%
  pivot_longer('1999':'2022', names_to = "Year", values_to = "value")%>%
  mutate(pollutant = "PM2.5",
    Year = as.numeric(Year))
  
pm2.5_annual<-read_xlsx("data/PM2.5_1999_2020_annual_weighted_mean.xlsx")%>%
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
## theme for plot ####
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
# Define UI for application
ui <- page_sidebar(
    # Application title
    title="Criteria Air Pollutant Trends",
    # Sidebar with a drop down menus to filter data
    sidebar=sidebar(
          selectInput("pollutant", label = strong("Select Pollutant:", 
                                                  style = "color:Navy;font-weight: bold;"),
                      choices = unique(criteriap$pollutant)),
            uiOutput("county"),
            uiOutput("station"),
          downloadBttn(
            outputId = "downloadPlot",  # Keep the same outputId
            label = "Download Plot",
            style = "jelly",           # Choose a stylish style
            color = "primary"        # Customize color
          ),br(),
          downloadBttn(
            outputId = "downloaddata",  # Give a new outputId
            label = "Download Data",
            style = "jelly",
            color = "success"  # Green color
          ),br(),
          # Adding a link to EPA website
          tags$a(href = "https://www.epa.gov/criteria-air-pollutants", target = "_blank",
                 icon("question-circle"), " Learn more")
        ),
    # Main panel
    card(
      layout_column_wrap(
        width = "250px",
        value_box(
          title = "Max Concentration",
          value = textOutput("box_max"),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          full_screen = FALSE, fill = TRUE
        ),
        value_box(
          title = "Min Concentration",
          value = textOutput("box_min"),
          showcase = bsicons::bs_icon("graph-down-arrow"),
          full_screen = FALSE, fill = TRUE
        ),
        value_box(
          title = "Average Concentration",
          value = textOutput("box_avg"),
          showcase = bsicons::bs_icon("bar-chart"),
          full_screen = FALSE, fill = TRUE
        )
      )),
           card(plotOutput("plot1")%>%
             withSpinner(type = 5, color = "blue"))
        )
    

###############################################################################
# Wrap your UI with secure_app
#ui <- secure_app(ui,background  = "linear-gradient(rgba(0, 0, 255, 0.5), 
#                    rgba(255, 255, 0, 0.5)),
#                    url('https://wetlandsinstitute.org/wp-content/uploads/2018/09/NJDEP-logo.jpg')no-repeat bottom fixed;")
###############################################################################
# Define server logic
server <- function(input, output,session) {
  
  #res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials)
  #)
  
  ### Create reactive dataframe based on pollutant info ###
  datasub<-reactive({
    foo <- subset(criteriap, pollutant == input$pollutant)
    return(foo)
  })
  
  output$county <- renderUI({
    selectizeInput("county_input",
                   label = strong("Select County:", style = "color:Navy;font-weight: bold;"),
                   choices = na.omit(unique(datasub()$County)),  # Exclude NAs
                   selected = na.omit(unique(datasub()$County)[1]))
  })
  
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
  
  # Function to calculate Max, Min, and Average concentrations along with the corresponding years
  calculate_summary_stats <- function(data) {
    max_row <- data[data$value == max(data$value), ]
    min_row <- data[data$value == min(data$value), ]
    
    max_val <- max(data$value)
    min_val <- min(data$value)
    avg_val <- mean(data$value)
    
    max_year <- max_row$Year
    min_year <- min_row$Year
    
    return(list(max_val = max_val, min_val = min_val, avg_val = avg_val,
                max_year = max_year, min_year = min_year))
  }
  
  # Reactive expression for Max, Min, and Average concentrations
  summary_stats <- reactive({
    req(input$pollutant, input$station_input)
    
    # Filter data based on selected pollutant, county, and station
    filtered_data <- datasub3()
    
    # Calculate Max, Min, and Average concentrations with years
    calculate_summary_stats(filtered_data)
  })
  
  # Render the value boxes with the calculated values
  output$box_max <- renderText({
    paste("Value:", summary_stats()$max_val, "\nYear:", summary_stats()$max_year)
  })
  
  output$box_min <- renderText({
    paste("Value:", summary_stats()$min_val,"\nYear:", summary_stats()$min_year)
  })
  
  output$box_avg <- renderText({
    paste("Value:", round(summary_stats()$avg_val))  # Average concentration
  })
  
  # Function to generate plots
  generate_plot <- function(data, pollutant, county,title,stations) {
    ggplot(data, aes(x = Year, y = value, color = Station_Name)) +
      geom_line(size = 1.3) +
      ggtitle(title) +
      ylab(get_ylab_text(pollutant)) +
      get_additional_layers(pollutant) +
      graph_theme
  }
  
  # Function to get NAAQS text
  get_naaqs_text <- function(pollutant) {
    naaqs_text <- switch(pollutant,
                         "ozone" = "4th-Highest Daily Maximum 8-Hour Concentration (ppm)",
                         "no2" = "98th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)",
                         "co" = "2nd Highest 8-Hour Average Concentration (ppm)",
                         "so2" = "99th Percentile of Daily Maximum 1-Hour Average Concentration (ppb)",
                         "PM10" = "2nd Highest 24-Hour Average Concentration (µg/m³)",
                         "PM2.5" = "98th Percentile 24-Hour Average Concentration (µg/m³)",
                         "PM2.5 Annual Average" = "Annual Average Concentration (µg/m³)"
    )
    return(naaqs_text)
  }
  
  # Function to get y-axis label text
  get_ylab_text <- function(pollutant) {
    ylab_text <- switch(pollutant,
                        "ozone" = "Concentration, Parts per Million (ppm)",
                        "no2" = "Concentration, Parts per Billion (ppb)",
                        "co" = "Concentration, Parts per Million (ppm)",
                        "so2" = "Concentration, Parts per Billion (ppb)",
                        "PM10" = expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")")),
                        "PM2.5" = expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")")),
                        "PM2.5 Annual Average" = expression(paste("Concentration, Micrograms per Cubic Meter (µg/m"^3,")"))
    )
    return(ylab_text)
  }
  
  # Function to add additional layers to the plot
  get_additional_layers <- function(pollutant) {
    additional_layers <- switch(pollutant,
                                "ozone" = list(
                                  geom_segment(aes(x = 1997, xend = 2008, y = 0.08, yend = 0.08), color = "red", size = 1.3, linetype = "dashed"),
                                  geom_segment(aes(x = 2008, xend = 2016, y = 0.075, yend = 0.075), color = "red", size = 1.3, linetype = "dashed"),
                                  geom_segment(aes(x = 2016, xend = 2018, y = 0.070, yend = 0.070), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(1990, 2022, by = 1)),
                                  annotate("text", x = c(2002, 2011, 2017), y = c(0.078, 0.059, 0.055),
                                           label = c("1997 8-Hour NAAQS = 0.08 ppm", "2008 8-Hour NAAQS = 0.075 ppm", "2016 8-Hour NAAQS = 0.070 ppm"),
                                           family = "", fontface = 3, size = 4)
                                ),
                                "no2" = list(
                                  geom_segment(aes(x = 2010, xend = 2018, y = 100, yend = 100), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(1990, 2022, by = 1)),
                                  annotate("text", x = c(2014), y = c(90), label = c("2010 1-Hour NAAQS = 100 ppb"),
                                           family = "", fontface = 3, size = 4)
                                ),
                                "co" = list(
                                  geom_segment(aes(x = 1990, xend = 2018, y = 9, yend = 9), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(1990, 2022, by = 1)),
                                  annotate("text", x = c(2010), y = c(8.5), label = c("8 Hour NAAQS = 9 ppm"),
                                           family = "", fontface = 3, size = 4)
                                ),
                                "so2" = list(
                                  geom_segment(aes(x = 2010, xend = 2018, y = 75, yend = 75), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(1990, 2022, by = 1)),
                                  annotate("text", x = c(2012), y = c(60), label = c("2010 1-Hour NAAQS = 75 ppb"),
                                           family = "", fontface = 3, size = 4)
                                ),
                                "PM10" = list(
                                  geom_segment(aes(x = 1990, xend = 2018, y = 150, yend = 150), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(1990, 2022, by = 1)),
                                  annotate("text", x = c(2010), y = c(145), label = c("24-Hour NAAQS = 150 µg/m^3"),
                                           family = "", fontface = 3, size = 4)
                                ),
                                "PM2.5" = list(
                                  geom_segment(aes(x = 1999, xend = 2006, y = 65, yend = 65), color = "red", size = 1.3, linetype = "dashed"),
                                  geom_segment(aes(x = 2006, xend = 2018, y = 35, yend = 35), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(1999, 2022, by = 1)),
                                  annotate("text", x = c(2002, 2012), y = c(60, 30), label = c("1999 24-Hour NAAQS = 65 µg/m^3",
                                                                                               "2006 24-Hour NAAQS = 35 µg/m^3"),
                                           family = "", fontface = 3, size = 4)
                                ),
                                "PM2.5 Annual Average" = list(
                                  geom_segment(aes(x = 2000, xend = 2018, y = 15, yend = 15), color = "red", size = 1.3, linetype = "dashed"),
                                  scale_x_continuous(breaks = seq(2000, 2022, by = 1)),
                                  annotate("text", x = c(2013), y = c(14), label = c("Annual NAAQS = 15 µg/m^3"),
                                           family = "", fontface = 3, size = 4)
                                )
    )
    return(additional_layers)
  }
  
  # Modify your renderPlot section to call the function
  output$plot1 <- renderPlot({
    req(input$pollutant)
    req(input$station_input)
    
    # Get the county name
    county_name <- datasub3()$County
    
    # Get the station name(s)
    station_name <- input$station_input
    
    # Check if there is more than 1 station selected
    if (length(station_name) > 1) {
      title_text  <- paste(county_name, " County ", input$pollutant, " Trend\n", get_naaqs_text(input$pollutant))
    } else {
      title_text  <- paste(station_name, input$pollutant, " Trend\n", get_naaqs_text(input$pollutant))
    }
    
    generate_plot(datasub3(), input$pollutant,county_name ,title_text , input$station_input)
  })
  
  
  
  ###############################################################################
  # Download button to save plots from app
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # Get the selected pollutant
      pollutant <- input$pollutant
      
      # Get the selected sites or county
      selected_locations <- if (length(input$station_input) == 1) {
        input$station_input
      } else {
        input$county_input
      }
      
      # Create a dynamic filename
      filename <- paste("plot_", pollutant, "_", paste(selected_locations, collapse = "_"),
                        ".pdf", sep = "")
      
      return(filename)
    },
    content = function(file) {
      ggsave(file, device = "pdf", width = 11.5, height = 8)
    }
  )
  
  # Download handler for the entire dataset
  output$downloaddata <- downloadHandler(
    filename = function() {
      "complete_dataset.csv"  # Adjust filename as needed
    },
    content = function(file) {
      write.csv(criteriap, file)
    }
  )
  
  
}
###############################################################################
# Run the application 
shinyApp(ui = ui, server = server)
