#
# This is a shiny app to display 
# the results of hours speed tests on the Cox network.
#

library(shiny)
library(DT)
library(parsedate)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
#ggplotly()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h3("Network Speed Tests", align="center")),

    tabsetPanel(id = "speed_notebooktabs",
                type = "tabs",
                tabPanel("Speed Plot", 
                  plotlyOutput("ts_plot")
                ),
                tabPanel("Data", 
                  DT::dataTableOutput("speed_dt")
                )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  speed_df <- read.csv("/Users/hubert/bin/speedtest_results.csv")

  #speed_df$test_date_gmt <- as_datetime(speed_df$date, 
  #                                  format = "%Y-%m-%dT%H:%M:%S%z",
  #                                  tz= "US/Central")

  speed_df$test_date_cst <- with_tz(
    as.POSIXct(speed_df$date, 
               format = "%Y-%m-%dT%H:%M:%S%z",
               tz = "US/Central")
  )
  speed_df$converted_download <- round(speed_df$download/125000, 0)
  speed_df$converted_upload <- round(speed_df$upload/125000,0)
  print(speed_df)
  
  display_df <- speed_df[, c("test_date_cst", "server.name", "converted_download", "converted_upload")]
  
  download_df <- speed_df[, c("test_date_cst", "converted_download")]
  download_df$type <- "Download"
  
  output$speed_dt <- DT::renderDT({datatable(
    display_df,
    rownames = FALSE,
    colnames = c("Test Date/Time (CST)", "Server", "Download Speed", "Upload Speed"),
    escape = FALSE
  )  %>% formatDate(c(1), "toLocaleString")}, 
  server = TRUE)
  
  gg <- ggplot(speed_df, 
               aes(x = test_date_cst))  + 
    geom_line(aes(y=converted_download), color="steelblue") + 
    geom_line(aes(y=converted_upload), color="orange") + 
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%m/%d\n %H%M") +
    ylim(0, 1200) + 
    labs(y="Mpbs",x="Test Date/Time", title = "Download and Upload Speeds") +
    geom_point(aes( x=test_date_cst, y=converted_download), color='steelblue') + 
    geom_point(aes( x=test_date_cst, y=converted_upload), color='orange')
  
  p <- plotly_build(gg)
  output$ts_plot <- renderPlotly(p)
}

# Run the application 
shinyApp(ui = ui, server = server)
