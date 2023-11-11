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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Network Speed Tests"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(
        width = 12,
        plotOutput("ts_plot")
      )
    ),
    fluidRow(
      column(
        width = 12,
        offset = 0,
        style = 'padding:10px;',
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
  
  output$speed_dt <- DT::renderDT({datatable(
    display_df,
    rownames = FALSE,
    escape = FALSE
  )  %>% formatDate(c(1), "toLocaleString")}, 
  server = TRUE)
  
  output$ts_plot <- renderPlot({

    ggplot(speed_df, 
           aes(x = test_date_cst, y= converted_download)) + geom_line() + 
      ylim(0, 1200) + labs(y="Download Mpbs",x="Test Date/Time") +
      scale_x_datetime(date_breaks = "2 hours", date_labels = "%m/%d %H")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
