#
# This is a shiny app to display 
# the results of hours speed tests on the Cox network.
#

library(shiny)
library(DT)
library(parsedate)
library(lubridate)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Network Speed Tests"),

    # Sidebar with a slider input for number of bins 
    
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
  speed_df$converted_download <- speed_df$download/125000
  speed_df$converted_upload <- speed_df$upload/125000
  print(speed_df)
  
  display_df <- speed_df[, c("test_date_cst", "server.name", "converted_download", "converted_upload")]
  
  output$speed_dt <- DT::renderDT({datatable(
    display_df,
    rownames = FALSE,
    escape = FALSE
  )  %>% formatDate(c(1), "toLocaleString")}, 
  server = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
