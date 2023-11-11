#
# This is a shiny app to display 
# the results of hours speed tests on the Cox network.
#

library(shiny)
library(DT)
library(parsedate)
library(lubridate)

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

  speed_df$test_date_gmt <- as_datetime(speed_df$date, 
                                    format = "%Y-%m-%dT%H:%M:%S%z",
                                    tz= "US/Central")
  speed_df$test_date_cst <- with_tz(speed_df$test_date_gmt, 
                                    "US/Central")
  #speed_df$test_date_3 <- with_tz(
  #  as.POSIXct(speed_df$date, 
  #             format = "%Y-%m-%dT%H:%M:%S%z",
  #             tz = "US/Central")
  #)
  print(speed_df)
  speed_dt <-
    datatable(
      speed_df,
      #selection = list(mode = "single", target = "row", selected = previousSelection),
      
      rownames = TRUE,
      escape = FALSE
    ) #%>% formatDate()
  
  output$speed_dt = DT::renderDT(speed_dt, server = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
