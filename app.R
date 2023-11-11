#
# This is a shiny app to display 
# the results of speed tests on the Cox network.
#

library(shiny)
library(DT)
library(parsedate)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinytitle)


ui <- fluidPage(
    title = "Network Speed Tests",
    use_shiny_title(),
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

server <- function(input, output, session) {
  
  #
  #the fileshare name is in the config default profile
  #
  config <- config::get()
  
  download.file(config$fileshare, 
                "sp.csv", quiet = TRUE, mode = "w",
                cacheOK = TRUE)
  # TODO: write to tmp file so it will work on shinyapps
  # TODO: move to reactives with an update button

  speed_df_unsorted <- read.csv("sp.csv")
  
  speed_df_unsorted$test_date_cst <- with_tz(
    as.POSIXct(speed_df_unsorted$date, 
               format = "%Y-%m-%dT%H:%M:%S%z",
               tz = "US/Central")
  )
  speed_df <- speed_df_unsorted[order(speed_df_unsorted$test_date_cst, decreasing = TRUE), ]
  
  speed_df$converted_download <- round(speed_df$download/125000, 0)
  speed_df$converted_upload <- round(speed_df$upload/125000,0)
  print(speed_df)
  
  display_df <- speed_df[, c("test_date_cst", "server.name", "converted_download", "converted_upload", "share.url")]
  display_df$result_url <- paste('<a href=', display_df$share.url, ' target=\"_blank\">',  display_df$share.url, '</a>'
                                 , sep='')
  download_df <- speed_df[, c("test_date_cst", "converted_download")]
  download_df$type <- "Download"
  colnames(download_df) <- c("test_date_cst", "speed", "type")
  
  upload_df <- speed_df[, c("test_date_cst", "converted_upload")]
  upload_df$type <- "Upload"
  colnames(upload_df) <- c("test_date_cst", "speed", "type")
  
  plot_df <- union(download_df, upload_df)
  
  output$speed_dt <- DT::renderDT({datatable(
    display_df,
    rownames = FALSE,
    colnames = c("Test Date/Time (CST)", "Server", "Download Speed", "Upload Speed", "Raw Url", "Results URL"),
    escape = FALSE,
    options = list(
    columnDefs = list(
      # Initially hidden columns
      list(
        visible = FALSE,
        targets = c(4)
        
      ) )
    )
  )  %>% formatDate(c(1), "toLocaleString")}, 
  server = TRUE)

  gg <- ggplot(plot_df,
                aes(x = test_date_cst, y=speed, group=type , color=type))  +
     geom_line() +
     geom_point() +
     ylim(0, 1200) +
     labs(y="Mpbs",x="Test Date/Time", title = "Download and Upload Speeds") 

  
  p <- plotly_build(gg)
  output$ts_plot <- renderPlotly(p)
  

}

# Run the application 
shinyApp(ui = ui, server = server)
