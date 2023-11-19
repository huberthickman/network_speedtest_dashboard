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
  titlePanel(h3("Network Speed Tests", align = "center")),
  
  tabsetPanel(
    id = "speed_notebooktabs",
    type = "tabs",
    tabPanel(
      "Speed Plot",
      br(),
      plotlyOutput("ts_plot"),
      hr(),
      h6(
        "All test results were obtained directly connected to the Panoramic modem via ethernet."
      ),
      br(),
      fluidRow(column(6, offset = 1, {
        textOutput("min_download_text", inline = TRUE)
      }),
      column(5, offset = 0, {
        textOutput("min_upload_text", inline = TRUE)
      }))
    ),
    tabPanel(
      "Data",
      DT::dataTableOutput("speed_dt"),
      shiny::downloadButton(outputId = "download_button",
                            label = "Download Speed Data")
    )
  )
  
)

server <- function(input, output, session) {
  #
  #the fileshare name is in the config default profile
  #
  config <- config::get()
  #print(Sys.getenv())
  
  #Force to Central Time, other users should change to the local time zone for
  #deployment to shinyapps.io.
  
  Sys.setenv(TZ = "US/Central")
  print(paste("Speedtest initializing at " , as.POSIXlt(Sys.time())))
  
  withProgress(message = "Reading data", value = 0.1 , {
    tf <-
      tempfile(pattern = "speedtestcsvdata",
               tmpdir = tempdir(),
               fileext = ".csv")
    # print(tf)
    download.file(
      config$fileshare,
      tf,
      quiet = TRUE,
      mode = "w",
      cacheOK = FALSE
    )
    # TODO: move to reactives with an update button
    
    
    
    speed_df_unsorted <- read.csv(tf)
    
    incProgress(0.2, message = "Transforming data")
    speed_df_unsorted$test_date_cst <- with_tz(
      as.POSIXct(speed_df_unsorted$date,
                 format = "%Y-%m-%dT%H:%M:%S%z",
                 tz = "US/Central")
    )
    speed_df <-
      speed_df_unsorted[order(speed_df_unsorted$test_date_cst, decreasing = TRUE),]
    
    speed_df$converted_download <- round(speed_df$download / 125000, 0)
    speed_df$converted_upload <- round(speed_df$upload / 125000, 0)
    min_download <- min(speed_df$converted_download,  na.rm = T)
    min_upload <- min(speed_df$converted_upload, na.rm = T)
    
    output$min_download_text <-
      renderText({
        paste("Minimum logged download speed:", min_download, 'Mbps')
      })
    output$min_upload_text <-
      renderText({
        paste("Minimum logged upload speed:", min_upload, 'Mbps')
      })
    
    display_df <-
      speed_df[, c(
        "test_date_cst",
        "server.name",
        "converted_download",
        "converted_upload",
        "share.url"
      )]
    display_df$result_url <-
      paste(
        '<a href=',
        display_df$share.url,
        ' target=\"_blank\">',
        display_df$share.url,
        '</a>'
        ,
        sep = ''
      )
    
    incProgress(0.4, message = "creating data table")
    output$speed_dt <- DT::renderDT({
      datatable(
        display_df,
        rownames = FALSE,
        colnames = c(
          "Test Date/Time (CST)",
          "Server",
          "Download Speed",
          "Upload Speed",
          "Raw Url",
          "Results URL"
        ),
        escape = FALSE,
        options = list(columnDefs = list(# Initially hidden columns
          list(
            visible = FALSE,
            targets = c(4)
            
          )))
      )  %>% formatDate(c(1), "toLocaleString")
    },
    server = TRUE)
    
    incProgress(0.6, message='Creating plot')
    p <- plot_ly(speed_df, x = speed_df$test_date_cst, mode = 'lines')
    incProgress(0.7, message='Creating plot')
    p <-
      p %>% add_trace(
        y = speed_df$converted_download,
        name = "Download",
        mode = 'lines+markers',
        type = 'scatter'
      )
    incProgress(0.8, message='Creating plot')
    p <-
      p %>% add_trace(
        y = speed_df$converted_upload,
        name = "Upload",
        mode = 'lines+markers',
        type = 'scatter'
      )
    p <- p %>% layout(
      title = "Upload and Download Speeds",
      xaxis = list(
        title = "Test Date/Time",
        rangeslider = list(type = "date"),
        
        rangeselector = list(buttons = list(
          list(
            count = 2,
            label = "2 days",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 7,
            label = "1 week",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 1,
            label = "1 month",
            step = "month",
            stepmode = "backward"
          ),
          list(step = "all")
        ))
        
        ,
        range = c({
          dt = max(speed_df$test_date_cst)
          lubridate::day(dt) = lubridate::day(dt) - 2
          dt
        }
        , max(speed_df$test_date_cst))
      ),
      yaxis = list (title = "Mbps")
    )
    
    
    output$ts_plot <- renderPlotly(p)
  })  # end with progress
  output$download_button <- shiny::downloadHandler(
    filename = paste0("speed_test_data-", Sys.Date(), ".csv"),
    content = function(file_path)
    {
      write.csv(display_df[-c(6)], file_path, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
