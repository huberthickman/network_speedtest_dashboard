#
# This is a shiny app to display
# the results of speed tests on the Cox network.
#

library(shiny)
library(DT)
library(lubridate)
library(plotly)
library(shinytitle)
library(vroom)
library(bslib)


ui <- fluidPage(
  title = "Network Speed Tests",
  use_shiny_title(),
  theme = bslib::bs_theme(version = 5),
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
      })),
      br(),
      fluidRow(
        column(3, offset=1, {
        actionButton("go_back", "Previous 30 days", icon = icon("arrow-left", lib="glyphicon")) 
        })
        ,
        column(3, offset=1, {
          actionButton("go_forward", "Next 30 days", icon = icon("arrow-right", lib="glyphicon")) 
        })
     )
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
  myVals <- reactiveValues()
  myVals$max_days_offset <- 0

  #Force to Central Time, other users should change to the local time zone for
  #deployment to shinyapps.io.
  
  Sys.setenv(TZ = "US/Central")
  print(paste("Speedtest initializing at " , as.POSIXlt(Sys.time())))
  speed_tibble <- vroom(config$fileshare, .name_repair = 'minimal')
  
  speed_df_unsorted <- as.data.frame(speed_tibble)
  
  speed_df_unsorted$test_date_cst <- with_tz(
    as.POSIXct(speed_df_unsorted$date,
               format = "%Y-%m-%dT%H:%M:%S%z",
               tz = "US/Central")
  )
  speed_df <-
    speed_df_unsorted[order(speed_df_unsorted$test_date_cst, decreasing = TRUE),]

  #convert the upload and download speeds
  
  speed_df$converted_download <- round(speed_df$download / 125000, 0)
  speed_df$converted_upload <- round(speed_df$upload / 125000, 0)
  
  min_download <- min(speed_df$converted_download,  na.rm = TRUE)
  min_upload <- min(speed_df$converted_upload, na.rm = TRUE)
  min_date <- min(speed_df$date, na.rm = TRUE)
  max_date <- max(speed_df$date, na.rm = TRUE)
  
  output$min_download_text <-
    renderText({
      paste("Minimum logged download speed:", min_download, 'Mbps')
    })
  output$min_upload_text <-
    renderText({
      paste("Minimum logged upload speed:", min_upload, 'Mbps')
    })
  print(paste0("Min date is ", min_date, " max date is ", max_date))
  
  # add in a days from max column so we can page through the data
  # easily 
  
  speed_df$days_from_max <- as.double(difftime(max_date, speed_df$date, units = c("days")))
  myVals$max_days_offset <- 0
  myVals$absolute_max_days <- max(speed_df$days_from_max)

  observe({
    myVals$speed_df_filtered <- speed_df[speed_df$days_from_max >= myVals$max_days_offset & speed_df$days_from_max<= myVals$max_days_offset+30,]
    myVals$min_date <- min(myVals$speed_df_filtered$date, na.rm = TRUE)
    myVals$max_date <- max(myVals$speed_df_filtered$date, na.rm = TRUE)
  })
  
  observe({
  withProgress(message = "Reading data", value = 0.1 , {
    #tf = "./speedtest_results.csv"
    #speed_df_unsorted <- read.csv


    display_df <-
      myVals$speed_df_filtered[, c(
        "test_date_cst",
        "server name",
        "converted_download",
        "converted_upload",
        "share url"
      )]
    display_df$result_url <-
      paste(
        '<a href=',
        myVals$speed_df_filtered$share.url,
        ' target=\"_blank\">',
        myVals$speed_df_filtered$share.url,
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
    p <- plot_ly(speed_df, x = myVals$speed_df_filtered$test_date_cst, mode = 'lines')
    incProgress(0.7, message='Creating plot')
    p <-
      p %>% add_trace(
        y = myVals$speed_df_filtered$converted_download,
        name = "Download",
        mode = 'lines+markers',
        type = 'scatter'
      )
    incProgress(0.8, message='Creating plot')
    p <-
      p %>% add_trace(
        y = myVals$speed_df_filtered$converted_upload,
        name = "Upload",
        mode = 'lines+markers',
        type = 'scatter'
      )
    p <- p %>% layout(
      title = "Upload and Download Speeds",
      xaxis = list(
        title = "Test Date/Time",
        range = c( myVals$max_date - as.difftime(7, unit="days") , myVals$max_date),
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
            label = "7 days",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 30,
            label = "30 days",
            step = "day",
            stepmode = "backward"
          )
          #,
          #list(step = "all")
        ))

        ,
        range = c({
          dt = max(myVals$speed_df_filtered$test_date_cst)
          lubridate::day(dt) = lubridate::day(dt) - 2
          dt
        }
        , max(myVals$speed_df_filtered$test_date_cst)
        )
      ),
      yaxis = list (title = "Mbps")
    )


    output$ts_plot <- renderPlotly(p)
    myVals$display_df <- display_df
  })  # end with progress
  
  })
  
  output$download_button <- shiny::downloadHandler(
    filename = paste0("speed_test_data-", Sys.Date(), ".csv"),
   content = function(file_path)
   {
     write.csv(myVals$display_df[-c(6)], file_path, row.names = FALSE)
   }
 )
  
  
  observeEvent(input$go_back, {
    myVals$max_days_offset <- min(myVals$absolute_max_days, myVals$max_days_offset + 30)
  })  
  
  observeEvent(input$go_forward, {
    myVals$max_days_offset <- max(myVals$max_days_offset - 30,0)
  })  
  
}

# Run the application
shinyApp(ui = ui, server = server)
