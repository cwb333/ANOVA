require(shiny)
require(dplyr)
require(tools)
require(DBI)
require(DT)
require(shinyjs)
require(utils)
require(httr)
require(reshape2)
require(data.table)
require(FactoMineR)
require(factoextra)

source("aNoVa.R")
source("tUkEy.R")

ui <- shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    title = "",
    windowTitle = "Statistics Gateway",
    navbarPage(
      position = 'static-top',
      img(
        src = "acacia edited.svg",
        height = "100px",
        width = "280px",
        style = " margin-left: 25px;
                             margin-right: 160px; margin-top: -5px;"
      ),
      tabPanel(
        h3("ANOVA Gateway"),
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #262b2e; color: #ffffff",
            tags$head(tags$style(
              ".progress-bar{background-color:#660000;}"
            )),
            tags$head(tags$style(".btn{background-color:#660000;}")),
            tags$head(tags$style(
              ".btn:hover{background-color:#34000d;}"
            )),
            titlePanel("ANOVA"),
            selectizeInput('IV', 'Select Number of Independent Variables', 
                                   choices = c("", 1:5), multiple = F),
            fileInput(
              "newfile",
              "Upload Data",
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                "text/tab-separated-values",
                "text/plain",
                ".xlsx",
                ".csv",
                ".tsv"
              )
            ),
            tags$head(
              tags$style(
                ".predictNew{background-color:#660000;}
                                                                             .predictNew{color: white;}
                                                                             .predictNew{border: none;}
                                                                             .predictNew:hover{background-color: #34000d;}"
              )
            )
          ),
          mainPanel(tabPanel(' ', DT::dataTableOutput('table')))
        ),
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #262b2e; color: #ffffff",
            tags$head(tags$style(
              ".progress-bar{background-color:#660000;}"
            )),
            tags$head(tags$style(".btn{background-color:#660000;}")),
            tags$head(tags$style(
              ".btn:hover{background-color:#34000d;}"
            )),
            titlePanel("Tukey's Test"),
            downloadButton('download', 'Save Tukey Table'),
            tags$head(
              tags$style(
                ".predictNew{background-color:#660000;}
                                                                             .predictNew{color: white;}
                                                                             .predictNew{border: none;}
                                                                             .predictNew:hover{background-color: #34000d;}"
              )
            )
          ),
          mainPanel(tabPanel(' ', DT::dataTableOutput('table1')))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  newTable <- reactive({
    input$refresh
    invalidateLater(1000 * 60 * 5, session)
    
    startTime <- format(Sys.time(), '%b-%d, %I:%M')
    
    temp <- tempdir()
    
    if (is.null(input$newfile)) {
      return(NULL)
    }
    
    initData <-
      read.csv(input$newfile$datapath,
               header = T)
    
    aNoVa(initData, as.numeric(input$IV))
    
  })
  
  output$table <- DT::renderDataTable({
    datatable(newTable(), rownames = T)
  })
  
  newTable1 <- reactive({
    input$refresh
    invalidateLater(1000 * 60 * 5, session)
    
    startTime <- format(Sys.time(), '%b-%d, %I:%M')
    
    temp <- tempdir()
    
    if (is.null(input$newfile)) {
      return(NULL)
    }
    
    initData <-
      read.csv(input$newfile$datapath,
               header = T)
    
    
    tUkEy(initData, as.numeric(input$IV))
    
  })
  
  output$table1 <- DT::renderDataTable({
    datatable(newTable1(), rownames = T)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("Tukey Table",  " ", input$newfile, '/', Sys.time(), '.csv')
    },
    content = function(file) {
      write.csv(newTable1(), file, row.names = FALSE)
    }
  )
})

shinyApp(ui, server)