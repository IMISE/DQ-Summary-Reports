
# Packages
library(shiny)
library(jsonlite)
library(ggplot2)
library(shinydashboard)
library(shinyFiles)
library(shinyjqui)
library(dplyr)

# Define UI 
ui <- dashboardPage(
  dashboardHeader(title = "DIZ Visualizer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Start Page", tabName = "start", icon = icon("home")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Combined Histogram", tabName = "combined", icon = icon("layer-group")),
      menuItem("Statistics", tabName = "statistics", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box.box-solid.box-primary>.box-header {
          background-color: #2E8B57;
        }
        .box.box-solid.box-primary {
          border-color: #2E8B57;
        }
        .box.box-solid.box-info>.box-header {
          background-color: #8FBC8F;
        }
        .box.box-solid.box-info {
          border-color: #8FBC8F;
        }
        .box.box-solid.box-success>.box-header {
          background-color: #2E8B57;
        }
        .box.box-solid.box-success {
          border-color: #2E8B57;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "start",
              fluidRow(
                box(
                  title = "Generate JSON Files", status = "primary", solidHeader = TRUE,
                  fileInput("jsonFiles", "Choose JSON File(s)", multiple = TRUE, accept = ".json"),
                  width = 6
                ),
                box(
                  title = "Information", status = "info", solidHeader = TRUE,
                  "Please generate JSON files to visualize their data as histograms. The visualizations will be displayed on the next page.",
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Generated Files", status = "primary", solidHeader = TRUE,
                  tableOutput("uploadedFiles"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "visualization",
              fluidRow(
                jqui_sortable(
                  div(
                    id = "plots",
                    lapply(1:6, function(i) {
                      box(
                        title = div(style = "display: flex; justify-content: space-between;",
                                    paste("Plot", i),
                                    checkboxInput(paste0("checkbox_", i), label = NULL, value = TRUE)
                        ), 
                        width = 4, solidHeader = TRUE, status = "success",
                        uiOutput(paste0("plotUI_", i))
                      )
                    })
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Overlay Histogram", status = "primary", solidHeader = TRUE, width = 12,
                  plotOutput("overlayPlot"),
                  checkboxGroupInput("overlayPlots", "Select Histograms to Overlay", choices = NULL),
                  selectInput("basePlot", "Select Base Histogram", choices = NULL),
                  sliderInput("transparency", "Set Transparency", min = 0, max = 1, value = 0.5, step = 0.1),
                  actionButton("overlay", "Overlay Selected Histograms")
                )
              )
      ),
      tabItem(tabName = "combined",
              fluidRow(
                box(
                  title = "Combined Histogram", status = "primary", solidHeader = TRUE, width = 12,
                  plotOutput("combinedPlot"),
                  checkboxGroupInput("selectedPlots", "Select Histograms to Display", choices = NULL),
                  actionButton("combine", "Combine Selected Histograms")
                )
              )
      ),
      tabItem(tabName = "statistics",
              fluidRow(
                box(
                  title = "Statistics", status = "primary", solidHeader = TRUE, width = 12,
                  tableOutput("statisticsTable")
                )
              )
      )
    )
  )
)

# Define server 
server <- function(input, output, session) {
  jsonData <- reactiveVal(list())
  
  observeEvent(input$jsonFiles, {
    files <- input$jsonFiles
    dataList <- jsonData()
    
    for (i in 1:nrow(files)) {
      tryCatch({
        fileContent <- readLines(files$datapath[i], warn = FALSE)
        if (length(fileContent) == 0) stop("File is empty or not properly formatted")
        
        fileData <- fromJSON(paste(fileContent, collapse = ""))
        if (!is.list(fileData$Histogram)) stop("Histogram data is not in expected format")
        
        dataList[[files$name[i]]] <- fileData$Histogram
      }, error = function(e) {
        showNotification(paste("Error in file:", files$name[i], ":", e$message), type = "error")
      })
    }
    
    jsonData(dataList)
    updateCheckboxGroupInput(session, "selectedPlots", choices = names(dataList))
    updateCheckboxGroupInput(session, "overlayPlots", choices = names(dataList))
    updateSelectInput(session, "basePlot", choices = names(dataList))
  })
  
  output$uploadedFiles <- renderTable({
    files <- names(jsonData())
    if (length(files) == 0) {
      return(data.frame(Files = "No files generated yet"))
    }
    data.frame(Files = files)
  })
  
  lapply(1:6, function(i) {
    output[[paste0("plotUI_", i)]] <- renderUI({
      plots <- jsonData()
      plotNames <- names(plots)
      if (length(plotNames) >= i && input[[paste0("checkbox_", i)]]) {
        plotOutput(outputId = paste0("plot_", plotNames[i]))
      } else {
        "No data"
      }
    })
  })
  
  observe({
    plots <- jsonData()
    for (name in names(plots)) {
      local({
        plotName <- name
        output[[paste0("plot_", plotName)]] <- renderPlot({
          data <- plots[[plotName]]
          
          categories <- sapply(data, function(x) x$Category$`@value`)
          counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))
          
          df <- data.frame(Category = categories, Count = counts)
          
          ggplot(df, aes(x = Category, y = Count)) + 
            geom_bar(stat = "identity") +
            ggtitle(plotName) +
            theme_minimal()
        })
      })
    }
  })
  
  observeEvent(input$combine, {
    selectedPlots <- input$selectedPlots
    plots <- jsonData()
    combinedData <- do.call(rbind, lapply(selectedPlots, function(name) {
      data <- plots[[name]]
      categories <- sapply(data, function(x) x$Category$`@value`)
      counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))
      df <- data.frame(Category = categories, Count = counts, Source = name)
      return(df)
    }))
    
    output$combinedPlot <- renderPlot({
      ggplot(combinedData, aes(x = Category, y = Count, fill = Source)) + 
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Combined Histogram") +
        theme_minimal()
    })
  })
  
  observeEvent(input$overlay, {
    basePlot <- input$basePlot
    overlayPlots <- input$overlayPlots
    transparency <- input$transparency
    plots <- jsonData()
    
    if (basePlot == "") return()
    
    baseData <- plots[[basePlot]]
    baseCategories <- sapply(baseData, function(x) x$Category$`@value`)
    baseCounts <- as.numeric(sapply(baseData, function(x) x$Count$`@value`))
    baseDf <- data.frame(Category = baseCategories, Count = baseCounts, Source = basePlot)
    
    overlayData <- do.call(rbind, lapply(overlayPlots, function(name) {
      data <- plots[[name]]
      categories <- sapply(data, function(x) x$Category$`@value`)
      counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))
      df <- data.frame(Category = categories, Count = counts, Source = name)
      return(df)
    }))
    
    output$overlayPlot <- renderPlot({
      ggplot() +
        geom_bar(data = baseDf, aes(x = Category, y = Count, fill = Source), stat = "identity") +
        geom_bar(data = overlayData, aes(x = Category, y = Count, fill = Source), stat = "identity", alpha = transparency) +
        ggtitle("Overlay Histogram") +
        theme_minimal()
    })
  })
  
  output$statisticsTable <- renderTable({
    plots <- jsonData()
    statsList <- lapply(names(plots), function(name) {
      data <- plots[[name]]
      counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))
      data.frame(
        File = name,
        Median = median(counts),
        Number_of_Probands = length(counts)
      )
    })
    do.call(rbind, statsList)
  })
}

# Run the application 
shinyApp(ui, server)
