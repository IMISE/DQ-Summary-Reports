# Pakete laden
library(shiny)           # Shiny
library(jsonlite)        # jsonlite für JSON-Verarbeitung
library(ggplot2)         # ggplot2 für Datenvisualisierung
library(shinydashboard)  # shinydashboard für das Dashboard-Layout
library(shinyFiles)      # shinyFiles für Datei-Uploads
library(dplyr)           # dplyr für Datenmanipulation
library(stats)           # stats für statistische Funktionen

# Benutzeroberfläche definieren
ui <- dashboardPage(
  dashboardHeader(title = "DIZ Visualizer"),  # Dashboard-Titel
  
  dashboardSidebar(  # Seitenleiste im Dashboard
    sidebarMenu(  # Sidebar-Menü
      menuItem("Start Page", tabName = "start", icon = icon("home")),  # Menüpunkt "Startseite"
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),  # Menüpunkt "Visualisierung"
      menuItem("Combined Histogram", tabName = "combined", icon = icon("layer-group")),  # Menüpunkt "Kombiniertes Histogramm"
      menuItem("Statistics", tabName = "statistics", icon = icon("calculator"))  # Menüpunkt "Statistik"
    )
  ),
  
  dashboardBody(  # Hauptbereich des Dashboards
    tags$head(  # HTML-Tags im Head-Bereich
      tags$style(HTML("  # CSS-Stile definieren
        .box.box-solid.box-primary>.box-header {  # Stile für primäre Boxen
          background-color: #2E8B57;  # Hintergrundfarbe
        }
        .box.box-solid.box-primary {  # Rahmen für primäre Boxen
          border-color: #2E8B57;  # Rahmenfarbe
        }
        .box.box-solid.box-info>.box-header {  # Stile für Info-Boxen
          background-color: #8FBC8F;  # Hintergrundfarbe
        }
        .box.box-solid.box-info {  # Rahmen für Info-Boxen
          border-color: #8FBC8F;  # Rahmenfarbe
        }
        .box.box-solid.box-success>.box-header {  # Stile für Erfolgs-Boxen
          background-color: #2E8B57;  # Hintergrundfarbe
        }
        .box.box-solid.box-success {  # Rahmen für Erfolgs-Boxen
          border-color: #2E8B57;  # Rahmenfarbe
        }
      "))
    ),
    
    tabItems(  # Tabs im Dashboard definieren
      
      tabItem(tabName = "start",  # Tab "Startseite"
              fluidRow(  
                box(  # Box für Datei-Upload
                  title = "Generate JSON Files", status = "primary", solidHeader = TRUE,  # Titel, Status und Kopfzeile der Box
                  fileInput("jsonFiles", "Choose JSON File(s)", multiple = TRUE, accept = ".json"),  # Datei-Upload-Eingabe
                  width = 6  # Breite der Box
                ),
                box(  # Info-Box
                  title = "Information", status = "info", solidHeader = TRUE,  # Titel, Status und Kopfzeile der Box
                  "Please generate JSON files to visualize their data as histograms. The visualizations will be displayed on the next page.",  # Informationstext
                  width = 6  # Breite der Box
                )
              ),
              fluidRow(  # Zweite fließende Zeile für das Layout
                box(  # Box für generierte Dateien
                  title = "Generated Files", status = "primary", solidHeader = TRUE,  # Titel, Status und Kopfzeile der Box
                  tableOutput("uploadedFiles"),  # Tabellenausgabe für hochgeladene Dateien
                  width = 12  # Breite der Box
                )
              )
      ),
      
      tabItem(tabName = "visualization",  # Tab "Visualisierung"
              fluidRow(  # Fließende Zeile für das Layout
                jqui_sortable(  # Sortierbare Elemente
                  div(  # DIV-Container
                    id = "plots",  # ID für den Container
                    lapply(1:6, function(i) {  # Lapply für 6 Elemente
                      box(  # Box für jedes Element
                        title = div(style = "display: flex; justify-content: space-between;",  # Titel-Stil
                                    uiOutput(paste0("file_name_", i)),  # UI-Ausgabe für Dateiname
                                    checkboxInput(paste0("checkbox_", i), label = NULL, value = TRUE)  # Checkbox-Eingabe
                        ), 
                        width = 4, solidHeader = TRUE, status = "success",  # Breite, Kopfzeile und Status der Box
                        uiOutput(paste0("plotUI_", i))  # UI-Ausgabe für Plot
                      )
                    })
                  )
                )
              )
      ),
      
      tabItem(tabName = "combined",  # Tab "Kombiniertes Histogramm"
              fluidRow(  # Fließende Zeile für das Layout
                box(  # Box für kombiniertes Histogramm
                  title = "Combined Histogram", status = "primary", solidHeader = TRUE, width = 12,  # Titel, Status, Kopfzeile und Breite der Box
                  plotOutput("combinedPlot"),  # Plot-Ausgabe für kombiniertes Histogramm
                  checkboxGroupInput("selectedPlots", "Select Histograms to Display", choices = NULL),  # Checkbox-Gruppen-Eingabe für ausgewählte Histogramme
                  actionButton("combine", "Combine Selected Histograms")  # Aktionsbutton für Kombination der ausgewählten Histogramme
                )
              )
      ),
      
      tabItem(tabName = "statistics",  # Tab "Statistik"
              fluidRow(  # Fließende Zeile für das Layout
                box(  # Box für Statistiken
                  title = "Statistics", status = "primary", solidHeader = TRUE, width = 12,  # Titel, Status, Kopfzeile und Breite der Box
                  tableOutput("statisticsTable")  # Tabellenausgabe für Statistik
                )
              )
      )
    )
  )
)

# Server definieren
server <- function(input, output, session) {
  jsonData <- reactiveVal(list())  # Variable für JSON-Daten initialisieren
  
  # Event-Handler für Datei-Upload
  observeEvent(input$jsonFiles, {
    files <- input$jsonFiles
    dataList <- list()
    
    for (i in 1:length(files$name)) {  # Schleife jede hochgeladene Datei
      tryCatch({
        fileContent <- readLines(files$datapath[i], warn = FALSE)  # Dateiinhalt lesen
        if (length(fileContent) == 0) stop("Datei ist leer oder nicht korrekt formatiert")  # Fehler bei leerer Datei
        
        fileData <- fromJSON(paste(fileContent, collapse = ""))  # JSON-Daten parsen
        if (!is.list(fileData$Histogram)) stop("Histogrammdaten haben ein unerwartetes Format")  # Fehler bei unerwartetem Format
        
        dataList[[files$name[i]]] <- fileData$Histogram  # Daten in Liste speichern
      }, error = function(e) {
        showNotification(paste("Fehler in Datei:", files$name[i], ":", e$message), type = "error")  # Fehlermeldung anzeigen
      })
    }
    
    jsonData(dataList)  # JSON-Daten in Variable speichern
    updateCheckboxGroupInput(session, "selectedPlots", choices = names(dataList))  # Checkbox-Auswahl aktualisieren
    
    # Dynamisch UI für Plot-Namen aktualisieren
    lapply(1:6, function(i) {
      output[[paste0("file_name_", i)]] <- renderUI({
        plots <- jsonData()
        plotNames <- names(plots)
        if (length(plotNames) >= i) {
          div(style = "display: flex; justify-content: space-between;",
              strong(plotNames[i]),
              checkboxInput(paste0("checkbox_", i), label = NULL, value = TRUE)
          )
        } else {
          NULL
        }
      })
    })
  })
  
  # Tabelle für hochgeladene Dateien rendern
  output$uploadedFiles <- renderTable({
    files <- names(jsonData())
    if (length(files) == 0) {
      return(data.frame(Files = "Noch keine Dateien generiert"))
    }
    data.frame(Files = files)
  })
  
  # Dynamisch UI für Plots rendern
  lapply(1:6, function(i) {
    output[[paste0("plotUI_", i)]] <- renderUI({
      plots <- jsonData()
      plotNames <- names(plots)
      if (length(plotNames) >= i && input[[paste0("checkbox_", i)]]) {
        plotOutput(outputId = paste0("plot_", plotNames[i]))  # Plot rendern, wenn Checkbox ausgewählt ist
      } else {
        "Keine Daten"
      }
    })
  })
  
  # Plots dynamisch rendern
  observe({
    plots <- jsonData()
    for (name in names(plots)) {  # Für jeden Plot
      local({
        plotName <- name
        output[[paste0("plot_", plotName)]] <- renderPlot({
          data <- plots[[plotName]]  # Daten für den aktuellen Plot erhalten
          
          categories <- sapply(data, function(x) x$Category$`@value`)  # Kategorien extrahieren
          counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))  # Zählwerte extrahieren
          
          df <- data.frame(Category = categories, Count = counts)  # Datenrahmen erstellen
          
          ggplot(df, aes(x = Category, y = Count)) +  # ggplot Histogramm erstellen
            geom_bar(stat = "identity") +
            ggtitle(plotName) +
            theme_minimal()
        })
      })
    }
  })
  
  # Event-Handler für die Kombination ausgewählter Histogramme
  observeEvent(input$combine, {
    selectedPlots <- input$selectedPlots
    plots <- jsonData()
    
    combinedData <- lapply(selectedPlots, function(name) {
      data <- plots[[name]]
      categories <- sapply(data, function(x) x$Category$`@value`)
      counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))
      data.frame(Category = categories, Count = counts, Quelle = name)
    }) %>%
      bind_rows() %>%
      group_by(Category, Quelle) %>%
      summarise(Count = sum(Count, na.rm = TRUE))  # Daten für gestapeltes Histogramm zusammenfassen
    
    output$combinedPlot <- renderPlot({  # Kombiniertes Histogramm rendern
      ggplot(combinedData, aes(x = Category, y = Count, fill = Quelle)) + 
        geom_bar(stat = "identity", position = "stack") +  # Gestapelte Balken verwenden
        ggtitle("Kombiniertes Histogramm") +
        theme_minimal()
    })
  })
  
  # Statistik-Tabelle rendern
  output$statisticsTable <- renderTable({
    plots <- jsonData()
    statsList <- lapply(names(plots), function(name) {
      data <- plots[[name]]
      counts <- as.numeric(sapply(data, function(x) x$Count$`@value`))
      
      # Plausibilitätsprüfung - > nur Beispiel
      plausible <- all(round(sum(counts), digits = 6) == 1)  # Beispiel: Summe der Counts sollte 1 sein
      
      # Ausreißererkennung (Beispiel: Zählwerte außerhalb von 3 Standardabweichungen)
      outliers <- boxplot.stats(counts)$out  # Beispiel: Ausreißer über boxplot erkennen
      
      # Clusteranalyse (Beispiel: Anzahl eindeutiger Kategorien)
      num_clusters <- length(unique(sapply(data, function(x) x$Category$`@value`)))  # Beispiel: Anzahl eindeutiger Kategorien
      
      data.frame(
        Datei = name,
        Mittelwert = mean(counts),
        Median = median(counts),
        Plausibilität = ifelse(plausible, "Ja", "Nein"),
        Anzahl_Ausreißer = length(outliers),
        Cluster_Anzahl = num_clusters,
        Minimum = min(counts),
        Maximum = max(counts)
      )
    })
    do.call(rbind, statsList)  # Statistikdatenrahmen zusammenfügen und zurückgeben
  })
}

# Shiny-App ausführen
shinyApp(ui, server)



