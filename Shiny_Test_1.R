#1. Install Packages-----
#install.packages("jsonlite")
#install.packages("yaml")
#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("DT")
#install.packages("tibble")
#install.packages("dplyr")

#2. Library R Packages -----
library(shiny)
library(shinyWidgets)
library(jsonlite)
library(yaml)
library(tibble)
library(dplyr)

config <- yaml.load_file("config.yaml")

filenames <- list.files(paste0(getwd(),config$path), pattern = "*.json", full.names = TRUE) 
json_files_list_all <- lapply(filenames, fromJSONfile <- function(x) rjson::fromJSON(file = x))

#error and general omitted bcs of missing counts
target_names <- c('Condition','Medication','MedicationAdministration','MedicationStatment','Procedure','Specimen')

#### Problems with inserting the col-names "Diagnostics" -> not displayed when executing the script----
get_diagnostics <- function(df, lable) {
  
  results_list <- list()
  
  for(l in lable) { 
    df_chunk <- df$validation[[l]]$issues
    r <- do.call(rbind, as.list(lapply(df_chunk, get_text_count <- function(x) { cbind(x$diagnostics, x$count) } ))) 
    r <- tibble(r)
    colnames(r) <- c( paste("diagnostics_",l), "counts")
    results_list <- append(results_list, r)
  }
  
  return(results_list)
}


get_diagnostics_from_list <- function(ldf, lable) {
  r <-  (lapply(ldf, call_diagnostics_with_lable <- function(x) {get_diagnostics(x,lable)} ))
  return(r)
}

diagnistics_counts_list_all <- get_diagnostics_from_list(json_files_list_all, target_names)

# Umbenennen der Spalten
df3 <- diagnistics_counts_list_all[[1]]
df4 <- df3[[2]] 
colnames(df4) <- c("Errorcode", "Häufigkeit")

####
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .error-box {
        background-color: #c2e6c2;
        border: 1px solid #a3c2a3;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 10px;
        color: #333;
      }
      body {
        background-color: #edf4ff;
      }
    "))
  ),
  fluidRow(
    column(
      width = 4,
      style = "overflow-y: auto;",
      lapply(1:nrow(df4), function(i) {
        div(
          class = "error-box",
          tags$h4(paste0("Fehler-Meldung ", i)),
          tags$h5(style = "margin-top: 0;", "Fehlercode: ", df4[i, "Errorcode"]),
          tags$h5("Häufigkeit: ", df4[i, "Häufigkeit"])
        )
      })
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)




