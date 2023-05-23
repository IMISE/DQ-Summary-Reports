#1. Install Packages-----
#install.packages("jsonlite")
#install.packages("yaml")
#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("DT")
#install.packages("tibble")
#install.packages("dplyr")
install.packages("gridExtra")
install.packages("grid")

#2. Library R Packages -----
library(shiny)
library(shinyWidgets)
library(jsonlite)
library(yaml)
library(tibble)
library(dplyr)
library(jsonlite)
library(gridExtra)
library(grid)


# Laden der Konfigurationsdatei
config <- yaml.load_file("config.yaml")

# Liste aller JSON-Dateien im angegebenen Pfad
filenames <- list.files(paste0(getwd(), config$path), pattern = "*.json", full.names = TRUE)

# Laden der JSON-Dateien
json_files_list_all <- lapply(filenames, function(x) rjson::fromJSON(file = x))

# ateinamen für den Vergleich 
target_names <- c('Condition', 'Medication', 'MedicationAdministration', 'MedicationStatment', 'Procedure', 'Specimen')

# Funktion zum Vergleich der Diagnosedaten
compare_diagnostics <- function(df1, df2, label) {
  comparison_result <- data.frame()
  
  for (l in label) {
    diagnostics_col <- paste0("diagnostics_", l)
    counts_col <- "counts"
    
    # Vergleichen der ausgewählten Spalten zwischen df1 und df2
    comparison <- df1[[diagnostics_col]] == df2[[diagnostics_col]]
    
    # Ergebnisse 
    comparison_result <- cbind(comparison_result, comparison)
  }
  
  return(comparison_result)
}

# Vergleichen der Diagnosedaten für jeweils zwei Dateien nebeneinander
for (i in 1:(length(json_files_list_all) - 1)) {
  dfHalle <- json_files_list_all[[i]]
  dfLeipzig <- json_files_list_all[[i + 1]]
}



# Überprüfen der Anzahl der Zeilen in beiden Dataframes
if (nrow(dfHalle) != nrow(dfLeipzig)) {
  stop("Die Dataframes haben unterschiedliche Anzahlen von Zeilen.")
}

# Zusammenfügen der Dataframes nebeneinander in einem neuen DataFrame
combined_df <- cbind(dfHalle, dfLeipzig)
combined_df


  
  # Konvertiere die Dataframes in grobs
  #grobHalle <- tableGrob(dfHalle)
  #grobLeipzig <- tableGrob(dfLeipzig)
  
  # Zeige beide Dataframes nebeneinander an
  #grid.arrange(grobHalle, grobLeipzig, ncol = 2)
#}







 
