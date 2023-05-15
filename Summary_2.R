### install.packages ----
install.packages("yaml")
install.packages("shiny")
install.packages("DT")
#Read a JSON file

### Load the package required to read JSON files. ----
##library(jsonlite)

library(rjson)
library(yaml) 
library(tidyverse)
library(tidytext)

### Read files ----
##read file path from yaml
##load data from json

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

df3 <- diagnistics_counts_list_all[[1]]
df4 <- df3[[2]] #conditions hal

#comparision in tibble
testing_comparision <- tibble(df4)
colnames(testing_comparision) <- c('text','count')

testing_comparision$text

#comparision based on length example
testing_comparision <- df4[,1]
length(testing_comparision)
comp <- sapply(testing_comparision,testF <- function(x) {sapply(strsplit(x, " "), length)} )
comp <- tibble(comp)

#comparision based on length example
testing_comparision <- df4[,1]
length(testing_comparision)
comp <- sapply(testing_comparision,testF <- function(x) {sapply(strsplit(x, " "), length)} )
comp <- tibble(comp)

#Add a new column to the tibble

comp <- comp %>% 
  mutate(lenght_values = length())


length('asd asd asd asd')

### testing ----

ldf_chunk <- ldf[[2]]$validation$Condition$issues

df <- do.call(rbind, as.list(sapply(ldf_chunk, testF <- function(x) x$diagnostics)))