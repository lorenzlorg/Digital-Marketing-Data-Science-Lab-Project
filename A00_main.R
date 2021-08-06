#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####

# Package names
packages <- c("dplyr",  "ggplot2", "forcats", "lubridate", "RQuantLib", "devtools",
              "data.table", "corrplot", "rpart", "rpart.plot", "MLmetrics", "caret",
              "rattle", "randomForest", "ROCR", "nnet", "e1071",
              "factoextra", "NbClust", "ggpubr", "cluster", 
              "dbscan", "rfm", "kableExtra", "scales") 


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Load package with potential compatibility issues
install_github("nicolasturaro/mapIT")
library(mapIT)

#### DIRECTORIES ####
working_dir = "~/Desktop/progetto digital/progetto_digital_marketing"  # <path for scripts directory>

data_dir = "/Users/lorenzolorgna/Google Drive/Progetto digital marketing/DMktg_DSLab_Data"  # <path for datasets directory>

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
PIPELINE_scripts <- c(
  'B01_ingestion.R'
  , 'C01_preparation_df1.R'
  , 'C02_preparation_df2.R'
  , 'C03_preparation_df3.R'
  , 'C04_preparation_df4.R'
  , 'C05_preparation_df5.R'
  , 'C06_preparation_df6.R'
  , 'C07_preparation_df7.R'
  , 'D01_churn.R'
  , 'D02_rfm.R'
  , 'D03_clustering.R'
)

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}