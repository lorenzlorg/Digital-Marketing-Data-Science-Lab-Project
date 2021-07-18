#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(devtools)
library(mapIT)
install_github("nicolasturaro/mapIT")

## add other libraries

#### DIRECTORIES ####
working_dir = "~/Desktop/progetto digital/progetto_digital_marketing"  # <path for scripts directory>
# working_dir = "E://Universit? Drive//Progetto digital marketing//DMktg_DSLab_R_Scripts"  # <path for scripts directory>

data_dir = "/Users/lorenzolorgna/Google Drive/Progetto digital marketing/DMktg_DSLab_Data"  # <path for datasets directory>
# data_dir = "E://Universit? Drive//Progetto digital marketing//DMktg_DSLab_Data"  # <path for datasets directory>

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
# Uncomment to execute the entire pipeline of scripts
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
  # , 'D02_rfm.R'
  # , 'D03_clustering'
  )

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}