#### INGESTION df_1 customers fidelity subscriptions ####

df_1_cli_fid <- read.csv2(
  file.path(data_dir,"raw_1_cli_fid.csv")
  , na.strings = c("NA", "")
  )

#### INGESTION df_2 customers accounts details ####
df_2_cli_account <- read.csv2(
  file.path(data_dir,"raw_2_cli_account.csv")
  , na.strings = c("NA", "")
  )

#### INGESTION df_3 customers addresses ####
df_3_cli_address <- read.csv2(
  file.path(data_dir,"raw_3_cli_address.csv")
  , na.strings = c("")
  )

#### INGESTION df_4 customers privacy data ####
df_4_cli_privacy <- read.csv2(
  file.path(data_dir,"raw_4_cli_privacy.csv")
  , na.strings = c("NA", "")
  )

#### INGESTION df_5 email campaign descriptions ####
df_5_camp_cat <- read.csv2(
  file.path(data_dir,"raw_5_camp_cat.csv")
  , na.strings = c("NA", "")
  )

#### INGESTION df_6 email events ####
df_6_camp_event <- read.csv2(
  file.path(data_dir,"raw_6_camp_event.csv")
  , na.strings = c("NA", "")
  )

#### INGESTION df_7 purchase tickets ####
df_7_tic <- read.csv2(
  file.path(data_dir,"raw_7_tic.csv")
  , na.strings = c("NA", "")
  , stringsAsFactors = FALSE
  )