# The data contains info about account of each customer:
# ID_CLI: identify client (key)
# EMAIL_PROVIDER: email account provider
# W_PHONE: phone number added (0/1)
# ID_ADDRESS: identify address (foreign key)
# TYP_CLI_ACCOUNT: type client account
# TYP_JOB: client job

#### INSIGHTS ####
# Non ci sono duplicati
# Ci sono 8087 ID_ADDRESS identici 
# Ci sono ben 20512 email providers diversi (il più utilizzato risulta essere gmail.com)
# Il 92% circa dei clienti ha aggiunto il proprio numero di telefono
# 6974 clienti hanno lo stesso ID_ADDRESS (900091) che non risulta essere mappato in df3
# Il 90% dei clienti ha un account di tipo 4
# Il 97% dei clienti (ovvero 360810 clienti) non ha inserito il tipo di lavoro. La tipologia più frequente risulta essere "Libero professionista"


#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### CLEANING DUPLICATE VALUES in df_2 ####

## check for duplicates
df_2_cli_account_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

# come osservato nello script relativo a df_1 vi sono 369472 ID_CLI univoci
#!!! NOTE:  no duplicates !!!#

# concentradosi su ID_ADDRESS
sum(n_distinct(df_2_cli_account_clean$ID_ADDRESS))  # 361385 
sum(duplicated(df_2_cli_account_clean$ID_ADDRESS))  # 8087
df_2_cli_account_clean$ID_ADDRESS[duplicated(df_2_cli_account_clean$ID_ADDRESS)]  # per identificare i duplicati
# perciò, tenendo conto anche della presenza di duplicati, per ogni cliente non vi è uno specifico id_address

#### CLEANING DATA TYPES in df_2 ####

## format boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## format numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0")) 
# in questo i valori <NA> sono stati sostituiti con 0

## MISSING VALUES mapped as new level in categorical columns ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))
# in questo i valori <NA> sono stati sostituiti con "(missing)"

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders
# ci sono ben 20512 email providers diversi. Si procederà successivamente nel considerare solo quelli più frequenti

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#


### Variable W_PHONE ###

## compute distribution
df_2_dist_w_phone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>% 
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_w_phone
# il 92% circa dei clienti ha aggiunto il proprio numero di telefono


### Variable ID_ADDRESS ###
df_2_dist_id_address <- df_2_cli_account_clean %>%
  group_by(ID_ADDRESS) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_id_address
# 6974 clienti hanno lo stesso ID_ADDRESS (900091) che risulta non essere mappato in df3
# vi sono parecchi indirizzi a cui sono associati 2 clienti


### Variable TYP_CLI_ACCOUNT ###
df_2_dist_typ_cli_account <- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_typ_cli_account
# si distinguono due tipologie di account, 4 e 2. Il 90% dei clienti ha un account di tipo 4

### Variable TYP_JOB ###
df_2_dist_typ_job <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_typ_job
# il 97% dei clienti (ovvero 360810 clienti) non ha inserito il tipo di lavoro. 
# La tipologia più frequente risulta essere "Libero professionista", con 3970 
# clienti per la precisione. Ad ogni modo per via della presenza di troppi valori
# mancanti tale variabile non è molto rilevante

#### RESHAPING df_2 ####

## keep the most frequent EMAIL_PROVIDER values and add a common factor level "OTHER" for the remaining ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

## add clean EMAIL_PROVIDER ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_emailproviderclean

## plot distribution
plot_df2_dist_emailproviderclean <- (
  ggplot(data=df2_dist_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="turquoise3") +
    theme_minimal() + 
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    xlab("Email provider") + ylab("Number of clients")
)

plot_df2_dist_emailproviderclean
# l'email provider più utilizzato risulta essere gmail.com

# EXPLORE the remaining df_2_cli_account_clean relevant variables

### Variable W_PHONE ###
plot_df2_dist_w_phone <- (
  ggplot(data=df_2_dist_w_phone
         , aes(x=W_PHONE, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="turquoise3") +
    theme_classic() + 
    xlab("Phone number") + ylab("Number of clients") +
    scale_x_discrete(labels=c("0" = "not added", "1" = "added")) +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25)
  )

plot_df2_dist_w_phone

### Variable TYP_CLI_ACCOUNT ###
plot_df2_dist_typ_cli_account <- (
  ggplot(data=df_2_dist_typ_cli_account
         , aes(x=TYP_CLI_ACCOUNT, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="turquoise3") +
    theme_classic() + 
    xlab("Type account") + ylab("Number of clients") +
    scale_x_discrete(labels=c("2" = "2", "4" = "4")) +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25)
)

plot_df2_dist_typ_cli_account

### Variable TYP_JOB ###
plot_df2_dist_typ_job <- (
  ggplot(data=df_2_dist_typ_job
         , aes(x=TYP_JOB, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="turquoise3") +
    theme_classic() + 
    xlab("Type job") + ylab("Number of clients") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25)
)

plot_df2_dist_typ_job

# per visualizzare meglio i dati si considera una scala di tipo logaritmica (rispetto all'asse y)
plot_df2_dist_typ_job <- (
  ggplot(data=df_2_dist_typ_job
         , aes(x=TYP_JOB, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="turquoise3") +
    theme_classic() + 
    xlab("Type job") + ylab("Number of clients") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(trans='log2')
)

plot_df2_dist_typ_job

#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)
