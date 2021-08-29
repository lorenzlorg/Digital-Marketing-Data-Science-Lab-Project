# The data contains info about address of each customer:
# ID_ADDRESS: identify address (key)
# CAP: postal code
# PRV: province
# REGION: region

#### INSIGHTS ####
# Ogni cliente non ha necessariamente un ID_ADDRESS diverso 
# Ci sono 850000 record che si ripetono, si provvede ad eliminarli
# Si eliminano i record mal strutturati
# I 3 CAP più frequenti sono 20090, 20900, 20060. 
# Le 3 province più frequenti sono MI, RM e NA. 
# Le 3 regioni più frequenti sono LOMBARDIA, LAZIO, CAMPANIA in accordo con le osservazioni fatte precedentemente

#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

# si calcola il numero esatto di duplicati considerando ID_ADDRESS
sum(duplicated(df_3_cli_address_clean$ID_ADDRESS))  # 850002

# si calcola il numero esatto di record duplicati
sum(duplicated(df_3_cli_address_clean))  # 850000

#!!! NOTE:  there are duplicates !!!#

# si procede all'eliminazione di 850000 record identici
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

# si calcola nuovamente il numero esatto di record duplicati
sum(duplicated(df_3_cli_address_clean))  # 0

# rimangono due ID_ADDRESS identici (come osservato precedentemente) che si 
# riferiscono a record differenti
sum(duplicated(df_3_cli_address_clean$ID_ADDRESS))
df_3_cli_address_clean$ID_ADDRESS[duplicated(df_3_cli_address_clean$ID_ADDRESS)]  # per identificare i duplicati in questione
# risultano essere gli indirizzi 201232 e 637914:
# 201232 18025 CN PIEMONTE
# 201232 18025 IM LIGURIA

# 637914 18025 CN PIEMONTE
# 637914 18025 IM LIGURIA

# si potrebbe eventualmente pensare di procedere nell'eliminazione di questi due record 
# dal momento in cui due id_address identici non possono riferirsi a due istanze diverse oppure
# si potrebbe contattare l'esperto di dominio per chiedere delucidazioni in merito
# df_3_cli_address_clean <- df_3_cli_address_clean[-c(110043, 110044, 162649, 162650), ]  # eliminazione duplicati id_address

# si procede dunque sul dataset privo di record identici

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

# vi sono diversi missing values, in alcuni casi i dati sono inseriti in modo errato dal cliente

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))
# si mantengono solo le righe del dataset dove si hanno valori validi per cap, provincia e regione
# vengono eliminate 23864 osservazioni

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

# vi sono 23919 indirizzi presenti in df_2 che non sono mappati in df_3
# ad esempio in df_2 è presente l'indirizzo 900091 che compare 6974 volte, ma non risulta essere mappato in df_3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

# df_3_cli_address_clean <- left_join(df_2_cli_account_clean, df_3_cli_address_clean , by = "ID_ADDRESS")
# si potrebbe pensare di eseguire un left join ma, allo stato attuale, non avendo 
# alcuna informazione aggiuntiva da integrare (dell'indirizzo 900091 non si ha nessun 
# dettaglio ad esempio) si è deciso di procedere ugualmente non considerando tali indirizzi non mappati

#### EXPLORE COLUMNS of df_3 ####

# EXPLORE the df_3_cli_address_clean relevant variables

### Variable CAP ###
df_3_dist_cap <- df_3_cli_address_clean %>%
  group_by(CAP) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

head(df_3_dist_cap)

df_3_dist_cap %>% 
  group_by(CAP) %>%
  summarise(number = n())
# i 3 CAP più frequenti sono 20090, 20900, 20060

### Variable PRV ###
df_3_dist_prv <- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

head(df_3_dist_prv)
# le 3 province piùà frequenti sono MI, RM e NA


### Variable REGION ###
df_3_dist_region <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%  
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_3_dist_region
# le 3 regioni più frequenti sono LOMBARDIA, LAZIO, CAMPANIA in accordo con le osservazioni fatte precedentemente

# mappa geografica
values = df_3_dist_region$TOT_ADDs  
id = df_3_dist_region$REGION
# mapIT(values = values, id = id, graphPar = list(guide.label = "Distribuzione ID_ADDRESS registrati"))  
# in questo caso non si sta considerando il numero di clienti totale perchè inizialmente si sono rimossi i duplicati

# possibili problemi di compatibilità con mapIT (miniatura presente nella slide 
# 5 della presentazione)
# per eseguire la funzione mapIT decommentare le righe 32 e 33 nello script A00_main.R

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)
