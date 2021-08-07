# The data contains info about fidelity subscriptions of each customer:
# ID_CLI: identity client (foreign key)
# ID_FID: identity fidelity program (key)
# ID_NEG: identity reference program
# TYP_CLI_FID: main account (1/0)
# COD_FID: type fidelity program
# STATUS_FDT: active account(1/0)
# DT_ACTIVE: date activation

#### INSIGHTS ####
# Ci sono 369472 id clienti univoci mentre ci sono 370135 coppie cliente-fidelty
# Infatti ci sono dei clienti a cui è associata più di una carta fedeltà. Nel dataset non risultano esserci duplicati
# Il 99.8% dei clienti, ovvero 368833 clienti, hanno associato 1 sottoscrizione in una sola data
# Per alcuni clienti ci sono più sottoscrizioni di carte fedeltà anche lo stesso giorno
# La maggior parte dei clienti (78.4%) per la loro prima attivazione hanno optato per una una sottoscrizione standard
# Il 99% dei clienti possiede una sottoscrizione attiva (rilevata sull'ultima data di registrazione)
# Nel 2018 sono state attivate più carte fedeltà rispetto al 2019


#### FIRST LOOK of df_1 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)
head(df_1_cli_fid)

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())

# ci sono 370135 osservazioni
# ci sono 369472 valori unici per gli ID cliente (ci sono degli ID cliente che si ripetono, per la precisione 663)
# ci sono 367925 valori unici per gli ID fidelity (ci sono degli ID fidelity che si ripetono)
# ci sono 370135 valori unici per coppie ciente-fidelity (essendo le osservazioni totali 370135, si può dire non vi siano duplicati nelle coppie id_cli-id_fid)

# concentrandosi sui duplicati di ID_CLI
sum(duplicated(df_1_cli_fid_clean$ID_CLI))  # 663
df_1_cli_fid_clean$ID_CLI[duplicated(df_1_cli_fid_clean$ID_CLI)]  # ID_CLI che si ripetono

# è possibile osservare che ci sono più registrazioni di carte fedeltà per ciascun 
# cliente dal momento in cui il numero di identificativi dei clienti è maggiore del 
# numero degli identificavi delle carte fedeltà

# nonostante ciò, non ci sono duplicati considerando la coppia cliente-fidelty. 
# Questo significa che ci sono clienti a cui è associata più di uan carta fidelty, 
# ma queste sono tra loro diverse.


#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean<- df_1_cli_fid_clean%>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID)) %>%
  mutate(COD_FID = as.factor(COD_FID)) # è stato trasformato anche COD_FID in factor
str(df_1_cli_fid_clean)


#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

## count the subscriptions for each client

num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
  )

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)


### compute the distribution of number of subscriptions

dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

# 368833 clienti con 1 sottoscrizione in una sola data
# 254 clienti con 2 sottoscrizioni in una sola data e 363 in due data diverse
# 7 clienti con 3 sottoscrizioni in una sola data e 8 in due date diverse e 5 in tre
# 2 clienti con 4 sottoscrizioni in una sola data

# viene mostrata la distribuzione dei clienti con più di una sottoscrizione

dist_num_fid_x_cli2<-dist_num_fid_x_cli[dist_num_fid_x_cli$NUM_FIDs>1,]
ggplot(dist_num_fid_x_cli2
       , aes(x=NUM_FIDs,y=TOT_CLIs, fill=factor(NUM_DATEs))) +
        geom_bar(stat="identity")+
  scale_fill_manual(values=c("gray85","lightblue","black"))+
  theme_classic() +
  xlab("Number of fidelity subscriptions") + ylab("Number of clients")+
  labs(fill= "Number of dates")


### let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 2)
num_fid_x_cli %>% filter(NUM_FIDs == 3)
num_fid_x_cli %>% filter(NUM_FIDs == 4)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)


# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

# ad esempio, il cliente 320880 ha sottoscritto 3 fidelity nello stesso negozio: 
# la prima sottoscrizione standard nel giorno 25-04-2018 il giorno dopo è passato 
# ad una sottoscrizione premium per poi lo stesso giorno ripassare ad una sottoscrizione 
# standard (le prime due sottoscrizioni non sono più attive)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

# si ricavano informazioni dalla prima sottoscrizione: registration date, store for registration
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()


# si osserva meglio qual è la tipologia di sottoscrizione più frequente in occasione dell'attivazione
df_1_cli_fid_first %>%
  group_by(COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_1_cli_fid_first %>% 
  count(COD_FID= factor(COD_FID )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = COD_FID, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'First code fidelity subscription', y = 'Percentage')+  theme_classic()

# la maggior parte dei clienti (78.4%) per la loro prima attivazione hanno optato per una una sottoscrizione standard


# si ricavano informazioni da'ultima sottoscrizione: type of fidelity, status
df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()


# si ricombinano le informazioni ricavate per ottenere un dataset unico
df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')
summary(df_1_cli_fid_clean)


#### EXPOLORE COLUMNS of df_1 ####

##compute the distribution LAST_STATUS_FID

df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_1_cli_fid_clean %>% 
  count(LAST_STATUS_FID = factor(LAST_STATUS_FID )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = LAST_STATUS_FID, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("0" = "unactive", "1" = "active"))+
    theme_classic()+
  labs(x = 'Last fidelity subscription status', y = 'Percentage')

# il 99% dei clienti possiede una sottoscrizione attiva (rilevata sull'ultima data di registrazione)


## compute the distribution: LAST_COD_FID

df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_1_cli_fid_clean %>% 
  count(LAST_COD_FID= factor(LAST_COD_FID )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = LAST_COD_FID, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  labs(x = 'Laste code fidelity subscription', y = 'Percentage')


# sproporzione elevata tra clienti con sottoscrizione standard rispetto agli altri con 
# sottocrizioni di categoria premium, con sottocategorizzazione tra premium e premium business
# standard e standard business, in concomitanza dell'utlima data registrata per cliente


## compute distribution of NUM_FIDs

df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_1_cli_fid_clean %>% 
  count(NUM_FIDs= factor(NUM_FIDs )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = NUM_FIDs, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Number of fidelity subscriptions', y = 'Percentage')+  theme_classic()

# quasi la totalità dei clienti posseggono una sola sottoscrizione

# si procede con un'ulteriore esplorazione: monthly activations 
num_monthly_activation <- df_1_cli_fid_clean %>%
  mutate(Month=floor_date(LAST_DT_ACTIVE, "month")) %>%
  group_by(Month)  %>%
  summarise(num_activation=n_distinct(ID_CLI))

num_monthly_activation_plot <- ggplot(data=num_monthly_activation, aes(Month, num_activation)) +
  geom_line(colour='turquoise3') +
  geom_point() +
  labs(y='Active Users') +
  scale_y_continuous(labels = scales::comma) +
  theme_light()
print(num_monthly_activation_plot)

# nel 2018 ci sono state più attivazioni rispetto al 2019


#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)
