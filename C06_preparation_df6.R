# The data contains info about the events (sents, opens, clicks..) related to the marketing email communications
# ID_EVENT: identify feedback event (key)
# ID_CLI: identify client (foreign key)
# ID_CAMP: identify email campaign (foreign key)
# ID_DELIVERY: identify delivery
# TYPE_EVENT: type feedback event (s=send, v=open, c=click, b=bounce, e=error)
# EVENT_DATE: datetime event


#### INSIGHTS ####
# Ci sono dei clienti a cui non è associata nessuna informazione relativa a comunicazioni email di marketing, si può pensare a clienti che non hanno dunque ricevuto alcuna comunicazione email
# Ci sono delle campagne (in df5) alle quali non sono associati particolari eventi, si può pensare a campagne che non sono state fatte ancora partire
# Le rilevazioni sono state effettuate da gennaio 2019 a marzo 2019 per un totale di 1556646 comunicazioni email di marketing che riguardano 190427 clienti
# Il tipo di campagna che registra più eventi (aperture, clicks, fallimenti) risulta essere national. Vengono coinvolti 177153 clienti 
# La maggior parte delle comunicazioni email di marketing (di qualsiasi tipologia di campagna) non vengono aperte
# La maggior parte dei clienti che apre la email lo fa il giorno stesso della ricezione di essa
# Circa il 90% dei clienti che aprono la email lo fanno entro 4,3,2,1 o 0 giorni dalla ricezione di essa
# Il 99.7% dei clienti non interagisce cliccando sulla comunicazione ricevuta
# Per la maggior parte dei clienti le comunicazioni vengono spedite correttamente, senza errori
# Si nota che nella maggior parte delle comunicazioni si registra una sola apertura e un solo click

#### FIRST LOOK of df_6 ####

str(df_6_camp_event)
summary(df_6_camp_event)

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

# tutti gli idcli in df6 sono mappati in df1 ma non il viceversa
# questo significa che ci sono dei clienti a cui non è associata nessuna informazione 
# relativa a comunicazioni email di marketing 


#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarize(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

# tutti gli idcamp in df6 sono mappati in df5 ma non vale il viceversa
# ci sono delle campagne (in df5) a cui non sono associate particolari eventi


#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))
# l'utente ha indicato ad esempio una email sbagliata e si ha un errore come feedback

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE = EVENT_DATE) %>%
  as.data.frame()

# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_OPENs = n_distinct(ID_EVENT_O))
  
df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
       , ID_CLI
       , ID_CAMP
       , TYP_CAMP
       , ID_DELIVERY
       , CLICK_DATETIME = EVENT_DATETIME
       , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
         )

#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_camp_event_clean_final %>% 
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview

# le rilevazioni sono state effettuate da gennaio 2019 a marzo 2019 per un totale 
# di 1556646 comunicazioni email di marketing che riguardano 190427 clienti

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP) %>%
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp

## plot aggregate: TYP_CAMP
ggplot(data=df6_overviewbytyp
       , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
  geom_bar(stat="identity"
           , fill="turquoise3") +
  theme_classic() + 
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  xlab("Type campaign") + ylab("Number of emails") +
  geom_text(aes(label=TOT_EVENTs), position=position_dodge(width=0.9), vjust=-0.25)

# il tipo di campagna che registra più eventi (aperture, clicks, fallimenti) 
# risulta essere national. Vengono coinvolti 177153 clienti 


### Variable OPENED ###

## compute aggregate
df6_dist_opened <- df_6_camp_event_clean_final %>%
  group_by(OPENED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened
# la maggior parte delle comunicazioni email di marketing (di qualsiasi tipologia 
# di campagna) non vengono aperte

## plot aggregate

plot_df6_dist_opened <- (
  ggplot(data=df6_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    xlab("Type campaign") + ylab("Number of emails") +
    theme_classic()
)

plot_df6_dist_opened

# andando a considerare per tipologia di campagna

### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_dist_openedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp

## plot aggregate
plot_df6_dist_openedbytyp <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    xlab("Type campaign") + ylab("Number of emails") +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    theme_classic()
)

plot_df6_dist_openedbytyp


## plot aggregate percent
plot_df6_dist_openedbytyp_percent <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_classic()
)

plot_df6_dist_openedbytyp_percent

### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_daystoopen

## plot aggregate
plot_df6_dist_daystoopen <- (
  ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="turquoise3") +
    xlab("Average days to open") + ylab("Number of clients") +
    scale_x_continuous(breaks=seq(0, 14, 1)) + 
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    theme_classic() + 
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25))

plot_df6_dist_daystoopen

# la maggior parte dei clienti tende ad aprire l'email il giorno stesso

### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    geom_line(colour='turquoise3') +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    xlab("Average days to open") + ylab(" Percent covered") +
    theme_classic()
)

plot_df6_dist_daystoopen_vs_cumulate
# circa il 90% dei clienti apre la email entro 4,3,2,1 o 0 giorni
# il 10% dei clienti circa aspetta più di 4 giorni prima di aprire l'email 


# EXPLORE the following relevant variables in df_6_camp_event_clean_final:

str(df_6_camp_event_clean_final)

# - CLICKED/CLICKED by TYP_CAMP

# plot distribution: CLICKED

## compute aggregate
df6_dist_clicked <- df_6_camp_event_clean_final %>%
  group_by(CLICKED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_clicked
# il 99.7% dei clienti non interagisce cliccando sulla comunicazione ricevuta

## plot aggregate
plot_df6_dist_clicked <- (
  ggplot(data=df6_dist_clicked
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    xlab("Type campaign") + ylab("Number of emails") +
    theme_classic()
)

plot_df6_dist_clicked


# plot distribution: CLICKED BY TYP_CAMP

## compute aggregate
df6_dist_clicked_bytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , CLICKED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_clicked_bytyp

## plot aggregate
plot_df6_dist_clicked_bytyp <- (
  ggplot(data=df6_dist_clicked_bytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    xlab("Type campaign") + ylab("Number of emails") +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    theme_classic()
)

plot_df6_dist_clicked_bytyp

## plot aggregate percent
plot_df6_dist_failed_bytyp_percent <- (
  ggplot(data=df6_dist_clicked_bytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    xlab("Type campaign") + ylab("Number of emails") +
    theme_classic()
)

plot_df6_dist_failed_bytyp_percent
# i grafici sopra confermano il fatto che per la quasi totalità di email non si registra l'evento click 


# - FAILED

# compute aggregate
df6_dist_failed <- df_6_camp_event_clean_final %>%
  group_by(FAILED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_failed
# solo 27928 comunicazioni email di marketing hanno un esito fallimentare nell'invio
# per la maggior parte dei clienti le comunicazioni vengono spedite correttamente, senza errori

## plot aggregate

plot_df6_dist_failed <- (
  ggplot(data=df6_dist_failed
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    xlab("Type campaign") + ylab("Number of emails") +
    theme_classic()
)

plot_df6_dist_failed


# FAILED BY TYP_CAMP 

df6_dist_failed_bytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , FAILED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_failed_bytyp

## plot aggregate
plot_df6_dist_failed_bytyp <- (
  ggplot(data=df6_dist_failed_bytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    xlab("Type campaign") + ylab("Number of emails") +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    theme_classic()
)

plot_df6_dist_failed_bytyp

## plot aggregate percent
plot_df6_dist_failed_bytyp_percent <- (
  ggplot(data=df6_dist_failed_bytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    xlab("Type campaign") + ylab("Number of emails") +
    theme_classic()
)

plot_df6_dist_failed_bytyp_percent


# - NUM_OPENs

df6_dist_numopens <- df_6_camp_event_clean_final %>%
  filter(!is.na(NUM_OPENs)) %>%  # si considerano i casi in cui c'è stata almeno una apertura 
  group_by(NUM_OPENs) %>% 
  summarize(TOT_ID = n_distinct(ID_EVENT_S)) %>% 
  mutate(PERCENT = TOT_ID/sum(TOT_ID)) %>%
  arrange(desc(PERCENT))   

df6_dist_numopens

# volendo analizzare con più attenzione i valori di numero aperture minori o uguali a 10
ggplot(data = df6_dist_numopens[1:10,],
       aes(x = NUM_OPENs,
           y = TOT_ID)) +        
  geom_bar(stat = "identity",
           fill = "turquoise3") + 
  xlab("Number of opens") + ylab("Number of emails") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +  
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  theme_classic() +
  geom_text(aes(label=TOT_ID), position=position_dodge(width=0.9), vjust=-0.25)      
# si nota che nella maggior parte dei casi si registra una sola apertura


# - NUM_CLICKs

df6_dist_numclicks <- df_6_camp_event_clean_final %>%
  filter(!is.na(NUM_CLICKs)) %>%  # si considerano i casi in cui CLICKED è true 
  group_by(NUM_CLICKs) %>% 
  summarize(TOT_ID = n_distinct(ID_EVENT_S)) %>%  
  mutate(PERCENT = TOT_ID/sum(TOT_ID)) %>% 
  arrange(desc(PERCENT))                        

# volendo analizzare con più attenzione i valori di numero clicks minori o uguali a 10
ggplot(data = df6_dist_numclicks[1:10,],
       aes(x = NUM_CLICKs,
           y = TOT_ID)) +        
  geom_bar(stat = "identity",
           fill = "turquoise3") + 
  xlab("Number of clicks") + ylab("Number of emails") +
  scale_x_continuous(breaks=seq(0, 10, 1))+ 
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  theme_classic() +
  geom_text(aes(label=TOT_ID), position=position_dodge(width=0.9), vjust=-0.25)   
# si nota che nella maggior parte dei casi si registra un click


#### FINAL REVIEW df_6_clean ####

str(df_6_camp_event_clean_final)
summary(df_6_camp_event_clean_final)
