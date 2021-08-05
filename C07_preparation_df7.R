# The data contains contains the purchase and refund transaction of each customer
# ID_SCONTRINO: identify the transaction (all the products purchased or refund within the same transaction have the same id)
# ID_CLI: identify client
# ID_NEG: identify reference store
# ID_ARTICOLO: identify the purchased or refund item
# COD_REPARTO: identify the business unit corresponding to the item
# DIREZIONE: purchase (1) or refund (-1)
# IMPORTO_LORDO: gross amount = nett amount + discount applied (negative if refund)
# SCONTO: discount applied (negative if refund)
# DATETIME: date and time of the transaction

#### INSIGHTS #### 
# I record sono scontrini. Gli scontrini corrispondono a transazioni (acquisti o refund)
# Si possono avere più scontrini (o transazioni) (acquisti/refund, in generale transazioni) per cliente
# La prima transazione risale al 1 maggio 2018 mentre l'ultima al 30 aprile 2019
# Valori di IMPORTO_LORDO negativi con DIREZIONE = 1 indicano dei coupon straordinari associati all'acquisto di un prodotto. Dovrebbero essere trascurabili sul totale delle transazioni	
# 907846 transazioni ovvero il 91% delle transazioni totale sono acquisti
# Si può notare che dalle 16 alle 20 vi è una maggiore frequenza di acquisti
# I codici reparto che registrano il maggior numero di purchase sono: 3, 10, 11
# I codici reparto che registrano il maggior numero di refund sono: 3, 6, 7
# La maggior parte delle transazioni si registrano nei giorni di settimana (lun - ven)
# In holiday, weekday, weekend gli acqusiti superano in gran misura i refund
# per scontrino: la media dell'importo lordo per purchase risulta essere 164, mentre per refund -110
# per scontrino: la media dello sconto per purchase risulta essere 11.8, mentre per refund -8.2
# per cod_reparto: la media dell'importo lordo per purchase risulta essere 10649178, mentre per refund -706252
# per cod_reparto: la media dello sconto per purchase risulta essere 762487, mentre per refund -53409
# per id_cli: la media dell'importo lordo per purchase risulta essere 703, mentre per refund -212
# per id_cli:la media dello sconto per purchase risulta essere 50.3, mentre per refund -16.0
# i 3 clienti che registrano il numero maggiore di acquisti sono: 376925, 117212, 248975 con rispettivamente 177, 155, 154 acquisti
# Gran parte dei clienti (173939, circa il 47% dei clienti) ha effettuato 1 o più acqusiti. Ad aver effettuato 6 o più acquisti sono 8504 clienti, circa lo 0,2% dei clienti
# Per circa il 90% dei clienti passano circa 70 giorni per il successivo acquisto


#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)
# la prima transazione risale al 1 maggio 2018 mentre l'ultima al 30 aprile 2019

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7
# 157348 ID_CLI sono presenti in df1 ma non in df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) 
  
# viene corretta TIC_DATE_WEEKDAY dato che domenica viene mappata come 1. Si vuole lunedì come 1

df_7_tic_clean_final$TIC_DATE_WEEKDAY <- dplyr::recode(df_7_tic_clean_final$TIC_DATE_WEEKDAY, `1` = 7, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5, `7` = 6)

df_7_tic_clean_final <- df_7_tic_clean_final %>% 
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )


#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)  # TOT_TICS indica il numero di scontrini (transazioni) univoci
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview
# significa che si hanno più scontrini per cliente

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction
# si può notare che 907846 transazioni ovvero il 91% delle transazioni totale sono acquisti
# queste 907846 transazioni sono associateal a 212124 clienti

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_classic()+
    xlab("Hour")+ylab("Number of transactions") +
    scale_x_continuous(breaks=seq(0, 24, 1))+
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))
)

plot_df7_dist_hour

# la maggior parte delle transazioni sono acquisti. Si può notare che dalle 16 alle 20 
# vi è una maggiore frequenza di acquisti.

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    xlab("Hour")+ylab("Number of transactions (%)") +
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))+
    theme_classic()
)

plot_df7_dist_hour_percent
# la percentuale dei refund non supera mai 0.125%

### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep


## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_classic()+
    xlab("Business unit code")+ylab("Number of transactions")+
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase")) 
)

plot_df7_dist_dep
# i codici reparto che registrano il maggior numero di purchase sono: 3, 10, 11
# i codici reparto che registrano il maggior numero di refund sono: 3, 6, 7


## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    xlab("Business unit code")+ylab("Number of transactions (%)")+
    geom_bar(stat="identity", position="fill" ) +
    theme_classic()+
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))
)

plot_df7_dist_dep_percent
# anche in questo per ogni reparto i purchase rappresentano la transazione più frequente.
# in corrispondenza del codice reparto 6 si registra la percentuale maggiore di refund

### Variable TIC_DATE considering only purchases ###

# per capire l'andamento generale del numero di purchase nell'arco temporale a disposizione
## compute aggregate
df7_dist_date <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(TIC_DATE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO))

df7_dist_date

## plot aggregate
ggplot(data=df7_dist_date, aes(x=TIC_DATE, y=TOT_TICs))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  labs(x = "Date", 
       y = "Number of transactions")


### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_classic()+
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    xlab("Date type")+ylab("Number of transactions")+
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))
)

plot_df7_dist_datetyp
# la maggior parte delle transazioni si registrano nei giorni di settimana (lun - ven)
# in holiday, weekday, weekend gli acqusiti superano in gran misura i refund

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    xlab("Date type")+ylab("Number of transactions (%)")+
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))+
    geom_bar(stat="identity", position="fill" ) +
    theme_classic()
)

plot_df7_dist_datetyp_percent
# per ogni categoria di giorno (holiday, weekday, weekend) la percentuale di purchase e di refund risulta essere simile

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

# si calcola per scontrino l'importo lordo e lo sconto (e i relativi valori medi)

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)  # ad uno scontrino possono essere associati più prodotti
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto
# per scontrino: la media dell'importo lordo per purchase risulta essere 164, mentre per refund -110
# per scontrino: la media dello sconto per purchase risulta essere 11.8, mentre per refund -8.2
# vi sono anche dei valori negativi  di purchase

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_classic()+
    xlab("Gross amount")+ylab("Count")+
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))
)

plot_df7_dist_importo



## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_classic()+
    xlab("Sconto")+ylab("Count")+
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase"))
  
)

plot_df7_dist_sconto

# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

# si calcola per codice reparto l'importo lordo e lo sconto (e i relativi valori medi)

# IMPORTO_LORDO and SCONTO by COD_REPARTO
df7_dist_importosconto_by_codreparto <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_importosconto_by_codreparto

# average IMPORTO_LORDO and average SCONTO by COD_REPARTO 
df7_dist_avgimportosconto_by_codreparto <- df7_dist_importosconto_by_codreparto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)  # media degli improti lordi dei vari cod_reparti
            , AVG_SCONTO = mean(SCONTO))
df7_dist_avgimportosconto_by_codreparto
# per cod_reparto: la media dell'importo lordo per purchase risulta essere 10649178, mentre per refund -706252
# per cod_reparto: la media dello sconto per purchase risulta essere 762487, mentre per refund -53409

# evetuali plot aggiuntivi

# IMPORTO LORDO
plot_df7_dist_importo_by_codreparto <- (
ggplot(data = df7_dist_importosconto_by_codreparto,
       aes(fill = DIREZIONE, x = COD_REPARTO, y = IMPORTO_LORDO)) +
  geom_bar(stat = "identity") + 
  xlab("Business unit code")+ylab("Gross amount")+
  scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase")) +
  theme_classic() +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
)

plot_df7_dist_importo_by_codreparto


# SCONTO
plot_df7_dist_sconto_by_codreparto <- (
  ggplot(data = df7_dist_importosconto_by_codreparto,
         aes(fill = DIREZIONE, x = COD_REPARTO, y = SCONTO)) +
    geom_bar(stat = "identity") + 
    xlab("Business unit code")+ylab("Sconto")+
    scale_fill_discrete(name = "DIREZIONE", labels = c("Refund", "Purchase")) +
    theme_classic() +
    scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) # millions
)
plot_df7_dist_sconto_by_codreparto

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

df7_dist_id_articolo <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_ARTICOLO) %>% 
  summarize(NUM_TICs = n_distinct(ID_SCONTRINO)) %>% 
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(NUM_TICs))
# i tre id_articolo per cui si registra il maggior numero di transazioni sono 33700716, 33817091, 34843564 
# ad esempio, per l'articolo con id 33700716 sono registrate 57806 transazioni


# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

# si calcola per id cliente reparto l'importo lordo e lo sconto (e i relativi valori medi)

# IMPORTO_LORDO and SCONTO by ID_CLI
df7_dist_importosconto_by_idcli <- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

# average IMPORTO_LORDO and average SCONTO by ID_CLI 
df7_dist_avgimportosconto_by_idcli <- df7_dist_importosconto_by_idcli %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))
df7_dist_avgimportosconto_by_idcli
# per id_cli: la media dell'importo lordo per purchase risulta essere 703, mentre per refund -212
# per id_cli:la media dello sconto per purchase risulta essere 50.3, mentre per refund -16.0


# compute the distribution of customers by number of purchases (as described in the slides)

df7_dist_num_purch_customer <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarize(NUM_PURCHASES = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(NUM_PURCHASES))
  
df7_dist_num_purch_customer
# i 3 id_cliente che registrano il numero maggiore di acquisti sono: 376925, 117212, 248975 con rispettivamente 177, 155, 154 acquisti

# compute the days for next purchase curve (as described in the slides) - check

data_for_next_purchase <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         TIC_DATE,
         DIREZIONE) %>% 
  arrange(ID_CLI)
data_for_next_purchase

# si seleziona per motivi computazionali un subset del dataset
data_for_next_purchase_diff <- data_for_next_purchase %>%
  filter(ID_CLI < 10000) %>% 
  arrange(TIC_DATE) %>% 
  group_by(ID_CLI) %>% 
  mutate(diff_days=TIC_DATE - lag(TIC_DATE))

data_for_next_purchase_diff


diff_days_diff <- as.data.frame(table(data_for_next_purchase_diff$diff_days))
diff_days_diff <- diff_days_diff[-1, ]  # si considera una differenza > 0
diff_days_diff$Perc <- diff_days_diff$Freq/sum(diff_days_diff$Freq)


ggplot(diff_days_diff, 
       aes(x = as.numeric(Var1),
           y = cumsum(Perc))) +
  labs(title = "days for next purchase curve",
       x = "days for next purchase",
       y = "% customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +   
  geom_vline(xintercept = 75, linetype = "dotted") +
  geom_vline(xintercept = 40, linetype = "dotted") +
  scale_x_continuous(breaks=seq(0, 300, 20)) +
  scale_y_continuous(breaks=seq(0, 1.00, 0.10)) +
  geom_line(size = 1) +
  theme_classic() 

# dopo circa 70 giorni dall'ultimo acquisto il 90% dei clienti totali ha effettuto nuovamente un acquisto ????
# dopo circa 40 giorni dall'ultimo acquisto l'80% dei clienti totali ha effettuato nuovamente un acquisto

# EXPLORE distribuzione net amount (importo lordo - sconto)

# considerando i soli acquisti, si calcola per ogni mese il nett amount complessivo, considerando in generale i clienti
df7_nett_amount_monthly<-df_7_tic_clean %>%
  filter(DIREZIONE==1) %>%
  mutate(nett_amount = IMPORTO_LORDO-SCONTO) %>%
  select(TIC_DATETIME, nett_amount) %>%
  group_by(Date=floor_date(TIC_DATETIME, "month")) %>% 
  summarise(nett_amount_monthly = sum(nett_amount)) 

df7_nett_amount_monthly

plot_df7_nett_amount_monthly<- ggplot(data=df7_nett_amount_monthly, aes(Date, nett_amount_monthly)) +
  geom_line(colour='darkblue') +
  geom_point() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(y='Nett amount') + 
  theme_light() +
  theme_classic()               

plot_df7_nett_amount_monthly
# il picco di nett amount è stato raggiunto tra Ottobre e Gennaio 2018

# si analizza più nel dettaglio il numero di purchase per cliente

num_purch_customer <- df7_dist_num_purch_customer %>% 
  group_by(NUM_PURCHASES) %>% 
  summarise(NUM_CLI = n_distinct(ID_CLI))

num_purch_customer_redux <- num_purch_customer %>% 
  filter(num_purch_customer < 7)

num_purch_customer_redux_cumulate <- num_purch_customer_redux %>%
  mutate(num = rev(cumsum(rev(NUM_CLI))))

num_purch_customer_redux_cumulate <- num_purch_customer_redux_cumulate %>%
  mutate(perc = (num/369472))

num_purch_customer_redux_cumulate

plot_num_purch_customer_redux_cumulate<- ggplot(data=num_purch_customer_redux_cumulate, aes(NUM_PURCHASES, num)) +
  geom_bar(stat = "identity") +
  labs(y='NUM_CUSTOMERS') + 
  theme_light() +
  theme_classic() +
  scale_x_discrete(limit = c("1", "2","3", "4", "5", "6"),
                   labels = c("1 or more purchases","2 or more purchases","3 or more purchases", 
                              "4 or more purchases", "5 or more purchases", "6 or more purchases")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") +
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  geom_text(aes(label=num), position=position_dodge(width=0.9), vjust=-0.25)

plot_num_purch_customer_redux_cumulate 


#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
