### RFM MODEL: provides a deterministic description of the value of each customer in term of purchase behaviour
# Le metriche utilizzate sono:
# Recency: How recently a customer has made a purchase
# Frequency: How often a customer makes a purchase
# Monetary Value: How much money a customer spends on purchases

# I clienti assumono maggior valore 
# - se hanno acquistato recentemente
# - se acquistano con frequenza
# - se spendono di più


# bisogna fissare una soglia di tempo oltre la quale i clienti verranno classificati come attivi

# le rilevazioni vanno dal 01/05/2018 al 30/04/2019
# si decide di utilizzare come reference date il 28/02/2018

rfm_threshold<-df_7_tic_clean_final  %>%
  filter(TIC_DATE > as.Date("28/02/2019", format = "%d/%m/%Y"))

rfm_threshold


#### RECENCY: days passed from last purchase ####

my_data_recency<-rfm_threshold %>%
                filter(DIREZIONE==1) %>%
                group_by(ID_CLI) %>%
                summarise(LAST_PURCHASE_DATE=max(TIC_DATE))

my_data_recency

# costruisco la variabile RECENCY 

my_data_recency$RECENCY<-difftime(as.Date("30/04/2019",
                                      format="%d/%m/%Y"),
                                  my_data_recency$LAST_PURCHASE_DATE,
                              units = "days")

my_data_recency$RECENCY

# la recency è ottenuta come differenza tra l'ultimo giorno di acquisto per cliente e l'ultimo giorno di rilevazione
# as esempio: il cliente con ID 5 ha acquistato l'ultima volta il 23 novembre 2018, ovvero 158 giorno entro l'ultima data di rilevazione.



# suddivido la recency in:
# low: below the 25th percentile of the distribution
# medium: from 25th to 75th percentile
# high: above 75th percentile


## definisco le classi per la variabile recency e ottengo i percentili di classe
summary(my_data_recency)


my_data_recency<- within(my_data_recency,
                      RECENCY_CLASS <- cut(as.numeric(my_data_recency$RECENCY),
                                       breaks = quantile(my_data_recency$RECENCY,
                                                         probs = c(0, .25, .75, 1)), 
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))         

recency_var <- as.data.frame(table(my_data_recency$RECENCY_CLASS))


#PLOT DISTRIBUTION
ggplot(data = recency_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                  
  labs(x     = "Recency classes",
       y     = "Total Purchase") +               
  theme_classic() +                             
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# La classe media è la più frequente: la maggior parte degli acquisti effettuati (36601) sono abbastanza recenti
# N.B la classe recency high corrisponde alla classe con il maggior numero di giorni dall'ultimo acquisto


#### FREQUENCY: total number of purchase in the reference range ####

my_data_frequency <- rfm_threshold %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI)  %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))

my_data_frequency

# suddivo in:
# low: below 2 total purchases
# medium: from 2 to 5 purchases
# high: above 5 (to 101) purchases

# costruisco le classi 

my_data_frequency <- within(my_data_frequency,
                        FREQUENCY_CLASS <- cut(my_data_frequency$TOT_PURCHASE,
                                          breaks = c(0, 2, 5, 101),             
                                          include.lowest = T,
                                          right = F,
                                          labels = c("low", "medium", "high"))) 
table(my_data_frequency$FREQUENCY_CLASS)


frequency_var<- as.data.frame(table(my_data_frequency$FREQUENCY_CLASS))


# PLOT DISTRIBUTION

ggplot(data = frequency_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                 
  labs(x     = "Frequency Type",
       y     = "Totla Purchase") +               
  theme_classic() +                             
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# la maggior parte degli acquisti (41817) appartiene  alla classe con minor frequenza di acquisto


#### MONETARY VALUE: amount spent in the reference range ####

my_data_monetary_value <- rfm_threshold %>%
  filter(DIREZIONE == 1)%>% 
  group_by(ID_CLI) %>% 
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            IMPORTO_NETTO = IMPORTO_LORDO - SCONTO) %>%
  ungroup()                                 %>%
  as.data.frame()                           %>%
  arrange(desc(IMPORTO_LORDO))

my_data_monetary_value

# suddivido in:
# low: below the 25th percentile of the distribution
# medium: from 25th to 75th percentile
# high: above 75th percentile


# Costruisco le classi

my_data_monetary_value <- within(my_data_monetary_value,
                       MONETARY_CLASS <- cut(my_data_monetary_value$IMPORTO_NETTO,
                                        breaks = quantile(my_data_monetary_value$IMPORTO_NETTO,
                                                          probs = c(0, .25, .75, 1)),
                                        include.lowest = T,
                               
                                                 labels = c("low", "medium", "high"))) 
table(my_data_monetary_value$MONETARY_CLASS)

monetary_var <- as.data.frame(table(my_data_monetary_value$MONETARY_CLASS))

# PLOT DISTRIBUTION

ggplot(data = monetary_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Monetary Classes",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)


# la classe più lucrosa, in termini monetari è la medium, ovvero 38275 degli acquisti sono abbastanza costosi

# unisco i tre dataset:

rfm <- merge(my_data_frequency,
             my_data_monetary_value,  
             by = "ID_CLI") 

rfm <- merge(rfm,           
             my_data_recency,  
             by = "ID_CLI")

summary(rfm)
str(rfm)

# combino le varie classi per la segmentazione della customer base

rfm$recency_frequency <- NA
# table(is.na(rfm$RECENCY_CLASS))
# table(is.na(rfm$FREQUENCY_CLASS))
# rfm<-na.omit(rfm)
# table(is.na(rfm))
for(i in c(1:nrow(rfm))){
  if(rfm$RECENCY_CLASS[i] == "low" && rfm$FREQUENCY_CLASS[i] == "low") rfm$recency_frequency[i] <- "One-Timer"
  if(rfm$RECENCY_CLASS[i] == "medium" && rfm$FREQUENCY_CLASS[i] == "low") rfm$recency_frequency[i] <- "One-Timer"
  if(rfm$RECENCY_CLASS[i] == "high" && rfm$FREQUENCY_CLASS[i] == "low") rfm$recency_frequency[i] <- "Leaving"
  if(rfm$RECENCY_CLASS[i] == "low" && rfm$FREQUENCY_CLASS[i] == "medium") rfm$recency_frequency[i] <- "Engaged"
  if(rfm$RECENCY_CLASS[i] == "medium" && rfm$FREQUENCY_CLASS[i] == "medium") rfm$recency_frequency[i] <- "Engaged"
  if(rfm$RECENCY_CLASS[i] == "high" && rfm$FREQUENCY_CLASS[i] == "medium") rfm$recency_frequency[i] <- "Leaving"
  if(rfm$RECENCY_CLASS[i] == "low" && rfm$FREQUENCY_CLASS[i] == "high") rfm$recency_frequency[i] <- "Top"
  if(rfm$RECENCY_CLASS[i] == "medium" && rfm$FREQUENCY_CLASS[i] == "high") rfm$recency_frequency[i] <- "Top"
  if(rfm$RECENCY_CLASS[i] == "high" && rfm$FREQUENCY_CLASS[i] == "high") rfm$recency_frequency[i] <- "Leaving Top"
}

table(rfm$recency_frequency)

# One-Timer: clienti che hanno acquistato recentemente/abbastanza recentementente ma con scarsa frequenza
# Leaving: clienti che non hanno acquistato recentemente e con scarsa/media frequenza
# Engaged: clienti che hanno acquistato recentemente/abbastanza recentemente e con media frequenza
# Top: cienti che hanno acquistato recentemnte/abbastanza recentemente e con alta frequenza
# Leaving Top: cienti che non hanno acquistato di recente ma con alta frequenza


# costruisco un dataset con le informazioni ottenute

recency_frequency_df <- as.data.frame(rbind(c("Leaving",         "High",   "Low",    34853),  # questi numeri da dove si ricavano?
                             c("Leaving",         "High",   "Medium", 34853),
                             c("Leaving Top", "High",   "High", 1339 ),
                             c("One-Timer",     "Medium", "Low",    36587),
                             c("Engaged",     "Medium", "Medium", 46139),
                             c("Top",     "Medium", "High",   27811),
                             c("One-Timer",   "Low",    "Low", 36587 ),
                             c("Engaged",   "Low",    "Medium", 46139),
                             c("Top",     "Low",    "High",   27811)))

colnames(recency_frequency_df) <-  c("Level", "Recency", "Frequency", "Value")

recency_frequency_df$Frequency <- factor(recency_frequency_df$Frequency,
                          levels = c("High", "Medium", "Low"))

recency_frequency_df$Recency <- factor(recency_frequency_df$Recency,
                        levels = c("High", "Medium", "Low"))
recency_frequency_df$Value <- as.numeric(recency_frequency_df$Value)


# plot segmentation


ggplot(recency_frequency_df, aes(x = Frequency, y = Recency, fill = Value)) + 
  geom_tile() +
  geom_text(aes(label = Level)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()


# plot RF classes

recency_frequency <- as.data.frame(table(rfm$recency_frequency))
recency_frequency

ggplot(data = recency_frequency,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                   
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "Recency-frequency Distribution",
       x     = "Recency-frequency Classes",
       y     = "Total Clients") +                
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Engaged", "Leaving", "Leaving Top",
                              "One Timer", "Top")) + 
  guides(fill = FALSE)

# la maggior parte dei cienti sono Engaged.

# combino le classi con i monetary group

rfm$recency_frequency_monetary <- NA

for(i in c(1:nrow(rfm))){
  if(rfm$recency_frequency[i] == "Top" && rfm$MONETARY_CLASS[i] == "low") rfm$recency_frequency_monetary[i] <- "Silver"
  if(rfm$recency_frequency[i] == "Leaving Top" && rfm$MONETARY_CLASS[i] == "low") rfm$recency_frequency_monetary[i] <- "Bronze"
  if(rfm$recency_frequency[i] == "Engaged" && rfm$MONETARY_CLASS[i] == "low") rfm$recency_frequency_monetary[i] <- "Copper"
  if(rfm$recency_frequency[i] == "Leaving" && rfm$MONETARY_CLASS[i] == "low") rfm$recency_frequency_monetary[i] <- "Tin"
  if(rfm$recency_frequency[i] == "One-Timer" && rfm$MONETARY_CLASS[i] == "low") rfm$recency_frequency_monetary[i] <- "Cheap"
  
  
  if(rfm$recency_frequency[i] == "Top" && rfm$MONETARY_CLASS[i] == "medium") rfm$recency_frequency_monetary[i] <- "Gold"
  if(rfm$recency_frequency[i] == "Leaving Top" && rfm$MONETARY_CLASS[i] == "medium") rfm$recency_frequency_monetary[i] <- "Silver"
  if(rfm$recency_frequency[i] == "Engaged" && rfm$MONETARY_CLASS[i] == "medium") rfm$recency_frequency_monetary[i] <- "Bronze"
  if(rfm$recency_frequency[i] == "Leaving" && rfm$MONETARY_CLASS[i] == "medium") rfm$recency_frequency_monetary[i] <- "Copper"
  if(rfm$recency_frequency[i] == "One-Timer" && rfm$MONETARY_CLASS[i] == "medium") rfm$recency_frequency_monetary[i] <- "Tin"
  
  
  if(rfm$recency_frequency[i] == "Top" && rfm$MONETARY_CLASS[i] == "high") rfm$recency_frequency_monetary[i] <- "Diamond"
  if(rfm$recency_frequency[i] == "Leaving Top" && rfm$MONETARY_CLASS[i] == "high") rfm$recency_frequency_monetary[i] <- "Gold"
  if(rfm$recency_frequency[i] == "Engaged" && rfm$MONETARY_CLASS[i] == "high") rfm$recency_frequency_monetary[i] <- "Silver"
  if(rfm$recency_frequency[i] == "Leaving" && rfm$MONETARY_CLASS[i] == "high") rfm$recency_frequency_monetary[i] <- "Bronze"
  if(rfm$recency_frequency[i] == "One-Timer" && rfm$MONETARY_CLASS[i] == "high") rfm$recency_frequency_monetary[i] <- "Copper"
  
}

table(rfm$recency_frequency_monetary)



rfm_df <- as.data.frame(rbind(c("Top", "High", "Diamond", 17341),
                              c("Top", "Medium", "Gold", 11157),
                                c("Top", "Low", "Silver", 11107),
                                c("Leaving Top", "High", "Gold", 11157),
                                c("Leaving Top", "Medium", "Silver", 457),
                                c("Leaving Top", "Low", "Bronze",  33249),
                                c("Engaged", "High", "Silver", 11107),
                                c("Engaged", "Medium", "Bronze", 33249),
                                c("Engaged", "Low", "Copper", 27460),
                                c("Leaving", "High", "Bronze", 33249),
                                c("Leaving", "Medium", "Copper", 27460),
                                c("Leaving", "Low", "Tin", 28319),
                                c("One Timer", "High", "Copper", 27460),
                                c("One Timer", "Medium", "Tin", 28319),
                                c("One Timer", "Low", "Cheap", 17639 )))


colnames(rfm_df) <- c("recency_frequency", "Monetary", "Level", "Value")

rfm_df$recency_frequency <- factor(rfm_df$recency_frequency,
                    levels = c("Top", "Leaving Top",
                               "Engaged", "Leaving","One-Timer"))

rfm_df$Monetary <- factor(rfm_df$Monetary,
                          levels = c("Low", "Medium", "High"))

rfm_df$Value <- as.numeric(rfm_df$Value)


# plot matrix

ggplot(rfm_df, aes(x = recency_frequency, y = Monetary, fill = Value)) + 
  geom_tile() +
  geom_text(aes(label = Level)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()

# plot  RFM classes
table(is.na(rfm$recency_frequency_monetary))

rfm_plot <- as.data.frame(table(rfm$recency_frequency_monetary))
ggplot(data = rfm_plot,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                   
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "RFM Distribution",
       x     = "RFM Classes",
       y     = "Total Clients") +                 
  theme_minimal() +                              
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Bronze", "Cheap", "Copper",
                              "Gold", "Silver", "Tin")) + 
  guides(fill = FALSE)

# la classe più frequente è la bronze


###### RFM table

# utilizzando libreria già fatta
library(rfm)
library(lubridate)
library(magrittr)

df_7_tic_clean_final$IMPORTO_NETTO<-(df_7_tic_clean_final$IMPORTO_LORDO)-(df_7_tic_clean_final$SCONTO)
analysis_date<-lubridate::as_date("2019/04/30")

result<-rfm_table_order(df_7_tic_clean_final, ID_CLI, TIC_DATE, IMPORTO_NETTO, analysis_date)

rfm_results<-as.data.frame(result$rfm)

# heat map

rfm_heatmap(result)

# monetary vaulue si concentra nella classe high frequency

# label of the various segments

segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

# set the upper and lower bounds for recency, frequency, and monetary

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

# We use the segments and the bounds we previously established to group our users into different segments


library(kableExtra)
segment <- rfm_segment(result,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)


head(segment) %>% 
  kable() %>% 
  kable_classic_2()

# RFM visuals

# median recency by segment 

rfm_plot_median_recency(segment)

# median monetary_value by segment 

rfm_plot_median_monetary(segment)

# Il grafico  mostra l'importanza del segmento Champions 
# in quanto hanno di gran lunga il valore monetario mediano più grande.



