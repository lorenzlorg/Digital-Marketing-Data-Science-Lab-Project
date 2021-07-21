### RFM MODEL: provides a deterministic description of the value of each customer in term of purchase behaviour

# ! Modificare un po la suddivisione delle classi
# ! Vedi aggiunte grafici

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

rfm_data<-df_7_tic_clean_final  %>%
  filter(TIC_DATE > as.Date("28/02/2019", format = "%d/%m/%Y"))
rfm_data


#### RECENCY VALUE: days passed from last purchase ####

recency_data<-rfm_data %>%
                filter(DIREZIONE==1) %>%
                group_by(ID_CLI) %>%
                summarise(LAST_PURCHASE_DATE=max(TIC_DATE))

recency_data

# costruisco la variabile RECENCY 
recency_data$RECENCY_VALUE <- difftime(as.Date("30/04/2019",
                                      format="%d/%m/%Y"),
                                  recency_data$LAST_PURCHASE_DATE,
                              units = "days")
recency_data$RECENCY_VALUE <- as.numeric(recency_data$RECENCY_VALUE, units="days")

library(ggplot2)
library(gridExtra)
ggplot(recency_data) + geom_density(aes(x= RECENCY_VALUE))

# la recency è ottenuta come differenza tra l'ultimo giorno di acquisto per cliente e l'ultimo giorno di rilevazione
# as esempio: il cliente con ID 5 ha acquistato l'ultima volta il 23 novembre 2018, ovvero 158 giorni entro l'ultima data di rilevazione


#### FREQUENCY VALUE: total number of purchase in the reference range ####

frequency_data <- rfm_data %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI)  %>% 
  summarise(FREQUENCY_VALUE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(FREQUENCY_VALUE))
frequency_data$FREQUENCY_VALUE <- as.numeric(frequency_data$FREQUENCY_VALUE)

frequency_data

ggplot(frequency_data) + geom_density(aes(x = FREQUENCY_VALUE))



#### MONETARY VALUE: amount spent in the reference range ####

monetary_data <- rfm_data %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI) %>% 
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            MONETARY_VALUE = IMPORTO_LORDO - SCONTO) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))
monetary_data$MONETARY_VALUE <- as.numeric(monetary_data$MONETARY_VALUE)

monetary_data
ggplot(monetary_data) + geom_density(aes(x = MONETARY_VALUE))



#### Merge Recency, Frequency, Monetary ####

rfm_data_clean <- merge(frequency_data,
             monetary_data,  
             by = "ID_CLI") 

rfm_data_clean <- merge(rfm_data_clean,           
             recency_data,  
             by = "ID_CLI")

sum(is.na(rfm_data_clean)) 

rfm_data_clean <- rfm_data_clean[,c(1,2,5,7)]

# analizzo le distribuzioni
hist(as.numeric(rfm_data_clean$RECENCY_VALUE), main = "Distribution RECENCY")
hist(as.numeric(rfm_data_clean$FREQUENCY_VALUE), main = "Distribution FREQUENCY")
hist(as.numeric(rfm_data_clean$MONETARY_VALUE), main = "Distribution MONETARY")

#### RECENCY CLASS ####

summary(rfm_data_clean$RECENCY_VALUE)  # si osservano i valori dei quantili qui riportati

quantile(rfm_data_clean$RECENCY_VALUE)
# 0%  25%  50%  75% 100% 
# 0   10   24   41   60  # cutpoints

rfm_data_clean$RECENCY_CLASS <- 0
rfm_data_clean$RECENCY_CLASS[rfm_data_clean$RECENCY_VALUE <= 10.00] <- "low"
rfm_data_clean$RECENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 10.00 & rfm_data_clean$RECENCY_VALUE <= 41.00] <- "medium"
rfm_data_clean$RECENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 41.00] <- "high"

rfm_data_clean %>% 
  group_by(RECENCY_CLASS) %>%
  summarise(Count = n())

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

# La classe media è la più frequente: la maggior parte dei clienti ha effettuato degli acquisti abbastanza recenti



#### FREQUENCY CLASS ####

summary(rfm_data_clean$FREQUENCY_VALUE)  # si osservano i valori dei quantili qui riportati

quantile(rfm_data_clean$FREQUENCY_VALUE)

rfm_data_clean$FREQUENCY_CLASS <- 0
rfm_data_clean$FREQUENCY_CLASS[rfm_data_clean$RECENCY_VALUE <= 2] <- "low"
rfm_data_clean$FREQUENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 2 & rfm_data_clean$RECENCY_VALUE <= 5] <- "medium"
rfm_data_clean$FREQUENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 5] <- "high"

rfm_data_clean %>%
  group_by(FREQUENCY_CLASS) %>%
  summarise(Count = n())

frequency_var <- as.data.frame(table(rfm_data_clean$FREQUENCY_CLASS))

ggplot(data = frequency_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                 
  labs(x     = "Frequency Type",
       y     = "Total Purchases") +               
  theme_classic() +                             
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("High", "Low", "Medium")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# la maggior parte dei clienti ha una frequency associata di tipo high



#### MONETARY CLASS ####
summary(rfm_data_clean$MONETARY_VALUE)

quantile(rfm_data_clean$MONETARY_VALUE)

rfm_data_clean$MONETARY_CLASS <- 0
rfm_data_clean$MONETARY_CLASS[rfm_data_clean$MONETARY_VALUE <= 31.30] <- "low"
rfm_data_clean$MONETARY_CLASS[rfm_data_clean$MONETARY_VALUE > 31.30 & rfm_data_clean$MONETARY_VALUE <= 247.51] <- "medium"
rfm_data_clean$MONETARY_CLASS[rfm_data_clean$MONETARY_VALUE > 247.51] <- "high"


rfm_data_clean %>% 
  group_by(MONETARY_CLASS) %>%
  summarise(Count = n())


monetary_var <- as.data.frame(table(rfm_data_clean$MONETARY_CLASS))

ggplot(data = monetary_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Monetary Classes",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("High", "Low", "Medium")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# più della metà dei clienti viene categorizzata come medium dal punto di vista di monetary

# avendo a disposizione maggiori informazioni da parte dell'esperto di dominio
# si potrebbe procedere ad una più attenta e precisa suddivisione in classi di
# recency, frequency, monetary

#### RECENCY E FREQUENCY COMBINED ####
# in modo tale che si vengano a creare delle nuove classi che descrivono i clienti

rfm_data_clean$RECENCY_FREQUENCY <- NA
for(i in c(1:nrow(rfm_data_clean))){
  if(rfm_data_clean$RECENCY_CLASS[i] == "low" && rfm_data_clean$FREQUENCY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY[i] <- "One-Timer"
  if(rfm_data_clean$RECENCY_CLASS[i] == "medium" && rfm_data_clean$FREQUENCY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY[i] <- "One-Timer"
  if(rfm_data_clean$RECENCY_CLASS[i] == "high" && rfm_data_clean$FREQUENCY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Leaving"
  if(rfm_data_clean$RECENCY_CLASS[i] == "low" && rfm_data_clean$FREQUENCY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Engaged"
  if(rfm_data_clean$RECENCY_CLASS[i] == "medium" && rfm_data_clean$FREQUENCY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Engaged"
  if(rfm_data_clean$RECENCY_CLASS[i] == "high" && rfm_data_clean$FREQUENCY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Leaving"
  if(rfm_data_clean$RECENCY_CLASS[i] == "low" && rfm_data_clean$FREQUENCY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Top"
  if(rfm_data_clean$RECENCY_CLASS[i] == "medium" && rfm_data_clean$FREQUENCY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Top"
  if(rfm_data_clean$RECENCY_CLASS[i] == "high" && rfm_data_clean$FREQUENCY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Leaving Top"
}

rfm_data_clean %>% 
  group_by(RECENCY_FREQUENCY) %>%
  summarise(Count = n())

table(rfm_data_clean$RECENCY_FREQUENCY)


recency_frequency_var <- as.data.frame(table(rfm_data_clean$RECENCY_FREQUENCY))

ggplot(data = recency_frequency_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Recency Frequency",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Engaged", "Leaving", "Leaving Top", "One-Timer", "Top")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# One-Timer: clienti che hanno acquistato recentemente/abbastanza recentementente ma con scarsa frequenza
# Leaving: clienti che non hanno acquistato recentemente e con scarsa/media frequenza
# Engaged: clienti che hanno acquistato recentemente/abbastanza recentemente e con media frequenza
# Top: cienti che hanno acquistato recentemnte/abbastanza recentemente e con alta frequenza
# Leaving Top: cienti che non hanno acquistato di recente ma con alta frequenza

# ! aggiungo qualche grafico
# ! correggere grafico, manca "Leaving"

#### RECENCY E FREQUENCY COMBINED with MONETARY ####

rfm_data_clean$RECENCY_FREQUENCY_MONETARY <- NA

for(i in c(1:nrow(rfm_data_clean))){
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Top" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Silver"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving Top" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Bronze"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Engaged" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Copper"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Tin"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "One-Timer" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Cheap"
  
  
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Top" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Gold"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving Top" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Silver"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Engaged" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Bronze"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Copper"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "One-Timer" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Tin"
  
  
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Top" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Diamond"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving Top" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Gold"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Engaged" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Silver"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Bronze"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "One-Timer" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Copper"
  
}

rfm_data_clean %>% 
  group_by(RECENCY_FREQUENCY_MONETARY) %>%
  summarise(Count = n())


recency_frequency_monetary_var <- as.data.frame(table(rfm_data_clean$RECENCY_FREQUENCY_MONETARY))

ggplot(data = recency_frequency_monetary_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="turquoise3") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Recency Frequency Monetary",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Bronze", "Cheap", "Copper", "Diamond", "Gold", "Silver", "Tin")) + 
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# Bronze-Timer: 
# Cheap: 
# Copper: 
# Diamond: 
# Gold: 
# Silver:
# Tin:

# ! aggiungo qualche grafico



























# ALTRO ---------
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







###### RFM table libreria --------

# utilizzando libreria già fatta
library(rfm)
library(lubridate)
library(magrittr)

df_7_tic_clean_final$MONETARY_VALUE<-(df_7_tic_clean_final$IMPORTO_LORDO)-(df_7_tic_clean_final$SCONTO)
analysis_date<-lubridate::as_date("2019/04/30")

result<-rfm_table_order(df_7_tic_clean_final, ID_CLI, TIC_DATE, MONETARY_VALUE, analysis_date)

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



