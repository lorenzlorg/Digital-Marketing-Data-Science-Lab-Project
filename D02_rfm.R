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

# ! aggiungo qualche grafico (matrice colorata con assi frequency e recency e valori RECENCY_FREQUENCY)
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

# ! aggiungo qualche grafico (matrice colorata con assi monetary e valori matrice precedente e valori RECENCY_FREQUENCY_MONETARY)





# In alternativa alla procedura seguita sopra si potrebbe optare per l'utilizzo 
# della libreria "rfm"




































