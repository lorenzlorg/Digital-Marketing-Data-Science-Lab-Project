# CLUSTERING

# preparazione dataset
churn_dataset <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATETIME >= as.Date("01/01/2019",
                                 format="%d/%m/%Y"))
  

churn_dataset <- churn_dataset %>%
  group_by(ID_CLI) %>%
  summarize(
            TOT_PURCHASE = sum(IMPORTO_LORDO),
            TOT_SCONTO = sum(SCONTO),
            NUM_OF_PURCHASES = n_distinct(ID_SCONTRINO),
  ) 



# si considerano i clienti che hanno effettuato più di 1 acquisto
churn_dataset <- churn_dataset %>%
  filter(NUM_OF_PURCHASES > 3)


# complessivamente
str(churn_dataset)

# controllo nuovamente la presenza di eventuali na
sapply(churn_dataset, function(x) sum(is.na(x)))


# attenzione agli outliers
# ----


customer_data <- churn_dataset[,-1]


# setting clustering

# standardizzazione
customer_data_stand <- scale(customer_data) 


# Number of Clusters

library(factoextra)

k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(customer_data_stand, k)$tot.withinss})

library(ggplot2)
g <- qplot(x = 1:k_max, y = twcss, geom = 'line')
g + scale_x_continuous(breaks = seq(0, 10, by = 1))




# K = 3
# possiamo selezionare n cluster = 3, applichiamo l'algoritmo kmeans
km <-kmeans(customer_data_stand, centers = 3, nstart=20)

# dimensioni dei cluster identificati
km$size
# 18 21836  2440

km$withinss
# 8260.103  7747.000 17878.634

# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig = t(apply(km$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))

# visualizzazione grafica
# fviz_cluster(km, data = customer_data_stand)

# TOT_PURCHASE TOT_SCONTO NUM_OF_PURCHASES
# 1   72378.6839 9631.48222         9.777778   # size: 18
# 2     695.0499   41.83827         5.925215   # size: 21836
# 3    3542.1070  340.72767        17.416803   # size: 2440






# K = 4
# possiamo selezionare n cluster = 4, applichiamo l'algoritmo kmeans
km <-kmeans(customer_data_stand, centers = 4, nstart=20)

# dimensioni dei cluster identificati
km$size
# 2393 21744     9   148

km$withinss
# 8884.459 8329.182 4647.728 4183.320

# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig = t(apply(km$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))

# visualizzazione grafica
# fviz_cluster(km, data = customer_data_stand)

# TOT_PURCHASE  TOT_SCONTO NUM_OF_PURCHASES
# 1    2439.4871   187.47748        17.972002   # size: 2393
# 2     725.5564    46.26548         5.864928   # size: 21744
# 3  104990.8267 12376.54111        12.111111   # size: 9
# 4   17321.3422  2380.42986         9.547297   # size: 148







# K = 5
# possiamo selezionare n cluster = 5, applichiamo l'algoritmo kmeans
km <-kmeans(customer_data_stand, centers = 5, nstart=20)

# dimensioni dei cluster identificati
km$size
# 4497   117 19096   575     9

km$withinss
# 6274.085 3538.018 3399.972 3313.790 4647.728

# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig = t(apply(km$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))

# visualizzazione grafica
# fviz_cluster(km, data = customer_data_stand)

# TOT_PURCHASE  TOT_SCONTO NUM_OF_PURCHASES
# 1    1999.1261   165.93408        11.740147   # size: 4491
# 2     575.0564    31.58948         5.334189   # size: 19094
# 3    3250.8821   235.70480        28.247818   # size: 573
# 4  104990.8267 12376.54111        12.111111   # size: 9
# 5   18557.4143  2547.20740         9.330709   # size: 127













