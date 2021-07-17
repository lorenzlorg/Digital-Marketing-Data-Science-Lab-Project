# CLUSTERING

#### preparazione dataset ####
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
# l'algorimto k-means è molto sensibile agli outliers
# sarebbe da considerare con più attenzione, sulla base di ulteriore dettagli forniti dal cliente

# non considero l'ID_CLI
customer_data <- churn_dataset[,-1]


#### setting clustering ####

# standardizzazione
customer_data_stand <- scale(customer_data) 


# Number of Clusters
library(factoextra)

k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(customer_data_stand, k)$tot.withinss})

library(ggplot2)
g <- qplot(x = 1:k_max, y = twcss, geom = 'line')
g + scale_x_continuous(breaks = seq(0, 10, by = 1))


# per individuare in maniera più precisa il numero ottimale di cluster da considerare
# si possono consideare i metodi sotto riportati (non eseguiti per limiti computazionali)

library(factoextra)
library(NbClust)

# Elbow method
# fviz_nbclust(customer_data_stand, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
#   labs(subtitle = "Elbow method") # add subtitle


# Silhouette method
# fviz_nbclust(customer_data_stand, kmeans, method = "silhouette") +
#   labs(subtitle = "Silhouette method")


# Gap statistic
# fviz_nbclust(customer_data_stand, kmeans,  # - TROPPO LENTO
#              nstart = 25,
#              method = "gap_stat",
#              nboot = 500 # reduce it for lower computation time (but less precise results)
# ) +
#   labs(subtitle = "Gap statistic method")


# NbClust
# nbclust_out <- NbClust(  
#   data = customer_data_stand,
#   distance = "euclidean",
#   min.nc = 2, # minimum number of clusters
#   max.nc = 5, # maximum number of clusters
#   method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
# )


# un metodo alternativo per trovare il numero ottimale di cluster è il seguente
duda <- NbClust(customer_data_stand, distance = "euclidean", method = "ward.D2", max.nc = 9, index = "duda")
pseudot2 <- NbClust(customer_data_stand, distance = "euclidean", method = "ward.D2", max.nc = 9, index = "pseudot2")


# il valore ottimale di numero di cluster dovrebbe coincidere con il valore più
# elevato di duda a cui corrisponde il valore pseudo-T2 minore
duda$All.index
pseudot2$All.index

# oppure più semplicemente
duda$Best.nc  # numero ottimale cluster = 5



#### K-means ####
# Using K-Means Instead of the Traditional Approach
# The traditional RFM approach requires you to manually rank customers from 1 to 5 on each of their RFM features.

# vengono provati tre scenari

#### K = 3 #### 
# possiamo selezionare n cluster = 3, applichiamo l'algoritmo kmeans
km <-kmeans(customer_data_stand, centers = 3, nstart=20)

# dimensioni dei cluster identificati
# km$size
table(km$cluster)
# 18 21836  2440

km$withinss
# 8260.103  7747.000 17878.634

# valutazione qualità: The quality of a k-means partition is found by calculating the percentage of 
# the TSS “explained” by the partition using the following formula: BSS / TSS * 100 
BSS <- km$betweenss
TSS <- km$totss
BSS / TSS * 100  #  53.50411, higher quality means a higher explained percentage
# where BSS and TSS stand for Between Sum of Squares and Total Sum of Squares, respectively. 
# The higher the percentage, the better the score (and thus the quality) because 
# it means that BSS is large and/or WSS is small


# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig = t(apply(km$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))

# visualizzazione grafica
# fviz_cluster(km, data = customer_data_stand)

# TOT_PURCHASE TOT_SCONTO NUM_OF_PURCHASES
# 1   72378.6839 9631.48222         9.777778   # size: 18
# 2     695.0499   41.83827         5.925215   # size: 21836
# 3    3542.1070  340.72767        17.416803   # size: 2440






####  K = 4 #### 
# possiamo selezionare n cluster = 4, applichiamo l'algoritmo kmeans
km <-kmeans(customer_data_stand, centers = 4, nstart=20)

# dimensioni dei cluster identificati
# km$size
table(km$cluster)
# 2393 21744     9   148

km$withinss
# 8884.459 8329.182 4647.728 4183.320

# valutazione qualità
BSS <- km$betweenss
TSS <- km$totss
BSS / TSS * 100   # 64.26311

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







####  K = 5 #### 
# possiamo selezionare n cluster = 5, applichiamo l'algoritmo kmeans
km <-kmeans(customer_data_stand, centers = 5, nstart=20)

# dimensioni dei cluster identificati
# km$size
table(km$cluster)
# 4497   117 19096   575     9

km$withinss
# 6274.085 3538.018 3399.972 3313.790 4647.728

# valutazione qualità
BSS <- km$betweenss
TSS <- km$totss
BSS / TSS * 100   # 70.94616

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

# N.B.
# With more classes, the partition will be finer, and the BSS contribution will be higher. 
# On the other hand, the “model” will be more complex, requiring more classes. 
# In the extreme case where k = n (each observation is a singleton class), we have BSS = TSS, 
# but the partition has lost all interest.



# per il clustering si potrebbero usare anche K-medians o DBSCAN che sono più robusti agli outliers

#### K-medians ####
install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)

# in base ai risultati precedenti, k = 5
pam.res <- pam(customer_data_stand, 5)
print(pam.res)

# The function pam() returns an object of class pam which components include:
# medoids: Objects that represent clusters
# clustering: a vector containing the cluster number of each object

# Cluster medoids
pam.res$medoids

# scale data -> original data
data.orig = t(apply(pam.res$medoids, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))



# TOT_PURCHASE TOT_SCONTO NUM_OF_PURCHASES
# [1,]       490.29      18.81                6
# [2,]       687.14      24.15                9
# [3,]       234.18       5.00                4
# [4,]      3812.71     405.53                7
# [5,]      1559.80      82.78               17

# Cluster numbers
head(pam.res$clustering)



#### DBSCAN ####
install.packages("fpc")
install.packages("dbscan")
library("fpc")
library("dbscan")


# in questo caso non è necessario stabilire a priori il numero di cluster da considerare
Dbscan_cl <- dbscan(customer_data_stand, eps = 0.45, MinPts = 5)

# The clustering contains 7 cluster(s) and 262 noise points.
# 
# 0     1     2     3     4     5     6     7 
# 262 23996     5     5     6    12     3     5 

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster, iris$Species)

