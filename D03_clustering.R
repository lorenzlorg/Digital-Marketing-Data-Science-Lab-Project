# CLUSTERING

# ! PCA
# ! check risultati commentati

#### preparazione dataset ####

# variabili da considerare
# NUMERO ACQUISTI
# NUMERO ARTICOLI
# TOT SPESA
# TOTO SCONTO

# si prende un considerazione un periodo temporale specifico
clustering_dataset <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATETIME >= as.Date("01/01/2019",
                                 format="%d/%m/%Y"))

# si selezionano le variabili di interesse
clustering_dataset <- clustering_dataset %>%
  group_by(ID_CLI) %>%
  summarize(
    NUM_OF_PURCHASES = n_distinct(ID_SCONTRINO),
    NUM_OF_ARTICLES = n_distinct(ID_ARTICOLO),
    TOT_PURCHASE = sum(IMPORTO_LORDO),
    TOT_SCONTO = sum(SCONTO)
  ) 

# si considerano i clienti che hanno effettuato più di 1 acquisto
clustering_dataset <- clustering_dataset %>%
  filter(NUM_OF_PURCHASES > 3)


# complessivamente
str(clustering_dataset)

# controllo la presenza di eventuali na
sapply(clustering_dataset, function(x) sum(is.na(x)))  # non sono presenti valori mancanti


# attenzione agli outliers
# l'algorimto k-means è molto sensibile agli outliers
# sarebbe da considerare con più attenzione, sulla base di ulteriorI dettagli forniti dal cliente

# non considero l'ID_CLI
customer_data <- clustering_dataset[,-1]


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
# questo grafico deve essere letto da destra verso sinistra
# si deve trovare il punto in cui la curva tende a salire in modo più consistente
# dal grafico sopra riportato si potrebbe pensare al valore 3, 4 o 5, come numero ottimale di cluster suggerito

# per individuare in maniera più precisa e analitica il numero ottimale di cluster da considerare
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
# 2      3      4      5      6      7      8      9 
# 0.5921 0.6235 0.6254 1.3995 0.7372 0.6309 0.4539 0.7500 

pseudot2$All.index
# 2          3          4          5          6          7          8         9
# 832.0622   723.8854 13827.2146    -1.9982  3541.1738   560.3696   287.5806  2371.9318  


# oppure più semplicemente
duda$Best.nc  # numero ottimale cluster = 5
# Number_clusters     Value_Index 
# 5.0000          1.3995 


#### K-means ####
# Using K-Means Instead of the Traditional Approach
# The traditional RFM approach requires you to manually rank customers from 1 to 5 on each of their RFM features.

# vengono provati tre scenari, K=3,4,5

# K = 3
# possiamo selezionare n cluster = 3, applichiamo l'algoritmo kmeans
km_3 <-kmeans(customer_data_stand, centers = 3, nstart=20)

# general info
str(km_3)

# dimensioni dei cluster identificati
# km_3$size
table(km_3$cluster)
# 1     2     3 
# 2833 21443    18 

km_3$withinss
# 25609.158 14007.608  8312.117

# valutazione qualità: The quality of a k-means partition is found by calculating the percentage of 
# the TSS “explained” by the partition using the following formula: BSS / TSS * 100 
BSS_km_3 <- km_3$betweenss
TSS_km_3 <- km_3$totss
BSS_km_3 / TSS_km_3 * 100  #  50.67624 
# higher quality means a higher explained percentage
# where BSS and TSS stand for Between Sum of Squares and Total Sum of Squares, respectively. 
# The higher the percentage, the better the score (and thus the quality) because 
# it means that BSS is large and/or WSS is small


# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig_km_3 <- t(apply(km_3$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))
data.orig_km_3[,c(1, 2)] <- round(data.orig_km_3[,c(1, 2)])
data.orig_km_3

# NUM_OF_PURCHASES NUM_OF_ARTICLES TOT_PURCHASE TOT_SCONTO
# 16                    73            3198.2312  287.73131
# 6                     21            688.3021   43.36208
# 10                    51            72378.6839 9631.48222

# lo scontrino medio sarebbe: TOT_PURCHASE/NUM_OF_PURCHASES

# visualizzazione grafica
fviz_cluster(km_3, data = customer_data_stand, palette=c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



#  K = 4
# possiamo selezionare n cluster = 4, applichiamo l'algoritmo kmeans
km_4 <-kmeans(customer_data_stand, centers = 4, nstart=20)

# general info
str(km_4)

# dimensioni dei cluster identificati
# km_4$size
table(km_4$cluster)
# 1     2     3     4 
# 18 18524  5037   715 

km_4$withinss
# 8312.117  6625.369 16067.988  8473.249

# valutazione qualità
BSS_km_4 <- km_4$betweenss
TSS_km_4 <- km_4$totss
BSS_km_4 / TSS_km_4 * 100   # 59.37233

# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig_km_4 <- t(apply(km_4$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))
data.orig_km_4[,c(1, 2)] <- round(data.orig_km_4[,c(1, 2)])
data.orig_km_4

# NUM_OF_PURCHASES NUM_OF_ARTICLES TOT_PURCHASE TOT_SCONTO
# 10                    51          72378.684     9631.48222
# 5                     18          559.086       32.39457
# 11                    48          2111.392      182.54776
# 25                    113         3955.607      315.22485

# visualizzazione grafica
fviz_cluster(km_4, data = customer_data_stand, palette=c("#2E9FDF", "#00AFBB", "#E7B800", "#FF0000"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



#  K = 5
# possiamo selezionare n cluster = 5, applichiamo l'algoritmo kmeans
km_5 <-kmeans(customer_data_stand, centers = 5, nstart=20)

# general info
str(km_5)

# dimensioni dei cluster identificati
# km_5$size
table(km_5$cluster)
# 1     2     3     4     5 
# 18261   700   113  5211     9 

km_5$withinss
# 6613.788 6997.301 3650.434 9528.098 4684.452

# valutazione qualità
BSS_km_5 <- km_5$betweenss
TSS_km_5 <- km_5$totss
BSS_km_5 / TSS_km_5 * 100   # 67.60994

# riconvertiamo i valori standardizzati per rendere chiaro l'output
data.orig_km_5 <- t(apply(km_5$centers, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))
data.orig_km_5[,c(1, 2)] <- round(data.orig_km_5[,c(1, 2)])
data.orig_km_5

# visualizzazione grafica
fviz_cluster(km_5, data = customer_data_stand, palette=c("#2E9FDF", "#00AFBB", "#E7B800","#01AFBB","#E8D800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# NUM_OF_PURCHASES NUM_OF_ARTICLES TOT_PURCHASE  TOT_SCONTO 
# 5                     18          570.7831      34.42337
# 25                    114         3548.1202     259.29213
# 9                     41          19366.6642    2691.37265
# 11                    48          1742.9595     132.93016
# 12                    62          104990.8267   12376.54111

# N.B.
# With more classes, the partition will be finer, and the BSS contribution will be higher. 
# On the other hand, the “model” will be more complex, requiring more classes. 
# In the extreme case where k = n (each observation is a singleton class), we have BSS = TSS, 
# but the partition has lost all interest.


# per il clustering si potrebbero usare anche K-medians o DBSCAN che sono più robusti agli outliers

#### K-medians ####
library(cluster)
library(factoextra)

# in base ai risultati precedenti, k = 5
pam.res <- pam(customer_data_stand, 5)  # esecuzione lenta
print(pam.res)

# The function pam() returns an object of class pam which components include:
# medoids: Objects that represent clusters
# clustering: a vector containing the cluster number of each object

# Cluster medoids
pam.res$medoids

# Number obs for each cluster
pam.res.list <- pam.res$clustering
pam.res.df <- data.frame(matrix(unlist(pam.res.list), nrow=length(pam.res.list), byrow=TRUE))
names(pam.res.df)[1] <- "cluster"
pam.res.df %>% 
  group_by(pam.res.df$cluster) %>%
  summarise(Count = n())
# cluster 1  7816
# cluster 2  2490
# cluster 3  8294
# cluster 4  4531
# cluster 5  1163


# scale data -> original data
data.orig_pam <- t(apply(pam.res$medoids, 1, function(r)r*attr(customer_data_stand,'scaled:scale') + 
                      attr(customer_data_stand, 'scaled:center')))

# NUM_OF_PURCHASES NUM_OF_ARTICLES TOT_PURCHASE TOT_SCONTO
# 6                     20            491.20      19.23
# 13                    46            1303.14     71.99
# 4                     11            276.16      13.50
# 7                     38            1086.10     64.72
# 19                    92            3234.32     234.36




#### DBSCAN ####
library("fpc")
library("dbscan")

# in questo caso non è necessario stabilire a priori il numero di cluster da considerare

# scaled data
Dbscan_cl <- dbscan(customer_data_stand, eps = 0.45, MinPts = 5)


# The clustering contains 11 cluster(s) and 748 noise points.
# 
# 0     1       2     3     4     5     6     7     8     9    10    11 
# 748 23497     3     5     6     5     7     3     5     4     6     5 

# N.b.
# noise point: A selected point that is neither a core point nor a border point. 
# It means these points are outliers that are not associated with any dense clusters

# Checking cluster
Dbscan_cl$cluster

# DBSCAN visti i risultati poco soddisfacienti non viene considerato