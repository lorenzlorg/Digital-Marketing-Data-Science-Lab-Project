# Churn Model: propensity modello supervised 
# Churn event: moment when the customer stops purchasing
# Dopo 60 (o 30) giorni di inattività si possono categorizzare i clienti come churner, secondo diverse ricerche pubblicate
# Riprendendo la "the days for next purchase curve" presente nel dataset 7, si nota per circa il 90% dei clienti passano circa 70 giorni per il successivo acquisto, per l'80% passano 40 giorni
# Questo significa che dato che la maggior parte delle persone acquista dopo 70 giorni, se un cliente non dovesse acquistare questo deve essere un campanello d'allarme, un possibile churn

# [ 1 maggio 2018 ------------lockback period------------ 28 febbraio 2019) [28 febbraio 2019 ------------holdout period ------------ 30 aprile 2019]

#### 1. choosing a reference date in the past (mettersi ad una di riferimento nel passato) ####
# eventualmente in presenza di clienti molto diversi tra loto si potrebbe optare per soglie differenti
# reference date: 28 febbraio 2019, 60 giorni prima dell'ultima rilevazione

reference_date <- as.Date("28/02/2019", format = "%d/%m/%Y")



#### 2. imposing the length of an holdout period after each reference date. The length correspondes to the frequencey of the distribution and/or the purchase time scale ####
# considerando una via di mezzo tra il 90% e l'80% dei clienti coinvolti, si seleziona come numero di giorni per l'acquisto successivo 60 (vedi days for next purchase curve df7) 
# 60 giorni
# holdout period: 28 febbraio 2019 - 30 aprile 2019

holdout_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE >= reference_date)

# tutti i clienti presenti nell'holdout sono no churner, dal momento in cui hanno effettuato un acquisto dopo la reference date
holdout_period['CHURN'] <- 0



#### 3. choosing the lenght of a lookback period before the reference date ####
# non si definisce solitmante una lunghezza fissa. Bisogna controllare cosa conviene, se dunque può essere vantaggioso o meno considerare tutti i dati a nostra disposizione, a partire dal 1 maggio 2018, o meno
# lookback period: 01 maggio 2018 - 28 febbraio 2019

lookback_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE < reference_date)

# assumo per comodità di calcolo che tutti i clienti presenti nel lookback period sono churner
lookback_period['CHURN'] <- 1



#### 4. assigning to each customer a target 0/1 variable such that 1 is assigned to customers who churned in the holout period ####

holdout_period_temp <- holdout_period %>%
  select(ID_CLI, CHURN)

# aggiorno la variabile CHURN per i clienti presenti nel lookback period
library(data.table)
setDT(lookback_period)[holdout_period_temp, CHURN := i.CHURN, on = .(ID_CLI)]

table(lookback_period$CHURN)



#### 5. defining a set of potentially relevant predictors variables to be computed within the lookback period ####

# si considera il lookback period
churn_dataset <- lookback_period

churn_dataset <- churn_dataset %>%
  group_by(ID_CLI) %>%
  summarize(FIRST_PURCHASE_DATE = min(TIC_DATE),
            LAST_PURCHASE_DATE = max(TIC_DATE),
            TOT_PURCHASE = sum(IMPORTO_LORDO),
            TOT_SCONTO = sum(SCONTO),
            TOT_SPESA = TOT_PURCHASE - TOT_SCONTO,
            NUM_OF_PURCHASES = n_distinct(ID_SCONTRINO),
            #RECENCY = difftime(as.Date("30/04/2019", format="%d/%m/%Y"), LAST_PURCHASE_DATE, units = "days"),
            CHURN =names(which.max(table(CHURN)))
  ) 

churn_dataset$CHURN <- as.factor(churn_dataset$CHURN)

churn_dataset$RECENCY<-difftime(as.Date("30/04/2019",
                                          format="%d/%m/%Y"),
                                churn_dataset$LAST_PURCHASE_DATE,
                                  units = "days")


# si considerano i clienti che hanno effettuato più di 1 acquisto
churn_dataset <- churn_dataset %>%
  filter(NUM_OF_PURCHASES > 1)

# non si considerano i clienti che hanno effettuato il primo acquisto dopo il 28/02/2019 dal momento in cu si rischia di avere poche informazioni su di essi
churn_dataset <- churn_dataset %>%
  filter(FIRST_PURCHASE_DATE < reference_date)

# complessivamente
str(churn_dataset)
table(churn_dataset$CHURN)

# controllo nuovamente la presenza di eventuali na
sapply(churn_dataset, function(x) sum(is.na(x)))


# aggiungo altre feature potenzialmente rilevanti, oltre a LAST_PURCHASE_DATE, TOT_PURCHASE, NUM_OF_PURCHASE, TOT_SPESA, TOT_SCONTO
# df1: LAST_COD_FID, ID_NEG
# df2: TYP_CLI_ACCOUNT
# df3: REGION
# df4:
# df5:
# df6:
# df7:

churn_dataset <- left_join(churn_dataset, df_1_cli_fid_clean[,c("ID_CLI", "LAST_COD_FID", "NUM_FIDs", "FIRST_ID_NEG")], by = "ID_CLI")  # LAST_COD_FID, FIRST_ID_NEG, NUM_FIDs
churn_dataset$FIRST_ID_NEG <- as.factor(churn_dataset$FIRST_ID_NEG)

churn_dataset <- left_join(churn_dataset, df_2_cli_account_clean[,c("ID_CLI", "TYP_CLI_ACCOUNT")], by = "ID_CLI")  # TYP_CLI_ACCOUNT


df_2_df_3 <- left_join(df_2_cli_account_clean, df_3_cli_address_clean, by = "ID_ADDRESS")
churn_dataset <- left_join(churn_dataset, df_2_df_3[,c("ID_CLI", "REGION")], by = "ID_CLI")  # REGION
# dal momento in cui vi sarebbero troppe variabili dummy si procede nel raggruppare le regioni in macroregioni (basandosi sulla seguente suddivisione: https://www.tuttitalia.it/statistiche/nord-centro-mezzogiorno-italia/)
NORD <- c("VALLE D'AOSTA", "PIEMONTE", "LOMBARDIA", "LIGURIA", "FRIULI VENEZIA GIULIA", "VENETO", "TRENTINO ALTO ADIGE", "EMILIA ROMAGNA")
CENTRO <- c("TOSCANA", "MARCHE", "LAZIO", "UMBRIA")
MEZZOGIORNO <- c("ABRUZZO", "MOLISE", "CAMPANIA", "BASILICATA", "PUGLIA", "CALABRIA", "SICILIA", "SARDEGNA")

nord_regioni <- churn_dataset %>%
  filter(REGION %in% NORD) %>%
  mutate(REGION = "NORD")

centro_regioni <- churn_dataset %>%
  filter(REGION %in% CENTRO) %>%
  mutate(REGION = "CENTRO")

mezzogiorno_regioni <- churn_dataset %>%
  filter(REGION %in% MEZZOGIORNO) %>%
  mutate(REGION = "MEZZOGIORNO")


# non considero le regioni con valore null (altrimenti errori nell'esecuzione di alcuni modelli)
sum(is.na(churn_dataset$REGION))  # 5922 regioni non indicate
churn_dataset <- na.omit(churn_dataset)
sum(is.na(churn_dataset$REGION))  # check

churn_dataset <- rbind(nord_regioni, centro_regioni, mezzogiorno_regioni)
churn_dataset$REGION <- as.factor(churn_dataset$REGION )

# churn_dataset <- left_join(churn_dataset, df_2_cli_account_clean[,c("ID_CLI", "TYP_JOB")], by = "ID_CLI")  # TYP_JOB non considero la seguente variabile dal momento in cui presenta troppi valori mancanti

str(churn_dataset)
summary(churn_dataset)
table(churn_dataset$CHURN)  # nel dataset vi sono 71753 churner e 56585 no churner

# vengono eliminate le variabili che non interessano per le analisi
churn_dataset$ID_CLI <- NULL
churn_dataset$FIRST_PURCHASE_DATE <- NULL
churn_dataset$LAST_PURCHASE_DATE <- NULL

# *si potrebbero considerare come predittori le seguenti variabili:
# TOT_PURCHASE + TOT_SCONTO + TOT_SPESA + NUM_OF_PURCHASES + REGION + LAST_COD_FID

var_num <- c("TOT_PURCHASE", "TOT_SCONTO", "TOT_SPESA", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])
# si nota, come prevedibile, TOT_PURCHASE e TOT_SPESA sono correlate in maniera molto elevata. Si procede dunque considerando solamente TOT_PURCHASE 

var_num <- c("TOT_PURCHASE", "TOT_SCONTO", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])
# si decide di considerare anche solo una tra TOT_PURCHASE E TOT_SCONTO

var_num <- c("TOT_PURCHASE", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])

# dunque i predittori considerati risultano essere:
# TOT_PURCHASE + NUM_OF_PURCHASES + REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + LAST_COD_FID + FIRST_ID_NEG

# per controllare complessivamente la correlazione tra le variabili
library(corrplot)
numeric.var <- sapply(churn_dataset, is.numeric)
corr.matrix <- cor(churn_dataset[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")



#### 6. Apply models #### 

# install.packages('caret')
library(caret)
train_index <- createDataPartition(churn_dataset$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- churn_dataset[train_index,]
test <- churn_dataset[-train_index,]

table(train$CHURN)
table(test$CHURN)

prop.table(table(train$CHURN))
prop.table(table(test$CHURN))
# check dataset sbilanciato 


#### Decision Trees #### 

# Fitting The Model
# install.packages("MLmetrics")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library("MLmetrics")
library(caret)

tree.model <- rpart(CHURN ~ TOT_PURCHASE + RECENCY + NUM_OF_PURCHASES + REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + LAST_COD_FID + FIRST_ID_NEG,  # bisognerebbe avere variabili categoriche
                    data = train, method = "class")

rpart.plot(tree.model, extra = "auto")  # la variabile più importante è NUM_OF_PURCHASES, chi ha fatto meno di 7 acquisti è più probabile che sia un churner
summary(tree.model) 
printcp(tree.model) 

# Making Predictions
tree.pred <- predict(tree.model, test, type = "class")  # viene utilizzato il moodello per fare le previsioni sul test set
tree.prob <- predict(tree.model, test, type="prob")

# Evaluating The Model
tree.result <- confusionMatrix(tree.pred, test$CHURN)

table(Predicted = tree.pred, Actual = test$CHURN)

tree.accuracy <- Accuracy(tree.pred, test$CHURN) # 0.69
tree.precision <- precision(tree.pred, test$CHURN,relevant = '1') # 0.73
tree.recall <- recall(tree.pred, test$CHURN,relevant = '1') # 0.82
tree.F1 <- F1_Score(tree.pred, test$CHURN,positive = '1') # 0.77


# ROC
library(ROCR)
pr <- prediction(tree.prob[,2], test$CHURN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC value 
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


####  Random Forest #### 

# Fitting The Model
# install.packages(randomForest)
library(randomForest)
# memory.limit(100000)
rf.model <- randomForest(CHURN ~  TOT_PURCHASE + RECENCY + NUM_OF_PURCHASES + REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + LAST_COD_FID + FIRST_ID_NEG,
                         data = train , ntree = 100)
print(rf.model)

varImpPlot(rf.model, sort=T, n.var = 4, main = 'Features Importance')
# come si può notare la variabile più importante risulta essere NUM_OF_PURCHASES


# Making Predictions
rf.pred <- predict(rf.model, test, type = "class")  
rf.prob <- predict(rf.model, test, type="prob")

# Evaluating The Model
rf.result <- confusionMatrix(rf.pred, test$CHURN)

table(Predicted = rf.pred, Actual = test$CHURN)

rf.accuracy <- Accuracy(rf.pred,test$CHURN) # 
rf.precision <- precision(rf.pred, test$CHURN,relevant = '1') # 
rf.recall <- recall(rf.pred, test$CHURN,relevant = '1') # 
rf.F1 <- F1_Score(rf.pred, test$CHURN,positive = '1') # 


# ROC
library(ROCR)
pr <- prediction(rf.prob[,2], test$CHURN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


# AUC 
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



####  Logistic Regression #### 
# There’s different types of GLMs, which includes logistic regression. To specify that we want to perform a binary logistic regression, we’ll use the argument family=binomial.

# Fitting The Model
logistic.model <- glm(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + REGION + LAST_COD_FID,
                      data = train, family=binomial)
summary(logistic.model)

# Making Predictions
logistic.prob <- predict(logistic.model, test, type="response")  
logistic.pred = rep("0", length(logistic.prob))
logistic.pred[logistic.prob > 0.5] = "1"
logistic.pred <- as.factor(logistic.pred)


# Evaluating The Model
logistic.result <- confusionMatrix(logistic.pred, test$CHURN)

table(Predicted = logistic.pred, Actual = test$CHURN)

logistic.accuracy <- Accuracy(logistic.pred,test$CHURN) # 
logistic.precision <- precision(logistic.pred, test$CHURN,relevant = '1') # 
logistic.recall <- recall(logistic.pred, test$CHURN,relevant = '1') # 
logistic.F1 <- F1_Score(logistic.pred, test$CHURN,positive = '1') # 



# ROC
library(ROCR)
pr <- prediction(logistic.prob, test$CHURN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


# AUC 
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



#### Confronto modelli ####
Modello <- c("Decision Tree", "Random Forest", "Logistic Regression")


# ACCURACY: ratio of correctly predicted observation to the total observations
tree.accuracy
rf.accuracy
logistic.accuracy

Accuracy <- c(tree.accuracy,
           rf.accuracy,
           logistic.accuracy)
Accuracy_results <- data.frame(Modello, Accuracy)
View(Accuracy_results)

# PRECISION: ratio of correctly predicted positive observations to the total predicted positive observations
tree.precision
rf.precision
logistic.precision

Precision <- c(tree.precision,
              rf.precision,
              logistic.precision)
Precision_results <- data.frame(Modello, Precision)
View(Precision_results)

# RECALL:  ratio of correctly predicted positive observations to the all observations in actual class
tree.recall
rf.recall
logistic.recall

Recall <- c(tree.precision,
               rf.precision,
               logistic.precision)
Recall_results <- data.frame(Modello, Recall)
View(Recall_results)

# F1: the weighted average of Precision and Recall
tree.F1
rf.F1
logistic.F1

F1_score <- c(tree.F1,
              rf.F1,
              logistic.F1)
F1_score_results <- data.frame(Modello, F1_score)
View(F1_score_results)

# overview risultati
overview_results <- data.frame(Modello,  Accuracy_results$Accuracy, Precision_results$Precision, F1_score_results$F1_score)
colnames(overview_results)<- c("Modello", "Accuracy","Precision","F1_score")
View(overview_results)

# si può notare che il Random Forest è il modello che rispetto agli altri ha performance leggermente migliori
# il seguente modello potrebbe essere utilizzato poi per fare previsioni


# overview ROC
library(ROCR)

preds_list <- list(tree.prob[,2], logistic.prob, rf.prob[,2])

m <- length(preds_list)
actuals_list <- rep(list(test$CHURN), m)

pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")

par(lwd= 4, lty= 6)
plot(rocs, col = as.list(1:m), main = "ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Logistic", "Random Forest"),
       fill = 1:m)
