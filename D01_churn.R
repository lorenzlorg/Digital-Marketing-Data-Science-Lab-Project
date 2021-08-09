# Churn Model: propensity model supervised 

#### INSIGHTS ####
#
#
#
#

# Evento di churn: il momento in cui il cliente smette di acquistare
# Dopo 60 (o 30) giorni di inattività si possono categorizzare i clienti come churner, 
# secondo diverse ricerche pubblicate

# Riprendendo la "the days for next purchase curve" presente nel dataset 7, 
# si nota per circa il 90% dei clienti passano circa 70 giorni per il successivo acquisto, 
# per l'80% passano 40 giorni

# Questo significa che dato che la maggior parte delle persone acquista dopo 70 giorni, 
# se un cliente non dovesse acquistare questo deve essere un campanello d'allarme, 
# il cliente potrebbe essere un possibile churn

# [ 1 maggio 2018 ------------lockback period------------ 28 febbraio 2019) [28 febbraio 2019 ------------holdout period ------------ 30 aprile 2019]


#### 1. choosing a reference date in the past (posizionarsi ad una data di riferimento nel passato) ####
# eventualmente in presenza di clienti molto diversi tra loto si potrebbe optare per soglie differenti
# reference date: 28 febbraio 2019, 60 giorni prima dell'ultima rilevazione

reference_date <- as.Date("28/02/2019", format = "%d/%m/%Y")



#### 2. imposing the length of an holdout period after each reference date. The length correspondes to the frequencey of the distribution and/or the purchase time scale ####
# considerando una via di mezzo tra il 90% e l'80% dei clienti coinvolti, 
# si seleziona come numero di giorni per l'acquisto successivo 60 (vedi days for next purchase curve df7) 
# 60 giorni
# holdout period: 28 febbraio 2019 - 30 aprile 2019

holdout_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE >= reference_date)

# tutti i clienti presenti nell'holdout sono no churner, dal momento in cui 
# hanno effettuato un acquisto dopo la reference date
holdout_period['CHURN'] <- 0



#### 3. choosing the lenght of a lookback period before the reference date ####
# non si definisce solitmante una lunghezza fissa. Bisogna controllare cosa conviene, 
# se dunque può essere vantaggioso o meno considerare tutti i dati a nostra 
# disposizione, a partire dal 1 maggio 2018, o meno
# lookback period: 01 maggio 2018 - 28 febbraio 2019

lookback_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE < reference_date)

# si assume, inzialmente, per comodità di calcolo che tutti i clienti presenti 
# nel lookback period siano churner
lookback_period['CHURN'] <- 1



#### 4. assigning to each customer a target 0/1 variable such that 1 is assigned to customers who churned in the holout period ####

holdout_period_temp <- holdout_period %>%
  select(ID_CLI, CHURN)

# si aggiorna la variabile CHURN per i clienti presenti nel lookback period
# tutti i clienti che comparivano nell'holdout saranno ovviamente no churner
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
            CHURN = names(which.max(table(CHURN)))
  ) 

churn_dataset$CHURN <- as.factor(churn_dataset$CHURN)

churn_dataset$RECENCY<-difftime(as.Date("30/04/2019", format="%d/%m/%Y"), 
                                churn_dataset$LAST_PURCHASE_DATE, units = "days")


# si considerano i clienti che hanno effettuato più di 1 acquisto
churn_dataset <- churn_dataset %>%
  filter(NUM_OF_PURCHASES > 1)

# non si considerano i clienti che hanno effettuato il primo acquisto dopo il 
# 28/02/2019 dal momento in cu si rischia di avere poche informazioni su di essi
churn_dataset <- churn_dataset %>%
  filter(FIRST_PURCHASE_DATE < reference_date)

# complessivamente
str(churn_dataset)
table(churn_dataset$CHURN)
prop.table(table(churn_dataset$CHURN))

# si controlla nuovamente la presenza di eventuali na
sapply(churn_dataset, function(x) sum(is.na(x)))


# si considerano altre features potenzialmente rilevanti, oltre a FIRST_PURCHASE_DATE, 
# LAST_PURCHASE_DATE, TOT_PURCHASE, TOT_SCONTO, TOT_SPESA, NUM_OF_PURCHASE, RECENCY:

# df1: LAST_COD_FID, FIRST_ID_NEG
# df2: TYP_CLI_ACCOUNT
# df3: REGION


churn_dataset <- left_join(churn_dataset, df_1_cli_fid_clean[,c("ID_CLI", "LAST_COD_FID", 
                                                                "NUM_FIDs", "FIRST_ID_NEG")], 
                           by = "ID_CLI")  # LAST_COD_FID, FIRST_ID_NEG, NUM_FIDs
churn_dataset$FIRST_ID_NEG <- as.factor(churn_dataset$FIRST_ID_NEG)

churn_dataset <- left_join(churn_dataset, df_2_cli_account_clean[,c("ID_CLI", 
                                                                    "TYP_CLI_ACCOUNT")], 
                           by = "ID_CLI")  # TYP_CLI_ACCOUNT


df_2_df_3 <- left_join(df_2_cli_account_clean, df_3_cli_address_clean, by = "ID_ADDRESS")
churn_dataset <- left_join(churn_dataset, df_2_df_3[,c("ID_CLI", "REGION")], by = "ID_CLI")  # REGION

# dal momento in cui vi sarebbero troppe variabili dummy si procede nel raggruppare 
# le regioni in macroregioni (basandosi sulla seguente suddivisione: https://www.tuttitalia.it/statistiche/nord-centro-mezzogiorno-italia/)
NORD <- c("VALLE D'AOSTA", "PIEMONTE", "LOMBARDIA", "LIGURIA", "FRIULI VENEZIA GIULIA", 
          "VENETO", "TRENTINO ALTO ADIGE", "EMILIA ROMAGNA")
CENTRO <- c("TOSCANA", "MARCHE", "LAZIO", "UMBRIA")
MEZZOGIORNO <- c("ABRUZZO", "MOLISE", "CAMPANIA", "BASILICATA", "PUGLIA", "CALABRIA", 
                 "SICILIA", "SARDEGNA")

nord_regioni <- churn_dataset %>%
  filter(REGION %in% NORD) %>%
  mutate(REGION = "NORD")

centro_regioni <- churn_dataset %>%
  filter(REGION %in% CENTRO) %>%
  mutate(REGION = "CENTRO")

mezzogiorno_regioni <- churn_dataset %>%
  filter(REGION %in% MEZZOGIORNO) %>%
  mutate(REGION = "MEZZOGIORNO")


# non si considerano le regioni con valore null (altrimenti si potrebbero verificare 
# errori nell'esecuzione di alcuni modelli)
sum(is.na(churn_dataset$REGION))  # 5922 regioni non riportate
churn_dataset <- na.omit(churn_dataset)
sum(is.na(churn_dataset$REGION))  # check

churn_dataset <- rbind(nord_regioni, centro_regioni, mezzogiorno_regioni)
churn_dataset$REGION <- as.factor(churn_dataset$REGION )

# churn_dataset <- left_join(churn_dataset, df_2_cli_account_clean[,c("ID_CLI", "TYP_JOB")], by = "ID_CLI")  # TYP_JOB non si considera la seguente variabile dal momento in cui presenta troppi valori mancanti

str(churn_dataset)
summary(churn_dataset)
table(churn_dataset$CHURN)  # nel dataset vi sono 71753 churner e 56585 no churner

# vengono eliminate le variabili che non interessano per le analisi
churn_dataset$ID_CLI <- NULL
churn_dataset$FIRST_PURCHASE_DATE <- NULL
churn_dataset$LAST_PURCHASE_DATE <- NULL

# si potrebbero considerare come predittori le seguenti variabili:
# TOT_PURCHASE + TOT_SCONTO + TOT_SPESA + NUM_OF_PURCHASES + RECENCY + REGION + LAST_COD_FID + FIRST_ID_NEG + TYP_CLI_ACCOUNT

var_num <- c("TOT_PURCHASE", "TOT_SCONTO", "TOT_SPESA", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])
# si nota, come prevedibile, TOT_PURCHASE e TOT_SPESA sono correlate in maniera molto elevata
# si procede dunque considerando solamente TOT_PURCHASE 

var_num <- c("TOT_PURCHASE", "TOT_SCONTO", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])
# si decide di considerare anche solo una tra TOT_PURCHASE E TOT_SCONTO

var_num <- c("TOT_PURCHASE", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])

# dunque i predittori considerati risultano essere:
# TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG 

# per controllare complessivamente la correlazione tra le variabili
numeric.var <- sapply(churn_dataset, is.numeric)
corr.matrix <- cor(churn_dataset[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")



#### 6. Apply models #### 

train_index <- createDataPartition(churn_dataset$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- churn_dataset[train_index,]
test <- churn_dataset[-train_index,]

table(train$CHURN)
table(test$CHURN)

# in generale essendo il numero di churner più elevato (secondo questa partizione) 
# si potrebbe pensare di migliorare le attività di retention

# verifica se dataset sia sbilanciato o meno
prop.table(table(train$CHURN))
prop.table(table(test$CHURN))


#### Decision Trees #### 

# Fitting The Model

tree.model <- rpart(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + 
                      LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,  
                    data = train, method = "class")
 
rpart.plot(tree.model, extra = 1)  # la variabile più importante è RECENCY

fancyRpartPlot(tree.model)

summary(tree.model) 
printcp(tree.model) 

# il numero 0/1 in alto nel nodo dice come sono state classificate tutte le osservazioni di quel nodo
# le percentuali 100%, 36% ecc. rappresentano il numero delle osservazioni in percentuale prese in considerazione in quel nodo
# i numeri di mezzo indicano per lo 0 qual'è la percentuale e per l'1 qual è la percentuale

# ad esempio: nodo 4, riguarda l'11% delle osservazioni totali, in questo nodo (recency < 104 & num_of_purchases >= 8.5) tutte 
# le osservazioni sono classificate come no churner (in particolare si hanno il 73%
# delle osservazioni no churner (0) e il 27% delle oss. churner (1). Di conseguenza prelave lo 0, no churner,
# che ha anche senso, ovvero sono clienti che non aspettano troppo e fanno tanti acquisti.

# in rpart.plot sopra le percentuali del numero totale di osservazioni viene
# riportata solo la percentuale degli 1, ovvero dei churner


# Making Predictions
tree.pred <- predict(tree.model, test, type = "class")  # viene utilizzato il moodello per fare le previsioni sul test set
tree.prob <- predict(tree.model, test, type="prob")
qplot(x=tree.prob[, "1"], geom="histogram")

# Evaluating The Model
tree.result <- confusionMatrix(tree.pred, test$CHURN)

table(Predicted = tree.pred, Actual = test$CHURN)
#               Actual
# Predicted     0     1
#           0  5402  3399
#           1  6417 19083

tree.accuracy <- Accuracy(tree.pred, test$CHURN) # 0.7138276
tree.precision <- precision(tree.pred, test$CHURN,relevant = '1') # 0.7483529
tree.recall <- recall(tree.pred, test$CHURN,relevant = '1') # 0.8488124
tree.F1 <- F1_Score(tree.pred, test$CHURN,positive = '1') # 0.7954233


# ROC
tree.pr <- prediction(tree.prob[,2], test$CHURN)
tree.prf <- performance(tree.pr, measure = "tpr", x.measure = "fpr")
plot(tree.prf, main = "ROC DECISION TREE")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC value 
tree.auc <- performance(tree.pr, measure = "auc")
tree.auc <- tree.auc@y.values[[1]]
tree.auc  # 0.6824019



####  Random Forest #### 

# Fitting The Model
# memory.limit(100000)
rf.model <- randomForest(CHURN ~  TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + 
                           REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,
                         data = train , ntree = 100)
print(rf.model)

plot(rf.model)  # stima dell’errore di test chiamato Out Of Bag (OOB) error
# fornisce supporto nella selezione del numero di alberi
# oltre 40 e 50 alberi non è possibile diminuire ulteriormente l'error


varImpPlot(rf.model, sort=T, n.var = 4, main = 'Features Importance')
# come si può notare la variabile più importante risulta essere RECENCY


# Making Predictions
rf.pred <- predict(rf.model, test, type = "class")  
rf.prob <- predict(rf.model, test, type="prob")
qplot(x=rf.prob[, "1"], geom="histogram")

# Evaluating The Model
rf.result <- confusionMatrix(rf.pred, test$CHURN)

table(Predicted = rf.pred, Actual = test$CHURN)
#               Actual
# Predicted     0     1
#           0  4596  2389
#           1  7223 20093

rf.accuracy <- Accuracy(rf.pred,test$CHURN) # 0.7197749
rf.precision <- precision(rf.pred, test$CHURN,relevant = '1') # 0.7355762
rf.recall <- recall(rf.pred, test$CHURN,relevant = '1') # 0.8937372
rf.F1 <- F1_Score(rf.pred, test$CHURN,positive = '1') # 0.8069802


# ROC
rf.pr <- prediction(rf.prob[,2], test$CHURN)
rf.prf <- performance(rf.pr, measure = "tpr", x.measure = "fpr")
plot(rf.prf, main = "ROC RANDOM FOREST")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
rf.auc <- performance(rf.pr, measure = "auc")
rf.auc <- rf.auc@y.values[[1]]
rf.auc  # 0.7386719



####  Logistic Regression #### 
# I modelli lineari generalizzati (GLM) sono una generalizzazione del più classico 
# modello lineare nell'ambito della regressione lineare. Tra questi si può individuare
# la logistic regression (si utilizza il parametro family=binomial)


# Fitting The Model
logistic.model <- glm(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + 
                        REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,
                      data = train, family=binomial)
summary(logistic.model)

# Making Predictions
logistic.prob <- predict(logistic.model, test, type="response") 
qplot(x=logistic.prob, geom="histogram")
logistic.pred <- rep("0", length(logistic.prob))
logistic.pred[logistic.prob > 0.5] <- "1"
logistic.pred <- as.factor(logistic.pred)


# Evaluating The Model
logistic.result <- confusionMatrix(logistic.pred, test$CHURN)

table(Predicted = logistic.pred, Actual = test$CHURN)
#               Actual
# Predicted     0     1
#           0  4389  2230
#           1  7430  20252

logistic.accuracy <- Accuracy(logistic.pred,test$CHURN) # 0.7183756
logistic.precision <- precision(logistic.pred, test$CHURN,relevant = '1') # 0.6537639
logistic.recall <- recall(logistic.pred, test$CHURN,relevant = '1') # 0.9008095
logistic.F1 <- F1_Score(logistic.pred, test$CHURN,positive = '1') # 0.8074316


# ROC
logistic.pr <- prediction(logistic.prob, test$CHURN)
logistic.prf <- performance(logistic.pr, measure = "tpr", x.measure = "fpr")
plot(logistic.prf, main = "ROC LOGISTIC REGRESSION")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
logistic.auc <- performance(logistic.pr, measure = "auc")
logistic.auc <- logistic.auc@y.values[[1]]
logistic.auc  # 0.7461111



#### Neural Network Model ####
nm.model <- nnet(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + 
                   LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG, data = train, size = 3)
summary(nm.model)

# Making Predictions
nm.prob <- predict(nm.model, test) 
qplot(x=nm.prob, geom="histogram")
nm.pred <- ifelse(nm.prob > 0.5, 1, 0)
nm.pred <- as.factor(nm.pred)



# Evaluating The Model
nm.result <- confusionMatrix(nm.pred, test$CHURN)

table(Predicted = nm.pred, Actual = test$CHURN)
#               Actual
# Predicted     0     1
#           0  5054  2822
#           1  6765 19660

nm.accuracy <- Accuracy(nm.pred,test$CHURN) # 0.7205038
nm.precision <- precision(nm.pred, test$CHURN,relevant = '1') # 0.7439924
nm.recall <- recall(nm.pred, test$CHURN,relevant = '1') # 0.8744774
nm.F1 <- F1_Score(nm.pred, test$CHURN,positive = '1') # 0.8039749


# ROC
nm.pr <- prediction(nm.prob, test$CHURN)
nm.prf <- performance(nm.pr, measure = "tpr", x.measure = "fpr")
plot(nm.prf, main = "ROC NEURAL NETWORK MODEL")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
nm.auc <- performance(nm.pr, measure = "auc")
nm.auc <- nm.auc@y.values[[1]]
nm.auc  # 0.7462316



#### Naive Bayes #### 
nb.model <- naiveBayes(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + 
                         REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG, 
                       data = train)
summary(nb.model)

# Making Predictions
nb.pred <- predict(nb.model, test, type = "class")  
nb.prob <- predict(nb.model, test, type = "raw")
qplot(x=nb.prob[, "1"], geom="histogram")

# Evaluating The Model
nb.result <- confusionMatrix(nb.pred, test$CHURN)

table(Predicted = nb.pred, Actual = test$CHURN)
#               Actual
# Predicted     0     1
#           0  3153  1480
#           1  8666  21002

nb.accuracy <- Accuracy(nb.pred,test$CHURN) # 0.7042069
nb.precision <- precision(nb.pred, test$CHURN,relevant = '1') # 0.7079008
nb.recall <- recall(nb.pred, test$CHURN,relevant = '1') # 0.9341696
nb.F1 <- F1_Score(nb.pred, test$CHURN,positive = '1') # 0.8054458


# ROC
nb.pr <- prediction(nb.prob[, "1"], test$CHURN)
nb.prf <- performance(nb.pr, measure = "tpr", x.measure = "fpr")
plot(nb.prf, main = "ROC NAIVE BAYES")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
nb.auc <- performance(nb.pr, measure = "auc")
nb.auc <- nb.auc@y.values[[1]]
nb.auc  # 0.7339499



#### Confronto modelli ####
Modello <- c("Decision Tree", "Random Forest", "Logistic Regression", 
             "Neural Network Model", "Naive Bayes")


# ACCURACY: ratio of correctly predicted observation to the total observations
tree.accuracy
rf.accuracy
logistic.accuracy
nm.accuracy
nb.accuracy

Accuracy <- c(tree.accuracy,
           rf.accuracy,
           logistic.accuracy,
           nm.accuracy,
           nb.accuracy)
Accuracy_results <- data.frame(Modello, Accuracy)

# PRECISION: ratio of correctly predicted positive observations to the total predicted positive observations
tree.precision
rf.precision
logistic.precision
nb.precision

Precision <- c(tree.precision,
              rf.precision,
              logistic.precision,
              nm.precision,
              nb.precision)
Precision_results <- data.frame(Modello, Precision)

# RECALL:  ratio of correctly predicted positive observations to the all observations in actual class
tree.recall
rf.recall
logistic.recall
nb.recall

Recall <- c(tree.recall,
               rf.recall,
               logistic.recall,
                nm.recall,
                nb.recall)
Recall_results <- data.frame(Modello, Recall)

# F1: the weighted average of Precision and Recall
tree.F1
rf.F1
logistic.F1
nb.F1

F1_score <- c(tree.F1,
              rf.F1,
              logistic.F1,
              nm.F1,
              nb.F1)
F1_score_results <- data.frame(Modello, F1_score)


# AUC
tree.auc
rf.auc
logistic.auc
nb.auc

AUC <- c(tree.auc,
         rf.auc,
         logistic.auc,
         nm.auc,
         nb.auc)
AUC_results <- data.frame(Modello, AUC)

# overview risultati
overview_results <- data.frame(Modello,  Accuracy_results$Accuracy, 
                               Precision_results$Precision, Recall_results$Recall, 
                               F1_score_results$F1_score, AUC_results$AUC)
colnames(overview_results) <- c("Modello", "Accuracy","Precision", "Recall", 
                                "F1_score", "AUC")
# View(overview_results)

# Modello              Accuracy     Precision    Recall       F1_score     AUC
# Decision Tree        0.7138276    0.7483529    0.8488124    0.7954233    0.6824019
# Random Forest        0.7197749    0.7355762    0.8937372    0.8069802    0.7386719
# Logistic Regression  0.7183756    0.7315945    0.9008095    0.8074316    0.7461111
# Neural Network Model 0.7205038    0.7439924    0.8744774    0.8039749    0.7462316
# Naive Bayes          0.7042069    0.7079008    0.9341696    0.8054458    0.7339499

# overview ROC
preds_list <- list(tree.prob[,2], logistic.prob, rf.prob[,2], nm.prob, nb.prob[,2])

m <- length(preds_list)
actuals_list <- rep(list(test$CHURN), m)

pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")

par(lwd= 4, lty= 6)
plot(rocs, col = as.list(1:m), main = "ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Logistic", "Random Forest", "Neural Network Model", 
                  "Naive Bayes"),
       fill = 1:m)
abline(a=0, b= 1, col=c("grey"))

# in alternativa

# plot ROC for each method

roc_tree <- data.frame(fpr=unlist(tree.prf@x.values), tpr=unlist(tree.prf@y.values))
roc_tree$method <- "Decision Tree"

roc_rf <- data.frame(fpr=unlist(rf.prf@x.values), tpr=unlist(rf.prf@y.values))
roc_rf$method <- "Random Forest"

roc_logistic <- data.frame(fpr=unlist(logistic.prf@x.values), tpr=unlist(logistic.prf@y.values))
roc_logistic$method <- "Logistic Regression"

roc_nm <- data.frame(fpr=unlist(nm.prf@x.values), tpr=unlist(nm.prf@y.values))
roc_nm$method <- "Neural Network Model"

roc_nb <- data.frame(fpr=unlist(nb.prf@x.values), tpr=unlist(nb.prf@y.values))
roc_nb$method <- "Naive Bayes"

rbind(roc_tree, roc_rf, roc_logistic, roc_nm, roc_nb) %>%
  ggplot(data=., aes(x=fpr, y=tpr, linetype=method, color=method)) + 
  geom_line() +
  geom_abline(a=1, b=0, linetype=2) +
  theme(legend.position=c(0.8,0.2), legend.title=element_blank())

# si può notare che il Neural Network Model insieme al modello Logistic è il modello 
# che rispetto agli altri ha performance migliori, anche se di poco.
# i seguenti modelli potrebbero essere utilizzati poi per fare previsioni
