# -------------------- Computacion Cientifica Actuarial --------------------
# ==================== Facultad de Ciencias Economicas ====================

# ------------ Trabajo Practico Nro 2 ------------

# ===== paquetes/instalaciones/sistema =====

# Activo ambiente virtual de R
#renv::activate('/Users/mripani/Documents/Personal/Facultad/Computacion Cientifica/computacion-cientifica-tp2/renv')

# Instalacion de paquetes
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("hrbrthemes")
#install.packages("ROCR")
#install.packages('caret')
#install.packages("e1071")

# Librerias
library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(caret)
library(ROCR)

# ===== Data =====

#### Datos crudos ####
raw_data = read_delim("/Users/mac/Facultad/Computacion Cientifica /TP2/computacion-cientifica-tp2/data.txt", delim=";")
raw_data

#### Dataset ####
ds = raw_data
View(ds)

#### Valores nulos ####
summarise_all(ds, funs(sum(is.na(.))))

#### Encode variable categorica ####
ds$famhist = ifelse(ds$famhist == "Present", 1,0)
names(ds) = c('OBS', 'BLOOD_PRES', 'TOBAC', 'CHOLESTEROL', 'ADIPOSITY', 'FAMHIST', 'TYPE_A', 'OBESITY', 'ALCOHOL', 'AGE', 'TARGET')


# ===== Estadistica descriptiva =====

#### Matriz correlacion ####
m_corr = cor(ds)
ggcorr(m_corr, method = c("everything", "pearson"), label=TRUE) +
  ggsave('graficos/correlacion/correlation_plot_1.png', width = 4, height = 4, units = 'cm')



#### Variable target ####
ggplot(data = ds, aes(x = TARGET), title='Variable target') + 
  geom_bar(aes(x=TARGET))  + # Target desbalanceados.
  theme_classic() + 
  ggtitle("Variable TARGET") +
  ggsave(file=paste("graficos/barras/plot_TARGET.png"))

  #### Densidades ####
plot_vars = c("BLOOD_PRES", "TOBAC", "CHOLESTEROL", "ADIPOSITY", "OBESITY", "ALCOHOL")
for(var in plot_vars){
  p = ggplot(data=ds, aes_string(x=var, group="TARGET", fill="TARGET")) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum() +
    ggtitle(paste('Densidad de ', var)) 
  ##ggsave(p, file=paste("graficos/densidades/plot_", var,".png"), width = 14, height = 10, units = "cm")
}

#### Scatter ####
ggplot(ds, aes(AGE, TOBAC, color=TARGET, size=ALCOHOL, alpha=0.8)) + 
  geom_point() +
  theme_ipsum() + 
  ggtitle("Relación AGE|TOBAC", subtitle = "Tamaño ALCOHOL") 
  ##ggsave("graficos/scatter/plot_ALC|TAB.png")

#### Violin Plot ####
ggplot(ds, aes(x=as.factor(TARGET), y=TOBAC, fill=as.factor(TARGET))) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  theme_ipsum() +
  theme(legend.position="right",plot.title = element_text(size=11)) +
  ggtitle(paste("TOBAC separado por variable TARGET")) +
  xlab("TARGET") +
  ylab("TOBAC") 
  ##ggsave(file=paste("graficos/violin/plot_TOBAC.png"))


# =====  Regresion Logistica  =====


#### Variables regresoras ####
regresoras = ds[2:11]
View(regresoras)

#### Regresion con una variable ####
logit0 = glm(TARGET ~ AGE, data=regresoras, family="binomial")
summary(logit0)

#### Regresion con todas las variables ####
logit1 = glm(TARGET ~ ., data=regresoras, family="binomial")
summary(logit1)



# ===== Clasificacion =====

#### Train/Test Split ####
set.seed(22)
sample <- sample.int(n = nrow(regresoras), size = floor(.75*nrow(regresoras)), replace = F)
train <- regresoras[sample, ] # 75%
test  <- regresoras[-sample, ] # 25%
dim(train)
dim(test)

#### Estandarizamos ####
preProcValues <- preProcess(train[1:9], method = c("center", "scale"))
t_train <- predict(preProcValues, train[1:9])
t_test <- predict(preProcValues, test[1:9])
t_train['TARGET'] = train['TARGET']
t_test['TARGET'] = test['TARGET']
dim(t_train)
dim(t_test)

# Chequeo estandarizacion
mean(t_train$ADIPOSITY)
var(t_train$ALCOHOL)

#### Clasificador ####
logit = glm(TARGET ~ ., data=t_train, family="binomial")
logit

#### Predicciones s/ train ####
y_pred = predict(logit, t_train, type="response")
y_pred = ifelse(y_pred > 0.5, 1, 0) # Valor de corte = 0.5

# Matriz de confusion
cm = confusionMatrix(as.factor(y_pred), as.factor(t_train$TARGET), positive = "1")
cm

# ===== Evaluacion sobre datos de testeo =====

#### Predicciones ####
y_pred = predict(logit, t_test, type="response")
y_pred
y_pred = ifelse(y_pred > 0.5, 1, 0) # Valor de corte 0.5

#### Evaluacion ####
eval.model = prediction(y_pred,t_test$TARGET)
class(eval.model)
slotNames(eval.model)

#### Curva ROC ####
eval.auc = performance(eval.model, "auc")
eval.auc@y.values[[1]]

# Grafico
eval.roc = performance(eval.model, measure = "tpr", x.measure = "fpr")
plot(eval.roc,lwd= 3,col = 'blue', main= "ROC Curve")
plot(eval.roc,lty=3,col="grey78", add = TRUE)

#### Matriz de confusion ####
cm = confusionMatrix(as.factor(y_pred), as.factor(t_test$TARGET), positive = "1")

#### Precision, recall, F1 ####
precision <- cm$byClass['Pos Pred Value'] # Especifidad
recall <- cm$byClass['Sensitivity'] # Sensibilidad
F1 <- (2 * precision * recall) / (precision + recall)
F1_model0 = F1
recall_model0 = recall
precision_model0 = precision
F1_model0
recall_model0
precision_model0

# ===== Analizamos valor de corte =====

#### Grafico valor de corte ####
sens <- data.frame(x=unlist(performance(predictions, "sens")@x.values), y=unlist(performance(predictions, "sens")@y.values))
spec <- data.frame(x=unlist(performance(predictions, "spec")@x.values), y=unlist(performance(predictions, "spec")@y.values))

sens %>% ggplot(aes(x,y)) + 
  geom_line(col='blue') + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") +
  theme_minimal() + 
  theme(axis.title.y.right = element_text(colour = "red"), 
        axis.title.y.left = element_text(colour = 'blue'),
        legend.position="none")  
  ##ggsave("graficos/evaluacion/valor_de_corte.png")


##### Distintos valores de corte #####
f1_scores = c()
threshold = c()
recalls = c()
precisions = c()
for(t in seq(0,1, 0.05)){
  # Predicciones con nuevo valor de corte 
  y_pred = predict(logit, t_train, type="response")
  y_pred = ifelse(y_pred > t, 1, 0) # Valor de corte 0.5
  y_pred
  
  # Evaluacion
  cm = confusionMatrix(as.factor(y_pred), as.factor(t_train$TARGET), positive = "1")
  cm
  
  # Precision, recall, F1
  precision <- cm$byClass['Pos Pred Value'] # Especifidad?
  recall <- cm$byClass['Sensitivity'] # Sensibilidad
  F1 <- (2 * precision * recall) / (precision + recall)
  
  f1_scores = c(f1_scores, F1)
  threshold = c(threshold, t)
  precisions = c(precisions, precision)
  recalls = c(recalls, recall)
}

results = data.frame(matrix(c(threshold, recalls, precisions, f1_scores), ncol = 4))
names(results) = c('thresh', 'recall','precision','F1')
results
ggplot(data = results) +
  geom_bar(aes(x=thresh, y=F1), col='blue', stat='identity') +
  geom_bar(aes(x=thresh, y=recall), col='orange', stat='identity', alpha=0.3) +
  geom_bar(aes(x=thresh, y=precision), col='yellow', stat='identity', alpha=0.3) +
  theme_minimal() + 
  ggtitle("Evaluacion del modelo para distintos valores de corte") +
  labs(x = "Valor de corte",y = "F1",color = "Legend") +
  theme(legend.position="right",plot.title = element_text(size=11)) + 
  scale_color_manual(values = c("F1" = 'blue', "recall" = 'orange', 'precision' = 'yellow')) 
  ##ggsave("graficos/evaluacion/metricas_thresh.png")

# Threshold con max(F1)
results %>% slice_max(F1)

##### Nuevo valor de corte: 0.3 #####
y_pred = predict(logit, t_test, type="response")
y_pred = ifelse(y_pred > 0.7, 1, 0) # Valor de corte 0.3

# Agregamos predicciones a dataset 
t_test$PRED_TARGET = y_pred

##### Matriz de confusion #####
cm = confusionMatrix(as.factor(y_pred), as.factor(t_test$TARGET))
tabla = data.frame(cm$table)

# Grafico matriz de confusion
plotTabla <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "bien", "mal")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('valor real') + 
  ylab('prediccion') + 
  xlim(rev(levels(table$Reference))) 
  ##ggsave("graficos/evaluacion/matriz_de_confusion_1.png")

##### Precision, recall, F1 ####
precision <- cm$byClass['Pos Pred Value'] # Especifidad
recall <- cm$byClass['Sensitivity'] # Sensibilidad
F1 <- (2 * precision * recall) / (precision + recall)
F1_model1 = F1
precision_model1 = precision
recall_model1 = recall

##### Comparacion modelo anterior ####
recall_model1 > recall_model0
precision_model1 > precision_model0
F1_model1 > F1_model0

# Evaluacion
eval.model = prediction(y_pred,t_test$TARGET)
class(eval.model)
slotNames(eval.model)

# Curva ROC
eval.auc = performance(eval.model, "auc")
eval.auc@y.values[[1]]

eval.roc = performance(eval.model, measure = "tpr", x.measure = "fpr")
plot(eval.roc,lwd= 3,col = 'blue', main= "ROC Curve")
plot(eval.roc,lty=3,col="grey78", add = TRUE)