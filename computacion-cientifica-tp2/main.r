########################################################
##### Computacion Cientifica Actuarial
##### Facultad de Ciencias Economicas
########################################################

########################################################
##### Trabajo Practico Nro 2
########################################################

# Activo ambiente virtual de R
renv::activate('/Users/mripani/Documents/Personal/Facultad/Computacion Cientifica/computacion-cientifica-tp2/renv')

# Instalacion de paquetes



install.packages("tidyverse")
install.packages("GGally")
install.packages("hrbrthemes")
install.packages("ROCR")
install.packages('caret')
install.packages("e1071")

# Librerias
library(tidyverse)

library(ggplot2)
library(hrbrthemes)
library(GGally)
 

library(caret)
library(ROCR)

# Data
raw_data = read_delim("data.txt", delim=";")
raw_data



# Pasamos al objeto sobre el que vamos a trabajar
ds = raw_data
ds

# Encodeamos la variable categorica
ds$famhist = ifelse(ds$famhist == "Present", 1,0)
names(ds) = c('OBS', 'BLOOD_PRES', 'TOBAC', 'CHOLESTEROL', 'ADIPOSITY', 'FAMHIST', 'TYPE_A', 'OBESITY', 'ALCOHOL', 'AGE', 'TARGET')
ds


### Estadistica descriptiva

## Graficos

# Matriz correlacion
m_corr = cor(ds)
ggcorr(m_corr, method = c("everything", "pearson"), label=TRUE) 

# Solucionar
density_vars = c("BLOOD_PRES", "TOBAC", "CHOLESTEROL", "ADIPOSITY", "OBESITY", "ALCOHOL")

for(var in density_vars){
  p = ggplot(data=ds, aes(x=ds[var], group=TARGET, fill=TARGET)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum()
  ggsave(p, file=paste0("graficos/densidades/plot_", var,".png"), width = 14, height = 10, units = "cm")
}



p1 = ggplot(data=ds, aes(x=ADIPOSITY, group=TARGET, fill=TARGET)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
#p1

p2 <- ggplot(data=ds, aes(x=CHOLESTEROL, group=TARGET, fill=TARGET)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()



p3 <- ggplot(data=ds, aes(x=TOBAC, group=TARGET, fill=TARGET)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


p4 <- ggplot(data=ds, aes(x=ALCOHOL, group=TARGET, fill=TARGET)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


p5 <- ggplot(data=ds, aes(x=OBESITY, group=TARGET, fill=TARGET)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


p6 <- ggplot(data=ds, aes(x=BLOOD_PRES, group=TARGET, fill=TARGET)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


p1, p2, p3, p4, p5, p6, p7



# Barplot
ggplot(data = ds, aes(x = TARGET), title='Variable target') + 
  geom_bar()  # Target desbalanceados.


# Scatter
# Guardar con for loop
ggplot(ds, aes(ADIPOSITY, OBESITY, color=TARGET, size=CHOLESTEROL**4, alpha=0.8)) + 
  geom_point() +
  theme_ipsum()


# Boxplot
ggplot(ds, aes(AGE)) + 
  geom_boxplot()

p <- ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()

# hacer para varias variables.
ggplot(ds, aes(x=FAMHIST, y=ADIPOSITY, fill=as.factor(FAMHIST))) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Acá va el título") +
  xlab("")



# Chequeo valores nulos? (chequear)
ds[is.na(ds)]

# Variables regresoras 
regresoras = ds[2:11]
View(regresoras)

# Regresion con una variable
logit0 = glm(TARGET ~ TOBAC, data=regresoras, family="binomial")
summary(logit0)

# Regresion con todas las variables
logit1 = glm(TARGET ~ ., data=regresoras, family="binomial")
summary(logit1)


################################# 
######    Clasificacion     ####
#################################

## Train/Test Split
set.seed(22)
sample <- sample.int(n = nrow(regresoras), size = floor(.75*nrow(regresoras)), replace = F)
train <- regresoras[sample, ]
test  <- regresoras[-sample, ]
dim(train)
dim(test)

## Estandarizamos

# Instancio objeto
preProcValues <- preProcess(train[1:9], method = c("center", "scale"))

# Transformamos
t_train <- predict(preProcValues, train[1:9])
t_test <- predict(preProcValues, test[1:9])

t_train['TARGET'] = train['TARGET']
t_test['TARGET'] = test['TARGET']

dim(t_train)
dim(t_test)


## Clasificador 
logit = glm(TARGET ~ ., data=t_train, family="binomial")
logit

# Predicciones s/ train
y_pred = predict(logit, t_train, type="response")
y_pred = ifelse(y_pred > 0.5, 1, 0) # Valor de corte = 0.5
y_pred

# Matriz de confusion
cm = confusionMatrix(as.factor(y_pred), as.factor(t_train$TARGET), positive = "1")
cm


## Evaluacion sobre datos de testeo

# Predicciones 
y_pred = predict(logit, t_test, type="response")
y_pred
y_pred = ifelse(y_pred > 0.5, 1, 0) # Valor de corte 0.5

eval.model = prediction(y_pred,t_test$TARGET)
class(eval.model)
slotNames(eval.model)


# Precision and recall
eval.pr = performance(eval.model, "prec", "rec")
plot(eval.pr,avg= "threshold",colorize=TRUE,lwd= 3,main= "Curva Precision/Recall")
plot(eval.pr,lty=3,col="grey78",add=TRUE)


# Curva ROC
eval.auc = performance(eval.model, "auc")
eval.auc@y.values[[1]]

eval.roc = performance(eval.model, measure = "tpr", x.measure = "fpr")
plot(eval.roc,lwd= 3,col = 'blue', main= "ROC Curve")
plot(eval.roc,lty=3,col="grey78", add = TRUE)


# Matriz de confusion
cm = confusionMatrix(as.factor(y_pred), as.factor(t_test$TARGET), positive = "1")
cm

# Precision, recall, F1
precision <- cm$byClass['Pos Pred Value'] # Especifidad?
recall <- cm$byClass['Sensitivity'] # Sensibilidad
F1 <- (2 * precision * recall) / (precision + recall)
F1

# Valor de corte
sens <- data.frame(x=unlist(performance(predictions, "sens")@x.values), y=unlist(performance(predictions, "sens")@y.values))
spec <- data.frame(x=unlist(performance(predictions, "spec")@x.values), y=unlist(performance(predictions, "spec")@y.values))

sens %>% ggplot(aes(x,y)) + 
  geom_line(col='blue') + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") +
  theme(axis.title.y.right = element_text(colour = "red"), 
        axis.title.y.left = element_text(colour = 'blue'),
        legend.position="none") 

### Analizamos valor de corte
f1_scores = c()
threshold = c()
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
}

results = data.frame(matrix(c(threshold, f1_scores), ncol = 2))
names(results) = c('thresh', 'F1')
results

ggplot(data = results) +
  geom_line(aes(thresh, F1), col='blue')

# Threshold con max(F1)
results %>% slice_max(F1)

# Nuevo valor de corte: 0.3
y_pred = predict(logit, t_test, type="response")
y_pred = ifelse(y_pred > 0.3, 1, 0) # Valor de corte 0.3
y_pred

# Matriz de confusion
cm = confusionMatrix(as.factor(y_pred), as.factor(t_test$TARGET), positive = "1")
cm

# Precision, recall, F1
precision <- cm$byClass['Pos Pred Value'] # Especifidad?
recall <- cm$byClass['Sensitivity'] # Sensibilidad
F1 <- (2 * precision * recall) / (precision + recall)
F1



















