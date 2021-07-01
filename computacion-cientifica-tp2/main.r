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


install.packages("languageserver")
install.packages("tidyverse")

# Librerias
library(tidyverse)

# Data
raw_data = read_delim("computacion-cientifica-tp2/data.txt", delim=";")
raw_data

# Pasamos al objeto sobre el que vamos a trabajar
ds = raw_data

# Encodeamos la variable categorica
ds$famhist = ifelse(ds$famhist == "Present", 1,0)
names(ds) = c('INDEX', 'BLOOD_PRES', 'TOBAC', 'CHOLESTEROL', 'ADIPOSITY', 'FAMHIST', 'TYPE_A', 'OBESITY', 'ALCOHOL', 'AGE', 'TARGET')
ds


### Estadistica descriptiva

## Graficos

# Barplot
ggplot(data = ds, aes(x = TARGET), title='Variable target') + geom_bar()  

# Scatter
ggplot(ds, aes(AGE, TOBAC)) + geom_point()

# Boxplot
ggplot(ds, aes(AGE)) + geom_boxplot()

# 
ggplot(ds, aes(AGE)) + geom_histogram(bins=15)



