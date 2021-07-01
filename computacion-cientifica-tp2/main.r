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
data = read_delim("computacion-cientifica-tp2/data.txt", delim=";")
data

