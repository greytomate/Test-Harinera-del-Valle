#### Harinera del Valle

library("dplyr")
library("ggplot2")
library("readxl")
library("Rcpp")


#### Creación de script para limpieza de datos

data <- read_xlsx("C:/Users/Usuario/Desktop/mindlabs/Harinera del Valle/data_harinera.xlsx")
data <- data[-1,]
