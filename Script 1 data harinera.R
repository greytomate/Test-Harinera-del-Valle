#### Harinera del Valle

library("dplyr")
library("ggplot2")
library("readxl")
library("Rcpp")
library("tidyverse")

#### Creación de script para limpieza de datos

data <- read_xlsx("C:/Users/Usuario/Desktop/mindlabs/Harinera del Valle/data_harinera.xlsx")








# ¿Cuáles de los siguientes electrodomésticos tiene en su hogar? (Marque todas las que apliquen)

df_long <- data %>%
  pivot_longer(cols = c(p6:p11), names_to = "Pregunta", values_to = "Electrodomestico") %>%
  drop_na()  # Eliminar valores vacíos

# Contar frecuencia de cada electrodoméstico
conteo <- df_long %>%
  group_by(Electrodomestico) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))