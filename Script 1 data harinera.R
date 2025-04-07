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
  pivot_longer(cols = p6:p11, names_to = "Pregunta", values_to = "Electrodomestico") %>%
  filter(Electrodomestico != "")

conteo <- df_long %>%
  group_by(Electrodomestico) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)


library(ggplot2)

ggplot(conteo, aes(x = reorder(Electrodomestico, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Para que las etiquetas sean más legibles
  labs(title = "Frecuencia de Electrodomésticos en los Hogares",
       x = "Electrodoméstico",
       y = "Frecuencia") +
  theme_minimal()

# De la siguiente lista de productos ¿cuáles utiliza?


df_long <- data %>%
  pivot_longer(cols = p17:p21, names_to = "Pregunta", values_to = "Productos") %>%
  filter(Productos != "")

conteo <- df_long %>%
  group_by(Productos) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

## De la siguiente lista de marcas, ¿cuáles marcas de harina de trigo reconoce?


df_long <- data %>%
  pivot_longer(cols = p28:p36, names_to = "Pregunta", values_to = "Marcas") %>%
  filter(Marcas != "")

conteo <- df_long %>%
  group_by(Marcas) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

ggplot(conteo, aes(x = reorder(Marcas, Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Para que las etiquetas sean más legibles
  labs(title = "Reconocimiento de marcas de harina en los Hogares",
       x = "Marca",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_gradient2(low = "red", high = "black")

### ¿Has consumido alguna de estas marcas?

df_long <- data %>%
  pivot_longer(cols = p37:p45, names_to = "Pregunta", values_to = "Marcas") %>%
  filter(Marcas != "")

tabla_frecuencias <- df_long %>%
  group_by(Pregunta, Marcas) %>%
  summarise(Conteo = n(), .groups = 'drop')  # Contar las ocurrencias

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)



