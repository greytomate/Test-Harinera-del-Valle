#### Harinera del Valle

library("dplyr")
library("ggplot2")
library("readxl")
library("Rcpp")
library("tidyverse")
library(ggplot2)


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

tabla_final <- tabla_frecuencias %>%
  pivot_wider(names_from = Marcas, values_from = Conteo, values_fill = 0)

# Mostrar la tabla final
print(tabla_final)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)

## Evalúa los atributos de las diferentes marcas, 
## donde 1 es "Muy malo" y 5 es "Excelente"

df_long <- data %>%
  pivot_longer(cols = p47:p51, names_to = "Pregunta", values_to = "Atributos") %>%
  filter(Atributos != "")

print(df_long)

evaluar_marcas <- function(data) {
  # Transformar el dataframe a formato largo
  df_long <- data %>%
    pivot_longer(cols = everything(), names_to = "Atributo", values_to = "Puntuacion") %>%
    filter(!is.na(Puntuacion) & Puntuacion != "")  # Filtrar NA y vacíos
  
  # Extraer la marca y el atributo de los nombres de las columnas
  df_long <- df_long %>%
    mutate(
      Marca = case_when(
        grepl("^p47|^p48|^p49|^p50|^p51", Atributo) ~ "Haz de Oros",
        grepl("^p52|^p53|^p54|^p55|^p56", Atributo) ~ "Paspan",
        grepl("^p57|^p58|^p59|^p60|^p61", Atributo) ~ "Robinson",
        grepl("^p62|^p63|^p64|^p65|^p66", Atributo) ~ "La nieve",
        grepl("^p67|^p68|^p69|^p70|^p71", Atributo) ~ "Tres Castillos",
        grepl("^p72|^p73|^p74|^p75|^p76", Atributo) ~ "Pardo",
        grepl("^p77|^p78|^p79|^p80|^p81", Atributo) ~ "San Miguel",
        grepl("^p82|^p83|^p84|^p85|^p86", Atributo) ~ "Gran Molino",
        grepl("^p87|^p88|^p89|^p90|^p91", Atributo) ~ "Otro"
      )
    )
  
  # Calcular el promedio de cada atributo por marca
  promedios <- df_long %>%
    group_by(Marca, Atributo) %>%
    summarise(Promedio = mean(Puntuacion, na.rm = TRUE)) %>%
    arrange(Marca, Atributo)
  
  return(promedios)

}

column_ranges <- list(
  47:51, 
  52:56, 
  57:61, 
  62:66, 
  67:71, 
  72:76, 
  77:81,
  82:86,
  87:91
)

evaluar_marcas(data[,c(47:56)])

resultados_marcas <- list()

for (i in seq_along(column_ranges)) {
  resultados_marcas[[i]] <- print(evaluar_marcas(data[,column_ranges[[i]]]))
}
Marcas <- c()

for (i in seq_along(resultados_marcas)) {
  # Extraer la tabla de la marca
  tabla_marca <- resultados_marcas[[i]]
  
  # Crear el nombre de la marca
  nombre_marca <- paste("Marca", i)
  
  # Graficar
  a<- ggplot(tabla_marca, aes(x = Atributo, y = Promedio)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Promedios de Atributos para", nombre_marca),
         x = "Atributo",
         y = "Promedio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
    #ggsave(filename = paste0("promedios_", nombre_marca, ".png")) # Guardar el gráfico como imagen
  print(a)
}


# ¿Cuáles de los siguientes atributos asocia con cada marca de 
# harina de trigo que conoce?(Seleccione las opciones que crea aplican a cada marca que conoce)


df_long <- data %>%
  pivot_longer(cols = p93:p101, names_to = "Pregunta", values_to = "atributos") %>%
  filter(atributos != "")

conteo <- df_long %>%
  group_by(atributos) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

## De las marcas de Harina que conoce y ha probado, en una escala de 1 a 5, 
## donde 1 es "Nada satisfecho" y 5 es "Muy satisfecho", 
## ¿Qué tan satisfecho está con las siguientes marcas de harina de trigo que ha usado?

df_long <- data %>%
  pivot_longer(cols = p102:p110, names_to = "Pregunta", values_to = "atributos") %>%
  filter(!is.na(atributos) & atributos != "")

conteo <- df_long %>%
  group_by(Pregunta, atributos) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  arrange(Pregunta, desc(atributos))

print(conteo)

resultado_tabulado <- conteo %>%
  pivot_wider(names_from = atributos, values_from = Frecuencia, values_fill = list(Frecuencia = 0))

print(resultado_tabulado)


## ¿Cuáles son los principales usos que le da a la harina de trigo? (Marque todas las que apliquen)


df_long <- data %>%
  pivot_longer(cols = p111:p117, names_to = "Pregunta", values_to = "Usos") %>%
  filter(Usos != "")

conteo <- df_long %>%
  group_by(Usos) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

## ¿Con qué frecuencia sueles realizar tus preparaciones?


df_long <- data %>%
  pivot_longer(cols = p118:p124, names_to = "Pregunta", values_to = "frecuencia") %>%
  filter(!is.na(frecuencia) & frecuencia != "")

conteo <- df_long %>%
  group_by(frecuencia) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

## ¿Para qué tipo de recetas la usa? (Marque todas las que apliquen)

df_long <- data %>%
  pivot_longer(cols = p136:p142, names_to = "Pregunta", values_to = "Preparaciones") %>%
  filter(!is.na(Preparaciones) & Preparaciones != "")

conteo <- df_long %>%
  group_by(Preparaciones) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

## ¿Qué atributos son más importantes al elegir una marca de harina de trigo? (Seleccione los 3 más importantes)

df_long <- data %>%
  pivot_longer(cols = p144:p154, names_to = "Pregunta", values_to = "Atributos") %>%
  filter(!is.na(Atributos) & Atributos != "")


conteo <- df_long %>%
  group_by(Atributos) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)


## ¿Qué tanto influyen los siguientes atributos en su decisión de compra? 
## (Escala de 1 a 5, donde 1 es para nada importante y 5 es muy importante)


df_long <- data %>%
  pivot_longer(cols = p156:p165, names_to = "Pregunta", values_to = "Atributos") %>%
  filter(!is.na(Atributos) & Atributos != "")


conteo <- df_long %>%
  group_by(Atributos) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)
