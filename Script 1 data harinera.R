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

##¿Con qué frecuencia compras los productos seleccionados?

data <- data.frame(
  Producto = c("Harina de trigo", "Tortillas", "Pan", "Azúcar morena"),
  Diariamente = c(1.91, 0.95, 69.66, 19.33),
  Semanalmente = c(39.23, 37.14, 25.28, 26.89),
  Quincenalmente = c(39.23, 41.90, 5.06, 31.93),
  Mensualmente = c(17.70, 16.19, 0.00, 20.17),
  Menos_de_una_vez_al_mes = c(1.91, 3.81, 0.00, 1.68)
)

# Transformar a formato largo
datos_largos <- data %>%
  pivot_longer(cols = -Producto, names_to = "Frecuencia", values_to = "Porcentaje") %>%
  filter(!is.na(Porcentaje)) %>%  # Filtrar NA si es necesario
  mutate(Frecuencia = recode(Frecuencia, 
                             Menos_de_una_vez_al_mes = "Menos de una vez al mes"))
# Asegurar orden lógico en las frecuencias
niveles_frecuencia <- c("Menos de una vez al mes", "Diariamente", "Semanalmente", "Quincenalmente", "Mensualmente")
datos_largos$Frecuencia <- factor(datos_largos$Frecuencia, levels = niveles_frecuencia)

# Graficar
ggplot(datos_largos, aes(x = Porcentaje, y = Frecuencia, fill = Frecuencia)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), hjust = -0.1, size = 4) +
  facet_wrap(~ Producto) +
  scale_fill_manual(values = c(
    "Menos_de_una_vez_al_mes" = "#1C6FAE",  # Color para "Menos de una vez al mes"
    "Diariamente" = "#3281B5",
    "Semanalmente" = "#6EB6D9",
    "Quincenalmente" = "#86CAE1",
    "Mensualmente" = "#B4D4DA"
  )) +
  labs(title = "Frecuencia de consumo por tipo de producto", 
       x = "Porcentaje (%)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "#2171b5", color = NA)) +
  xlim(0, max(datos_largos$Porcentaje, na.rm = TRUE) + 10)

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


# Transformar el dataframe a formato largo y asignar marcas
atributos <- c("Calidad del producto", "Relación calidad-precio", 
               "Disponibilidad en tiendas", "Presentación y empaque", 
               "Versatilidad para recetas")

# Transformar el dataframe a formato largo y asignar marcas
data_relevante <- data %>%
  select(p47:p91)  # Selecciona las columnas desde p47 hasta p91

# Verificar los tipos de datos de las columnas seleccionadas
str(data_relevante)

# Convertir todas las columnas a tipo character (si es necesario)
data_relevante <- data_relevante %>%
  mutate(across(everything(), as.character))

# Transformar el dataframe a formato largo y asignar marcas
df_long <- data_relevante %>%
  pivot_longer(cols = everything(), names_to = "Pregunta", values_to = "Atributos") %>%
  filter(Atributos != "") %>%
  mutate(
    Marca = case_when(
      Pregunta %in% c("p47", "p48", "p49", "p50", "p51") ~ "Haz de Oros",
      Pregunta %in% c("p52", "p53", "p54", "p55", "p56") ~ "Paspan",
      Pregunta %in% c("p57", "p58", "p59", "p60", "p61") ~ "Robinson",
      Pregunta %in% c("p62", "p63", "p64", "p65", "p66") ~ "La nieve",
      Pregunta %in% c("p67", "p68", "p69", "p70", "p71") ~ "Tres Castillos",
      Pregunta %in% c("p72", "p73", "p74", "p75", "p76") ~ "Pardo",
      Pregunta %in% c("p77", "p78", "p79", "p80", "p81") ~ "San Miguel",
      Pregunta %in% c("p82", "p83", "p84", "p85", "p86") ~ "Gran Molino",
      Pregunta %in% c("p87", "p88", "p89", "p90", "p91") ~ "Otro",
      TRUE ~ NA_character_
    ),
    Atributo = rep(atributos, length.out = n()) # Ajuste para longitud correcta
  )

# Imprimir el dataframe transformado
View(df_long)

df_long <- df_long %>%
  mutate(Atributos = as.numeric(Atributos))

# Calcular los promedios de cada marca para cada atributo
promedios <- df_long %>%
  group_by(Marca, Atributo) %>%
  summarise(Promedio = mean(Atributos, na.rm = TRUE), .groups = 'drop')  # na.rm = TRUE para ignorar NA

# Imprimir el dataframe de promedios
print(promedios)

#####

marcas_a_graficar <- c("Haz de Oros","Robinson","La nieve")  # Especifica las marcas que deseas
promedios_filtrados <- promedios %>%
  filter(Marca %in% marcas_a_graficar)

# Crear un gráfico de barras horizontales para las marcas seleccionadas
ggplot(promedios_filtrados, aes(x = Promedio, y = reorder(Atributo, Promedio), fill = Atributo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Promedio, 2)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2) +  # Ajustar la posición del texto
  labs(title = "Evaluación de los atributos",
       x = "Promedio",
       y = "Atributo") +
  scale_x_continuous(limits = c(0,5))+
  theme_minimal() +
  facet_wrap(~ Marca, ncol = 2, nrow = 2) +  # Crear un gráfico separado para cada marca
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(strip.text = element_text(size = 11))  # Ajustar el tamaño del texto de los paneles

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

# Ahora, vamos a crear tablas separadas para cada columna
# Usamos lapply para iterar sobre cada pregunta
tablas_separadas <- df_long %>%
  group_by(Pregunta, Atributos) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  split(.$Pregunta)

# Ahora, cada elemento de 'tablas_separadas' es un data frame para cada pregunta
# Puedes acceder a cada tabla usando el nombre de la pregunta
# Por ejemplo, para acceder a la tabla de la pregunta p156:
tabla_p156 <- tablas_separadas[["p156"]]
tabla_p157 <- tablas_separadas[["p157"]]
# Y así sucesivamente para cada pregunta

# Si deseas guardar cada tabla en el entorno global con un nombre específico
for (nombre in names(tablas_separadas)) {
  assign(paste0("tabla_", nombre), tablas_separadas[[nombre]], envir = .GlobalEnv)
  }

nombres_tablas <- c("tabla_p156", "tabla_p157", "tabla_p158", "tabla_p159", 
                    "tabla_p160", "tabla_p161", "tabla_p162", "tabla_p163", 
                    "tabla_p164", "tabla_p165")

for (nombre in nombres_tablas) {
  cat("\nTabla:", nombre, "\n")
  print(get(nombre))  # Usamos get() para obtener el objeto por su nombre
}

## Después de ver la imagen del empaque,
## ¿cómo evalúa los siguientes aspectos? (Escala de 1 a 5, donde 1 es muy malo y 5 es excelente)


df_long <- data %>%
  pivot_longer(cols = p169:p172, names_to = "Pregunta", values_to = "Aspectos") %>%
  filter(!is.na(Aspectos) & Aspectos != "")


conteo <- df_long %>%
  group_by(Aspectos) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(conteo)

