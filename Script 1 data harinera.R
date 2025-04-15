#### Harinera del Valle

library("dplyr")
library("ggplot2")
library("readxl")
library("Rcpp")
library("tidyverse")
library("ggplot2")


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


data <- data %>%
  rename(
    `Haz de Oros` = p102,
    `Paspan` = p103,
    `Robinson` = p104,
    `La nieve` = p105,
    `3 Castillos` = p106,
    `Pardo` = p107,
    `San Miguel` = p108,
    `Gran Molini` = p109,
    `Otro` = p110
  )

df_long <- data %>%
  pivot_longer(cols = `Haz de Oros`:`Otro`, names_to = "Marca", values_to = "Satisfaccion") %>%
  filter(!is.na(Satisfaccion) & Satisfaccion != "")  # Filtrar valores NA y vacíos

# Contar las frecuencias de cada nivel de satisfacción por marca
conteo <- df_long %>%
  group_by(Marca, Satisfaccion) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  arrange(Marca, desc(Satisfaccion))

# Calcular el total de respuestas por marca
totales <- conteo %>%
  group_by(Marca) %>%
  summarise(Total = sum(Frecuencia), .groups = 'drop')

# Unir los totales con el conteo
conteo <- conteo %>%
  left_join(totales, by = "Marca") %>%
  mutate(Porcentaje = (Frecuencia / Total) * 100)  # Calcular el porcentaje

# Filtrar para las marcas seleccionadas
marcas_seleccionadas <- c("Haz de Oros", "Robinson", "La nieve")

df_filtrado <- conteo %>%
  filter(Marca %in% marcas_seleccionadas)

# Definir el orden de los niveles de satisfacción
df_filtrado <- df_filtrado %>%
  mutate(Satisfaccion = factor(Satisfaccion, 
                               levels = c("Muy satisfecho", "Satisfecho", "Neutral", "Insatisfecho", "Muy insatisfecho"),
                               ordered = TRUE))

# Crear el gráfico de barras para los porcentajes
ggplot(df_filtrado, aes(x = Satisfaccion, y = reorder(Satisfaccion,-Porcentaje) , fill = Satisfaccion)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Porcentaje de Satisfacción con Marcas Seleccionadas",
       x = "Grado de Satisfacción",
       y = "Porcentaje (%)") +
  geom_text(aes(label = round(Porcentaje, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2) +
  theme_minimal() +
  facet_wrap(~ Marca, ncol = 2, nrow = 2) +  # Crear un gráfico separado para cada marca
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(strip.text = element_text(size = 12))+
  coord_flip()
## ¿Cuáles son los principales usos que le da a la harina de trigo? (Marque todas las que apliquen)
data <- data.frame(
  Preparacion = c("Fritas", "Pancakes", "Apanados", "Tortas", "Arepas", "Cremas y salsas", "Otros"),
  A_Diario = c(1.77, 14.00, 0.92, 0.00, 16.19, 0.00, 0.00),
  Dos_a_Tres_Veces = c(46.02, 54.67, 22.02, 4.29, 36.19, 13.64, 12.50),
  Una_Vez_a_la_Semana = c(38.05, 27.33, 52.29, 47.14, 34.29, 79.55, 37.50),
  Rara_Vez = c(14.16, 4.00, 24.77, 48.57, 13.33, 6.82, 50.00)
)

# Transformar el dataframe a formato largo
data_long <- data %>%
  pivot_longer(cols = A_Diario:Rara_Vez, names_to = "Frecuencia", values_to = "Porcentaje")

data_long$Frecuencia <- str_replace_all(data_long$Frecuencia, "_", " ")

# Verificar el dataframe transformado
print(data_long)


ggplot(data_long, aes(x = Frecuencia, y = Porcentaje, fill = Frecuencia)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Frecuencia de Preparaciones",
       x = "Tiempo de Consumo",
       y = "Porcentaje (%)") +
  geom_text(aes(label = round(Porcentaje, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.5) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  facet_wrap(~ Preparacion, ncol = 2) +  # Crear un gráfico separado para cada tipo de preparación
  coord_flip()


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

data <- data.frame(
  Atributos = c("Calidad", "Rendimiento", "Precio", "Facilidad de uso"),
  Porcentaje = c(77.51, 63.16, 52.63, 36.36)
)

# Ordenar los datos de mayor a menor
data <- data %>%
  arrange((Porcentaje)) %>%
  mutate(Atributos = factor(Atributos, levels = Atributos))  # Reordenar los niveles del factor

# Verificar el dataframe
print(data)

# Crear el gráfico de barras horizontales con etiquetas de porcentaje
ggplot(data, aes(x = Atributos, y = Porcentaje, fill = Atributos)) +
  geom_bar(stat = "identity",width = 0.7) +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_nudge(y = 5),  # Mover las etiquetas hacia arriba
            color = "black") +  # Color de las etiquetas
  labs(title = "Porcentaje de Participantes por Atributo",
       y = "% de Participantes",
       x = "Atributos") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  coord_flip()# Alinear etiquetas del eje x
# Hacer las barras horizontales






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


###
data <- data.frame(
  Atributos = c("Sabor", "Rendimiento", "Versatilidad en recetas", "Precio", 
                "Calidad de la harina", "Disponibilidad en tiendas", 
                "Tradición", "Empaque", "Publicidad", "Marca Tradicional"),
  Muy_importante = c(70.05, 62.80, 50.96, 50.48, 71.15, 40.10, 37.32, 31.10, 25.48, 38.46),
  Importante = c(28.50, 34.78, 42.31, 44.23, 27.40, 54.59, 54.07, 52.63, 40.38, 48.08),
  Algo_importante = c(0.48, 1.45, 5.77, 4.33, 0.96, 4.35, 6.70, 12.92, 26.44, 10.10),
  Poco_importante = c(0.97, 0.97, 0.48, 0.48, 0.48, 0.97, 1.91, 2.87, 7.69, 3.37),
  Nada_importante = c(0.00, 0.00, 0.48, 0.48, 0.00, 0.00, 0.00, 0.48, 0.00, 0.00)
)

# Transformar los datos a formato largo
data_long <- data %>%
  pivot_longer(cols = -Atributos, names_to = "Importancia", values_to = "Porcentaje")

# Verificar el dataframe transformado
print(data_long)


data_filtered <- data %>%
  slice(1:4)  # Seleccionar las primeras cuatro filas

# Transformar los datos a formato largo
data_long <- data_filtered %>%
  pivot_longer(cols = -Atributos, names_to = "Importancia", values_to = "Porcentaje")

data_long$Importancia <- factor(data_long$Importancia, 
                                levels = c("Nada_importante", "Poco_importante", 
                                           "Algo_importante", "Importante", 
                                           "Muy_importante"))
# Verificar el dataframe transformado
print(data_long)

### 1 al 4
# Crear el gráfico de paneles 2x2 para las primeras cuatro categorías
g1 <- ggplot(data_long, aes(x = Importancia, y = Porcentaje, fill = Importancia)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Barras separadas
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_dodge(width = 0.7),  # Ajustar posición de las etiquetas
            hjust = -0.15,  # Mover etiquetas hacia arriba
            color = "black") +  # Color de las etiquetas
  labs(title = "Importancia de Atributos",
       x = "Categoría de Importancia",
       y = "Porcentaje (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Alinear etiquetas del eje x
  facet_wrap(~ Atributos, nrow = 2) +  # Crear paneles 2x2
  coord_flip() +  # Hacer las barras horizontales
  scale_y_continuous(limits = c(0, 100),  # Establecer límites del eje y
                     breaks = seq(0, 100, by = 20),  # Establecer los puntos de ruptura
                     labels = scales::percent_format(scale = 1))  # Formato de porcentaje


## 5 al 8

data_filtered <- data %>%
  slice(5:8)  # Seleccionar las primeras cuatro filas

# Transformar los datos a formato largo
data_long <- data_filtered %>%
  pivot_longer(cols = -Atributos, names_to = "Importancia", values_to = "Porcentaje")

data_long$Importancia <- factor(data_long$Importancia, 
                                levels = c("Nada_importante", "Poco_importante", 
                                           "Algo_importante", "Importante", 
                                           "Muy_importante"))
levels(data_long$Importancia) <- gsub("_", " ", levels(data_long$Importancia))


# Crear el gráfico de paneles 2x2 para las primeras cuatro categorías
g2 <- ggplot(data_long, aes(x = Importancia, y = Porcentaje, fill = Importancia)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Barras separadas
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_dodge(width = 0.7),  # Ajustar posición de las etiquetas
            hjust = -0.15,  # Mover etiquetas hacia arriba
            color = "black") +  # Color de las etiquetas
  labs(title = "Importancia de Atributos",
       x = "Categoría de Importancia",
       y = "Porcentaje (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Alinear etiquetas del eje x
  facet_wrap(~ Atributos, nrow = 2) +  # Crear paneles 2x2
  coord_flip() +  # Hacer las barras horizontales
  scale_y_continuous(limits = c(0, 100),  # Establecer límites del eje y
                     breaks = seq(0, 100, by = 20),  # Establecer los puntos de ruptura
                     labels = scales::percent_format(scale = 1)) 


print(g1)
print(g2)

#### Después de ver la imagen del empaque, 
##¿cómo evalúa los siguientes aspectos? (Escala de 1 a 5, donde 1 es muy malo y 5 es excelente)

data <- data.frame(
  Pregunta = c("Se ve de buena calidad la harina", 
               "Me gusta el diseño del empaque", 
               "Claridad de información", 
               "El empaque protege"),
  Excelente = c(59.33, 57.42, 57.42, 60.77),
  Muy_bueno = c(34.45, 32.06, 33.01, 31.10),
  Bueno = c(5.74, 8.13, 9.09, 7.18),
  Regular = c(0.48, 1.44, 0.48, 0.96),
  Malo = c(0.00, 0.96, 0.00, 0.00)
)

# Transformar los datos a formato largo
data_long <- data %>%
  pivot_longer(cols = -Pregunta, names_to = "Calificacion", values_to = "Porcentaje")




# Reemplazar guiones bajos por espacios en los nombres de las calificaciones
data_long$Calificacion <- gsub("_", " ", data_long$Calificacion)

data_long$Calificacion <- factor(data_long$Calificacion, 
                                 levels = c("Malo", "Regular", "Bueno", "Muy bueno", "Excelente"),
                                 ordered = TRUE)

# Verificar el dataframe transformado
print(data_long)

# Crear el gráfico de barras
ggplot(data_long, aes(x = Calificacion, y = Porcentaje, fill = Calificacion)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Barras separadas
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_dodge(width = 0.7),  # Ajustar posición de las etiquetas
            hjust = -0.5,  # Mover etiquetas hacia arriba
            color = "black") +  # Color de las etiquetas
  labs(title = "Evaluación de la Calidad del Producto",
       x = "Pregunta",
       y = "Porcentaje") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Pregunta, nrow = 2) + # Alinear etiquetas del eje x
  coord_flip()+
  scale_y_continuous(limits = c(0, 100),  # Establecer límites del eje y
                     breaks = seq(0, 100, by = 20),
                     labels = scales::percent_format(scale = 1)) # Establecer los puntos de ruptura
  


## De acuerdo con lo que conoce de la marca y lo que acaba de ver en la 
## información del empaque ¿Cómo califica la marca Haz de Oros en los siguientes aspectos? (Use una escala de 1 a 5, donde 1 es "Muy malo" y 5 es "Excelente")


data <- data.frame(
  Pregunta = c("Calidad del producto", 
               "Confianza en la marca", 
               "Tradición y reconocimiento", 
               "Relación calidad-precio", 
               "Presentación y empaque"),
  Malo = c(0.48, 0.48, 0.48, 0.48, 0.48),
  Regular = c(0.00, 0.00, 0.00, 0.48, 0.97),
  Bueno = c(1.92, 4.83, 9.66, 8.70, 5.31),
  Muy_bueno = c(47.12, 43.00, 44.93, 48.79, 37.68),
  Excelente = c(50.48, 51.69, 44.93, 41.55, 55.56)
)

# Transformar los datos a formato largo
data_long <- data %>%
  pivot_longer(cols = -Pregunta, names_to = "Calificacion", values_to = "Porcentaje")

# Reemplazar guiones bajos por espacios en los nombres de las calificaciones
data_long$Calificacion <- gsub("_", " ", data_long$Calificacion)

# Convertir la columna "Calificacion" en un factor con el orden deseado
data_long$Calificacion <- factor(data_long$Calificacion, 
                                 levels = c("Malo", "Regular", "Bueno", "Muy bueno", "Excelente"),
                                 ordered = TRUE)  # Asegurarse de que sea un factor ordenado

# Verificar el dataframe transformado
print(data_long)

# Crear el gráfico de barras
ggplot(data_long, aes(x = Calificacion, y = Porcentaje, fill = Calificacion)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Barras separadas
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_dodge(width = 0.7),  # Ajustar posición de las etiquetas
            hjust = -0.5,  # Mover etiquetas hacia arriba
            color = "black") +  # Color de las etiquetas
  labs(title = "Evaluación - harina Haz de oros",
       x = "Pregunta",
       y = "Porcentaje") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Pregunta, nrow = 2) + # Alinear etiquetas del eje x
  coord_flip()+
  scale_y_continuous(limits = c(0, 100),  # Establecer límites del eje y
                     breaks = seq(0, 100, by = 20),
                     labels = scales::percent_format(scale = 1)) # Establecer los puntos de ruptura
