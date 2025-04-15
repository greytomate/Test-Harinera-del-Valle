datos <- data.frame(
  Color = rep(c("Color Crema/Amarillo claro", "Color rosado"), each = 5),
  Atractivo = rep(c("Muy atractivo", "Atractivo", "Neutro", "Poco atractivo", "Nada atractivo"), 2),
  Porcentaje = c(15.56, 22.45, 8.67, 1.28, 1.02,
                 21.43, 22.45, 5.61, 1.28, 0.26)
)

# Para mantener el orden original
datos$Atractivo <- factor(datos$Atractivo,
                          levels = c("Muy atractivo", "Atractivo", "Neutro", "Poco atractivo", "Nada atractivo"))

# Colores personalizados por nivel de atractivo
colores <- c("Muy atractivo" = "#1a9641",
             "Atractivo" = "#a6d96a",
             "Neutro" = "#ffffbf",
             "Poco atractivo" = "#fdae61",
             "Nada atractivo" = "#d7191c")

# Gráfico
ggplot(datos, aes(x = Porcentaje, y = Atractivo, fill = Atractivo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            hjust = -0.1, size = 4.5) +
  facet_wrap(~ Color) +
  scale_fill_manual(values = colores) +
  labs(x = "Porcentaje (%)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", color = "white"),
    strip.background = element_rect(fill = "#2171b5", color = NA),
    panel.spacing = unit(2, "lines")
  ) +
  xlim(0, max(datos$Porcentaje) + 10)

###########
library(dplyr)
library(tidyr)
library(ggplot2)

# Crear el dataframe con los datos proporcionados

library(dplyr)
library(tidyr)
library(ggplot2)

data$Muy_satisfecho <- as.numeric(data$Muy_satisfecho)
data$Satisfecho <- as.numeric(data$Satisfecho)
data$Neutral <- as.numeric(data$Neutral)
data$Poco_satisfecho <- as.numeric(data$Poco_satisfecho)
data$Nada_satisfecho <- as.numeric(data$Nada_satisfecho)

# Crear el dataframe a partir de la tabla proporcionada
data <- data.frame(
  Marca = c("Haz de oros", "Paspan", "Robinson", "La nieve", "3 castillos", "Pardo", "San Miguel", "Gran molino", "Otro"),
  Muy_satisfecho = c(42.95, 0.00, 48.35, 10.00, 0.00, 50.00, 0.00, 0.00, 50.00),
  Satisfecho = c(55.70, 66.67, 46.70, 73.33, 0.00, 50.00, 100.00, 66.67, 33.33),
  Neutral = c(0.67, 33.33, 4.95, 15.00, 0.00, 0.00, 0.00, 33.33, 16.67),
  Poco_satisfecho = c(0.67, 0.00, 0.00, 1.67, 0.00, 0.00, 0.00, 0.00, 0.00),
  Nada_satisfecho = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
)

df_long <- data %>%
  pivot_longer(cols = Muy_satisfecho:Nada_satisfecho, 
               names_to = "Satisfaccion", 
               values_to = "Porcentaje", 
               values_ptypes = list(Porcentaje = double())) %>%
  filter(!is.na(Porcentaje) & Porcentaje != 0)
# Verificar la estructura del dataframe
str(data)

data_filtered <- data %>%
  filter(Marca %in% c("Haz de oros", "Robinson", "La nieve"))

# Convertir a formato largo
df_long <- data_filtered %>%
  pivot_longer(cols = -Marca, names_to = "Satisfaccion", values_to = "Porcentaje")


# Definir el orden de los niveles de satisfacción
df_long <- df_long %>%
  mutate(Satisfaccion = factor(Satisfaccion, 
                               levels = rev(c("Muy_satisfecho", "Satisfecho", "Neutral", "Poco_satisfecho", "Nada_satisfecho")),
                               labels = rev(c("Muy satisfecho", "Satisfecho", "Ni satisfecho ni insatisfecho", "Poco satisfecho", "Nada satisfecho")),
                               ordered = TRUE))

# Crear el gráfico de barras para los porcentajes
ggplot(df_long, aes(x = Satisfaccion, y = Porcentaje, fill = Satisfaccion)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Porcentaje de Satisfacción por Marca",
       x = "Grado de Satisfacción",
       y = "Porcentaje (%)") +
  geom_text(aes(label = round(Porcentaje, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.5) +
  facet_wrap(~ Marca, ncol = 2)+
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +  # Cambiar la paleta de colores
  theme(strip.text = element_text(size = 12)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90),  # Establecer límites del eje y
                     breaks = seq(0, 100, by = 20),  # Establecer los puntos de ruptura
                     labels = scales::percent_format(scale = 1))
####################

avg_porcentaje <- df_long %>%
  group_by(Satisfaccion) %>%
  summarise(Promedio = mean(Porcentaje, na.rm = TRUE)) %>%
  arrange(desc(Promedio))

# Convertir Satisfaccion a factor ordenado
df_long <- df_long %>%
  mutate(Satisfaccion = factor(Satisfaccion, levels = avg_porcentaje$Satisfaccion))

# Crear gráfico de barras
ggplot(df_long, aes(x = Satisfaccion, y = Porcentaje, fill = Marca)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Marca, ncol = 2) +  # Crear cuadrícula de 2 columnas
  theme_minimal() +
  labs(title = "Satisfacción por Marca (Ordenado de Mayor a Menor)", 
       x = "Nivel de Satisfacción", 
       y = "Porcentaje") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Formato de porcentaje
