# Establecer la semilla
library(ggpubr)
library(tidyr)
set.seed(30) 

# Leer el archivo CSV
data <- read.csv("datos-felicidad.csv")  # Reemplaza por la ruta de tu archivo
data$Sexo <- ifelse(data$Sexo == "mujer", 0, 1) # Transformar sexo a variable numérica

# Obtener una muestra aleatoria de 70 elementos
muestra <- data[sample(nrow(data), 70), ]


# Crear la muestra de control con los elementos que no están en la muestra
muestra_control <- data[!(1:nrow(data) %in% rownames(muestra)), ]

# Variable dependiente: Felicidad
# Variables predictoras: Edad, n de amigos, n de mascotas
# Variable de control: Sexo

# Crear el dataframe con las variables de interés
df <- data.frame(
  Felicidad = muestra$Felicidad,
  Edad = muestra$Edad,
  Amigos = muestra$N_amigos,
  Mascotas = muestra$N_mascotas
)

# Convertir a formato largo
df_long <- pivot_longer(df, 
                        cols = everything(),
                        names_to = "variable", 
                        values_to = "valor")

# Crear el histograma
p <- gghistogram(
  df_long,
  x = "valor",
  fill = "variable",
  bins = 9
) +
  facet_grid(~ variable, scales = "free_x") +
  theme_pubr() +
  theme(legend.position = "none")

print(p)

print(cor(muestra))

df_amigos <- muestra[, c("N_amigos", "Felicidad")]
df_mascotas <- muestra[, c("N_mascotas", "Felicidad")]

# Veamos si hay una relación lineal entre estas variables.
g_felicidad_amigos <- ggscatter(df_amigos, x = "N_amigos", y = "Felicidad",
                                add = "reg.line",
                                conf.int = TRUE)

print(g_felicidad_amigos)

g_felicidad_mascotas <- ggscatter(df_mascotas, x = "N_mascotas", y = "Felicidad",
                                  add = "reg.line",
                                  conf.int = TRUE)
print(g_felicidad_mascotas)

modelo_amigos <- lm(Felicidad ~ N_amigos, muestra)
cat("\n\n")
cat("Modelo directo 'n de amigos' --> 'felicidad'\n")
cat("--------------------------------------------\n")
print(summary(modelo_amigos))

modelo_mascotas <- lm(Felicidad ~ N_mascotas, muestra)
cat("\n\n")
cat("Modelo directo 'n de mascotas' --> 'felicidad'\n")
cat("--------------------------------------------\n")
print(summary(modelo_mascotas))