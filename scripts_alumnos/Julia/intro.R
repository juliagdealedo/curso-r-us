#############################################
# Curso US 10-11-2025
# Introducción a R 
# Script para correr los códigos de la presentación simultáneamente
#############################################


# Operaciones con números
2 + 3
10 - 4
3 * 5
20 / 4
4 %% 2

# Operaciones lógicas
20 / 4 >= 10
20 / 4 == 10
20 / 4 < 10

valores <- c(TRUE, FALSE, TRUE, TRUE)
sum(valores)
mean(valores)

# Acumular información
x <- 10
y <- 5
suma <- x + y
suma
suma + 7

# Objetos y nombres
yo_uso_la_serpiente <- 10
OtraGenteUsaElCamello <- 9
otra.gente.usa.los.puntos <- 9
Hayque_evitar.esto10 <- 2

# Tipos de objetos
numero <- 1:10
caracter <- "del uno al diez"
una_lista <- list(0:10, 10:20, 20:30)

# Transformar tipos
numeros_como_caracteres <- as.character(numero)
numeros <- as.numeric(numeros_como_caracteres)

# Vectores
numeros <- c(1, 2, 3, 4, 6)
otros_numeros <- 1:40
otros_numeros + 10

# Generar secuencias
# EJERCICIO
# escribe aquí tu código



# Operaciones con vectores
promedio <- mean(numeros)
promedio
promedio * numeros
numeros / otros_numeros
(numeros / otros_numeros)[7]

# Funciones básicas
x <- c(4, 6, 8, 3, 9, 12, 12)
mean(x)
median(x)
sd(x)
min(x)
max(x)
range(x)
summary(x)

# Ayuda en R
?mean
help(mean)
??"dplyr"

# Más funciones
x <- rpois(8, 8)
sample(x, 3)
rev(x)
sort(x)
length(x)

table(x)
unique(x)
length(unique(x))

log(x)
sqrt(x)
log10(x)
x ^ 4
abs(-0.23)
round(0.1245, 2)

# Distribuciones
runif(5)
x <- runif(1000, min = 0, max = 100)
hist(x, col = "orange")

rnorm(4)
x <- rnorm(1000, mean = 50, sd = 10)
hist(x, col = "skyblue")

rpois(5, lambda = 3)
x <- rpois(1000, lambda = 3)
hist(x, col = "lightgreen")

caras <- rbinom(200, size = 1, prob = 0.5)
table(caras)
barplot(table(caras), col = "steelblue")

# Strings y caracteres
un_nombre <- "Laura"
nombres <- c("Laura", "Rocio", "Violeta", "Rocio", "Rocio")
unique(nombres)
table(nombres)
rep(nombres, 5)
sample(nombres, 3, replace = TRUE)

# Comparaciones con strings
nombres == "Laura"
nombres != "Laura"
nombres == c("Laura", "Rocio") # cuidado!
nombres %in% c("Laura", "Rocio")

# Ejercicios
# Escribe aquí tu código

# Función propia
log_vect <- function(y) {
  mean <- mean(y)
  mean_log <- mean(log(y + 1))
  sd <- sd(y)
  sd_log <- sd(log(y + 1))
  return(c(mean, mean_log, sd, sd_log))
}
vector3 <- sample(40)
log_vect(vector3)

# Bucles
for (i in 1:5) {
  print(paste("Aquí está el número", i))
}

valores <- c(10, 20, 30, 40)
for (v in valores) {
  print(mean(log(v + 1)))
}

# Bucle con strings
nombres <- c("Laura", "Rocio", "Violeta")
for (i in nombres) {
  print(paste("Aquí está la alumna", i))
}

# Bucle con objeto vacío
tiro <- c()
for (i in 1:100) {
  tiro[[i]] <- sample(1:6, 1)
}
unlist(tiro)

# Bucle medias normales
filas <- c()
for (i in 1:100) {
  filas[[i]] <- mean(rnorm(50))
}
unlist(filas)

# Bucle con data frame
tabla <- data.frame(numero = numeric(),
                    cuadrado = numeric())

for (i in 1:5) {
  nueva_fila <- data.frame(numero = i, cuadrado = i^2)
  tabla <- rbind(tabla, nueva_fila)
}
tabla

# Condicionales
x <- 10
if (x > 5) {
  print("El número es mayor que 5")
}

x <- 3
if (x > 5) {
  print("El número es mayor que 5")
} else {
  print("El número es menor o igual que 5")
}

edades <- c(15, 22, 35, 12)
ifelse(edades >= 18, "Adulto", "Menor")

# Ejercicio ifelse
# escribe aquí tu código

# Base de datos
nombres <- c("Laura", "Rocio", "Violeta")
edad <- c(23, 35, 29)
altura <- c(1.65, 1.80, 1.70)
profesion <- c("Bióloga", "Informática", "Médica")

personas <- data.frame(NAME = nombres,
                       AGE = edad,
                       HEIGHT = altura,
                       JOB = profesion)
personas

# Seleccionar datos
personas[1,]
personas[,1]
personas$NAME

# Leer bases de datos
library(here)
datos_csv <- read.csv(here("data/personas.csv"))
head(datos_csv)

library(readxl)
datos_excel <- read_excel(here("data/herbarium_df.xlsx"))

library(googlesheets4)
# Hay que configurar con el gmail, lo dejamos para más adelante
# datos_gs <- read_sheet("https://docs.google.com/spreadsheets/d/1qC4lsXPpEcX1NmLdwiMJMC8X_WKX-EFQm4ahPdrNCZc/edit?usp=sharing")

# Guardar bases de datos
library(readr)
write_csv(datos_gs, "data/datos_gs.csv")
fwrite(datos_gs, "data/datos_gs.csv")

# Ejercicio con base de datos
# escribe aquí tu código

## Fin de la clase!