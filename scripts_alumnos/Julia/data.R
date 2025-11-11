#############################################
# MANEJO DE DATOS EN R CON DPLYR
# Curso de Introducción a R
#############################################

# Librerías necesarias
library(dplyr)
library(palmerpenguins)
library(scico)
library(here)
library(readxl)
library(stringr)
library(tm)
library(wordcloud2)
library(DT)


# 1. EXPLORACIÓN DE DATOS

# Cargar base de datos de ejemplo
data("penguins", package = "palmerpenguins")

# Estructura de la base de datos
glimpse(penguins)


# 2. RENAME - Renombrar columnas

penguins %>% 
  rename(
    long_pico = bill_length_mm, 
    anchura_pico = bill_depth_mm, 
    long_aleta = flipper_length_mm, 
    masa = body_mass_g,
    sexo = sex,
    año = year,
    especies = species,
    isla = island
  )


# 3. SELECT - Seleccionar columnas

# Seleccionar y reordenar columnas
penguins %>% 
  select(year, sex, species, long_pico = bill_length_mm, anchura_pico = bill_depth_mm)

# Excluir columnas
penguins %>% select(-c(sex, year))


# 4. DISTINCT - Valores únicos

penguins %>% distinct(island)
penguins %>% distinct(species, island)


# 5. FILTER - Filtrar datos

penguins_filtered <- penguins %>% 
  filter(year == 2007, sex == "female")

# Filtrar por varias condiciones
penguins_filtered <- penguins %>% 
  filter(year > 2008, sex == "female", island %in% c("Dream", "Biscoe"))

# Filtrar excluyendo valores
penguins %>% filter(!is.na(body_mass_g))
penguins %>% filter(year != 2007)


# 6. MUTATE - Crear nuevas variables

# Crear masa en kilogramos
penguins %>% 
  mutate(body_mass_kg = body_mass_g / 1000)

# Crear una relación entre longitud y anchura del pico
penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  select(bill_length_mm, bill_depth_mm, bill_ratio)


# 7. SUMMARISE & GROUP_BY

# Calcular media de masa corporal
penguins %>%
  summarise(promedio_peso = mean(body_mass_g, na.rm = TRUE))

# Agrupar por especie
penguins %>% 
  group_by(species) %>%
  summarise(
    promedio_peso_kg = mean(body_mass_g, na.rm = TRUE) / 1000,
    conteo_años = n_distinct(year)
  )


# 8. IF_ELSE - Condicionales

# Cambiar valores específicos
penguins %>% 
  mutate(year_2 = if_else(year == 2008, 2010, year))

# Rellenar NA con una categoría
penguins %>% 
  mutate(family = if_else(is.na(sex), "chick", sex)) %>% 
  select(sex, family) %>% distinct()

# Crear variable binaria
penguins %>% 
  mutate(sex_binary = if_else(sex == "male", 0, 1))


# 9. JOIN - Combinar bases de datos

taxonomy <- data.frame(
  species = unique(penguins$species),
  species_name = c("Pygoscelis adeliae", "Pygoscelis papua", "Pygoscelis antarcticus")
)

penguins_join <- penguins %>% 
  left_join(taxonomy, by = "species")

glimpse(penguins_join)


# 10. WORDCLOUD - Ejemplo con datos del curso

datos_curso <- read_excel(here("data/datos-curso.xlsx"))

datos_curso_filtrado <- datos_curso %>%
  rename(
    curso = "Curso de la primera matrícula en el programa de doctorado",
    experiencia = "Cuéntanos tu experiencia con R",
    comentarios = "Observaciones",
    email = "Correo electrónico"
  ) %>%
  select(curso, experiencia, comentarios, email) %>%
  mutate(email = str_replace(email, "anonymous", "anónimo"))

# Limpiar texto y crear nube de palabras
words <- tolower(unlist(strsplit(datos_curso_filtrado$experiencia, " ")))
words <- gsub("[[:punct:][:digit:]]", "", words)
stop_es <- stopwords("spanish")
words_clean <- words[!words %in% stop_es]

wordcloud2(
  data = table(words_clean),
  size = 0.5,
  color = rep(RColorBrewer::brewer.pal(11, "PRGn"), 20)
)


# 11. EJERCICIO FLORES

datos_curso <- read.csv(here("data/flores.csv"))
datos_curso %>% 
  summarise (mean(abundance)) %>% 
  group_by(species) 






# 12. EJERCICIO STARWARS

