############################################################
# VISUALIZACIÓN DE DATOS EN R
# Curso de introducción a R
############################################################

# 1. Cargar librerías
library(dplyr)
library(ggplot2)
library(readr)
library(palmerpenguins)
library(scico)
library(GGally)
library(patchwork)
library(rgbif)
library(rnaturalearth)
library(sf)
library(FloraIberica)

# 2. Datos
data(package = "palmerpenguins")

# 3. Primer gráfico base
plot(x = rnorm(100), y = rnorm(100))

# 4. R base vs ggplot
plot(penguins$bill_length_mm, penguins$bill_depth_mm)

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

# 5. Gráfico más complejo
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = body_mass_g)) +
  scico::scale_color_scico(palette = "bamako", direction = -1) +
  scale_x_continuous(breaks = 3:6 * 10, limits = c(30, 60)) +
  scale_y_continuous(breaks = seq(12.5, 22.5, by = 2.5),
                     limits = c(12.5, 22.5)) +
  labs(
    title = "Bill Dimensions of Brush-Tailed Penguins (Pygoscelis)",
    subtitle = "A scatter plot of bill depth versus bill length.",
    caption = "data = Gorman, Williams & Fraser (2014) PLoS ONE",
    x = "Bill Length (mm)", y = "Bill Depth (mm)",
    color = "Body mass (g)"
  ) + theme_minimal()

# con plot
cols <- scico(100, palette = "bamako", direction = -1)
color_values <- cols[
  as.numeric(cut(penguins$body_mass_g, breaks = 100))
]

plot(
  penguins$bill_length_mm, penguins$bill_depth_mm,
  col = color_values, pch = 21, bg = color_values,  
  xlim = c(30, 60),  ylim = c(12.5, 22.5),
  xaxt = "n",  yaxt = "n",
  xlab = "Bill Length (mm)",
  ylab = "Bill Depth (mm)",
  main = "Bill Dimensions of Brush-Tailed Penguins (Pygoscelis)",
  sub = "A scatter plot of bill depth versus bill length."
)

# 6. Capas básicas ggplot
ggplot(penguins)
ggplot(penguins, aes(x = flipper_length_mm))
penguins %>% ggplot(aes(x = flipper_length_mm)) + geom_histogram()

# 7. Gráficos de densidad
ggplot(penguins, aes(x = flipper_length_mm)) + geom_density(bw = 1)

# 8. Barras
penguins %>% ggplot(aes(x = species)) + geom_bar()

# Contar
penguins %>%
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_bar(stat = "identity")

# Media
penguins %>%
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_bar(stat = "summary", fun = "mean")

# Colores
penguins %>%
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  geom_bar(stat = "identity")

# 9. Personalización
penguins %>%
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) +
  labs(
    title = "Longitud del pico (mm)",
    subtitle = "Suma por especie",
    x = "Especie de pingüino",
    y = "Longitud del pico"
  ) + theme_minimal()

# 10. Añadir variable adicional
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#72874EFF", "#FED789FF")) +
  theme_minimal()

# 11. Boxplot y Violin
penguins %>% ggplot(aes(x = island, y = bill_length_mm)) + geom_boxplot()
penguins %>% ggplot(aes(x = island, y = bill_length_mm, fill = island)) + geom_boxplot()
penguins %>% ggplot(aes(x = island, y = bill_length_mm, color = island)) + geom_boxplot()

violin_plot <- penguins %>%
  ggplot(aes(x = island, y = bill_length_mm, fill = island)) +
  geom_jitter(size = 3, alpha = 0.1, fill = "grey") +
  geom_violin(alpha = 0.8) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#72874EFF", "#FED789FF", "#023743FF")) +
  theme_light()
violin_plot

 ggsave("output/violin_plot.png", violin_plot, width = 20, height = 20, units = "cm", scale = .5)

# 12. Scatterplot + correlaciones
penguins %>%
  ggpairs(columns = 3:5, ggplot2::aes(colour = species))

# Geom_point
ggplot(penguins, aes(x = flipper_length_mm, y = bill_length_mm)) + 
  geom_point()

# 13. Textos
ggplot(penguins, aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_text(aes(label = species, color = species))

# 14. Color y escala
penguins %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) +
  geom_point() + theme_light()

# Dónde va color
penguins %>% 
  ggplot () + 
  geom_point(aes(x = flipper_length_mm, y = bill_length_mm, color="blue")) 

# Dónde va color
penguins %>% 
  ggplot () + 
  geom_point(aes(x = flipper_length_mm, y = bill_length_mm), color="blue") 

# Por isla
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) + 
  geom_point() + theme_light() 

# Modificamos color 
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) + 
  geom_point() + 
  scale_colour_manual(values=c("#72874EFF", "#FED789FF", "#023743FF")) +
  theme_light() 

# Color continuo
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = bill_length_mm)) + 
  geom_point() + theme_light() 

# Gradientes
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = bill_length_mm)) + 
  geom_point() +
  scale_color_gradient(low="#00AFBB", high="#E7B800")+
  theme_light() 

# viridis
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = bill_length_mm)) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma") + 
  theme_light() 

# brewer
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = bill_length_mm)) + 
  geom_point() +
  scale_color_distiller(palette = "Spectral") +  
  theme_light() 

# Tamaño del punto y color

penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, size = body_mass_g, color=body_mass_g)) +
  scale_color_gradient(low="orange",high="black")+
  geom_point(alpha=0.8) + theme_light() 


# densidad 2d
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm))+
  geom_density_2d(color="black") + theme_minimal ()

# 15. Modelos

# Method = "loess"
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm)) + 
  geom_point() + geom_smooth()

# Lineal
penguins %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point() + geom_smooth(method = "lm") + theme_light()

# Interacción
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) +
  geom_point () +
  scale_colour_manual (values=c("lightblue", "purple", "pink")) +
  theme_light() + geom_smooth(method = "lm")


# 16. Facets
penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) + 
  scale_colour_manual (values=c("lightblue", "purple", "pink")) +
  geom_point() + theme_light() +  facet_wrap(~species, scales = "free")

penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) + 
  scale_colour_manual (values=c("lightblue", "purple", "pink")) +
  geom_point() + theme_light() +  facet_wrap(species~island, scales = "free")

penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, colour = island)) + 
  scale_colour_manual (values=c("lightblue", "purple", "pink")) +
  geom_point() + theme_light() +  facet_grid(species~island, scales = "free")


# 17. Patchwork
p1 <- penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, color = species, fill=species)) +
  geom_point(alpha=.5) + theme_light()  + geom_smooth(method = "lm")

p2 <- penguins %>% 
  ggplot (aes(fill = species, x = bill_length_mm)) + 
  geom_density(alpha=.5) + theme_light() + coord_flip()

p1 + p2 

p1 <- penguins %>% 
  ggplot (aes(x = flipper_length_mm, y = bill_length_mm, color = species, fill=species)) +
  geom_point(alpha=.5) + theme_light()  + geom_smooth(method = "lm")

p2 <- penguins %>% 
  ggplot (aes(fill = species, x = flipper_length_mm)) + 
  geom_density(alpha=.5) + theme_light() 

p1 / p2 

# 18. Mapas
datos <- occ_search(scientificName = "Lynx pardinus", limit = 100)
df <- datos$data
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey30") +
  geom_point(data = df, aes(x = decimalLongitude, y = decimalLatitude),
             color = "darkred", size = 2, alpha = 0.7) +
  coord_sf() +
  theme_minimal()

iberian_peninsula <- ne_countries(country = c("spain", "portugal"))
ggplot() +
  geom_sf(data = iberian_peninsula, fill = "grey99", color = "grey30") +
  geom_point(data = df, aes(x = decimalLongitude, y = decimalLatitude),
             color = "darkgreen", size = 2, alpha = 0.7) +
  coord_sf() +
  theme_minimal()

# 19. Flora Iberica
map_distribution(genus = "Lavandula", species = "stoechas", size = 0.9)

# Ejercicios
