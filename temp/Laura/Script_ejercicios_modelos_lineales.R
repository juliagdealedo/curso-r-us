# SOLUCIÓN EJERCICIOS MODELOS LINEALES
library(faraway)
library(ggplot2)
library(patchwork)
library(GGally)
library(car)
library(broom)
library(tidyverse)
library(dplyr)
library(here)

# EJERCICIO REGRESIÓN MÚLTIPLE

### cargar los datos

data(gala)
head(gala)

### 1. Análisis exploratorios
  
plot1 <- ggplot(data = gala, aes(x = Area , y = Species)) +  
  geom_point(color = "blue", size = 1.5) +      
  labs(x = "Área de la isla",         
       y = "Riqueza de especies",                   
       title = "Relación entre riqueza de especies y área de la isla"   
  ) +
  theme_minimal()   

plot2 <- ggplot(data = gala, aes(x = Elevation , y = Species)) +  
  geom_point(color = "red", size = 1.5) +      
  labs(x = "Elevación de la isla",         
       y = "",                   
       title = "Relación entre riqueza de especies y elevación de la isla"   
  ) +
  theme_minimal()

plot3 <- ggplot(data = gala, aes(x = Nearest , y = Species)) +  
  geom_point(color = "darkgreen", size = 1.5) +      
  labs(x = "Distancia isla más próxima",         
       y = "",                   
       title = "Relación entre riqueza de especies y distancia isla más próxima"   
  ) +
  theme_minimal() 

plot1 + plot2 + plot3

### 2. Correlación variables explicativas
  
ggcorr(gala[,c(1,3:5)], label = TRUE)

### 3. Ajuste modelo lineal e interpretación modelo
  
lm.gal0<-lm(Species~Area+Elevation+Nearest, gala)
anova(lm.gal0)

### 4. Comprobación supuestos del modelo
  
par(mfcol=c(2,2))
plot(lm.gal0)

vif(lm.gal0)

gala$log_Sp <- log(gala$Species)
plot1 <- ggplot(data = gala, aes(x = Area , y = log_Sp)) +  
  geom_point(color = "blue", size = 1.5) +      
  labs(x = "Área de la isla",         
       y = "Riqueza de especies",                   
       title = "Relación entre riqueza de especies y área de la isla"   
  ) +
  theme_minimal()   
plot2 <- ggplot(data = gala, aes(x = Elevation , y = log_Sp)) +  
  geom_point(color = "red", size = 1.5) +      
  labs(x = "Elevación de la isla",         
       y = "",                   
       title = "Relación entre riqueza de especies y elevación de la isla"   
  ) +
  theme_minimal()
plot3 <- ggplot(data = gala, aes(x = Nearest , y = log_Sp)) +  
  geom_point(color = "darkgreen", size = 1.5) +      
  labs(x = "Distancia isla más próxima",         
       y = "",                   
       title = "Relación entre riqueza de especies y distancia isla más próxima"   
  ) +
  theme_minimal() 

plot1 + plot2 + plot3

gala$log_area <- log(gala$Area)
gala$log_elevacion <- log(gala$Elevation)
gala$log_near <- log(gala$Nearest)

plot1 <- ggplot(data = gala, aes(x = log_area , y = log_Sp)) +  
  geom_point(color = "blue", size = 2) +      
  labs(x = "Área de la isla",         
       y = "Riqueza de especies",                   
       title = "Relación entre riqueza de especies y área de la isla"   
  ) +
  theme_minimal()   

plot2 <- ggplot(data = gala, aes(x = log_elevacion , y = log_Sp)) +  
  geom_point(color = "red", size = 2) +      
  labs(x = "Elevación de la isla",         
       y = "",                   
       title = "Relación entre riqueza de especies y elevación de la isla"   
  ) +
  theme_minimal()

plot3 <- ggplot(data = gala, aes(x = log_near , y = log_Sp)) +  
  geom_point(color = "darkgreen", size = 2) +      
  labs(x = "Distancia isla más próxima",         
       y = "",                   
       title = "Relación entre riqueza de especies y distancia isla más próxima"   
  ) +
  theme_minimal() 

plot1 + plot2 + plot3

# hacer modelo y comprobación supuestos

lm.gal1 <- lm(log_Sp~log_area+log_elevacion+log_near, gala)
par(mfcol=c(2,2))
plot(lm.gal1)

# interpretación variables significativas
Anova(lm.gal1,type="III")

vif(lm.gal1)

# eliminamos area o elevación
lm.gal2 <- lm(log_Sp~log_area+log_near,gala)
Anova(lm.gal2,type="III")

# eliminamos distancia isla más próxima
lm.gal3 <- lm(log_Sp~log_area,gala)
Anova(lm.gal3,type="III")

summary(lm.gal3)

# predicciones del modelo
preds <- augment(lm.gal3, type.predict = "response", se_fit = TRUE)
preds <- preds %>%
  mutate(lower = .fitted - 1.96 * .se.fit,upper = .fitted + 1.96 * .se.fit)

ggplot(data = preds, aes(x = log_area, y = log_Sp)) +  
  geom_point(color = "blue", size = 2) +     
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Área de la isla",         
       y = "Riqueza de especies",                   
       title = "Predicciones modelo de regresión"   
  ) +
  theme_minimal()        

## Ejercicio 2: Modelo factorial
  
### Cargar los datos
  
cobertura <- read.csv(here("data/cobertura.csv"))
head(cobertura)

### 1. Análisis exploratorios

ggplot(cobertura, aes(x = biomasa, y = cobertura)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ sequia) +
  labs(
    title = "Relación biomasa leñosa y cobertura arbórea",
    x = "Cobertura arbórea",
    y = "Biomasa leñosa"
  ) +
  theme_minimal()

#### Ajuste modelo de regresión

cobertura <- cobertura %>%
  mutate(sequia = as.factor(sequia))
lm_bio1 <- lm(biomasa ~ cobertura*sequia, data = cobertura)

#### Comprobación de hipótesis

anova(lm_bio1)

#### Supuestos del modelo

par(mfrow=c(2,2))
plot(lm_bio1)

summary(lm_bio1)

#### Gráfica de predicciones
  
newdata <- expand.grid(
  cobertura = seq(min(cobertura$cobertura), max(cobertura$cobertura), length.out = 100),
  sequia = levels(cobertura$sequia)
)
predicciones_grid <- augment(lm_bio1, newdata = newdata)

ggplot(cobertura, aes(x = cobertura, y = biomasa, color = sequia)) +
  geom_point(alpha = 0.5) +
  geom_line(data = predicciones_grid, aes(x= cobertura, y = .fitted, group = sequia, color = sequia), size = 1.2) +
  scale_color_manual(values = c("#B3E5FC", "#F8BBD0")) +
  labs(
    title = "ANCOVA: predicciones por nivel de sequía",
    x = "Cobertura",
    y = "Biomasa",
    color = "Sequía"
  ) +
  theme_minimal()

## Probando con augment - Julia

library(dplyr)
data("penguins", package = "palmerpenguins")

# eliminamos NAs variables que vamos a utilizar y pasamos masa corporal a kg

penguins_clean <- penguins %>%
  filter(!is.na(flipper_length_mm), !is.na(body_mass_g))
penguins_clean$body_mass_kg <- penguins_clean$body_mass_g/1000

ggplot(data = penguins_clean, aes(x = flipper_length_mm, y = body_mass_kg)) +  
  geom_point(color = "blue", size = 2) +      
  labs(x = "Longitud aleta (mm)",         
       y = "Masa corporal (kg)",                   
       title = "Relación entre masa corporal y longitud aleta "   
  ) +
  theme_minimal()        

# 2. Ajuste modelo de regresión

lm_reg1 <- lm(body_mass_kg ~ flipper_length_mm, data = penguins_clean)

preds <- augment(lm_reg1, type.predict = "response", se_fit = TRUE)

preds <- preds %>%
  mutate(lower = .fitted - 1.96 * .se.fit,upper = .fitted + 1.96 * .se.fit)
ggplot(data = preds, aes(x = flipper_length_mm, y = body_mass_kg)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Longitud aleta (mm)",
       y = "Masa corporal (kg)",
       title = "Predicciones modelo de regresión"
  ) +
  theme_minimal()


#### Incluyendo los niveles del factor

lm_5 <- lm(body_mass_g ~ flipper_length_mm*species, data = datos_limpios)
anova(lm_5)
summary(lm_5)

predicciones_grid <- augment(lm_5, type.predict = "response", se_fit = TRUE)
ggplot(datos_limpios, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = predicciones_grid, aes(x= flipper_length_mm, y = .fitted, group = species, color = species), size = 1.2) +
  scale_color_manual(values = c("#F8BBD0", "#C8E6C9", "#B3E5FC")) +
  labs(
    title = "ANCOVA: predicciones por especie",
    x = "Longitud aleta (mm)",
    y = "Masa corporal (g)",
    color = "Especie"
  ) +
  theme_minimal()

