# Paquetes que se van a utilizar
library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)
library(lmtest)
library(car)
library(multcomp)

## Modelos de regresión

#### 1. Leer los datos en R y explorar gráficamente la relación entre las variables

data(penguins)
head(penguins)

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

# 3. Variación explicada por el modelo y resolución hipótesis
  
anova(lm_reg1)

# 4. Interpretación del modelo
  
summary(lm_reg1)

# 5. Predicciones con el modelo
  
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

# 6. Revisión supuestos del modelo
  
par(mfrow=c(2,2))
plot(lm_reg1)      

# tests

# Normalidad

shapiro.test(residuals(lm_reg1))

# Linealidad

resettest(lm_reg1)

# Homocedasticidad

ncvTest(lm_reg1)

## Análisis de la varianza (ANOVA)
  
# 1. Explorar gráficamente la relación entre las variables
  
# Calculamos ratio del pico
penguins2 = penguins %>%
  mutate(bill_ratio = bill_length_mm / bill_depth_mm)

ggplot(data = penguins2, aes(x = species, y = bill_ratio, fill = species)) +  
  geom_boxplot(alpha = 0.8, color = "gray30") +
  scale_fill_manual(values = c("#F8BBD0", "#C8E6C9", "#B3E5FC")) +
  labs(x = "Species",         
       y = "Ratio del pico",                   
       title = "Diferencias ratio del pico entre especies"   
  ) +
  theme_minimal()        

# 2. Ajuste modelo factorial

penguins2 <- penguins2 %>%
  mutate(species = as.factor(species))
lm_reg2 <- lm(bill_ratio ~ species, data = penguins2)

# 3. Variación explicada por el modelo y resolución hipótesis
  
anova(lm_reg2)

# 4. Interpretación del modelo 

summary(lm_reg2)

# Ver el nivel de referencia del factor

contrasts(penguins2$species)

# Comparaciones post-hoc entre los distintos grupos
  
posthoc_species <- glht(lm_reg2, linfct=mcp(species="Tukey"))
summary(posthoc_species)

# 5. Representación gráfica diferencias significativas
  
ggplot(data = penguins2, aes(x = species, y = bill_ratio, fill = species, color = species)) +  
  geom_jitter(width = 0.15, alpha = 0.6, size = 2, shape=21, color="black") +
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_manual(values = c("#F8BBD0", "#C8E6C9", "#B3E5FC")) +
  scale_color_manual(values = c("#F8BBD0", "#C8E6C9", "#B3E5FC")) +
  labs(x = "Species",         
       y = "Ratio del pico",                   
       title = "Diferencias longitud aleta entre especies"   
  ) +
  theme_minimal() +
  geom_text(data = penguins2[2,], aes(y = 4, label = "a"), color="black", 
            size=5, nudge_x = 0) +
  geom_text(data = penguins2[2,], aes(y = 4, label = "b"), color="black", 
            size=5, nudge_x = 1) +
  geom_text(data = penguins2[2,], aes(y = 4, label = "c"), color="black", 
            size=5, nudge_x = 2) 

# 6. Revisión supuestos del modelo
  
par(mfrow=c(2,2))
plot(lm_reg2)      

# Cambiar nivel de referencia del factor
  
levels(penguins2$species)
penguins2$species <- relevel(penguins2$species, ref = "Gentoo")
levels(penguins2$species)

## Análisis de la covarianza (ANCOVA)
  
ggplot(subset(penguins, !is.na(sex)), aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ sex) +
  labs(
    title = "Relación longitud pico y masa corporal",
    x = "Longitud pico (mm)",
    y = "Masa corporal (g)"
  ) +
  theme_minimal()

# Ajuste modelo 
  
lm_3 <- lm(body_mass_g ~ bill_length_mm*sex, data = penguins)

# Comprobación de hipótesis

anova(lm_3)

# Sin interaccion 

lm_4 <- lm(body_mass_g ~ bill_length_mm+sex, data = penguins)
anova(lm_4)

ggplot(subset(penguins, !is.na(sex)), aes(x = bill_length_mm, y = body_mass_g, color = sex)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#F8BBD0", "#B3E5FC")) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relación longitud pico y masa corporal",
    x = "Longitud pico (mm)",
    y = "Masa corporal (g)",
    color = "Sexo"
  ) +
  theme_minimal()

ggplot(data = subset(penguins, !is.na(sex)), aes(x = sex, y = body_mass_g, fill = sex, color = sex)) +  
  geom_boxplot(alpha = 0.8, color = "black") +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2, shape=21, color="black") +
  scale_fill_manual(values = c("#F8BBD0", "#B3E5FC")) +
  scale_color_manual(values = c("#F8BBD0", "#B3E5FC")) +
  labs(x = "Sexo",         
       y = "Masa corporal (g)",                   
       title = "Diferencias masa corporal entre sexo"   
  ) +
  theme_minimal() +
  geom_text(data = penguins[2,], aes(y = 6500, label = "a"), color="black", 
            size=5, nudge_x = 0) +
  geom_text(data = penguins[2,], aes(y = 6500, label = "b"), color="black", 
            size=5, nudge_x = 1)         

# Ejemplo pendientes significativas

ggplot(subset(penguins, !is.na(species)), aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ species) +
  labs(
    title = "Relación longitud aleta y masa corporal",
    x = "Longitud aleta (mm)",
    y = "Masa corporal (kg)"
  ) +
  theme_minimal()

datos_limpios <- penguins %>%
  filter(!is.na(flipper_length_mm))

lm_5 <- lm(body_mass_g ~ flipper_length_mm*species, data = datos_limpios)
anova(lm_5)

summary(lm_5)

newdata <- expand.grid(
  flipper_length_mm = seq(min(datos_limpios$flipper_length_mm), max(datos_limpios$flipper_length_mm), length.out = 100),
  species = levels(datos_limpios$species)
)
predicciones_grid <- augment(lm_5, newdata = newdata)

ggplot(datos_limpios, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = predicciones_grid, aes(x= flipper_length_mm, y = .fitted, group = species, color = species), linewidth = 1.2) +
  scale_color_manual(values = c("#F8BBD0", "#C8E6C9", "#B3E5FC")) +
  labs(
    title = "ANCOVA: predicciones por especie",
    x = "Longitud aleta (mm)",
    y = "Masa corporal (g)",
    color = "Especie"
  ) +
  theme_minimal()

# Supuestos del modelo

par(mfrow=c(2,2))
plot(lm_5)