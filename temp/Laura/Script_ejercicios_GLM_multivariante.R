# EJERCICIOS GLM Y ANÁLISIS MULTIVARIANTE
library(enmpa)
library(ggplot2)
library(patchwork)
library(corrgram)
library(factoextra)
library(aods3)
library(MASS)
library(visreg)
library(car)
library(here)

# Ejercicio 1: GLM binomial
  
# Cargar los datos
  
data(enm_data)
head(enm_data)

# Análisis exploratorios

p1 <- ggplot(enm_data, aes(x = bio_1, y = Sp)) +
  geom_point(color = "blue") +
  labs(title = "Presencia vs Temperatura") +
  theme_minimal(base_size = 25)
p2 <- ggplot(enm_data, aes(x = bio_12, y = Sp)) +
  geom_point(color = "red") +
  labs(title = "Presencia vs Precipitación") +
  theme_minimal(base_size = 25)

p1 + p2  

# Ajuste modelo
  
glm_bino1 <- glm(Sp ~ bio_1 * bio_12, enm_data, family = "binomial")
anova(glm_bino1, test = "Chisq")

# Interpretación del modelo
  
summary((glm_bino1))

# Supuestos del modelo
  
par(mfcol=c(2,2))
plot(glm_bino1)

# Gráficos de predicciones
  
# Graficando la temperatura en el eje x

newdat <- with(enm_data, expand.grid(bio_1 = seq(min(bio_1), max(bio_1), length = 100),
                                     bio_12 = quantile(bio_12, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))))
pred <- predict(glm_bino1, newdat, type = "link", se.fit = TRUE)
newdat$pred <- plogis(pred$fit)
newdat$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
newdat$upr <- plogis(pred$fit + 1.96 * pred$se.fit)
p3 <- ggplot(newdat, aes(x = bio_1, y = pred, color = as.factor(bio_12))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = as.factor(bio_12)), 
              alpha = 0.2, color = NA) +
  labs(y = "Probabilidad predicha ± IC95%",
       x = "Temperatura",
       color = "Precipitación (deciles)",
       fill = "Precipitación (deciles)") +
  theme_minimal(base_size = 25)

# Graficando la precipitación en el eje x

newdat2 <- with(enm_data, expand.grid(bio_12 = seq(min(bio_12), max(bio_12), length = 100),
                                      bio_1 = quantile(bio_1, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))))
pred <- predict(glm_bino1, newdat2, type = "link", se.fit = TRUE)
newdat2$pred <- plogis(pred$fit)
newdat2$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
newdat2$upr <- plogis(pred$fit + 1.96 * pred$se.fit)
p4 <- ggplot(newdat2, aes(x = bio_12, y = pred, color = as.factor(bio_1))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = as.factor(bio_1)), 
              alpha = 0.2, color = NA) +
  labs(x = "Precipitación",
       color = "Temperatura (deciles)",
       fill = "Temperatura (deciles)") +
  theme_minimal(base_size = 25)

p3 + p4

# Ejercicio 2: GLM y multivariante
  
# Cargar los datos
here()
poli <- read.table("data/polinizacion.txt",header=T)
head(poli)

# Análisis exploratorios
  
corrgram(poli[,27:34], lower.panel = panel.pts,upper.panel = panel.conf, diag.panel = panel.density)

# Hacer PCA
  
pca_poli <- prcomp(poli[,-c(1:26)], scale = T)
summary(pca_poli)
pca_poli$rotation[,1:2]

# Interpretar matriz factorial y correlacionar variables originales con PCA
  
fviz_pca_var(pca_poli)

# Añadir los dos primeros ejes a los datos
  
poli$pca1<-pca_poli$x[,1]
poli$pca2<-pca_poli$x[,2]

# Hacer modelo

glm_poli1<-glm(peak_colony~pca1+pca2,poli,family = "poisson")
Anova(glm_poli1, type = "III")

# Analizar supuestos del modelo
  
par(mfcol=c(2,2))
plot(glm_poli1)
gof(glm_poli1)

# Modelo binomial negativo
  
glm_poli2 <- glm.nb(peak_colony ~ pca1 + pca2, poli)
Anova(glm_poli2, type="III")

# Supuestos del modelo
  
par(mfcol=c(2,2))
plot(glm_poli2)
gof(glm_poli2)

# Interpretación del modelo
  
summary(glm_poli2)

par(mfcol=c(2,2))
visreg(glm_poli2, scale="response")
