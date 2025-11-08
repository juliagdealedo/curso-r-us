## MODELOS LINEARES GENERALIZADOS
library(ADER)
library(ggplot2)
library(aods3)
library(MASS)
library(car)
library(visreg)
library(multcomp)

## Ejemplo con datos tipo conteo

data(ara)
str(ara)

### Exploración gráfica de los datos
  
ggplot(ara, aes(x = dungs, y = seedlings)) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ property) +
  labs(
    title = "Relación nº plántulas y nº bostas",
    x = "Nº bostas",
    y = "Nº plántulas"
  ) +
  theme_minimal()
 
#### Construcción modelo

# esta linea de codigo aborta la sesión de R  
# glm_arau1 <- glm(seedlings ~ dungs * property, ara, family = "poisson")
# solución
saveRDS(glm(seedlings ~ dungs * property, ara, family = "poisson"),"name.RDS")
glm_arau1 <- readRDS("name.RDS")
anova(glm_arau1, test = "Chi")

print(D2 <- 1 - (198.02/347.56))

summary(glm_arau1)

#### Supuestos del modelo
  
par(mfrow=c(2,2))
plot(glm_arau1)      

gof(glm_arau1)      

### Modelo binomial negativo
  
# esta linea de codigo aborta la sesión de R  
# glm_arau2 <- glm.nb(seedlings ~ dungs*property, ara)
saveRDS(glm.nb(seedlings ~ dungs*property, ara),"name.RDS")
glm_arau2 <- readRDS("name.RDS")
summary(glm_arau2)

### Supuestos del modelo
  
par(mfrow=c(2,2))
plot(glm_arau2)      

gof(glm_arau2)      

#### Interpretación del resultado y predicciones modelo
  
Anova(glm_arau2, type = "III")      

visreg(glm_arau2, "dungs", by ="property", overlay = T, scale = "response", ylab = "Nº plántulas", xlab = "Nº bostas")

## Ejemplo con distribución de errores binomial

data(algas)
str(algas)

#### Exploración de los datos
  
ggplot(algas, aes(x = Long, y = Estado, color = Sp)) +
  geom_point(alpha = 0.4, position = position_jitter(height = 0.05)) +
  facet_wrap(~Sp) +
  labs(
    x = "Talla individuo (cm)",
    y = "Estructura reproductora",
  ) +
  theme_minimal()

#### Ajuste del modelo

glm_algas1 <- glm(Estado ~ Long*Sp, algas, family = "binomial")
anova(glm_algas1, test= "Chi")

glm_algas2 <- glm(Estado ~ Long+Sp, algas, family = "binomial")
anova(glm_algas2, test= "Chi")
print(D2 <- 1 - (341.6/476.38))

#### Supuestos del modelo
  
par(mfrow=c(2,2))
plot(glm_algas2)      

### Estructura del modelo
  
summary(glm_algas2)

#### Gráfico de predicciones

# a. sin dividir por niveles del factor

# sacar nuevos datos
x_seq <- seq(min(algas$Long), max(algas$Long), length.out = 100)
newdata <- expand.grid(Long = x_seq, Sp = levels(algas$Sp))

# Obtener predicciones en escala de respuesta (probabilidades)
pred <- predict(glm_algas2, newdata, type = "link", se.fit = TRUE)

# Calcular intervalos de confianza
crit <- qnorm(0.975)  # 95%
newdata$fit_link <- pred$fit
newdata$lwr_link <- pred$fit - crit * pred$se.fit
newdata$upr_link <- pred$fit + crit * pred$se.fit

# Convertir a escala de probabilidad (logística)
inv_logit <- function(x) exp(x) / (1 + exp(x))
newdata$fit <- inv_logit(newdata$fit_link)
newdata$lwr <- inv_logit(newdata$lwr_link)
newdata$upr <- inv_logit(newdata$upr_link)

# Promediar sobre el factor 
pred_marginal <- aggregate(cbind(fit, lwr, upr) ~ Long, data = newdata, mean)

# Graficar relación promedio con intervalos de confianza
ggplot(pred_marginal, aes(x = Long, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#0072B2", alpha = 0.2) +
  geom_line(color = "#0072B2", linewidth = 1.3) +
  labs(
    x = "Talla individuo (cm)",
    y = "Predicción probabilidad estructuras reproductoras",
  ) +
  theme_minimal(base_size = 14)

# b. Separando por niveles del factor

newdat <- expand.grid(
  Long = seq(min(algas$Long), max(algas$Long), length = 100),
  Sp = levels(algas$Sp)
)

# Predicciones de probabilidad
pred <- predict(glm_algas2, newdat, type = "link", se.fit = TRUE)

# Convertir de escala logit a probabilidad
newdat$fit <- plogis(pred$fit)
newdat$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
newdat$upr <- plogis(pred$fit + 1.96 * pred$se.fit)

ggplot(newdat, aes(x = Long, y = fit, color = Sp, fill = Sp)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, color = NA) +
  labs(
    x = "Talla individuo (cm)",
    y = "Predicción probabilidad estructuras reproductoras",
  ) +
  theme_minimal(base_size = 13)

# Diferencias significativas entre grupos
summary(glht(glm_algas2, linfct = mcp(Sp = "Tukey")))
