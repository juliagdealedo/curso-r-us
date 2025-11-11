# ANÁLISIS MULTIVARIANTES: PCA Y NMDS
library(corrgram)
library(factoextra)
library(aods3)
library(MASS)
library(vegan)
library(visreg)
library(here)

# cargar los datos
here()
clima <- read.table("data/exoticas.txt", header = T, sep = "\t")
str(clima)

# Explorar correlación de las variables
corrgram(clima[,-1], lower.panel=panel.pts, upper.panel=panel.conf, diag.panel=panel.density)

# Hacer PCA
pca_1 <- prcomp(clima[,-1], scale = T)
summary(pca_1)

# Análisis de la matriz factorial
pca_1$rotation[,1:2]

# Interpretación de los factores
fviz_pca_var(pca_1, col.var = "black")

# Ajustar modelo estadístico riqueza especies exóticas frente a los dos ejes principales
clima$pca1 <- pca_1$x[,1]
clima$pca2 <- pca_1$x[, 2]
glm_exoticas1 <- glm(Alien ~ pca1 + pca2, clima, family = "poisson")
anova(glm_exoticas1)

# Supuestos del modelo
par(mfcol=c(2,2))
plot(glm_exoticas1)
gof(glm_exoticas1)

# Modelo binomial negativo
glm_exoticas2 <- glm.nb(Alien ~ pca1 + pca2, clima)

par(mfcol=c(2,2))
plot(glm_exoticas2)
gof(glm_exoticas2)

anova(glm_exoticas2)

# Gráficos predicciones
par(mfcol=c(1,2))
visreg(glm_exoticas2, scale="response", ylab="Riqueza de especies")

# Escalamiento multidimensional no métrico (NMDS)

# Gradiente de composición florística en bosques tropicales montanos

bio <- read.table("MANOVA-bio.txt", header=T, sep="\t")
env <- read.table("MANOVA-env.txt", header=T, sep="\t")

# Hacer NMDS
set.seed(0)
nmds_1 <- metaMDS(bio, distance = "bray", try = 200, trymax = 200)

# Representar NMDS
plot(nmds_1)

# Gráfico con diferencias por tipo de bosque y relación lineal con variables
coordenadas <- as.data.frame(scores(nmds_1)$sites)
coordenadas$Forest = env$Forest
envfit_res <- envfit(nmds_1, env, permutations = 999)
env_vectors <- as.data.frame(scores(envfit_res, display = "vectors"))
env_vectors$Variable <- rownames(env_vectors)

ggplot(coordenadas, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 4, aes( shape = Forest, colour = Forest)) +
  # Vectores ambientales (flechas)
  geom_segment(data = env_vectors,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +
  # Nombres de las variables
  geom_text(data = env_vectors,
            aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = Variable),
            color = "black",
            size = 4) +
  theme_minimal()
