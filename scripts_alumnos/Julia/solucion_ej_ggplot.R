library(dplyr)
library(ggplot2)

cobertura <- read.csv(here("data/cobertura.csv"))

# boxplot
cobertura %>%
  ggplot(aes(x = sequia, y = cobertura)) +
  geom_jitter (aes(color = sequia), size = 2, alpha = 0.5) +
  geom_boxplot (aes(fill = sequia), alpha = 0.8) +
  scale_fill_manual (values = c("#5F5AA2", "#F4D06F")) +
  scale_color_manual (values = c("#5F5AA2", "#F4D06F")) +
  theme_minimal() +
  theme (axis.text.x = element_text(size = 15, face = "bold")) +
  labs(
    x = "Tipo de sequía estival",
    y = "Cobertura (%)",
    title = "Comparación entre los tipos de sequía en la cobertura de árboles",
    subtitle = "Nos interesa conocer la relación entre estas dos variables",
    caption = "Datos obtenidos de: misdatos2025"
  )

ggsave("output/cobertura_boxplot.png") # tip : en ggsave, si no especificamos un objeto, nos guarda el ultimo que hayamos ploteado



# scatterplot
cobertura %>%
  ggplot(aes(x = biomasa, y = cobertura, size = cobertura, color = sequia)) +
  geom_point(alpha = 0.6) +
  scale_color_manual (values = c("#5F5AA2", "#F4D06F")) +
  theme_minimal() +
  theme (
    axis.text.x = element_text(
      size = 10,
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      face = "italic"
    ),
    axis.text.y = element_text(size = 10, face = "bold")
  ) +
  labs(
    x = "Biomasa en g/m3",
    y = "Cobertura (%)",
    title = "Biomasa de árboles en España",
    subtitle = "Nos interesa conocer la relación entre estas dos variables",
    caption = "Y su diferencia entre lluvia y sequía"
  )


ggsave("output/cobertura_scatter.png") 
