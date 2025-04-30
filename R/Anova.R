# Cargar bibliotecas necesarias
library(ggplot2)

# Fijar semilla para reproducibilidad
set.seed(42)

# Simulación de datos
grupo_A <- rnorm(30, mean = 50, sd = 5)
grupo_B <- rnorm(30, mean = 55, sd = 5)
grupo_C <- rnorm(30, mean = 60, sd = 5)

# Crear marco de datos
datos <- data.frame(
  Grupo = factor(rep(c("A", "B", "C"), each = 30)),
  Valor = c(grupo_A, grupo_B, grupo_C)
)

# Análisis de ANOVA
modelo <- aov(Valor ~ Grupo, data = datos)
anova_results <- summary(modelo)

print(anova_results)

# Visualización
ggplot(datos, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Distribución de valores por grupo") +
  theme_minimal()
