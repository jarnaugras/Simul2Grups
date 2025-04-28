# ==============================================================================
# Gráficos
# ==============================================================================

# Tabla de resultados
output$resultsTable <- renderTable({
  results()
})

# Gráfico de resultados
output$resultsPlot <- renderPlot({
  res <- results()
  barplot(res$Proporcion, names.arg = res$Prueba, 
          main = "Proporción de Rechazos (Error Tipo I)",
          ylab = "Proporción", ylim = c(0, 1),
          col = "skyblue", las = 2)
  abline(h = input$alpha, col = "red", lty = 2)
})

# Distribución de datos
output$dataDistribution <- renderPlot({
  data <- sample_data()
  df <- data.frame(
    value = c(data$data1, data$data2),
    group = rep(c("Grupo 1", "Grupo 2"), each = input$n1)
  )
  
  ggplot(df, aes(x = value, fill = group)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(x = "Valor", y = "Densidad", title = "Distribución de Datos")
})

# QQ-Plot
output$qqPlot <- renderPlot({
  data <- sample_data()
  par(mfrow = c(1, 2))
  qqnorm(data$data1, main = "QQ-Plot Grupo 1")
  qqline(data$data1)
  qqnorm(data$data2, main = "QQ-Plot Grupo 2")
  qqline(data$data2)
})

# Distribución de p-valores
output$pvaluesPlot <- renderPlot({
  res <- results()
  ggplot(res, aes(x = Prueba, y = Proporcion)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_hline(yintercept = input$alpha, color = "red", linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Prueba", y = "Proporción de Rechazos", 
         title = "Proporción de Rechazos por Prueba")
})

# Distribución bootstrap
output$bootstrapPlot <- renderPlot({
  data <- sample_data()
  if (input$design_type == "paired") {
    differences <- data$data1 - data$data2
    n <- length(differences)
    n_bootstrap <- 1000
    
    bootstrap_stats <- numeric(n_bootstrap)
    for (i in 1:n_bootstrap) {
      resample_indices <- sample(1:n, size = n, replace = TRUE)
      resample_dif <- differences[resample_indices]
      bootstrap_stats[i] <- mean(resample_dif)
    }
  } else {
    n1 <- length(data$data1)
    n2 <- length(data$data2)
    n_bootstrap <- 1000
    
    bootstrap_stats <- numeric(n_bootstrap)
    for (i in 1:n_bootstrap) {
      resample1 <- sample(data$data1, size = n1, replace = TRUE)
      resample2 <- sample(data$data2, size = n2, replace = TRUE)
      bootstrap_stats[i] <- mean(resample1) - mean(resample2)
    }
  }
  
  ggplot(data.frame(statistic = bootstrap_stats), aes(x = statistic)) +
    geom_density(fill = "skyblue", alpha = 0.5) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(x = "Estadística Bootstrap", y = "Densidad",
         title = "Distribución Bootstrap de la Media de Diferencias")
}) 