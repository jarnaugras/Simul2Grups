# ==============================================================================
# Funciones de descarga
# ==============================================================================

# Descargar resultados
output$downloadResults <- downloadHandler(
  filename = function() {
    paste("resultados_simulacion_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(results(), file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)

# Descargar gráfico de resultados
output$downloadResultsPlot <- downloadHandler(
  filename = function() {
    paste("grafico_resultados_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 800, height = 600, res = 100)
    res <- results()
    barplot(res$Proporcion, names.arg = res$Prueba, 
            main = "Proporción de Rechazos (Error Tipo I)",
            ylab = "Proporción", ylim = c(0, 1),
            col = "skyblue", las = 2)
    abline(h = input$alpha, col = "red", lty = 2)
    dev.off()
  }
)

# Descargar gráfico de distribución de datos
output$downloadDataDist <- downloadHandler(
  filename = function() {
    paste("distribucion_datos_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = output$dataDistribution(), device = "png", 
           width = 8, height = 6, dpi = 100)
  }
)

# Descargar QQ-Plot
output$downloadQQPlot <- downloadHandler(
  filename = function() {
    paste("qqplot_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 800, height = 600, res = 100)
    data <- sample_data()
    par(mfrow = c(1, 2))
    qqnorm(data$data1, main = "QQ-Plot Grupo 1")
    qqline(data$data1)
    qqnorm(data$data2, main = "QQ-Plot Grupo 2")
    qqline(data$data2)
    dev.off()
  }
)

# Descargar gráfico de p-valores
output$downloadPvaluesPlot <- downloadHandler(
  filename = function() {
    paste("pvalues_plot_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = output$pvaluesPlot(), device = "png", 
           width = 8, height = 6, dpi = 100)
  }
)

# Descargar gráfico de bootstrap
output$downloadBootstrapPlot <- downloadHandler(
  filename = function() {
    paste("bootstrap_plot_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = output$bootstrapPlot(), device = "png", 
           width = 8, height = 6, dpi = 100)
  }
) 