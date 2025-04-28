# Simulación Error Tipo I: Diseño de dos Grupos
# GitHub: https://github.com/jarnau/simula2g
# Licencia: MIT

#' @title Simulación Error Tipo I
#' @description Aplicación Shiny para simular y analizar el Error Tipo I en diseños de dos grupos
#' @author Jarnau G
#' @version 0.1.0
#' @import shiny
#' @import shinydashboard
#' @import SimMultiCorrData
#' @import WRS2
#' @import conflicted
#' @import ggplot2
#' @import gridExtra

# Limpiar el entorno de trabajo
rm(list = ls())
gc()

# Resolver conflicto de box
conflicts_prefer(shinydashboard::box)

# Función para generar datos con la transformación de Fleishman
#' @param n Tamaño de la muestra
#' @param mean Media
#' @param sd Desviación estándar
#' @param skew Asimetría
#' @param kurt Curtosis
#' @return Vector de datos generados
generate_fleishman <- function(n, mean, sd, skew, kurt) { 
  rnorm_data <- rnorm(n)
  z <- (rnorm_data - mean) / sd
  x <- mean + sd * (z + 0.5 * (z^2 - 1) * skew + (z^3 - 3*z) * (kurt / 24))
  return(x)
}

# Función para realizar bootstrap normal (apareados)
#' @param data1 Primer conjunto de datos
#' @param data2 Segundo conjunto de datos
#' @param n_bootstrap Número de iteraciones bootstrap
#' @return p-valor bootstrap
bootstrap_paired_normal <- function(data1, data2, n_bootstrap) {
  differences <- data1 - data2
  n <- length(differences)
  mean_differences <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    resample_indices <- sample(1:n, size = n, replace = TRUE)
    resample_dif <- differences[resample_indices]
    mean_differences[i] <- mean(resample_dif)
  }
  
  p_value <- mean(mean_differences >= 0)
  return(min(p_value, 1 - p_value) * 2)
}

# Función para realizar bootstrap con trimming (apareados)
#' @param data1 Primer conjunto de datos
#' @param data2 Segundo conjunto de datos
#' @param n_bootstrap Número de iteraciones bootstrap
#' @param trimming Proporción de trimming
#' @return p-valor bootstrap
bootstrap_paired_trim <- function(data1, data2, n_bootstrap, trimming) {
  differences <- data1 - data2
  n <- length(differences)
  t_stats <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    resample_indices <- sample(1:n, size = n, replace = TRUE)
    resample_dif <- differences[resample_indices]
    trimmed_mean <- mean(resample_dif, trim = trimming)
    std_error <- sd(resample_dif) / sqrt(n)
    t_stats[i] <- trimmed_mean / std_error
  }
  
  p_value <- mean(abs(t_stats) >= abs(mean(t_stats)))
  return(min(p_value, 1 - p_value) * 2)
}

# Función para realizar bootstrap normal (independientes)
#' @param data1 Primer conjunto de datos
#' @param data2 Segundo conjunto de datos
#' @param n_bootstrap Número de iteraciones bootstrap
#' @return p-valor bootstrap
bootstrap_independent_normal <- function(data1, data2, n_bootstrap) {
  n1 <- length(data1)
  n2 <- length(data2)
  mean_differences <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    resample1 <- sample(data1, size = n1, replace = TRUE)
    resample2 <- sample(data2, size = n2, replace = TRUE)
    mean_differences[i] <- mean(resample1) - mean(resample2)
  }
  
  p_value <- mean(mean_differences >= 0)
  return(min(p_value, 1 - p_value) * 2)
}

# Función para realizar bootstrap con trimming (independientes)
#' @param data1 Primer conjunto de datos
#' @param data2 Segundo conjunto de datos
#' @param n_bootstrap Número de iteraciones bootstrap
#' @param trimming Proporción de trimming
#' @return p-valor bootstrap
bootstrap_independent_trim <- function(data1, data2, n_bootstrap, trimming) {
  n1 <- length(data1)
  n2 <- length(data2)
  t_stats <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    resample1 <- sample(data1, size = n1, replace = TRUE)
    resample2 <- sample(data2, size = n2, replace = TRUE)
    trimmed_mean1 <- mean(resample1, trim = trimming)
    trimmed_mean2 <- mean(resample2, trim = trimming)
    std_error <- sqrt(var(resample1)/n1 + var(resample2)/n2)
    t_stats[i] <- (trimmed_mean1 - trimmed_mean2) / std_error
  }
  
  p_value <- mean(abs(t_stats) >= abs(mean(t_stats)))
  return(min(p_value, 1 - p_value) * 2)
}

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(
    title = "Simulación Error Tipo I: Diseño de dos Grupos",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      menuItem("Parámetros de Simulación", tabName = "params", icon = icon("sliders")),
      menuItem("Resultados", tabName = "results", icon = icon("chart-bar")),
      menuItem("Distribuciones", tabName = "distributions", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
        fluidRow(
          box(title = "Bienvenido a la Simulación de Error Tipo I", width = 12,
            h3("Descripción"),
            p("Esta aplicación permite simular y analizar el Error Tipo I en diseños de dos grupos, tanto apareados como independientes."),
            h3("Características"),
            tags$ul(
              tags$li("Generación de datos con distribución asimétrica usando la transformación de Fleishman"),
              tags$li("Dos tipos de diseño:"),
              tags$ul(
                tags$li("Grupos apareados"),
                tags$li("Grupos independientes")
              ),
              tags$li("Comparación de diferentes pruebas estadísticas:"),
              tags$ul(
                tags$li("Prueba t"),
                tags$li("Prueba de Wilcoxon"),
                tags$li("Prueba de Yuen"),
                tags$li("Versiones bootstrap de cada prueba")
              ),
              tags$li("Visualización de distribuciones y resultados")
            ),
            h3("Instrucciones de Uso"),
            tags$ol(
              tags$li("Seleccione el tipo de diseño (apareado o independiente)"),
              tags$li("Configure los parámetros de los grupos en la pestaña 'Parámetros de Simulación'"),
              tags$li("Ajuste el número de simulaciones y el nivel de significancia"),
              tags$li("Haga clic en 'Ejecutar Simulación'"),
              tags$li("Revise los resultados en las pestañas 'Resultados' y 'Distribuciones'")
            )
          )
        )
      ),
      
      tabItem(tabName = "params",
        fluidRow(
          box(title = "Tipo de Diseño", width = 12,
            radioButtons("design_type", "Seleccione el tipo de diseño:",
                        choices = c("Grupos Apareados" = "paired",
                                  "Grupos Independientes" = "independent"),
                        selected = "paired")
          )
        ),
        fluidRow(
          box(title = "Parámetros Grupo 1", width = 6,
            numericInput("n1", "Tamaño muestra:", 30, min = 10, max = 1000),
            numericInput("mu1", "Media:", 0),
            numericInput("sd1", "Desviación estándar:", 1, min = 0.1),
            numericInput("skew1", "Asimetría:", 0.4),
            numericInput("kurt1", "Curtosis:", 0.8)
          ),
          box(title = "Parámetros Grupo 2", width = 6,
            numericInput("n2", "Tamaño muestra:", 30, min = 10, max = 1000),
            numericInput("mu2", "Media:", 0),
            numericInput("sd2", "Desviación estándar:", 1, min = 0.1),
            numericInput("skew2", "Asimetría:", 1),
            numericInput("kurt2", "Curtosis:", 1.5)
          )
        ),
        fluidRow(
          box(title = "Parámetros de Simulación", width = 12,
            numericInput("n_simul", "Número de simulaciones:", 1000, min = 100, max = 10000),
            numericInput("alpha", "Nivel de significancia:", 0.05, min = 0.01, max = 0.1),
            numericInput("n_bootstrap", "Número de bootstrap:", 1000, min = 100, max = 10000),
            numericInput("trimming", "Proporción de trimming:", 0.2, min = 0, max = 0.5),
            actionButton("run", "Ejecutar Simulación", class = "btn-primary")
          )
        )
      ),
      
      tabItem(tabName = "results",
        fluidRow(
          box(title = "Resultados de la Simulación", width = 12,
            tableOutput("resultsTable"),
            plotOutput("resultsPlot"),
            downloadButton("downloadResults", "Descargar Resultados (CSV)"),
            downloadButton("downloadResultsPlot", "Descargar Gráfico de Resultados (PNG)")
          )
        )
      ),
      
      tabItem(tabName = "distributions",
        fluidRow(
          box(title = "Distribución de Datos", width = 6,
            plotOutput("dataDistribution"),
            downloadButton("downloadDataDist", "Descargar Gráfico (PNG)")
          ),
          box(title = "QQ-Plot", width = 6,
            plotOutput("qqPlot"),
            downloadButton("downloadQQPlot", "Descargar Gráfico (PNG)")
          )
        ),
        fluidRow(
          box(title = "Distribución de p-valores", width = 6,
            plotOutput("pvaluesPlot"),
            downloadButton("downloadPvaluesPlot", "Descargar Gráfico (PNG)")
          ),
          box(title = "Bootstrap Distributions", width = 6,
            plotOutput("bootstrapPlot"),
            downloadButton("downloadBootstrapPlot", "Descargar Gráfico (PNG)")
          )
        )
      )
    )
  )
)

# Servidor
server <- function(input, output) {
  
  # Datos reactivos para los gráficos
  sample_data <- reactive({
    data1 <- generate_fleishman(input$n1, input$mu1, input$sd1, input$skew1, input$kurt1)
    data2 <- generate_fleishman(input$n2, input$mu2, input$sd2, input$skew2, input$kurt2)
    list(data1 = data1, data2 = data2)
  })
  
  results <- eventReactive(input$run, {
    # Inicialización de contadores
    rechazos_ttest <- 0
    rechazos_bootstrap_t <- 0
    rechazos_wilcoxon <- 0
    rechazos_bootstrap_w <- 0
    rechazos_yuen <- 0
    rechazos_bootstrap_y <- 0
    
    # Simulaciones
    withProgress(message = 'Realizando simulaciones...', value = 0, {
      for (sim in 1:input$n_simul) {
        # Generar datos
        data1 <- generate_fleishman(input$n1, input$mu1, input$sd1, input$skew1, input$kurt1)
        data2 <- generate_fleishman(input$n2, input$mu2, input$sd2, input$skew2, input$kurt2)
        
        if (input$design_type == "paired") {
          # Prueba t apareada
          ttest_result <- t.test(data1, data2, paired = TRUE)
          if (ttest_result$p.value < input$alpha) {
            rechazos_ttest <- rechazos_ttest + 1
          }
          
          # Bootstrap t apareado
          p_bootstrap_t <- bootstrap_paired_normal(data1, data2, input$n_bootstrap)
          if (p_bootstrap_t < input$alpha) {
            rechazos_bootstrap_t <- rechazos_bootstrap_t + 1
          }
          
          # Prueba de Wilcoxon apareada
          wilcoxon_result <- wilcox.test(data1, data2, paired = TRUE)
          if (wilcoxon_result$p.value < input$alpha) {
            rechazos_wilcoxon <- rechazos_wilcoxon + 1
          }
          
          # Bootstrap Wilcoxon apareado
          p_bootstrap_w <- bootstrap_paired_normal(data1, data2, input$n_bootstrap)
          if (p_bootstrap_w < input$alpha) {
            rechazos_bootstrap_w <- rechazos_bootstrap_w + 1
          }
          
          # Prueba de Yuen apareada
          yuen_result <- yuend(data1, data2, tr = input$trimming)
          if (yuen_result$p.value < input$alpha) {
            rechazos_yuen <- rechazos_yuen + 1
          }
          
          # Bootstrap Yuen apareado
          p_bootstrap_y <- bootstrap_paired_trim(data1, data2, input$n_bootstrap, input$trimming)
          if (p_bootstrap_y < input$alpha) {
            rechazos_bootstrap_y <- rechazos_bootstrap_y + 1
          }
        } else {
          # Prueba t independiente
          ttest_result <- t.test(data1, data2, paired = FALSE)
          if (ttest_result$p.value < input$alpha) {
            rechazos_ttest <- rechazos_ttest + 1
          }
          
          # Bootstrap t independiente
          p_bootstrap_t <- bootstrap_independent_normal(data1, data2, input$n_bootstrap)
          if (p_bootstrap_t < input$alpha) {
            rechazos_bootstrap_t <- rechazos_bootstrap_t + 1
          }
          
          # Prueba de Wilcoxon independiente
          wilcoxon_result <- wilcox.test(data1, data2, paired = FALSE)
          if (wilcoxon_result$p.value < input$alpha) {
            rechazos_wilcoxon <- rechazos_wilcoxon + 1
          }
          
          # Bootstrap Wilcoxon independiente
          p_bootstrap_w <- bootstrap_independent_normal(data1, data2, input$n_bootstrap)
          if (p_bootstrap_w < input$alpha) {
            rechazos_bootstrap_w <- rechazos_bootstrap_w + 1
          }
          
          # Prueba de Yuen independiente
          df <- data.frame(
            value = c(data1, data2),
            group = factor(rep(c("g1", "g2"), c(length(data1), length(data2))))
          )
          yuen_result <- yuen(value ~ group, data = df, tr = input$trimming)
          if (yuen_result$p.value < input$alpha) {
            rechazos_yuen <- rechazos_yuen + 1
          }
          
          # Bootstrap Yuen independiente
          p_bootstrap_y <- bootstrap_independent_trim(data1, data2, input$n_bootstrap, input$trimming)
          if (p_bootstrap_y < input$alpha) {
            rechazos_bootstrap_y <- rechazos_bootstrap_y + 1
          }
        }
        
        incProgress(1/input$n_simul)
      }
    })
    
    # Crear tabla de resultados
    data.frame(
      Prueba = c("t-test", "Bootstrap t", "Wilcoxon", 
                "Bootstrap Wilcoxon", "Yuen", "Bootstrap Yuen"),
      Rechazos = c(rechazos_ttest, rechazos_bootstrap_t, rechazos_wilcoxon, 
                  rechazos_bootstrap_w, rechazos_yuen, rechazos_bootstrap_y),
      Proporcion = c(rechazos_ttest/input$n_simul, rechazos_bootstrap_t/input$n_simul,
                    rechazos_wilcoxon/input$n_simul, rechazos_bootstrap_w/input$n_simul,
                    rechazos_yuen/input$n_simul, rechazos_bootstrap_y/input$n_simul)
    )
  })
  
  output$resultsTable <- renderTable({
    results()
  })
  
  output$resultsPlot <- renderPlot({
    res <- results()
    barplot(res$Proporcion, names.arg = res$Prueba, 
            main = "Proporción de Rechazos (Error Tipo I)",
            ylab = "Proporción", ylim = c(0, 1),
            col = "skyblue", las = 2)
    abline(h = input$alpha, col = "red", lty = 2)
  })
  
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
  
  output$qqPlot <- renderPlot({
    data <- sample_data()
    par(mfrow = c(1, 2))
    qqnorm(data$data1, main = "QQ-Plot Grupo 1")
    qqline(data$data1)
    qqnorm(data$data2, main = "QQ-Plot Grupo 2")
    qqline(data$data2)
  })
  
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
  
  # Funciones de descarga para los resultados
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("resultados_simulacion_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
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
  
  # Funciones de descarga para los gráficos de distribución
  output$downloadDataDist <- downloadHandler(
    filename = function() {
      paste("distribucion_datos_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = output$dataDistribution(), device = "png", 
             width = 8, height = 6, dpi = 100)
    }
  )
  
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
  
  output$downloadPvaluesPlot <- downloadHandler(
    filename = function() {
      paste("pvalues_plot_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = output$pvaluesPlot(), device = "png", 
             width = 8, height = 6, dpi = 100)
    }
  )
  
  output$downloadBootstrapPlot <- downloadHandler(
    filename = function() {
      paste("bootstrap_plot_", format(Sys.Date(), "%Y%m%d"), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = output$bootstrapPlot(), device = "png", 
             width = 8, height = 6, dpi = 100)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server) 