tabItems(
  # Pestaña de inicio
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
  
  # Pestaña de parámetros
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
  
  # Pestaña de resultados
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
  
  # Pestaña de distribuciones
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