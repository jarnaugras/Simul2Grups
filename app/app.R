#' Simulación Error Tipo I: Diseño de dos Grupos
#' 
#' @description
#' Aplicación Shiny para simular y analizar el Error Tipo I en diseños de dos grupos,
#' tanto apareados como independientes.
#' 
#' @author Jaume Arnau Gras
#' 
#' @import shiny
#' @import shinydashboard
#' @import SimMultiCorrData
#' @import WRS2
#' @import conflicted
#' @import ggplot2
#' @import gridExtra
#' @import dplyr
#' @import tidyr
#' @import DT
#' @import shinyjs
#' @import shinythemes

# Resolver conflictos de funciones
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Activar shinyjs
useShinyjs()

# Cargar módulos UI
source("../ui/tabs.R")

# Cargar módulos del servidor
source("../server/downloads.R")
source("../server/plots.R")
source("../server/simulation_results.R")
source("../server/reactive_data.R")

# Cargar funciones auxiliares
source("../R/2Grups.R")
source("../R/Anova.R")

# UI Principal
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Simulación Error Tipo I: Diseño de dos Grupos"),
    
    # Pestañas principales
    tabsetPanel(
        id = "mainTabs",
        type = "tabs",
        
        # Contenido de tabs.R
        tabPanel("Configuración",
            sidebarLayout(
                sidebarPanel(
                    # Controles de configuración
                ),
                mainPanel(
                    # Resultados y visualizaciones
                )
            )
        ),
        tabPanel("Resultados",
            # Contenido de resultados
        ),
        tabPanel("Ayuda",
            # Documentación y ayuda
        )
    )
)

# Servidor
server <- function(input, output, session) {
    # Datos reactivos
    source("../server/reactive_data.R", local = TRUE)
    
    # Gráficos
    source("../server/plots.R", local = TRUE)
    
    # Resultados de simulación
    source("../server/simulation_results.R", local = TRUE)
    
    # Descargas
    source("../server/downloads.R", local = TRUE)
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server) 