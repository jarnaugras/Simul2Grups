# ==============================================================================
# Simulación Error Tipo I: Diseño de dos Grupos
# GitHub: https://github.com/jarnau/simula2g
# Licencia: MIT
# ==============================================================================

#' @title Simulación Error Tipo I
#' @description Aplicación Shiny para simular y analizar el Error Tipo I en diseños de dos grupos
#' @author Jarnau G
#' @version 0.1.0

# ==============================================================================
# Configuración inicial
# ==============================================================================

# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(SimMultiCorrData)
library(WRS2)
library(conflicted)
library(ggplot2)
library(gridExtra)

# Limpiar el entorno de trabajo
rm(list = ls())
gc()

# Resolver conflicto de box
conflicts_prefer(shinydashboard::box)

# ==============================================================================
# Funciones auxiliares
# ==============================================================================

# Función para generar datos con la transformación de Fleishman
generate_fleishman <- function(n, mean, sd, skew, kurt) { 
  rnorm_data <- rnorm(n)
  z <- (rnorm_data - mean) / sd
  x <- mean + sd * (z + 0.5 * (z^2 - 1) * skew + (z^3 - 3*z) * (kurt / 24))
  return(x)
}

# Funciones para bootstrap
source("R/bootstrap_functions.R")

# ==============================================================================
# UI Definition
# ==============================================================================

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
    source("ui/tabs.R", local = TRUE)$value
  )
)

# ==============================================================================
# Server Definition
# ==============================================================================

server <- function(input, output) {
  source("server/reactive_data.R", local = TRUE)
  source("server/simulation_results.R", local = TRUE)
  source("server/plots.R", local = TRUE)
  source("server/downloads.R", local = TRUE)
}

# ==============================================================================
# Run the application
# ==============================================================================

shinyApp(ui = ui, server = server)

