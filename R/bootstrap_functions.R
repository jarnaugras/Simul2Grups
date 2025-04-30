# ==============================================================================
# Funciones de Bootstrap y Generación de Datos
# ==============================================================================

#' Funciones de Bootstrap y Generación de Datos
#' @description
#' Conjunto de funciones para realizar bootstrap y generar datos con la transformación de Fleishman

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

#' Función para realizar bootstrap normal (apareados)
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

#' Función para realizar bootstrap con trimming (apareados)
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

#' Función para realizar bootstrap normal (independientes)
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

#' Función para realizar bootstrap con trimming (independientes)
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