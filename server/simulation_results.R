# ==============================================================================
# Resultados de la simulación
# ==============================================================================

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