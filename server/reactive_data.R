# ==============================================================================
# Datos reactivos
# ==============================================================================

# Datos reactivos para los gráficos
sample_data <- reactive({
  data1 <- generate_fleishman(input$n1, input$mu1, input$sd1, input$skew1, input$kurt1)
  data2 <- generate_fleishman(input$n2, input$mu2, input$sd2, input$skew2, input$kurt2)
  list(data1 = data1, data2 = data2)
}) 