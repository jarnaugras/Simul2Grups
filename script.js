document.addEventListener('DOMContentLoaded', () => {
  const simulationForm = document.getElementById('simulationForm');
  const runSimulationBtn = document.getElementById('runSimulation');
  const resultsDiv = document.getElementById('results');
  
  runSimulationBtn.addEventListener('click', () => {
    // Obtener valores del formulario
    const designType = document.getElementById('designType').value;
    const n1 = parseInt(document.getElementById('n1').value);
    const n2 = parseInt(document.getElementById('n2').value);
    const alpha = parseFloat(document.getElementById('alpha').value);
    
    // Validar inputs
    if (n1 < 2 || n2 < 2) {
      alert('El tamaño de muestra debe ser al menos 2');
      return;
    }
    
    if (alpha <= 0 || alpha >= 1) {
      alert('El nivel de significancia debe estar entre 0 y 1');
      return;
    }
    
    // Mostrar mensaje de carga
    resultsDiv.innerHTML = '<p>Simulando... Esto puede tomar unos momentos.</p>';
    
    // Simular datos y calcular resultados
    setTimeout(() => {
      const results = simulateData(designType, n1, n2, alpha);
      displayResults(results);
    }, 1000);
  });
  
  function simulateData(designType, n1, n2, alpha) {
    // Esta es una simulación simplificada
    // En una aplicación real, aquí iría la lógica de simulación real
    const simulations = 1000;
    let rejections = 0;
    
    for (let i = 0; i < simulations; i++) {
      const pValue = Math.random(); // Simulación simplificada
      if (pValue < alpha) {
        rejections++;
      }
    }
    
    return {
      simulations,
      rejections,
      type1Error: (rejections / simulations).toFixed(4)
    };
  }
  
  function displayResults(results) {
    const { simulations, rejections, type1Error } = results;
    
    resultsDiv.innerHTML = `
    <h3>Resultados de la Simulación</h3>
      <p>Número de simulaciones: ${simulations}</p>
      <p>Rechazos de H₀: ${rejections}</p>
      <p>Error Tipo I estimado: ${type1Error}</p>
      <p>Nivel de significancia objetivo: ${document.getElementById('alpha').value}</p>
      
      <div class="interpretation">
        <h4>Interpretación:</h4>
        <p>El Error Tipo I estimado es ${type1Error}, lo que significa que aproximadamente 
      ${(type1Error * 100).toFixed(1)}% de las veces rechazamos incorrectamente la hipótesis nula 
      cuando en realidad es verdadera.</p>
        </div>
        `;
  }
}); 