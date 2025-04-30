# Simulación Error Tipo I: Diseño de dos Grupos

Esta aplicación Shiny permite simular y analizar el Error Tipo I en diseños de dos grupos, tanto apareados como independientes.

## Características Principales

- Generación de datos con distribución asimétrica usando la transformación de Fleishman
- Dos tipos de diseño:
  - Grupos apareados
  - Grupos independientes
- Comparación de diferentes pruebas estadísticas:
  - Prueba t
  - Prueba de Wilcoxon
  - Prueba de Yuen
  - Versiones bootstrap de cada prueba
- Visualización de distribuciones y resultados

## Instalación

```r
# Instalar dependencias
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr"))
```

## Uso

1. Clone el repositorio
2. Abra R o RStudio
3. Ejecute la aplicación:
   ```r
   shiny::runApp("app")
   ```

## Documentación

Para más información, visite nuestra [página de documentación](https://jarnaugras.github.io/Simul2Grups/).

## Estructura del Proyecto

```
Simul2Grups/
├── app/           # Aplicación Shiny principal
├── R/             # Funciones auxiliares
├── docs/          # Documentación
├── tests/         # Tests unitarios
└── data/          # Datos de ejemplo
```

## Contribuir

Las contribuciones son bienvenidas. Por favor, abra un issue para discutir los cambios propuestos.

## Licencia

Este proyecto está bajo la Licencia MIT. Ver el archivo [LICENSE](LICENSE) para más detalles. 