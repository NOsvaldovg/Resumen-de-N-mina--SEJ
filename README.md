# Resumen de Nómina-SEJ
Visualizador que ayuda a obtener resúmenes de nómina a partir de un archivo.

---

##  Características principales

-  **Carga de archivos validada automáticamente (csv, xslx o rds)**
-  **Limpieza y normalización de datos**
-  **Dashboards y tablas dinámicas con filtros avanzados**
-  **Descarga de datos procesados**

---

##  Estructura del proyecto

├── www/ # Archivos estáticos (CSS, js, fuentes e íconos)
├── data/ # Archivos .rds de entrada
├── server.R # Lógica del servidor
├── ui.R # Interfaz de usuario
└── README.md # Documentación general

---

##  Requisitos

- R >= 4.1
- Paquetes requeridos:

```r 
install.packages(c(
  "htmltools", "shiny", "shinyWidgets", "dplyr", "stringr",
  "lubridate", "readxl", "DT", "pryr"
))

source("run.R")
```

Desarrollado por Néstor Osvaldo Valdés González

Correo: nosvaldovg@gmail.com
Versión: 1.0.0 – Última actualización: junio 2025

