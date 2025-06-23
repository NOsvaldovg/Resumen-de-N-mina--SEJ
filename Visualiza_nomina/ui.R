# Visualizador de datos de nómina
library(htmltools)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(lubridate)
library(DT)
library(readxl)
library(pryr)

# Inicia la UI
shinyUI(
  tagList(
    tags$style(type="text/css", ".shiny-server-account { z-index: 1000; }"),
    includeCSS("./www/style.css"),
    
    # Título
    fluidPage(
      fluidRow(id = "super_titulo_imagen",
               HTML("</br>")
      ),
      fluidRow(id = "super_titulo_imagen",
               HTML("Resumen de Nómina </br>")
      ),
      fluidRow(id = "super_titulo_imagen",
               HTML("</br>")
      ),
      
      # Barra de carga de archivos, filtros y botón de filtros
      fluidRow(id = "barra_lateral",
               align="center",
               HTML("</br>"),
               
               # Barra de carga de archivos
               column(4,
                      fluidRow(id = "boton_accion",
                               align="center",
                               fileInput('nomina', 
                                         label = 'Agregue el archivo de nómina en la caja',
                                         multiple = FALSE,
                                         accept = c(".xlsx"),
                                         buttonLabel = "Buscar",
                                         placeholder = "Ningún archivo seleccionado")
                      ),
               ),
               
               # Indica cómo se agrupa la información
               column(3,
                      fluidRow(id = "boton_accion",
                               align="center",
                               radioButtons('agrupar',
                                            label = '¿Qué hacer con la información?', 
                                            choices = c("No agrupar", "Agrupar"), 
                                            selected = "No agrupar", 
                                            inline = FALSE)
                      )
               ),
               
               # Botón de aplicar filtros
               column(3,
                      fluidRow(id = "boton_accion",
                               align="center",
                               tags$br(),
                               actionBttn(
                                 inputId = "boton_filtro",
                                 size = "lg",
                                 label = "Aplicar filtros",
                                 color = "success",
                                 style = "jelly")
                      )
               ),
               # Botón de descarga
               column(2,
                      fluidRow(
                        id = "titulo_imagen",
                        conditionalPanel(condition = "input.boton_filtro >= '1'",
                                         downloadButton("descarga_tabla", "Descarga tabla")
                        )
                      ),
                      fluidRow(
                        id = "titulo_imagen",
                        conditionalPanel(condition = "input.boton_filtro >= '1'",
                                         downloadButton("descarga_reporte", "Descarga reporte")
                        )
                      )
               )
      ),
      
      # Parto en 2: una que indica qué información quiero.
      # La otra que muestra la información que he seleccionado.
      fluidRow(
        
        # Parte lateral de selección de filtros
        # Los filtros van a depender de lo que encuentre en la nómina
        column(4,
               align="center",
               
               # Título lateral
               fluidRow(
                 id = "titulo_imagen",
                 HTML("Seleccione las variables de interés")
               ),
               
               fluidRow(
                 HTML("<br>")
               ),
               
               # Elige los niveles de interés
               fluidRow(
                 uiOutput('select_nivel')
               ),
               
               # Elige el sostenimiento de interés
               fluidRow(
                 selectInput('select_sostenimiento',
                             label = "Sostenimiento", 
                             choices = c("ESTATAL", "FEDERALIZADO", "AMBOS"), 
                             selected = "AMBOS", 
                             multiple = FALSE)
               ),
               
               # Elige la función
               fluidRow(
                 uiOutput('select_funcion')
               ),
               
               # Elige el motivo del movimiento
               fluidRow(
                 uiOutput('select_mot_mov')
               ),
               
               # Elige la categoría de la clave presupuestaria
               fluidRow(
                 uiOutput('select_cat_puesto')
               ),
               
               # Elige el status del centro de trabajo
               fluidRow(
                 uiOutput('select_cct')
               ),
               
               # Elige la Región
               fluidRow(
                 uiOutput('select_region')
               ),
               
               # Elige el sexo
               fluidRow(
                 selectInput('select_sexo',
                             label = "Sexo", 
                             choices = c("Hombre", "Mujer", "Otro"), 
                             selected = c("Hombre", "Mujer", "Otro"), 
                             multiple = TRUE)
               ),
               
               # Elige el rango de edad
               fluidRow(
                 column(2),
                 column(4,
                        numericInput("edad_min", "Edad mín:", value = 18, 
                                     min = 0, max = 120)),
                 column(4,
                        numericInput("edad_max", "Edad máx:", value = 80, 
                                     min = 0, max = 120))
               ),
        ),
        
        # Panel principal donde muestro las tablas y el mapa
        column(8,
               
               # La forma en que se agrupa, en caso de que se quiera agrupar
               fluidRow(
                 conditionalPanel(condition = "input.agrupar=='Agrupar'",
                                  fluidRow(
                                    align="center",
                                    # Selecciono cuántas varaibles uso para agrupar: de 1 a 3
                                    column(4, 
                                           numericInput('nvars_agrupa', 
                                                        "Número de vars. a agrupar", 
                                                        1, 1, 4)
                                    ),
                                    # Las variables con las que voy a agrupar (permito hasta 4)
                                    # Las primeras 2
                                    column(4,
                                           fluidRow(
                                             uiOutput("agrupa1_input")
                                           ),
                                           fluidRow(
                                             uiOutput("agrupa2_input")
                                           )
                                    ),
                                    # Las otras 2
                                    column(4,
                                           fluidRow(
                                             uiOutput("agrupa3_input")
                                           ),
                                           fluidRow(
                                             uiOutput("agrupa4_input")
                                           )
                                    )
                                  )
                 )
               ),
               
               # Lo que se va a mostrar
               fluidRow(
                 dataTableOutput("tabla_mostrar"),
                 htmlOutput("mensaje_vacio"),
                 htmlOutput("mensaje_prueba")
               )
        )
      ),
      
      
      # Fin del camino con imágenes 
      fluidRow(
        column(6,
               div(style = "text-align: right;",
                   tags$img(src = "Educacion.png",
                            style = "height: 8em; object-fit: contain;", alt = "GobJal")
               )
        ),
        column(6,
               tags$img(src = "JAL--02.png",
                        style = "height: 8em; object-fit: contain;", alt = "SEJ")
        )
      )
      
      
      
    )
  )
)
