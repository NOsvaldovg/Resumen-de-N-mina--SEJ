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


options(shiny.maxRequestSize = 256*1024^2)
# setwd("C:/Users/nestor.valdes/Documents/SEJ/Transparencia/Visualiza_nomina")
# nomina <- readRDS("C:/Users/nestor.valdes/Documents/SEJ/Transparencia/nomina.rds")
# nomina <- read.xlsx("C:/Users/nestor.valdes/Documents/SEJ/Transparencia/Activos Nomina 202511.xlsx", sep.names = " ")

# Categoría de las plazas. Para saber la función.
cat_plazas <- readRDS("datos/categorías plazas.rds")

# Necesito porder clasificar el nivel de los cct
cct <- readRDS("datos/cct.rds")
nomina_vacia <- readRDS("datos/molde_vacio.rds") %>% 
  rename(nivel_1 = nivel)

# Inicia el server
function(input, output, session) {
  
  # Para ver que esté trabajando:
  contador_clicks <- reactiveVal(0)
  
  observeEvent(input$boton_filtro, {
    contador_clicks(contador_clicks() + 1)
    # Muestra en la consola
    cat("Número de clics al botón 'Aplicar filtros':", contador_clicks(), "\n")
  })
  
  # Aquí se lee el archivo de la nómina
  df_nomina <- reactive({
    
    cat("Inicia la revisión de que haya base mamá", "\n")
    cat("Memoria usada antes de cargar el archivo: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
    
    if (is.null(input$nomina)) {
      df <- nomina_vacia
      cat("No se cargó una base mamá", "\n")
      
    } else {
      filepath <- input$nomina$datapath
      filename <- input$nomina$name
      ext <- tools::file_ext(filename)
      
      cat("Sí se cargó una base mamá", "\n")
      
      if (ext == "csv") {
        cat("La base mamá es un csv", "\n")
        df <- read.csv(filepath, encoding = "latin1", check.names = FALSE)
        
      } else if (ext %in% c("xlsx", "xls")) {
        cat("La base mamá es un excel", "\n")
        df <- readxl::read_excel(filepath)
        
      } else if (ext == "rds") {
        cat("La base mamá es un RDS", "\n")
        df <- readRDS(filepath)
        
      } else {
        showNotification("Formato no soportado. Use un archivo .csv, .xlsx, .xls o .rds.", type = "error")
        return(nomina_vacia)
      }
      
      cat("Archivo leído", "\n")
      cat("Memoria después de cargar: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
      
      # Validación de columnas SOLO si se cargó un archivo
      columnas_requeridas <- c('clave de plaza', 'cod_pago', 'unidad', 'subunidad', 
                               'cat_puesto', 'horas', 'cons_plaza', 'qna_fin', 
                               'qna_ini', 'estatus_plaza', 'mot_mov', 'num_doc', 
                               'ent_fed', 'ct_clasif', 'ct_id', 'ct_secuencial', 
                               'ct_digito_ver', 'modelo', 'rfc', 'curp', 'nombre', 
                               'sostenimiento')
      
      columnas_faltantes <- setdiff(columnas_requeridas, names(df))
      
      # Mostrar un mensaje de qué columnas hacen falta para que no arroje un error
      if (length(columnas_faltantes) > 0) {
        showNotification(
          paste0(
            "El archivo cargado NO contiene las siguientes columnas requeridas: ",
            paste(columnas_faltantes, collapse = ", ")
          ),
          type = "error", duration = NULL
        )
        cat("Columnas faltantes:", paste(columnas_faltantes, collapse = ", "), "\n")
        return(nomina_vacia)
      }
    }
    
    # Inicia el procesamiento de la base que se cargó
    if (nrow(df) > 0) {
      cat("Inicia el filtro de la base mamá", "\n")
      
      df <- df %>%
        mutate(cct = paste0(ct_clasif, ct_id, ct_secuencial,
                            str_pad(ct_digito_ver, side = "left", 4, 0), 
                            modelo),
               cat_puesto = trimws(cat_puesto),
               id_nivel = paste0(ct_id, ct_secuencial))
      
      gc()
      cat("Memoria antes del primer join: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
      
      df <- left_join(df, cct, by = c("cct")) %>% 
        mutate(cat_sost = paste0(cat_puesto, "_", sostenimiento))
      
      cat("Memoria antes del segundo join: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
      gc()
      
      df <- left_join(df, cat_plazas, "cat_sost") %>% 
        rename(funcion = FUNCION_SERV_DOCENTE_BASICA) %>% 
        mutate(nivel_1 = case_when(
          id_nivel %in% c("ADG", "FAC", "FAM", "FEI") ~ "UNIDAD ADMINISTRATIVA",
          id_nivel %in% c("DBA", "EBA") ~ "EDUCACION PARA ADULTOS",
          id_nivel %in% c("DBT", "EBT") ~ "CAPACITACION PARA EL TRABAJO",
          id_nivel == "DCC" ~ "PREESCOLAR INDIGENA",
          id_nivel %in% c("DDI", "EDI") ~ "EDUCACION INICIAL",
          id_nivel %in% c("DEF", "FFS") ~ "EDUCACION FISICA",
          id_nivel %in% c("DES", "EES", "EST", "FIS", "FZT") ~ "SECUNDARIA GENERAL",
          id_nivel == "DIN" ~ "EDUCACION INICIAL INDIGENA",
          id_nivel %in% c("DIX", "DPR", "EPR", "FIZ", "FJS") ~ "PRIMARIA",
          id_nivel %in% c("DJN", "EJN", "FJZ", "FTM", "FZP", "ICI") ~ "PREESCOLAR",
          id_nivel %in% c("DML", "EML", "FRB", "FSE", "FUA") ~ "EDUCACION ESPECIAL",
          id_nivel %in% c("DNL", "ENE", "ENL") ~ "NORMALES",
          id_nivel %in% c("DBP", "FJI", "FZI") ~ "PRIMARIA INDIGENA",
          id_nivel %in% c("DST", "FBB") ~ "SECUNDARIA TECNICA",
          id_nivel %in% c("DTV", "FTV") ~ "TELESECUNDARIA",
          id_nivel %in% c("DUP") ~ "UPN",
          id_nivel %in% c("EFJ") ~ "FEMENINO JUBILADO",
          id_nivel %in% c("FMB", "FMP") ~ "FORMACION CONTINUA",
          id_nivel %in% c("HMC") ~ "MISION CULTURAL",
          TRUE ~ N_NIVEL
        )) %>% 
        mutate(nivel_2 = case_when(
          nivel_1 %in% c("CAPACITACION PARA EL TRABAJO", "MISION CULTURAL") ~ "CAPACITACION PARA EL TRABAJO",
          nivel_1 %in% c("EDUCACION ESPECIAL", "EDUCACION FISICA", "EDUCACION INICIAL", "EDUCACION INICIAL INDIGENA",
                         "EDUCACION PARA ADULTOS", "PREESCOLAR", "PREESCOLAR INDIGENA", "PRIMARIA", "PRIMARIA INDIGENA",
                         "SECUNDARIA GENERAL", "SECUNDARIA TECNICA", "TELESECUNDARIA") ~ "BASICA",
          nivel_1 == "FEMENINO JUBILADO" ~ "FEMENINO JUBILADO",
          nivel_1 %in% c("FORMACION CONTINUA", "NORMALES", "UPN") ~ "SUPERIOR",
          nivel_1 == "UNIDAD ADMINISTRATIVA" ~ "BASICA",
          substr(cat_puesto, 1, 2) == "CF" & nivel_1 == "UNIDAD ADMINISTRATIVA" ~ "CONFIANZA",
          TRUE ~ "SEPA LA BOLA"
        ),
        sexo = case_when(
          substr(curp, 11, 11) == "M" ~ "Mujer",
          substr(curp, 11, 11) == "H" ~ "Hombre",
          TRUE ~ "Otro"
        ),
        fecha_nac = str_sub(curp, 5, 10),
        fecha_nac = ymd(ifelse(
          as.numeric(str_sub(curp, 5, 6)) > 30,
          paste0("19", str_sub(curp, 5, 6), str_sub(curp, 7, 10)),
          paste0("20", str_sub(curp, 5, 6), str_sub(curp, 7, 10))
        )),
        edad = floor(time_length(difftime(Sys.Date(), fecha_nac), unit = "years"))
        ) %>%
        select(any_of(c(names(nomina_vacia), "N_STATUS", "N_SERREG")))
      
      cat("Termina el filtro de la base mamá", "\n")
    }
    
    gc()
    cat("Memoria tras procesar y limpiar: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
    df
  })
  
  
  # Input - Filtro de Nivel Educativo
  output$select_nivel <- renderUI({
    if (nrow(df_nomina()) > 0 && "nivel_1" %in% names(df_nomina())) {
      pickerInput(
        inputId = "s_nivel",
        label = "Nivel",
        choices = sort(unique(df_nomina()$nivel_1)),
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `deselect-all-text` = "Quitar selección",
          `select-all-text` = "Seleccionar todo",
          `none-selected-text` = "Ninguno"
        ),
        multiple = TRUE
      )
    }
  })
  
  # Input - Filtro de Función
  output$select_funcion <- renderUI({
    if (nrow(df_nomina()) > 0 && "funcion" %in% names(df_nomina())) {
      pickerInput(
        inputId = "s_funcion",
        label = "Función",
        choices = sort(unique(df_nomina()$funcion)),
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `deselect-all-text` = "Quitar selección",
          `select-all-text` = "Seleccionar todo",
          `none-selected-text` = "Ninguno"
        ),
        multiple = TRUE
      )
    }
  })
  
  # Input - Motivo del movimiento
  output$select_mot_mov <- renderUI({
    if (nrow(df_nomina()) > 0 && "mot_mov" %in% names(df_nomina())) {
      pickerInput(
        inputId = "s_mot_mov",
        label = "Motivo del movimiento",
        choices = sort(unique(df_nomina()$mot_mov)),
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `deselect-all-text` = "Quitar selección",
          `select-all-text` = "Seleccionar todo",
          `none-selected-text` = "Ninguno"
        ),
        multiple = TRUE
      )
    }
  })
  
  # Input - Filtro de Categoría
  output$select_cat_puesto <- renderUI({
    if (nrow(df_nomina()) > 0 && "cat_puesto" %in% names(df_nomina())) {
      pickerInput(
        inputId = "s_cat_puesto",
        label = "Categoría",
        choices = sort(unique(df_nomina()$cat_puesto)),
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `deselect-all-text` = "Quitar selección",
          `select-all-text` = "Seleccionar todo",
          `none-selected-text` = "Ninguno"
        ),
        multiple = TRUE
      )
    }
  })
  
  # Input - Filtro de Status de Centro de Trabajo
  output$select_cct <- renderUI({
    if (nrow(df_nomina()) > 0 && "N_STATUS" %in% names(df_nomina())) {
      pickerInput(
        inputId = "s_cct",
        label = "Status de cct",
        choices = sort(unique(df_nomina()$N_STATUS)),
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `deselect-all-text` = "Quitar selección",
          `select-all-text` = "Seleccionar todo",
          `none-selected-text` = "Ninguno"
        ),
        multiple = TRUE
      )
    }
  })
  
  # Input - Filtro de Región
  output$select_region <- renderUI({
    if (nrow(df_nomina()) > 0 && "N_SERREG" %in% names(df_nomina())) {
      pickerInput(
        inputId = "s_region",
        label = "Región",
        choices = sort(unique(df_nomina()$N_SERREG)),
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `deselect-all-text` = "Quitar selección",
          `select-all-text` = "Seleccionar todo",
          `none-selected-text` = "Ninguno"
        ),
        multiple = TRUE
      )
    }
  })
  
  # Corrijo la edad mínima o máxima para evitar errores
  observe({
    req(input$edad_min, input$edad_max)
    
    if (input$edad_max < input$edad_min) {
      updateNumericInput(session, "edad_max", value = input$edad_min)
    }
  })
  
  # Renderiza el input para si se pide agrupar: input.agrupar=='Agrupar' y nvars_agrupa>=1
  output$agrupa1_input <- renderUI({
    if (input$agrupar=='Agrupar') {
      if (input$nvars_agrupa >= 1) {
        selectInput("agrupa_sel1",
                    label = "Columna para agrupar (1)",
                    choices = names(df_nomina()),
                    multiple = FALSE)
      }
    }
  })
  
  output$agrupa2_input <- renderUI({
    if (input$agrupar=='Agrupar') {
      if (input$nvars_agrupa >= 2) {
        selectInput("agrupa_sel2",
                    label = "Columna para agrupar (2)",
                    choices = names(df_nomina()),
                    multiple = FALSE)
      }
    }
  })
  
  output$agrupa3_input <- renderUI({
    if (input$agrupar=='Agrupar') {
      if (input$nvars_agrupa >= 3) {
        selectInput("agrupa_sel3",
                    label = "Columna para agrupar (3)",
                    choices = names(df_nomina()),
                    multiple = FALSE)
      }
    }
  })
  
  output$agrupa4_input <- renderUI({
    if (input$agrupar=='Agrupar') {
      if (input$nvars_agrupa >= 4) {
        selectInput("agrupa_sel4",
                    label = "Columna para agrupar (4)",
                    choices = names(df_nomina()),
                    multiple = FALSE)
      }
    }
  })
  
  # Proceso en el que se aplican los filtros y la agrupación
  df_filtrada <- eventReactive(input$boton_filtro, {
    df <- df_nomina()
    
    gc()
    
    # Filtro nivel educativo
    if (!is.null(input$s_nivel) && length(input$s_nivel) > 0 && "nivel_1" %in% names(df)) {
      df <- df %>% filter(nivel_1 %in% input$s_nivel)
    }
    
    gc()
    
    # Filtro sostenimiento
    if (!is.null(input$select_sostenimiento) && input$select_sostenimiento != "AMBOS" &&
        "sostenimiento" %in% names(df)) {
      df <- df %>% filter(sostenimiento == input$select_sostenimiento)
    }
    
    gc()
    
    # Filtro función
    if (!is.null(input$s_funcion) && length(input$s_funcion) > 0 && "funcion" %in% names(df)) {
      df <- df %>% filter(funcion %in% input$s_funcion)
    }
    
    gc()
    
    # Filtro categoría
    if (!is.null(input$s_cat_puesto) && length(input$s_cat_puesto) > 0 && "cat_puesto" %in% names(df)) {
      df <- df %>% filter(cat_puesto %in% input$s_cat_puesto)
    }
    
    gc()
    
    # Filtro sexo
    if (!is.null(input$select_sexo) && "sexo" %in% names(df)) {
      df <- df %>% filter(sexo %in% input$select_sexo)
    }
    
    gc()
    
    # Filtro de edad
    df <- df %>%
      filter(edad >= input$edad_min,
             edad <= input$edad_max)
    
    gc()
    
    # Fin de los filtros
    
    # Esta es la tabla que voy a mostrar y que se podrá descargar
    # Puede ser toda la información de la nómina filtrada
    # O un pequeño cuadro de resumen
    # Si no se agrupa, la base filtrada es suficiente
    if (input$agrupar=="No agrupar") {
      df
    } else if (input$agrupar=="Agrupar" & input$nvars_agrupa == 1) {
      grupo <- input$agrupa_sel1
      df <- df %>%
        group_by(across(all_of(grupo))) %>%
        summarise(Total = n(), .groups = "drop")
      
      gc()
      
      df
      
    } else if (input$agrupar=="Agrupar" & input$nvars_agrupa == 2) {
      grupo <- unique(c(input$agrupa_sel1, input$agrupa_sel2))
      df <- df %>%
        group_by(across(all_of(grupo))) %>%
        summarise(Total = n(), .groups = "drop")
      
      gc()
      
      df
      
    } else if (input$agrupar=="Agrupar" & input$nvars_agrupa == 3) {
      grupo <- unique(c(input$agrupa_sel1, input$agrupa_sel2,
                        input$agrupa_sel3))
      df <- df %>%
        group_by(across(all_of(grupo))) %>%
        summarise(Total = n(), .groups = "drop")
      
      gc()
      
      df
      
    } else if (input$agrupar=="Agrupar" & input$nvars_agrupa == 4) {
      grupo <- unique(c(input$agrupa_sel1, input$agrupa_sel2,
                        input$agrupa_sel3, input$agrupa_sel4))
      df <- df %>%
        group_by(across(all_of(grupo))) %>%
        summarise(Total = n(), .groups = "drop")
      gc()
      # Fin
      df
    }
    
  })
  
  # Aquí se muestra la tabla de la información seleccionada
  output$tabla_mostrar <- renderDataTable({
    req(df_filtrada())
    df <- df_filtrada()
    cat("Memoria antes del DT: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
    
    if (nrow(df) > 0) {
      tabla <- datatable(df,
                         rownames = FALSE,
                         filter = 'top',
                         options = list(
                           pageLength = 100,
                           deferRender = TRUE,
                           scrollY = 350,
                           scroller = TRUE,
                           searching = FALSE,
                           language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                         ))
      gc()
      cat("Memoria después del DT: ", round(pryr::mem_used() / 1024^2, 2), "MB\n")
      return(tabla)
    }
  })
  
  
  # Botón de descarga - Tabla de la información seleccionada
  output$descarga_tabla <- downloadHandler(
    filename = function() {
      fecha <- Sys.Date()
      if (!is.null(input$tiempo_local)) {
        fecha <- tryCatch({
          as.Date(dmy_hms(input$tiempo_local))
        }, error = function(e) Sys.Date())
      }
      paste0("consulta_", fecha, ".csv")
    },
    content = function(file) {
      req(df_filtrada())
      write.csv(df_filtrada(), file, row.names = FALSE, na = "")
    }
  )
  
  # Mensaje de vacío
  output$mensaje_vacio <- renderUI({
    req(df_filtrada())
    if (nrow(df_filtrada()) == 0) {
      HTML("<b>No hay información que cumpla con las características seleccionadas</b>")
    }
  })
  
  # # Mensaje de prueba
  # output$mensaje_prueba <- renderUI({
  #   req(df_nomina())
  #   df <- df_nomina()
  #   HTML(paste0("La columna nivel_1 <b>", ifelse("nivel_1" %in% names(df), "sí", "no"),
  #               "</b> existe en la base de datos. <br>",
  #               "Cuento con los siguientes valores únicos:",
  #               paste0(sort(unique(df$nivel_1)), collapse = ", ")))
  # })
  
  
  
}





