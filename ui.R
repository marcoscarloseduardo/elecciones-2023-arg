# UI

# 1. Librerías
# ------------
library(shiny)
library(dplyr)
library(flextable)


# 2. Cargar datos
# ---------------
resultados <- readRDS("data/resultados.rds")
colores <- readRDS("data/colores.rds")
circuitos <- readRDS("data/circuitos.rds")

cargos_lst <-
  resultados |> select(cargo_nombre) |> distinct() |> arrange() |> pull()

eleccion_tipo_lst <-
  resultados |> select(eleccion_tipo) |> distinct() |> arrange() |> pull()

mesa_tipo_lst <-
  resultados |> select(mesa_tipo) |> distinct() |> arrange() |> pull()

votos_tipo_lst <-
  resultados |> select(votos_tipo) |> distinct() |> arrange() |> pull()

agrupacion_lst <-
  resultados |> filter(votos_tipo == "Positivo") |> select(agrupacion_nombre) |> distinct() |> arrange(agrupacion_nombre) |> pull()

var_graph <- c("% votantes/electores", "Cantidad de Votos")
max_bins <- resultados |> select(mesa_id) |> distinct() |> nrow()

# 3. UI
# ------------------------------------

ui <- fluidPage(
  h4("Análisis Exploratorio de Elecciones 2023 en el Distrito de Trenque Lauquen, Pcia. de Buenos Aires, Argentina", style = "color:steelblue"),
  
  tags$style(type = 'text/css', "select {font-size: 10px !important} "),
  
  navbarPage(
    h5(HTML("<b>Datos Provisorios</b>"), style = "color:steelblue"),
    tabPanel(h5("Filtros"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId = "cargo_obj",
                   label = "Cargo:",
                   choices = cargos_lst,
                   selected = "Presidente",
                   multiple = FALSE,
                   width = "240px"
                 ),
                 
                 selectInput(
                   inputId = "eleccion_tipo_obj",
                   label = "Tipo Elección:",
                   choices = c("Todos", eleccion_tipo_lst),
                   selected = "Todos",
                   multiple = FALSE,
                   width = "240px"
                 ),
                 
                 selectInput(
                   inputId = "circuito_obj",
                   label = "Circuito:",
                   choices = c("Todos", circuitos$descripcion),
                   selected = "Todos",
                   multiple = FALSE,
                   width = "240px"
                 ),
                 
                 selectInput(
                   inputId = "mesa_tipo_obj",
                   label = "Tipo de Mesa:",
                   choices = c("Todos", mesa_tipo_lst),
                   selected = c("Todos"),
                   multiple = FALSE,
                   width = "240px"
                 ),
                 
                 selectInput(
                   inputId = "votos_tipo_obj",
                   label = "Tipo de Votos:",
                   choices = c("Todos", votos_tipo_lst),
                   selected = c("Todos"),
                   multiple = FALSE,
                   width = "240px"
                 ),
                 
                 selectInput(
                   inputId = "agrupacion_obj",
                   label = "Agrupación:",
                   choices = c("Todos", agrupacion_lst),
                   selected = c("Todos"),
                   multiple = FALSE,
                   width = "300px"
                 ),
                 
                 textInput(
                   inputId = "mesa",
                   label = "Mesa (0 = Todas)",
                   value = "0",
                   width = "200px"
                 )
               ),
               
               mainPanel(width = 9,
                         tabsetPanel(
                           tabPanel(
                             title = h5("Resultados"),
                             h5("Comparativo resultados", style = "color:steelblue"),
                             DT::dataTableOutput('filtro_tbl')
                           ),
                           
                           tabPanel(h5("Gráficos"),
                                    fluidRow(
                                      column(3,
                                             wellPanel(
                                               sliderInput(
                                                 inputId = "bins_elegidos",
                                                 label = "Número de bins para el histograma",
                                                 min = 1,
                                                 max = max_bins,
                                                 value = ceiling(max_bins / 2),
                                                 step = ceiling(max_bins / 10)
                                               ),
                                               
                                               tags$small(HTML(
                                                 paste0(
                                                   "<u>Nota</u>: Los extranjeros, mayores de edad, que sepan leer y escribir en idioma nacional, con dos años de residencia
                                      inmediata en el territorio de la Provincia de Buenos Aires, pueden elegir Gobernador, Legisladores Provinciales,
                                      Intendentes Municipales, Concejales, Consejeros Escolares y Diputados Constituyentes."
                                                 )
                                               ))
                                             )),
                                      column(9,
                                             plotOutput("plot_histograma_votantes", height = "300px"))
                                    ),
                                    fluidRow(
                                      column(12,
                                             h5("Votos en las 3 fuerzas mayoritarias", style = "color:steelblue"),
                                             h5(
                                               HTML(
                                                 "<u>Nota</u>: Si el Tipo de Elección seleccionado es 'Todos', solo se visualizará la instancia 'General'"
                                                 )
                                               ),
                                             plotOutput("plot_agrupaciones", height = "350px")
                                             )))
                           ))
               )),
    
    tabPanel(h5("Inconsistencias"),
             fluidRow(
               h4("Mesas con diferencias de electores entre cargos", style = "color:steelblue"),
               h5(
                 "Se indica la diferencia por respecto de la categoría con mayor cantidad de votos registrados."
               ),
               h5(HTML(
                 "<u>Nota</u>: En la mesa 9001, que corresponde a votantes extranjeros y no todas las categorías están cubiertas,
                              los valores deben interpretarse de forma relativa, no absoluta."
               )),
               column(6,
                      wellPanel(
                        HTML("<h5><b>Elección Paso</b></h5>"),
                        textOutput("prc_inconsistencias_paso"),
                        uiOutput("inconsistencias_paso")
                      )),
               column(6,
                      wellPanel(
                        HTML("<h5><b>Elección General</b></h5>"),
                        textOutput("prc_inconsistencias_general"),
                        uiOutput("inconsistencias_general")
                      ))
             )),
    tabPanel(
      h5("Tabla"),
      h4("Tabla unificada Paso/General", style = "color:steelblue"),
      
      tags$head(tags$style(
        HTML(
          '.dataTables_wrapper .dataTables_length select,
                                .dataTables_wrapper .dataTables_filter input,
                                .dataTables_wrapper .dataTables_info,
                                .dataTables_wrapper .dataTables_paginate {
                                font-size: 10px; /* Tamaño del fuente deseado */
                                }
                                '
        )
      )),
      DT::dataTableOutput('tabla_original')
    )
  )
)
