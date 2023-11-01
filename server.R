# 1. Librerías
# ------------------------------------
library(shiny)
library(DT)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(hrbrthemes)
library(showtext)
library(tidyr)
library(flextable)


# 2. Cargar datos
# ------------------------------------
resultados <- readRDS("data/resultados.rds")
colores <- readRDS("data/colores.rds")
ambito <- readRDS("data/ambitos.rds")
circuitos <- readRDS("data/circuitos.rds")


# 3. Limpieza y manipulación de datos
# ------------------------------------
colores$agrupacion_color <- tolower(colores$agrupacion_color)
colores <- colores |> select(agrupacion_nombre, agrupacion_color) |> distinct()
colores$agrupacion_color[is.na(colores$agrupacion_color)] <- "#ffffff"

agrupaciones <- resultados |> 
  group_by(agrupacion_nombre) |> summarise(n = n(), .groups = "drop") |> filter(n > 0) |> 
  ungroup() |> select(agrupacion_nombre) |> distinct() |> arrange() |> pull()

nombres_columnas <- c("Elección", "Circuito", "Mesa", "Mesa Tipo", "Electores", "Cargo", "Agrupación", "Tipo Voto", "Votos", "Lista", "Color")

# Unir agrupaciones con sus colores
resultados <- left_join(x = resultados, y = colores , by = "agrupacion_nombre")
resultados$agrupacion_color <- str_to_title(resultados$agrupacion_nombre)
resultados <- resultados |> select(-c(agrupacion_color.x, agrupacion_color.y))

# Cálculo de algunas estadísticas
mesas_totales_paso <- resultados |> filter(eleccion_tipo == "Paso") |> group_by(mesa_id) |> summarise(n = n(), .groups = "drop") |> distinct() |> nrow()
mesas_totales_general <- resultados |> filter(eleccion_tipo == "General") |> group_by(mesa_id) |> summarise(n = n(), .groups = "drop") |> distinct() |> nrow()

# Definición de una función para calcular la moda
moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}


# 3. servidor
# ------------------------------------
server <- function(input, output, session) {
  
  # Definición de cargos
  cargos_lst <- rev(c("Presidente", "Parlamento Mercosur Nacional", "Senadores Nacionales", "Diputados Nacionales", 
                      "Parlamento Mercosur Regional", "Gobernador", "Diputados Provinciales", "Intendente"))
  
  # Tabla reactiva para disparar actualizaciones en tablas y gráficos
  tabla_filtrada <- reactive({
    cargo_elegido <- input$cargo_obj
    eleccion_tipo_elegido <- input$eleccion_tipo_obj
    circuito_elegido <- input$circuito_obj
    mesa_tipo_elegido <- input$mesa_tipo_obj
    votos_tipo_elegido <- input$votos_tipo_obj
    agrupacion_elegido <- input$agrupacion_obj
    mesa_n <- input$mesa
    
    resultados |>
      filter(
        if (cargo_elegido == "Todos") TRUE else cargo_nombre == cargo_elegido,
        if (eleccion_tipo_elegido == "Todos") TRUE else eleccion_tipo == eleccion_tipo_elegido,
        if (circuito_elegido == "Todos") TRUE else circuito_id == circuitos |> filter(descripcion == circuito_elegido) |> pull(circuito),
        if (mesa_tipo_elegido == "Todos") TRUE else mesa_tipo == mesa_tipo_elegido,
        if (votos_tipo_elegido == "Todos") TRUE else votos_tipo == votos_tipo_elegido,
        if (agrupacion_elegido == "Todos") TRUE else agrupacion_nombre == agrupacion_elegido,
        if (mesa_n == 0) TRUE else mesa_id == mesa_n
      ) |> 
      arrange(circuito_id,
              mesa_id,
              cargo_nombre,
              agrupacion_nombre,
              votos_tipo,
              votos_cantidad) |> 
      mutate(eleccion_tipo = str_to_title(eleccion_tipo),
             cargo_nombre = str_to_title(cargo_nombre),
             agrupacion_nombre = str_to_title(agrupacion_nombre),
             votos_tipo = str_to_title(votos_tipo),
             lista_numero = ifelse(lista_numero > 0, lista_numero, NA)
      )
  })
  
  # Nueva tabla reactiva ya que no comparte algunas variables de actualización con la anterior por reflejar todos los cargos
  tabla_filtrada_agrupaciones <- reactive({
    eleccion_tipo_elegido <- input$eleccion_tipo_obj
    circuito_elegido <- input$circuito_obj
    mesa_tipo_elegido <- input$mesa_tipo_obj
    votos_tipo_elegido <- input$votos_tipo_obj
    agrupacion_elegido <- input$agrupacion_obj
    mesa_n <- input$mesa
    
    resultados |>
      filter(
        if (eleccion_tipo_elegido == "Todos") TRUE else eleccion_tipo == eleccion_tipo_elegido,
        if (circuito_elegido == "Todos") TRUE else circuito_id == circuitos |> filter(descripcion == circuito_elegido) |> pull(circuito),
        if (mesa_tipo_elegido == "Todos") TRUE else mesa_tipo == mesa_tipo_elegido,
        if (votos_tipo_elegido == "Todos") TRUE else votos_tipo == votos_tipo_elegido,
        if (agrupacion_elegido == "Todos") TRUE else agrupacion_nombre == agrupacion_elegido,
        if (mesa_n == 0) TRUE else mesa_id == mesa_n
      ) |> 
      arrange(circuito_id,
              mesa_id,
              cargo_nombre,
              agrupacion_nombre,
              votos_tipo,
              votos_cantidad) |> 
      mutate(eleccion_tipo = str_to_title(eleccion_tipo),
             cargo_nombre = str_to_title(cargo_nombre),
             agrupacion_nombre = str_to_title(agrupacion_nombre),
             votos_tipo = str_to_title(votos_tipo),
             lista_numero = ifelse(lista_numero > 0, lista_numero, NA)
      )
  })
  
  # Función para generar gráfico de histograma
  generar_histograma <- function(tbl_actualizada) {
    
    tabla <- tbl_actualizada %>%
      filter(eleccion_tipo %in% c("General", "Paso")) %>%
      select(mesa_id, eleccion_tipo, mesa_electores, votos_cantidad) %>%
      group_by(mesa_id, eleccion_tipo, mesa_electores) %>%
      summarise(n = sum(votos_cantidad), .groups = "drop") %>%
      ungroup() %>%
      filter(n > 0) %>%
      mutate(prc_votantes = ceiling(n / mesa_electores * 100))
    
    n_mesas <- tabla |> select(mesa_id) |> distinct() |> nrow()
    
    # Actualiza el slider para elegir bins en base a la muestra
    updateSliderInput(session, "bins_elegidos", max = n_mesas*2, step = 1)
    
    avg_general <- tabla |> filter(eleccion_tipo == "General") |> summarise(n = mean(prc_votantes)) |> pull(n)
    avg_paso <- tabla |> filter(eleccion_tipo == "Paso") |> summarise(n = mean(prc_votantes)) |> pull(n)

    p <- tabla %>%
      ggplot(aes(x = prc_votantes, fill = eleccion_tipo)) +
      geom_histogram(
        color = "#e9ecef",
        alpha = 0.7,
        position = "identity",
        bins = floor(input$bins_elegidos)
      ) +
      scale_x_continuous(limits = c(40, 100)) +
      scale_fill_manual(values = c("firebrick", "steelblue")) +
      theme_minimal() +
      labs(title = paste0("Porcentaje de votantes/mesa según tipo de elección - Cantidad de mesas: ", n_mesas), 
           fill = "",
           x = "% Votantes",
           y = "Cantidad de mesas") +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(vjust = 10),
            legend.position = c(0.01, 0.8),
            legend.justification = c(0, 1),
            legend.margin = margin(b = -10, l = 30),
            title = element_text(size = 10)) +
      geom_vline(aes(xintercept = avg_general), color = "firebrick") +
      geom_vline(aes(xintercept = avg_paso), color = "steelblue") +
      geom_text(aes(x = avg_general, y = 1, label = paste("General:", round(avg_general, 1))), vjust = -0.5, hjust = 0) +
      geom_text(aes(x = avg_paso, y = 1, label = paste("Paso:", round(avg_paso, 1))), vjust = -0.5, hjust = 0)
    
    return(p)
  }
  
  # Función para generar tabla de agrupaciones mayoritarias
  tbl_agrup <- function(df, agrupacion, eleccion) {
    df |>
      filter(agrupacion_nombre == str_to_title(agrupacion),
             eleccion_tipo == eleccion) |>
      group_by(cargo_nombre) |>
      summarise(votos = sum(votos_cantidad)) |>
      mutate(
        cargo_nombre = factor(cargo_nombre, levels = cargos_lst),
        prc_votos = round(votos / max(votos) * 100, 0)
      )
  }
  
  # Función para generar gráfico a partir de la tabla de agrupaciones mayoritarias
  fig_agrup <- function(df, agrupacion, posicion, eleccion, color, abreviatura) {
    
    tabla <- tbl_agrup(df, agrupacion, eleccion)
    
    p <- ggplot(data = tabla, aes(x = cargo_nombre, y = votos)) +
      geom_bar(fill = color, aes(alpha = 0.5),
               stat = "identity",
               width = 0.9,
               group = color) +
      geom_text(aes(label = paste0(scales::label_number(big.mark = ".", decimal.mark = ",")(votos)," (", prc_votos, "%)"), group = color), 
                hjust = 1.2, size = 3,
                position = position_dodge(width = 0.9),
                inherit.aes = TRUE
      ) +
      labs(
        title = paste0("    ", abreviatura),
        caption = ifelse(posicion == 4, "Fuente: Elaboración propia en base a datos de DINE", ""),
        x = "",
        y = ""
      ) +
      coord_flip() +
      theme_ipsum() +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 10)
      )
    
    if (posicion != 1) {
      p <- p + theme(axis.text.y = element_blank())
    }
    
    return(p)
  }
  
  
  # Función para generar tabla con inconsistencias en el número de electores por cargo
  generar_tabla_inconsistencias <- function(tipo_eleccion) {
    
    mesas_incoherencias <- resultados |> filter(eleccion_tipo == tipo_eleccion) |> 
      group_by(mesa_id, cargo_nombre) |> 
      summarise(n = sum(votos_cantidad), .groups = "drop") |> 
      select(mesa_id, n) |> 
      distinct() |> 
      group_by(mesa_id) %>% 
      filter(n() > 1) %>% 
      ungroup() |> 
      select(mesa_id) |>  
      unique() |> 
      pull()
    
    tabla <- resultados %>%
      filter(eleccion_tipo == tipo_eleccion, mesa_id %in% mesas_incoherencias) %>%
      group_by(mesa_id, cargo_nombre) %>%
      summarise(n = sum(votos_cantidad), .groups = "drop") %>%
      pivot_wider(names_from = cargo_nombre, values_from = n, values_fill = 0) %>%
      rowwise() %>%
      mutate(max_valor = moda(c_across(-mesa_id))) |> 
      mutate(across(!c(mesa_id, max_valor), ~ . - max_valor, .names = "dif_{.col}")) |> 
      select(mesa_id, dif_Presidente, `dif_Parlamento Mercosur Nacional`, `dif_Senadores Nacionales`, dif_Gobernador, `dif_Parlamento Mercosur Regional`,
             `dif_Diputados Provinciales`, dif_Intendente)
    
    tabla <- data.frame(lapply(tabla, function(x) ifelse(x == 0, NA, x)))
    
    return(tabla)
  }
  
  # Función para visualizar tabla de inconsistencias en electores por cargo   
  visualizar_tabla_inconsistencias <- function(tabla) { 
    tabla |> 
      flextable() |> 
      set_header_labels(
        "mesa_id" = "Mesa",
        "dif_Presidente" = "Pres.", 
        "dif_Parlamento.Mercosur.Nacional" = "Parlasur.Nac",
        "dif_Senadores.Nacionales" = "Sen.Nac",
        "dif_Diputados.Nacionales" = "Dip.Nac",
        "dif_Gobernador" = "Gob.", 
        "dif_Parlamento.Mercosur.Regional" = "Parlasur.Reg", 
        "dif_Diputados.Provinciales" = "Dip.Prov", 
        "dif_Intendente" = "Int."
      ) |> 
      colformat_num(na_str = NA) |> 
      theme_zebra(odd_body = "white", even_body = "lightsteelblue") |> 
      fontsize(size = 8, part = "all") |> 
      align(align = "center", part = "all") |> 
      autofit()  |> 
      htmltools_value()    
  }
  
  # Generar tablas de inconsistencias por tipo de elección
  mesas_inconsistencias_paso <- generar_tabla_inconsistencias("Paso")
  mesas_inconsistencias_general <- generar_tabla_inconsistencias("General")
  
  # Renderizar la tabla original
  output$tabla_original <- renderDataTable({
    
    tabla <- resultados
    colnames(tabla) <- nombres_columnas
    
    datatable(
      tabla,
      selection = "none",
      filter = list(position = 'top', clear = FALSE),
      options = list(
        server = FALSE,
        pageLength = 20,
        autoWidth = TRUE,
        lengthChange = FALSE,
        columnDefs = list(
          list(width = "50px", targets = c("Elección", "Circuito", "Mesa", "Mesa Tipo", "Electores","Tipo Voto", "Votos", "Lista")),
          list(width = "30px", targets = c("Color")),
          list(width = "250px", targets = c("Cargo")),
          list(width = "500px", targets = c("Agrupación"))
        )
      ),
      rownames = FALSE
    ) |> 
      formatStyle(columns = 1:10, fontSize = "11px") |> 
      formatStyle(columns = 11, fontSize = "1px") |> 
      formatStyle("Color", backgroundColor = styleEqual(str_to_title(colores$agrupacion_nombre), colores$agrupacion_color)) |> 
      formatStyle("Color", color = styleEqual(str_to_title(colores$agrupacion_nombre), colores$agrupacion_color))
  })
  
  # Renderizar la tabla de datos filtrados
  output$filtro_tbl <- renderDataTable({
    
    tabla <- tabla_filtrada() |> 
      group_by(cargo_nombre, eleccion_tipo, circuito_id, mesa_tipo, votos_tipo, mesa_id, agrupacion_nombre, lista_numero) |> 
      summarise(votos_total = sum(votos_cantidad), .groups = "drop")
  })
  
  # Renderizar el gráfico con porcentajes de electores
  output$plot_histograma_votantes <- renderPlot({
    tbl_actualizada <- tabla_filtrada()
    generar_histograma(tbl_actualizada)
  })
  
  # Observar cambios en la tabla filtrada y actualizar el gráfico en consecuencia
  observeEvent(input$filtro_tbl, {
    output$plot_histograma_votantes <- renderPlot({
      tbl_actualizada <- tabla_filtrada()
      generar_histograma(tbl_actualizada)
      generar_histograma()
    })
  })
  
  # Renderizar la leyenda con inconsistencias en el número de sufragios por cargo para las elecciones PASO
  output$errores_paso <- renderText({
    errores <- tabla_filtrada() |> 
      filter(eleccion_tipo == "Paso") |> 
      group_by(mesa_id, cargo_nombre) |> 
      summarise(n = sum(votos_cantidad), .groups = "drop") |>
      select(mesa_id, n) |> distinct() |> filter(n() > 1) %>%
      ungroup() |>
      select(mesa_id) |>
      unique() |> 
      nrow()
    
    paste0("Paso: ", errores)
  })
  
  # Renderizar lel gráfico de agrupaciones mayoritarias
  output$plot_agrupaciones <- renderPlot({
    
    tbl_actualizada <- tabla_filtrada_agrupaciones()
    
    eleccion_obj <- ifelse(input$eleccion_tipo_obj == "Paso", "Paso", "General")
    
    fig1 <- fig_agrup(df = tbl_actualizada, agrupacion = "Juntos por el Cambio", posicion = 1, eleccion = eleccion_obj, color = "#FEDD00", abreviatura = "JxC")
    fig2 <- fig_agrup(df = tbl_actualizada, agrupacion = "Union por la Patria", posicion = 2, eleccion = eleccion_obj, color = "#009CDE", abreviatura = "UxP")
    fig3 <- fig_agrup(df = tbl_actualizada, agrupacion = "La Libertad Avanza", posicion = 3, eleccion = eleccion_obj, color = "#753BBD", abreviatura = "LLA")
    
    grid.arrange(fig1, fig2, fig3, ncol = 3)
  })
  
  # Renderizar la tabla de inconsistencia para la elección PASO
  output$inconsistencias_paso <- renderUI({
    visualizar_tabla_inconsistencias(mesas_inconsistencias_paso)
  })
  
  # Renderizar la tabla de inconsistencia para la elección General
  output$inconsistencias_general <- renderUI({
    visualizar_tabla_inconsistencias(mesas_inconsistencias_general)
  })
  
  # Renderizar la leyenda de inconsistencia para elección PASO
  output$prc_inconsistencias_paso <- renderText({
    paste0("Mesas con al menos una inconsistencia ", 
           nrow(mesas_inconsistencias_paso), " (",ceiling(nrow(mesas_inconsistencias_paso)/mesas_totales_paso*100), "%)")
  })
  
  # Renderizar la leyenda de inconsistencia para elección General
  output$prc_inconsistencias_general <- renderText({
    paste0("Mesas con al menos una inconsistencia ", 
           nrow(mesas_inconsistencias_general), " (",ceiling(nrow(mesas_inconsistencias_general)/mesas_totales_general*100), "%)")
  })
}