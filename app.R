# Cargando Paquetes
library(dplyr)
library(DT)
library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(shinythemes)
library(lubridate)
library(colorspace)
library(leaflet.minicharts)
library(leaflet)
library(mapview)

tipo_edad_filtro <- c('Adulto Mayor', 'Desconocido', 'Menor de edad', 'Mayor de edad')
genero_filtro <- c('HOMBRE', 'MUJER', 'DESCONOCIDO')
provincia <- c('ALAJUELA', 'CARTAGO', 'HEREDIA', 'GUANACASTE', 'LIMON', 'PUNTARENAS', 'SAN JOSE')
location_data <- read.csv("./data/location_data.csv")

# Creating UI
ui <- navbarPage(
  "AsalView",
  tabPanel(
    "Asaltos Arma Blanca",
    fluidPage(
      theme = shinytheme("sandstone"),
      h1("Datos de Asaltos en Costa Rica 2015-2019"),
      br(),
      # Filters########################################
      column(2,
             h2("Filtros"),
             dateRangeInput(
               "date_AB",
               "Seleccione el rango de fecha de los datos",
               min = "2015-01-01",
               max = "2019-12-31",
               start = "2015-01-01",
               end = "2019-12-31",
               format = "yyyy-mm-dd"
             ),
             br(),
             selectInput(
               'filtro_genero_AB',
               "Filtro por Género",
               choices = genero_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_edad_AB',
               "Filtro por Tipo de Edad",
               choices = tipo_edad_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_provincia_AB',
               "Filtro por Provincia",
               choices = provincia,
               multiple = TRUE
             ),
             br()
      ),
      # Plots###############################################
      column(10,
             column(6,
                    h3("Gráfico por Fecha"),
                    plotlyOutput("grafico_armablanca_fecha")
             ),
             column(6,
                    h3("Gráfico por Género"),
                    plotlyOutput("grafico_armablanca_genero")
             ),
             column(6,
                    h3("Gráfico por Edad"),
                    plotlyOutput("grafico_armablanca_edad")
             ),
             column(6,
                    h3("Gráfico por Provincia"),
                    leafletOutput("grafico_armablanca_provincia")
             )
             
      ),
      # Original Data#################################
      h2("Datos Originales"),
      br(),
      DTOutput("tabla_armablanca")
      ###############################################
    )
  ),
  
  tabPanel(
    "Asaltos Golpes",
    fluidPage(
      theme = shinytheme("sandstone"),
      # Filters########################################
      column(2, 
             h2("Filtros"),
             dateRangeInput(
               "date_G",
               "Seleccione el rango de fecha de los datos",
               min = "2015-01-01",
               max = "2019-12-31",
               start = "2015-01-01",
               end = "2019-12-31",
               format = "yyyy-mm-dd"
             ),
             br(),
             selectInput(
               'filtro_genero_G',
               "Filtro por Género",
               choices = genero_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_edad_G',
               "Filtro por Tipo de Edad",
               choices = tipo_edad_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_provincia_G',
               "Filtro por Provincia",
               choices = provincia,
               multiple = TRUE
             ),
             br()
      ),
      # Plots###############################################
      column(10,
             column(6,
                    h3("Gráfico por Fecha"),
                    plotlyOutput("grafico_golpes_fecha")
             ),
             column(6,
                    h3("Gráfico por Género"),
                    plotlyOutput("grafico_golpes_genero")
             ),
             column(6,
                    h3("Gráfico por Edad"),
                    plotlyOutput("grafico_golpes_edad")
             ),
             column(6,
                    h3("Gráfico por Provincia"),
                    leafletOutput("grafico_golpes_provincia")
             )
             
      ),
      # Original Data#################################
      h2("Datos Originales"),
      br(),
      DTOutput("tabla_golpes")
      ###############################################
    )
  ),
  
  tabPanel(
    "Asaltos Arma Fuego",
    fluidPage(
      theme = shinytheme("sandstone"),
      # Filters#######################################
      column(2,
             h2("Filtros"),
             dateRangeInput(
               "date_AF",
               "Seleccione el rango de fecha de los datos",
               min = "2015-01-01",
               max = "2019-12-31",
               start = "2015-01-01",
               end = "2019-12-31",
               format = "yyyy-mm-dd"
             ),
             br(),
             selectInput(
               'filtro_genero_AF',
               "Filtro por Género",
               choices = genero_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_edad_AF',
               "Filtro por Tipo de Edad",
               choices = tipo_edad_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_provincia_AF',
               "Filtro por Provincia",
               choices = provincia,
               multiple = TRUE
             ),
             br()
      ),
      # Plots###############################################
      column(10,
             column(6,
                    h3("Gráfico por Fecha"),
                    plotlyOutput("grafico_armafuego_fecha")
             ),
             column(6,
                    h3("Gráfico por Género"),
                    plotlyOutput("grafico_armafuego_genero")
             ),
             column(6,
                    h3("Gráfico por Edad"),
                    plotlyOutput("grafico_armafuego_edad")
             ),
             column(6,
                    h3("Gráfico por Provincia"),
                    leafletOutput("grafico_armafuego_provincia")
             )
             
      ),
      # Original Data##################################
      h2("Datos Originales"),
      br(),
      DTOutput("tabla_armafuego")
      ###############################################
    )
  ),
  
  tabPanel(
    "Asaltos Arrebato",
    fluidPage(
      theme = shinytheme("sandstone"),
      # Filters########################################
      column(2,
             h2("Filtros"),
             dateRangeInput(
               "date_A",
               "Seleccione el rango de fecha de los datos",
               min = "2015-01-01",
               max = "2019-12-31",
               start = "2015-01-01",
               end = "2019-12-31",
               format = "yyyy-mm-dd"
             ),
             br(),
             selectInput(
               'filtro_genero_A',
               "Filtro por Género",
               choices = genero_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_edad_A',
               "Filtro por Tipo de Edad",
               choices = tipo_edad_filtro,
               multiple = TRUE
             ),
             br(),
             selectInput(
               'filtro_provincia_A',
               "Filtro por Provincia",
               choices = provincia,
               multiple = TRUE
             ),
             br()
      ),
      # Plots###############################################
      column(10,
             column(6,
                    h3("Gráfico por Fecha"),
                    plotlyOutput("grafico_arrebato_fecha")
             ),
             column(6,
                    h3("Gráfico por Género"),
                    plotlyOutput("grafico_arrebato_genero")
             ),
             column(6,
                    h3("Gráfico por Edad"),
                    plotlyOutput("grafico_arrebato_edad")
             ),
             column(6,
                    h3("Gráfico por Provincia"),
                    leafletOutput("grafico_arrebato_provincia")
             )
             
      ),
      # Original Data##################################
      h2("Datos Originales"),
      br(),
      DTOutput("tabla_arrebato")
      ###############################################
    )
  )
)

# Creando servidor
server <- function(input, output) {
  
  provincesColors <- colorFactor(
    c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC"), 
    domain = c('ALAJUELA', 'CARTAGO', 'HEREDIA', 'GUANACASTE', 'LIMON', 'PUNTARENAS', 'SAN JOSE')
  )
  
  # Reactive data to load CSV files#############################################
  
  datosArmaBlanca <- reactive({
    datos <- read.csv('./data/Aarmablanca.csv')
    return(datos)
  })
  
  datosArmaFuego <- reactive({
    datos <- read.csv('./data/Aarmafuego.csv')
    return(datos)
  })
  
  datosArrebato <- reactive({
    datos <- read.csv('./data/Aarrebato.csv')
    return(datos)
  })
  
  datosGolpes <- reactive({
    datos <- read.csv('./data/Agolpes.csv')
    return(datos)
  })
  
  
  # Filtros de Fecha##########################################################
  
  filtroFechaReactiveAB <- reactive({
    fechasUsuario <- input$date_AB
    
    data <- datosArmaBlanca()
    data$Fecha <- as.Date(data$Fecha, "%m/%d/%y")
    data <- data[data$Fecha  %between% fechasUsuario, ]
    return(data)
  })
  
  filtroFechaReactiveAF <- reactive({
    fechasUsuario <- input$date_AF
    
    data <- datosArmaFuego()
    data$Fecha <- as.Date(data$Fecha, "%m/%d/%y")
    data <- data[data$Fecha  %between% fechasUsuario, ]
    return(data)
  })
  
  filtroFechaReactiveA <- reactive({
    fechasUsuario <- input$date_A
    
    data <- datosArrebato()
    data$Fecha <- as.Date(data$Fecha, "%m/%d/%y")
    data <- data[data$Fecha  %between% fechasUsuario, ]
    return(data)
  })
  
  filtroFechaReactiveG <- reactive({
    fechasUsuario <- input$date_G
    
    data <- datosGolpes()
    data$Fecha <- as.Date(data$Fecha, "%m/%d/%y")
    data <- data[data$Fecha  %between% fechasUsuario, ]
    return(data)
  })
  
  # Filtros de genero#####################################################
  
  filtroGeneroReactiveAB <- reactive({
    seleccionGenero <- input$filtro_genero_AB
    
    validate(need(!is.null(seleccionGenero), "Seleccione Géneros"))
    
    data <- filtroFechaReactiveAB()
    retorno <- data[data$Genero %in% seleccionGenero, ]
    return(retorno)
  })
  
  filtroGeneroReactiveAF <- reactive({
    seleccionGenero <- input$filtro_genero_AF
    
    validate(need(!is.null(seleccionGenero), "Seleccione Géneros"))
    
    data <- filtroFechaReactiveAF()
    retorno <- data[data$Genero %in% seleccionGenero, ]
    return(retorno)
  })
  
  filtroGeneroReactiveA <- reactive({
    seleccionGenero <- input$filtro_genero_A
    
    validate(need(!is.null(seleccionGenero), "Seleccione Géneros"))
    
    data <- filtroFechaReactiveA()
    retorno <- data[data$Genero %in% seleccionGenero, ]
    return(retorno)
  })
  
  filtroGeneroReactiveG <- reactive({
    seleccionGenero <- input$filtro_genero_G
    
    validate(need(!is.null(seleccionGenero), "Seleccione Géneros"))
    
    data <- filtroFechaReactiveG()
    retorno <- data[data$Genero %in% seleccionGenero, ]
    return(retorno)
  })
  
  # Filtros de edad###########################################################
  
  filtroEdadReactiveAB <- reactive({
    seleccionEdad <- input$filtro_edad_AB
    
    validate(need(!is.null(seleccionEdad), "Seleccione Edad"))
    
    data <- filtroFechaReactiveAB()
    retorno <- data[data$Edad %in% seleccionEdad, ]
    return(retorno)
  })
  
  filtroEdadReactiveAF <- reactive({
    seleccionEdad <- input$filtro_edad_AF
    
    validate(need(!is.null(seleccionEdad), "Seleccione Edad"))
    
    data <- filtroFechaReactiveAF()
    retorno <- data[data$Edad %in% seleccionEdad, ]
    return(retorno)
  })
  
  filtroEdadReactiveA <- reactive({
    seleccionEdad <- input$filtro_edad_A
    
    validate(need(!is.null(seleccionEdad), "Seleccione Edad"))
    
    data <- filtroFechaReactiveA()
    retorno <- data[data$Edad %in% seleccionEdad, ]
    return(retorno)
  })
  
  filtroEdadReactiveG <- reactive({
    seleccionEdad <- input$filtro_edad_G
    
    validate(need(!is.null(seleccionEdad), "Seleccione Edad"))
    
    data <- filtroFechaReactiveG()
    retorno <- data[data$Edad %in% seleccionEdad, ]
    return(retorno)
  })
  
  # Filtros de provincia########################################################
  
  filtroProvinciaReactiveAB <- reactive({
    seleccionProvincia <- input$filtro_provincia_AB
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveAB()
    data <- data[data$Provincia %in% seleccionProvincia, ]
    
    data <- aggregate(Delito ~ Provincia, data, FUN=length)
    data <- merge(data, location_data, by = "Provincia")
    upper_bound <- max(data$Delito)
    lower_bound <- min(data$Delito)
    lower_bound <- ifelse(lower_bound == upper_bound, lower_bound-1, lower_bound)
    data$Radio <- 15 + (40 / (upper_bound - lower_bound)) * (data$Delito - lower_bound)
    return(data)
  })
  
  filtroProvinciaReactiveAF <- reactive({
    seleccionProvincia <- input$filtro_provincia_AF
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveAF()
    data <- data[data$Provincia %in% seleccionProvincia, ]
    
    data <- aggregate(Delito ~ Provincia, data, FUN=length)
    data <- merge(data, location_data, by = "Provincia")
    upper_bound <- max(data$Delito)
    lower_bound <- min(data$Delito)
    lower_bound <- ifelse(lower_bound == upper_bound, lower_bound-1, lower_bound)
    data$Radio <- 15 + (40 / (upper_bound - lower_bound)) * (data$Delito - lower_bound)
    return(data)
  })
  
  filtroProvinciaReactiveA <- reactive({
    seleccionProvincia <- input$filtro_provincia_A
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveA()
    data <- data[data$Provincia %in% seleccionProvincia, ]
    
    data <- aggregate(Delito ~ Provincia, data, FUN=length)
    data <- merge(data, location_data, by = "Provincia")
    upper_bound <- max(data$Delito)
    lower_bound <- min(data$Delito)
    lower_bound <- ifelse(lower_bound == upper_bound, lower_bound-1, lower_bound)
    data$Radio <- 15 + (30 / (upper_bound - lower_bound)) * (data$Delito - lower_bound)
    return(data)
  })
  
  filtroProvinciaReactiveG <- reactive({
    seleccionProvincia <- input$filtro_provincia_G
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveG()
    data <- data[data$Provincia %in% seleccionProvincia, ]
    
    data <- aggregate(Delito ~ Provincia, data, FUN=length)
    data <- merge(data, location_data, by = "Provincia")
    upper_bound <- max(data$Delito)
    lower_bound <- min(data$Delito)
    lower_bound <- ifelse(lower_bound == upper_bound, lower_bound-1, lower_bound)
    data$Radio <- 15 + (40 / (upper_bound - lower_bound)) * (data$Delito - lower_bound)
    return(data)
  })
  
  # Agrupación de Fecha##########################################################
  
  agrupacionFechaReactiveAB <- reactive({
    data <- filtroFechaReactiveAB()
    data$Fecha <- format(data$Fecha, "%m/%y")
    data <- aggregate(Delito ~ Fecha, data, FUN=length)
    return(data)
  })
  
  agrupacionFechaReactiveAF <- reactive({
    data <- filtroFechaReactiveAF()
    data$Fecha <- format(data$Fecha, "%m/%y")
    data <- aggregate(Delito ~ Fecha, data, FUN=length)
    return(data)
  })
  
  agrupacionFechaReactiveA <- reactive({
    data <- filtroFechaReactiveA()
    data$Fecha <- format(data$Fecha, "%m/%y")
    data <- aggregate(Delito ~ Fecha, data, FUN=length)
    return(data)
  })
  
  agrupacionFechaReactiveG <- reactive({
    data <- filtroFechaReactiveG()
    data$Fecha <- format(data$Fecha, "%m/%y")
    data <- aggregate(Delito ~ Fecha, data, FUN=length)
    return(data)
  })
  
  # Tablas de datos originales################################################
  
  output$tabla_armablanca <- renderDT({ return(datosArmaBlanca()) })
  
  output$tabla_armafuego <- renderDT({ return(datosArmaFuego()) })
  
  output$tabla_arrebato <- renderDT({ return(datosArrebato()) })
  
  output$tabla_golpes <- renderDT({ return(datosGolpes()) })
  
  # Gráficas Género ###########################################################
  
  output$grafico_armablanca_genero <- renderPlotly({
    # Guardando grafico en un objeto
    g <- ggplot(filtroGeneroReactiveAB())+
      geom_bar(mapping= aes(x=Genero, fill=Genero))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Género de la victima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(g))
  })
  
  output$grafico_golpes_genero <- renderPlotly({
    # Guardando grafico en un objeto
    g <- ggplot( filtroGeneroReactiveG ())+
      geom_bar(mapping= aes(x=Genero, fill=Genero))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Género de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(g))
  })
  
  output$grafico_armafuego_genero <- renderPlotly({
    # Guardando grafico en un objeto
    g <- ggplot(filtroGeneroReactiveAF())+
      geom_bar(mapping= aes(x=Genero, fill=Genero))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Género de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(g))
  })
  
  output$grafico_arrebato_genero <- renderPlotly({
    # Guardando grafico en un objeto
    g <- ggplot(filtroGeneroReactiveA())+
      geom_bar(mapping= aes(x=Genero, fill=Genero))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Género de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(g))
  })
  
  # Gráficas Edad #############################################################
  
  output$grafico_armablanca_edad <- renderPlotly({
    # Guardando grafico en un objeto
    e <- ggplot(filtroEdadReactiveAB())+
      geom_bar(mapping= aes(x=Edad, fill=Edad))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Edad de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(e))
  })
  
  output$grafico_golpes_edad <- renderPlotly({
    # Guardando grafico en un objeto
    d <- ggplot( filtroEdadReactiveG())+
      geom_bar(mapping= aes(x=Edad, fill=Edad))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Edad de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(d))
  })
  
  output$grafico_armafuego_edad <- renderPlotly({
    # Guardando grafico en un objeto
    e <- ggplot(filtroEdadReactiveAF())+
      geom_bar(mapping= aes(x=Edad, fill=Edad))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Edad de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(e))
  })
  
  output$grafico_arrebato_edad <- renderPlotly({
    # Guardando grafico en un objeto
    e <- ggplot(filtroEdadReactiveA())+
      geom_bar(mapping= aes(x=Edad, fill=Edad))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Edad de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(e))
  })
  
  # Gráficas Provincia ########################################################
  
  output$grafico_armablanca_provincia <- renderLeaflet({
    map_provincia_ab <- leaflet(filtroProvinciaReactiveAB()) %>%
      setView(lat = 9.9355438, lng = -84.1483647, zoom = 7) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~Long,
        lat=~Lat,
        color=~provincesColors(Provincia),
        radius=~Radio,
        stroke=FALSE,
        fillOpacity=0.5,
        popup=~paste(
          "<b>", Provincia, "</b><br/>",
          "Cantidad: ", as.character(Delito), "<br/>"
        )
      )%>%
      addLegend(
        "bottomleft",
        pal=provincesColors,
        values=~Provincia,
        opacity = 1,
        title="Provincias"
      )
  })
  
  output$grafico_golpes_provincia <- renderLeaflet({
    map_provincia_g <- leaflet(filtroProvinciaReactiveG()) %>%
      setView(lat = 9.9355438, lng = -84.1483647, zoom = 7) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~Long,
        lat=~Lat,
        color=~provincesColors(Provincia),
        radius=~Radio,
        stroke=FALSE,
        fillOpacity=0.5,
        popup=~paste(
          "<b>", Provincia, "</b><br/>",
          "Cantidad: ", as.character(Delito), "<br/>"
        )
      )%>%
      addLegend(
        "bottomleft",
        pal=provincesColors,
        values=~Provincia,
        opacity = 1,
        title="Provincias"
      )
  })
  
  output$grafico_armafuego_provincia <- renderLeaflet({
    map_provincia_af <- leaflet(filtroProvinciaReactiveAF()) %>%
      setView(lat = 9.9355438, lng = -84.1483647, zoom = 7) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~Long,
        lat=~Lat,
        color=~provincesColors(Provincia),
        radius=~Radio,
        stroke=FALSE,
        fillOpacity=0.5,
        popup=~paste(
          "<b>", Provincia, "</b><br/>",
          "Cantidad: ", as.character(Delito), "<br/>"
        )
      )%>%
      addLegend(
        "bottomleft",
        pal=provincesColors,
        values=~Provincia,
        opacity = 1,
        title="Provincias"
      )
  })
  
  output$grafico_arrebato_provincia <- renderLeaflet({
    map_provincia_a <- leaflet(filtroProvinciaReactiveA()) %>%
      setView(lat = 9.9355438, lng = -84.1483647, zoom = 7) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~Long,
        lat=~Lat,
        color=~provincesColors(Provincia),
        radius=~Radio,
        stroke=FALSE,
        fillOpacity=0.5,
        popup=~paste(
          "<b>", Provincia, "</b><br/>",
          "Cantidad: ", as.character(Delito), "<br/>"
        )
      )%>%
      addLegend(
        "bottomleft",
        pal=provincesColors,
        values=~Provincia,
        opacity = 1,
        title="Provincias"
      )
  })
  
  # Graficas Fecha #############################################################
  
  output$grafico_armablanca_fecha <- renderPlotly({
    
    # Guardando grafico en un objeto
    f <- ggplot(agrupacionFechaReactiveAB(), aes(x=Fecha, y=Delito))+
      geom_line(aes(group=1), colour="#FDCDAC")+
      geom_point(aes(), colour="azure4") +
      xlab("Fecha de Asalto")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(f))

    })
  
  output$grafico_golpes_fecha <- renderPlotly({
    # Guardando grafico en un objeto
    f <- ggplot(agrupacionFechaReactiveG(), aes(x=Fecha, y=Delito))+
      geom_line(aes(group=1), colour="#FDCDAC")+
      geom_point(aes(), colour="azure4") +
      xlab("Fecha de Asalto")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(f))
    
  })
  
  output$grafico_armafuego_fecha <- renderPlotly({
    # Guardando grafico en un objeto
    f <- ggplot(agrupacionFechaReactiveAF(), aes(x=Fecha, y=Delito))+
      geom_line(aes(group=1), colour="#FDCDAC")+
      geom_point(aes(), colour="azure4") +
      xlab("Fecha de Asalto")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(f))
    
  })
  
  output$grafico_arrebato_fecha <- renderPlotly({
    # Guardando grafico en un objeto
    f <- ggplot(agrupacionFechaReactiveA(), aes(x=Fecha, y=Delito))+
      geom_line(aes(group=1), colour="#FDCDAC")+
      geom_point(aes(), colour="azure4") +
      xlab("Fecha de Asalto")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(f))
    
  })
  
  
}

# Ejecucion de la aplicacion
shinyApp(ui = ui, server = server)
