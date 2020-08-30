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
  
  cargaDatosBase <- function(filePath) {
    datos <- read.csv(filePath)
    return(datos)
  }
  
  datosArmaBlanca <- reactive({
    return(cargaDatosBase('./data/Aarmablanca.csv'))
  })
  
  datosArmaFuego <- reactive({
    return(cargaDatosBase('./data/Aarmafuego.csv'))
  })
  
  datosArrebato <- reactive({
    return(cargaDatosBase('./data/Aarrebato.csv'))
  })
  
  datosGolpes <- reactive({
    return(cargaDatosBase('./data/Agolpes.csv'))
  })
  
  
  # Filtros de Fecha##########################################################
  
  filtroFechaBase <- function(data, fechasUsuario) {
    data$Fecha <- as.Date(data$Fecha, "%m/%d/%y")
    data <- data[data$Fecha  %between% fechasUsuario, ]
    return(data)
  }
  
  filtroFechaReactiveAB <- reactive({
    fechasUsuario <- input$date_AB
    
    data <- datosArmaBlanca()
    return(filtroFechaBase(data, fechasUsuario))
  })
  
  filtroFechaReactiveAF <- reactive({
    fechasUsuario <- input$date_AF
    
    data <- datosArmaFuego()
    return(filtroFechaBase(data, fechasUsuario))
  })
  
  filtroFechaReactiveA <- reactive({
    fechasUsuario <- input$date_A
    
    data <- datosArrebato()
    return(filtroFechaBase(data, fechasUsuario))
  })
  
  filtroFechaReactiveG <- reactive({
    fechasUsuario <- input$date_G
    
    data <- datosGolpes()
    return(filtroFechaBase(data, fechasUsuario))
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
  
  filtroProvinciaBase <- function(data, seleccionProvincia) {
    data <- data[data$Provincia %in% seleccionProvincia, ]
    
    data <- aggregate(Delito ~ Provincia, data, FUN=length)
    data <- merge(data, location_data, by = "Provincia")
    upper_bound <- max(data$Delito)
    lower_bound <- min(data$Delito)
    lower_bound <- ifelse(lower_bound == upper_bound, lower_bound-1, lower_bound)
    data$Radio <- 15 + (40 / (upper_bound - lower_bound)) * (data$Delito - lower_bound)
    return(data)
  }
  
  filtroProvinciaReactiveAB <- reactive({
    seleccionProvincia <- input$filtro_provincia_AB
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveAB()
    return(filtroProvinciaBase(data, seleccionProvincia))
  })
  
  filtroProvinciaReactiveAF <- reactive({
    seleccionProvincia <- input$filtro_provincia_AF
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveAF()
    return(filtroProvinciaBase(data, seleccionProvincia))
  })
  
  filtroProvinciaReactiveA <- reactive({
    seleccionProvincia <- input$filtro_provincia_A
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveA()
    return(filtroProvinciaBase(data, seleccionProvincia))
  })
  
  filtroProvinciaReactiveG <- reactive({
    seleccionProvincia <- input$filtro_provincia_G
    
    validate(need(!is.null(seleccionProvincia), "Seleccione Provincia"))
    
    data <- filtroFechaReactiveG()
    return(filtroProvinciaBase(data, seleccionProvincia))
  })
  
  # Agrupación de Fecha##########################################################
  
  agrupacionBase <- function(data) {
    data$Fecha <- format(data$Fecha, "%m/%y")
    data <- aggregate(Delito ~ Fecha, data, FUN=length)
    return(data)
  }
  
  agrupacionFechaReactiveAB <- reactive({
    return(agrupacionBase(filtroFechaReactiveAB()))
  })
  
  agrupacionFechaReactiveAF <- reactive({
    return(agrupacionBase(filtroFechaReactiveAF()))
  })
  
  agrupacionFechaReactiveA <- reactive({
    return(agrupacionBase(filtroFechaReactiveA()))
  })
  
  agrupacionFechaReactiveG <- reactive({
    return(agrupacionBase(filtroFechaReactiveG()))
  })
  
  # Tablas de datos originales################################################
  
  output$tabla_armablanca <- renderDT({ return(filtroFechaReactiveAB()) })
  
  output$tabla_armafuego <- renderDT({ return(filtroFechaReactiveAF()) })
  
  output$tabla_arrebato <- renderDT({ return(filtroFechaReactiveA()) })
  
  output$tabla_golpes <- renderDT({ return(filtroFechaReactiveG()) })
  
  # Gráficas Género ###########################################################
  
  graficasGeneroBase <- function(data) {
    #Guardando gráfico en un objeto
    g <- ggplot(data)+
      geom_bar(mapping= aes(x=Genero, fill=Genero))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Género de la victima")+
      ylab("Total de víctimas")
    # Transformando el gráfico a plotly
    return(ggplotly(g))
}
  
  output$grafico_armablanca_genero <- renderPlotly({
    return(graficasGeneroBase(filtroGeneroReactiveAB()))
  })
  
  output$grafico_golpes_genero <- renderPlotly({
    return(graficasGeneroBase(filtroGeneroReactiveG()))
  })
  
  output$grafico_armafuego_genero <- renderPlotly({
    return(graficasGeneroBase(filtroGeneroReactiveAF()))
  })
  
  output$grafico_arrebato_genero <- renderPlotly({
    return(graficasGeneroBase(filtroGeneroReactiveA()))
  })
  
  # Gráficas Edad #############################################################
  
  graficasEdadBase <- function(data) {
    # Guardando grafico en un objeto
    e <- ggplot(data)+
      geom_bar(mapping= aes(x=Edad, fill=Edad))+
      scale_fill_brewer(palette = "Pastel2")+
      xlab("Edad de la víctima")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(e)) 
  }
  
  output$grafico_armablanca_edad <- renderPlotly({
    return(graficasEdadBase(filtroEdadReactiveAB()))
  })
  
  output$grafico_golpes_edad <- renderPlotly({
    return(graficasEdadBase(filtroEdadReactiveG()))
  })
  
  output$grafico_armafuego_edad <- renderPlotly({
    return(graficasEdadBase(filtroEdadReactiveAF()))
  })
  
  output$grafico_arrebato_edad <- renderPlotly({
    return(graficasEdadBase(filtroEdadReactiveA()))
  })
  
  # Gráficas Provincia ########################################################
  
  graficasProvinciaBase <- function(data) {
    map_provincia_ab <- leaflet(data) %>%
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
  }
  
  output$grafico_armablanca_provincia <- renderLeaflet({
    graficasProvinciaBase(filtroProvinciaReactiveAB())
  })
  
  output$grafico_golpes_provincia <- renderLeaflet({
    graficasProvinciaBase(filtroProvinciaReactiveG())
  })
  
  output$grafico_armafuego_provincia <- renderLeaflet({
    graficasProvinciaBase(filtroProvinciaReactiveAF())
  })
  
  output$grafico_arrebato_provincia <- renderLeaflet({
    graficasProvinciaBase(filtroProvinciaReactiveA())
  })
  
  # Graficas Fecha #############################################################
  
  graficasFechaBase <- function(data) {
    # Guardando grafico en un objeto
    f <- ggplot(data, aes(x=Fecha, y=Delito))+
      geom_line(aes(group=1), colour="#FDCDAC")+
      geom_point(aes(), colour="azure4") +
      xlab("Fecha de Asalto")+
      ylab("Total de víctimas")
    
    # Transformando el gráfico a plotly
    return(ggplotly(f))
    
  }
  
  output$grafico_armablanca_fecha <- renderPlotly({
    return(graficasFechaBase(agrupacionFechaReactiveAB()))
  })
  
  output$grafico_golpes_fecha <- renderPlotly({
    return(graficasFechaBase(agrupacionFechaReactiveG()))
  })
  
  output$grafico_armafuego_fecha <- renderPlotly({
    return(graficasFechaBase(agrupacionFechaReactiveAF()))
  })
  
  output$grafico_arrebato_fecha <- renderPlotly({
    return(graficasFechaBase(agrupacionFechaReactiveA()))
  })
  
  
}

# Ejecucion de la aplicacion
shinyApp(ui = ui, server = server)
