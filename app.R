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
provincia <- c('ALAJUELA', 'CARTAGO', 'HEREDIA', 'GUANACASTE', 'LIMON', 'PUNTARENAS','SAN JOSE')
location_data <- read.csv("./data/location_data.csv")

# Creating UI
ui <- uiOutput("uiStub")

# Creando servidor
server <- function(input, output, session) {
  
  output$uiStub <- renderUI(uiOutput("pageStub"))
  
  # load server code for page specified in URL
  validFiles = c("demo-homepage.R", "asalview-page.R")
  fname = isolate(session$clientData$url_search)
  if(nchar(fname)==0) { fname = "?demo-homepage" }
  fname = paste0(substr(fname, 2, nchar(fname)), ".R")
  source(fname, local=TRUE)
}

# Ejecucion de la aplicacion
shinyApp(ui = ui, server = server)
