library(shiny)

source("www/presentacion.R")
source("www/recoleccion.R")
source("www/transformacion.R")
source("www/modelo.R")
source("www/visualizacion.R")


ui <- fluidPage(
  ui <- navbarPage(title = "Proyecto Ejemplo de Ciencia de Datos",
                   tabPanel("Presentacion",presentacion),
                   tabPanel( "Recoleccion",recoleccion),
                   tabPanel("Transformacion",transformacion),
                   tabPanel("Modelo",modelo),
                   tabPanel("Visualizacion",visualizacion)
                   )
    
)


server <- function(input, output) {

}


shinyApp(ui = ui, server = server)
