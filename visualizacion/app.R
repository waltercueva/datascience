library(shiny)
library(dplyr)
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
  
  
  #####################################################Recoleccion###############################################
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  ##################################################### Fin Recoleccion ###############################################
  ##################################################### transformacion ###############################################
  dt<-read.csv("dataset/1.csv")
  output$resDplyr<-renderDataTable({
    selector<-input$transDplyr
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      return (dt)
    }
    if(selector=="2"){
      return (dt%>%filter(C4>15))
    }
    if(selector=="3"){
      return (dt%>%filter(grepl('Carla',Alumno)))
    }
    
  })
}


shinyApp(ui = ui, server = server)
