library(shiny)
library(dplyr)
library(sqldf)
library(RMySQL)
library(plotly)
source("www/presentacion.R")
source("www/recoleccion.R")
source("www/transformacion.R")
source("www/modelo.R")
source("www/exploracion.R")


ui <- fluidPage(
  ui <- navbarPage(title = "Proyecto Ejemplo de Ciencia de Datos",
                   tabPanel("Presentacion",presentacion),
                   tabPanel( "Recoleccion",recoleccion),
                   tabPanel("Transformacion",transformacion),
                   tabPanel("Visualizacion",exploracion),
                   tabPanel("Modelo",modelo),
                   tabPanel("Interpretacion")
                   
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
  output$resSQLDF<-renderDataTable({
    selector<-input$transSQLDF
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      query1<-sqldf('Select ID,Alumno, C1 from dt;')
      return (query1)
    }
    if(selector=="2"){
      query2<-sqldf('Select ID,Alumno, C1 from dt where C1>17;')
      return (query2)
    }
    if(selector=="3"){
      query3<-sqldf('Select ID,Alumno, avg(C1) as promedio from dt group by C2 order by promedio ASC;')
      return (query3)
    }
    
  })
  ##################################################### exploracion ###############################################
  output$resGgplot2<-renderPlot({
    selector<-input$expGgplot
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      gr1<- ggplot(dt,aes(x=C1))+geom_bar()
      return (gr1)
    }
    if(selector=="2"){
      
    }
    if(selector=="3"){
      
    }
    
  })
  output$resPlotly<-renderPlotly({
    #selector<-input$expPlotly
    
    trace_0 <- rnorm(100, mean = 5)
    trace_1 <- rnorm(100, mean = 0)
    trace_2 <- rnorm(100, mean = -5)
    x <- c(1:100)
    
    data <- data.frame(x, trace_0, trace_1, trace_2)
    
    fig <- plot_ly(data, x = ~x)
    fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
    fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
    
    return (fig)
    
    
  })
  
  
}


shinyApp(ui = ui, server = server)
