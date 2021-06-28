library('move')
library('shiny')
library('foreach')
library('fields')
library('geosphere')


Sys.setenv(tz="GMT")
#data <- readRDS("input_lowres_geese_74X.rds")


shinyModuleUserInterface <- function(id, label, posi_lon=0, posi_lat=0) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Distance to Location over Time"),
    numericInput(ns("posi_lon"), "Longitude of Reference Location:", posi_lon, min = -180, max = 180,step=0.00001),
    verbatimTextOutput("value1"),
    
    numericInput(ns("posi_lat"), "Latitude of Reference Location:", posi_lat, min = -90, max = 90,step=0.00001),
    verbatimTextOutput("value2"),

    plotOutput(ns("timeline"),height="90vh")
  )
}


shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()

  print(ns('posi_lon'))
  configuration["posi_lon"] <- input[[ns('posi_lon')]]

  print(ns('posi_lat'))
  configuration["posi_lat"] <- input[[ns('posi_lat')]]

  data.frame(configuration) #try if this removes the error in the UI, even if it is not completely generic
}


shinyModule <- function(input, output, session, data, posi_lon=NULL, posi_lat=NULL) {
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  timestamp_range <- range(timestamps(data))
  dist_range <- reactive({
    range(distVincentyEllipsoid(coordinates(data),c(input$posi_lon,input$posi_lat)))
  })
  
  data.split <- move::split(data)
  namen <- namesIndiv(data)
  cols <- tim.colors(length(namen))
  
  dist_to_loc <- reactive({
    foreach(datai = data.split) %do% {
      disti <- distVincentyEllipsoid(coordinates(datai),c(input$posi_lon,input$posi_lat))
      timei <- timestamps(datai)
      data.frame(disti,timei)
    }
  })
  
#output
  output$value1 <- renderText({ input$posi_lon})
  output$value2 <- renderText({ input$posi_lat})
  
  output$timeline <- renderPlot({
    plot(timestamp_range,dist_range(),type="l",xlim=timestamp_range,ylim=dist_range(),xlab="time",ylab="distance to reference location (m)",col="white")
    for (i in seq(along=namen))
    {
      lines(dist_to_loc()[[i]]$timei,dist_to_loc()[[i]]$disti,col=cols[i],lwd=2)
      legend("topright",legend,namen,fill=cols)
    }
  })
  
  return(reactive({ current() })) 
}


