library('move')
library('shiny')
library('foreach')
library('fields')
library('geosphere')


Sys.setenv(tz="UTC")
options(scipen=999)
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
  timestamp_unit30 <- units(difftime(timestamp_range[1],seq(timestamp_range[1],timestamp_range[2],len=30)[2]))
  timestamp_labs <- round(seq(timestamp_range[1],timestamp_range[2],len=30),unit=timestamp_unit30)
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
  
  csv_out <- foreach(datai = data.split, .combine = rbind) %do% {
    idi <- rep(namesIndiv(datai),n.locs(datai))
    cooi <- coordinates(datai)
    timei <- timestamps(datai)
    disti <- distVincentyEllipsoid(coordinates(datai),c(posi_lon,posi_lat))
    data.frame("individual_local_identifier"=idi,"timestamp"=timei,"location_long"=cooi[,1],"location_lat"=cooi[,2],"distance_to_location (m)"=disti)
  }
  write.csv(csv_out,file=paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"distance_table.csv"),row.names=FALSE) #this is the output only for the initial setting
  
  
#output
  output$value1 <- renderText({ input$posi_lon})
  output$value2 <- renderText({ input$posi_lat})
  
  output$timeline <- renderPlot({
    par(mar=c(12,4,4,2)+0.1,lab=c(10,6,20))
    plot(timestamp_range,dist_range(),type="l",xlim=timestamp_range,ylim=dist_range(),xlab="",ylab="distance to reference location (m)",col="white",axes=FALSE)
    box()
    axis(2)
    axis(1,at=as.POSIXct(timestamp_labs),lab=as.character(timestamp_labs),las=2)
    if (timestamp_unit30 %in% c("secs","mins","hours")) mtext("time", side=1, line=10)
    if (timestamp_unit30 %in% c("days","weeks")) mtext("time",side=1, line=7)
    for (i in seq(along=namen))
    {
      lines(dist_to_loc()[[i]]$timei,dist_to_loc()[[i]]$disti,col=cols[i],lwd=2)
      legend("topright",legend,namen,fill=cols)
    }
  })
  
  return(reactive({ current() })) 
}


