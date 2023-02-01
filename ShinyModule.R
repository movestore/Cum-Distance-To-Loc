library('move')
library('shiny')
library('foreach')
library('fields')
library('geosphere')
library("shinycssloaders")


Sys.setenv(tz="UTC")
options(scipen=999)
#data <- readRDS("input_lowres_geese_74X.rds")

## spinner option can be modified to different shapes and colors
## zoom in to plot ==> not finished 
## download plot ==> adjust height/width

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Distance to Location over Time"),
    fluidRow(
      column(3, numericInput(ns("posi_lon"), "Longitude of Reference Location:", value=0, min = -180, max = 180,step=0.00001)),
      column(3, numericInput(ns("posi_lat"), "Latitude of Reference Location:", value=0, min = -90, max = 90,step=0.00001)),
      column(2, actionButton(ns("GoButton"), label="Update!", icon=icon("rotate")),style = 'top: 25px;position:relative;'),
      column(2, downloadButton(ns('savePlot'), 'Save Plot'),style = 'top: 25px;position:relative;'),
      
    ),
    withSpinner(plotOutput(ns("timeline"), height="80vh"))
    # withSpinner(plotOutput(ns("timeline"),dblclick = ns("plot_dblclick"), brush = brushOpts(id =ns("plot_brush"),resetOnNew = TRUE), height="80vh")), ## still have to add zoom in plot axis
    
  )
}


shinyModule <- function(input, output, session, data) {
  # dataObj <- reactive({ data })
  current <- reactiveVal(data)
  userCorrds <- reactiveValues(posi_lon = 0, posi_lat = 0)
  observeEvent(input$GoButton, {
    userCorrds$posi_lon <- input$posi_lon
    userCorrds$posi_lat <- input$posi_lat
  })
  timestamp_range <- range(timestamps(data))
  timestamp_unit30 <- units(difftime(timestamp_range[1],seq(timestamp_range[1],timestamp_range[2],len=30)[2]))
  timestamp_labs <- round(seq(timestamp_range[1],timestamp_range[2],len=30),unit=timestamp_unit30)
  dist_range <- reactive({
    range(distVincentyEllipsoid(coordinates(data),c(userCorrds$posi_lon,userCorrds$posi_lat)))
  })
  
  data.split <- move::split(data)
  namen <- namesIndiv(data)
  cols <- tim.colors(length(namen))
  
  dist_to_loc <- reactive({
    foreach(datai = data.split) %do% {
      disti <- distVincentyEllipsoid(coordinates(datai),c(userCorrds$posi_lon,userCorrds$posi_lat))
      timei <- timestamps(datai)
      data.frame(disti,timei)
    }
  })
  
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
    sv_plot <- recordPlot()
    
    ### save plot ###
    output$savePlot <- downloadHandler(
      filename = "distance_plot.png",
      content = function(file) {
        png(file, width = 960, height = 480)
        replayPlot(sv_plot)
        dev.off()
      }
    )
    ### save table ###
    csv_out <- foreach(datai = data.split, .combine = rbind) %do% {
      idi <- rep(namesIndiv(datai),n.locs(datai))
      cooi <- coordinates(datai)
      timei <- timestamps(datai)
      disti <- distVincentyEllipsoid(coordinates(datai),c(userCorrds$posi_lon,userCorrds$posi_lat))
      data.frame("individual_local_identifier"=idi,"timestamp"=timei,"location_long"=cooi[,1],"location_lat"=cooi[,2],"distance_to_location (m)"=disti)
    }
    write.csv(csv_out,file=appArtifactPath("distance_table.csv"),row.names=FALSE)
  })
  
  return(reactive({ current() })) 
}


