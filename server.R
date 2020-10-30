library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(lubridate)
library(shiny)
library(shinyjs)


if(file.exists('datasets/coverage.Rds') & file.info('datasets/a0-coverage.csv')$mtime < file.info('datasets/coverage.Rds')$mtime){
  coverage <- readRDS('datasets/coverage.Rds')
}else{
  coverage <- read.csv('datasets/a0-coverage.csv',stringsAs=F)
  coverage$Day <- round(coverage$coverage,1)
  #coverage$Week <- 0
  #coverage$Month <- 0
  coverage$date <- as.Date(coverage$date)
  coverage$cases <- as.numeric(coverage$cases)
  coverage <- subset(coverage,!is.na(date))
  # for(la in 1:length(unique(coverage$LTLA19CD))){
  #   laids <- which(coverage$LTLA19CD==unique(coverage$LTLA19CD)[la])
  #   lasub <- coverage[laids,]
  #   week_case <- sapply(1:nrow(lasub),function(x){
  #     d <- lasub$date[x]; 
  #     keepd <- lasub$date <= (3+d) & lasub$date >= (-3+d); 
  #     cases <- lasub$cases[keepd];
  #     samples <- lasub$samples[keepd]; 
  #     sum(samples,na.rm=T)/sum(as.numeric(cases),na.rm=T)
  #   })
  #   week_case[week_case>1] <- 1
  #   coverage$Week[laids] <- round(week_case,1)
  #   month_case <- sapply(1:nrow(lasub),function(x){
  #     d <- lasub$date[x]; 
  #     keepd <- lasub$date <= (15+d) & lasub$date >= (-15+d); 
  #     cases <- lasub$cases[keepd];
  #     samples <- lasub$samples[keepd]; 
  #     sum(samples,na.rm=T)/sum(as.numeric(cases),na.rm=T)
  #   })
  #   month_case[month_case>1] <- 1
  #   coverage$Month[laids] <- round(month_case,1)
  # }
  saveRDS(coverage,'datasets/coverage.Rds')
}

if(file.exists('datasets/la_shape.Rds')){
  shp <- readRDS('datasets/la_shape.Rds')
}else{
  # set the URL from where the file can be collected
  geojson_url <- 'https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson'
  # set the file path where we'll download to
  file_path <- "regions.geojson"
  # download from the URL to the filepath specified
  download.file(geojson_url, file_path)
  shp <- geojsonio::geojson_read(file_path,what='sp')
  saveRDS(shp,'datasets/la_shape.Rds')
}

map <- leaflet::leaflet()# %>%


shiny::shinyServer(function(input, output, session) {
  dte <- file.info('datasets/coverage.Rds')$mtime
  output$date_text <- renderText({ 
    paste0("Metadata date: ", year(dte),'-',month(dte),'-',day(dte))
  })
  updateSliderInput(session,inputId = "ti_window", label = "Select date",
                    min = min(coverage$date),
                    max = max(coverage$date),
                    value=max(coverage$date),
                    timeFormat="%Y-%m-%d")
  updateSelectizeInput(session,inputId='ti_la',label='LTLA',choices=c('',as.character(unique(coverage$LTLA19NM))),
                       selected='')
  
  ## initialise reactive values
  ddate <- min(coverage$date)
  agg <- 'Day'
  subs <- coverage[coverage$date==as.character(ddate),]
  subs$Covered <- subs[[agg]]
  shp$Covered <- sapply(shp$lad19cd,function(x){
    if(as.character(x)%in%subs$LTLA19CD){
      subs$Covered[subs$LTLA19CD==as.character(x)]
    }else{
      NA
    }})
  shp$labl  <- with(shp@data, paste(
    "<p> <b>", lad19nm, "</b> </br>",
    "Coverage:", Covered,
    "</p>"))
  pal <- colorNumeric("Blues", 
                      shp$Covered,
                      na.color = "#808080",
                      #n = 5,
                      domain = c(0,1))
  
  output$map <- renderLeaflet({
    map_lad <- map %>%
      leaflet::addPolygons(
        data = shp,  # LAD polygon data from geojson
        weight = 1,  # line thickness
        opacity = 1,  # line transparency
        color = "black",  # line colour
        fillOpacity = 1,
        fillColor = ~pal(Covered),
        label = ~labl  # LAD name as a hover label
      ) %>% addLegend(position = "topright",pal = pal, values=0:1, opacity=1)
    map_lad  
  })
  
  wide <<- dcast(data.table(coverage),
                 formula=LTLA19CD~date,
                 value.var = agg)
  la_order <<- match(shp$lad19cd,wide$LTLA19CD)
  
  
  ## observe input events
  
  ## render map ########################################################
  ## visualise map
  # observeEvent({
  #   input$ti_aggregation
  # },{
  #   agg <- input$ti_aggregation
  #   wide <<- dcast(data.table(coverage),
  #                 formula=LTLA19CD~date,
  #                 value.var = agg)
  #   la_order <<- match(shp$lad19cd,wide$LTLA19CD)
  # })
  observeEvent({
    input$ti_window
    #input$ti_aggregation
  },{
    ddate <- as.character(input$ti_window)
    new_zoom <- 6
    cen <- c(-3.441199, 55.75554)
    if (!is.null(input$map_zoom)) {
      new_zoom <- input$map_zoom
      cen <- input$map_center
    }
    shp$Covered <- wide[[ddate]][la_order]
    shp$labl  <- with(shp@data, paste(
      "<p> <b>", lad19nm, "</b> </br>",
      "Coverage:", Covered,
      "</p>"))
    pal <- colorNumeric("Blues", 
                        shp$Covered,
                        na.color = "#808080",
                        #n = 5,
                        domain = c(0,1))
    leafletProxy("map", data = shp) %>%
      setView(lng=cen[1],lat=cen[2],zoom=new_zoom) %>%
      leaflet::addPolygons(
        data = shp,  # LAD polygon data from geojson
        weight = 1,  # line thickness
        opacity = 1,  # line transparency
        color = "black",  # line colour
        fillOpacity = 1,
        fillColor = ~pal(Covered),
        popup = ~labl  # LAD name as a hover label
      ) 
    
    #})
    
  })
  
  ## line plot
  observeEvent({
    input$ti_la
  },{
    la_name <- input$ti_la
    covla <- subset(coverage,LTLA19NM==la_name)
    covla$coverage <- covla$coverage*100
    mdf <- reshape2::melt(covla[,colnames(covla)%in%c('date','cases','samples','coverage')],id.vars="date")
    
    output$lines <- renderPlot({
      ggplot(mdf, aes( x=date, y=value, colour=variable, group=variable )) + 
        geom_line() +
        scale_color_manual(values=c("cases"="navyblue","samples"="hotpink","coverage"="darkorange")) +
        scale_linetype_manual(values=c("cases"="solid","samples"="solid","coverage"="solid")) +
        xlab('Date') + ylab('Count (for samples and cases); percent (for coverage)') + 
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14) ,
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              legend.position='top', 
              legend.justification='left',
              legend.title=element_blank(), 
              legend.text=element_text(size=12)) 
    })
  })
  
  ## data tab
  output$data <- DT::renderDataTable({
    DT::datatable(coverage[,colnames(coverage)%in%c("cases","samples","date","LTLA19CD","LTLA19NM", "Day", "Week", "Month")], filter = 'top',options = list("pageLength" = 500),rownames=F)
  })
  
  ## download button
  output$save_data <- downloadHandler(
    filename = function() { 
      paste("sampling-coverage-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(coverage[,colnames(coverage)%in%c("cases","samples","date","LTLA19CD","LTLA19NM", "Day", "Week", "Month")], file, row.names = F)
    })
    
  output 
  
}
)
