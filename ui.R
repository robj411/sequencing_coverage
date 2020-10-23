shinyUI(
  bootstrapPage(
    theme = shinythemes::shinytheme("spacelab"),
    tags$head(
      tags$style(HTML(".leaflet-container { background: #fff; }"))
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "shortcut icon", type = "image/x-icon",
                href = "favicon.ico")
    ),
    # include the js code
    shinyjs::useShinyjs(),
    includeScript("scripts.js"),
    column(12,
           HTML(
             "
             <meta name='keywords' content='infectious,disease,epidemiology,genome,coronavirus,SARS-CoV-2,phylodynamic,phylogenetics,lineage,mutation,Spike,UK'>
             <meta name='author' content='Rob Johnson'>
             <h1>Sequence coverage</h1>
             "
           ),
      #tabsetPanel(
      #  tabPanel("About",
          HTML(
             "
             <p>Coverage is the number of sequences divided by the number of cases.  
             </p>
             <p>Sequences compiled by the 
             <a href='https://www.cogconsortium.uk/' target='_blank'>UK COVID-19 Sequencing consortium</a>.</p>
             "
        #     )
       # )
        ),
    tags$hr(style="background: #cccccc; height: 4px;")
    ),
    tabsetPanel(
      tabPanel("Map",
               column(4, id = "control",
                      sliderInput("ti_window", label = "Select date",
                                  min = as.Date("2020-02-03","%Y-%m-%d"),
                                  max = as.Date(lubridate::today(),"%Y-%m-%d"),
                                  value=c(as.Date("2020-03-15","%Y-%m-%d")),
                                  timeFormat="%Y-%m-%d",
                                  animate = animationOptions(interval = 50, loop = F)),
                      radioButtons(inputId='ti_aggregation',choices = c('Day','Week','Month'),selected = 'Day',label='Aggregation'),align='center'
               ),
               column(8, id = "plot",leafletOutput( 'map', width = "100%", height = "1000px"), align="left")
      ),
      tabPanel("Data",
               downloadButton("save_data", "Download data"),
               fluidRow(
                 DT::dataTableOutput(outputId = 'data')
                 , style = "font-size:15px", align="left")
      )
    )
  )
)
