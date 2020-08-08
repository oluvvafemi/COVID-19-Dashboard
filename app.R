library(shiny)


source("./global.R")
source("./tokens.R")

#----for maps

Sys.setenv('MAPBOX_TOKEN' = mapbox_token)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("darkly"),
    # Application title
    # titlePanel(title = "COVID-19 Dashboard", windowTitle = "COVID-19"),
    tags$head(
      tags$meta(name="google-site-verification", content=google_site_verification),
      includeHTML("google-analytics.html")),
    h1("COVID-19 Dashboard", style="text-align: center ;"),
    fluidRow(
                    align = "center", 
                    selectInput("pick_region", "select region", country_vec)
             ),
    hr(),
    
    fluidRow(
        column(3,
               tags$div(
                 class="card text-white bg-info mb-3",
                 h4(style="text-align:center", "Total Cases"),
                 h2(style="text-align:center", textOutput('total_cases'))
               )
            
        ),
        column(3,
           tags$div(
             class="card text-white bg-warning mb-3",
             h4(style="text-align:center", "Active Cases"),
             h2(style="text-align:center", textOutput('active_cases'))
           )
               
        ),
        column(3,
               tags$div(
                 class="card text-white bg-success mb-3",
                 h4(style="text-align:center", "Recovered"),
                 h2(style="text-align:center", textOutput('recovered'))
               )
               
        ),
        column(3,
               tags$div(
                 class="card text-white bg-danger mb-3",
                 h4(style="text-align:center", "Deaths"),
                 h2(style="text-align:center", textOutput('deaths'))
               )
               
        ),
    ),
    
    fluidRow(
      align = "center", 
      radioButtons("total_cases_radio", "Choose trends", choices = radiobutton_trends, inline = T)

    ),
    
    fluidRow(
      column(5,
              plotlyOutput("casesPlot", height = 600)
        
      ),
      column(7,
             plotlyOutput("choroplethMap", height = 600, width = '100%')
      )
    ),
   
    fluidRow(
      align = "center",
      a(href="https://oluvvafemi.github.io/","Oluwafemi Chris" , class = 'btn btn-link')
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    cases_data <- reactive({
      cases_categ_selected_region <- subset(df_wom, Country==input$pick_region)
      confirmed_selected_region <- as.numeric(subset(df_timeSeries_total, Country.Region==input$pick_region, 
                        select = -c(Province.State, Country.Region, Lat, Long) )[1,])
      deaths_selected_region <- as.numeric(subset(df_death_timeSeries, Country.Region==input$pick_region, 
                        select = -c(Province.State, Country.Region, Lat, Long) )[1,])
      list(cases_categ_selected_region, confirmed_selected_region, deaths_selected_region)
    })
    
    output$active_cases <- renderText({ ifelse(as.character(cases_data()[[1]][['ActiveCases']]) =='',
                                               '-', as.character(cases_data()[[1]][['ActiveCases']]))
      })
    output$total_cases <- renderText({ ifelse(as.character(cases_data()[[1]][['TotalCases']]) =='',
                                              '-', as.character(cases_data()[[1]][['TotalCases']]))
      })
   output$recovered <- renderText({ ifelse(as.character(cases_data()[[1]][['TotalRecovered']]) =='',
                                           '-', as.character(cases_data()[[1]][['TotalRecovered']]))
     })
   output$deaths <- renderText({ ifelse(as.character(cases_data()[[1]][['TotalDeaths']]) =='',
                                       '-', as.character(cases_data()[[1]][['TotalDeaths']]))
     })
   
   plotData <- reactive({
     if (input$total_cases_radio=="Total Cases"){
     y <- cases_data()[[2]]
     list(y, 'scatter', 'lines')
     }else if(input$total_cases_radio=="Daily New Cases"){
       val <- cases_data()[[2]]
       y <- val - c(0, val[1:length(val)-1])
       y[y<0] <- 0
       list(y, 'bar', NULL)
     }else if(input$total_cases_radio=="Total Deaths"){
       y <- cases_data()[[3]]
       list(y, 'scatter', 'lines')
     }else if(input$total_cases_radio=="Daily Deaths"){
       val <- cases_data()[[3]]
       y <- val - c(0, val[1:length(val)-1])
       y[y<0] <- 0
       list(y, 'bar', NULL)
     }
   })
    output$casesPlot <- renderPlotly({
         plot_ly(x=date_df, y=plotData()[[1]], type =plotData()[[2]]
                  , mode = plotData()[[3]]
          )%>%
          layout(xaxis = layout_params, yaxis = layout_params) %>% 
            layout(plot_bgcolor='rgb(51, 51, 51)') %>% 
            layout(paper_bgcolor='rgb(51, 51, 51)')%>%
           layout(font=layout_font)
        
    })
    
    output$choroplethMap<- renderPlotly({
      
      plot_ly()%>%add_trace(df_chloropleth, type='choroplethmapbox', locations=df_chloropleth$Code,
                            z=df_chloropleth$total_confirmed,
                            text=df_chloropleth$Country.Region,
                            colors = 'Oranges',
                            geojson = paste(c(
                              "https://raw.githubusercontent.com/python-visualization/folium/master/examples/data/world-countries.json"
                            ), collapse = ""))%>%  layout(
                              mapbox = list(
                                style = "dark",
                                zoom = 1,
                                center = list(lon = 0, lat = 0)
                              )
                            )%>%layout(paper_bgcolor='rgb(51, 51, 51)')%>%layout(font=t)%>%
        config(
          mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"),
          # Workaround to make sure image download uses full container
          # size https://github.com/plotly/plotly.js/pull/3746
          toImageButtonOptions = list(
            format = "svg",
            width = NULL,
            height = NULL
          ))
    })
    
    autoInvalidate <- reactiveTimer(30000)
    observe({
      autoInvalidate()
      cat(".")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
