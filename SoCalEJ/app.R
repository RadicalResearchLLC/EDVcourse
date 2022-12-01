#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SoCal Enviroscreen App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'metric',
                           label = 'Pick a metric',
                           choices = list$variable, 
                           selected = 'DieselPM_P')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      # dataset is SoCal_narrow1 - no geometry
      SoCal_narrow1 %>% 
        # filter based on the input$metric
        filter(variable == input$metric)  %>% 
        #create a ggplot by county, value
        ggplot(aes(x = value, fill = County)) +
        geom_histogram() +
        theme_bw() +
        facet_wrap(~County) +
        labs(x = input$metric, 
             y = 'Count of census tracts') 
    })
    
    output$map <- renderLeaflet({
      
      # filter geometry dataset for the user input
     metricSoCal <- SoCal_narrow %>% 
       filter(variable == input$metric)
    
      # Create a color palette based on that dataset
     palM <- colorNumeric(palette = 'magma', domain = metricSoCal$value)
      
        #pass the dataset to leaflet
      leaflet(metricSoCal) %>% 
        addTiles() %>% 
        setView(lat = 33.8, lng = -117.60, zoom = 9) %>% 
        addPolygons(color = ~palM(value),
                    weight = 1) %>% 
        addLegend(pal = palM,
                  title = input$metric,
                  values = ~value)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
