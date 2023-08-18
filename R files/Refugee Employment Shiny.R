library(shiny)
library(tidyverse)
library(rio)
library(sf)
library(plotly)
library(rjson)

interactive_df <- rio::import("Processed Data/Shiny Data.rds")
boundaries_json <- rjson::fromJSON(file="Geographic Data/Switzerland boundaries.geojson")

# Define UI for the application
ui <- fluidPage(
  titlePanel("Refugee Employment Data"),
  sidebarLayout(
    sidebarPanel(
      h4("Plot parameters"),
      sliderInput("year", 
                  "Drag the slider to change the year",
                  min = as.Date("2022-01-01","%Y-%m-%d"),
                  max = as.Date("2022-12-01","%Y-%m-%d"),
                  value=as.Date("2022-12-01"),
                  timeFormat="%Y-%m-%d")
    ),
    
    # Add a main panel around the plot and table
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    g <- list(
      fitbounds = "locations",
      visible = FALSE)
    
    swiss_map <- plot_ly(hoverinfo = "text",
                         text = ~paste("Canton:", interactive_df$name, "<br>",
                                       "Employed:", interactive_df$Employed_percent,"%", "<br>",
                                       "Total:", interactive_df$Total, "refugees")) %>%  
      add_trace(
        type="choropleth",
        geojson=boundaries_json,
        locations=interactive_df$name,
        z=interactive_df$Employed_percent,
        frame=~interactive_df$Date,
        colorscale="Blues",
        reversescale =T,
        #zmin=20,
        #zmax=70,
        featureidkey="properties.name") %>% 
      layout(geo = g) %>%  
      colorbar(thickess=20,
               #orientation="h",
               len=0.5) %>% 
      layout(title = "2022 Refugee Employment in Switzerland")
  })
  output$table <- renderTable({
    cars[1:input$num, ]
  })
}

# Run the application
shinyApp(ui = ui, server = server)






























