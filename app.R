source("./functional-code.R", echo=TRUE)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  dataset <- shiny::reactive({
    input$dataset
  })
  
  transformation <- shiny::reactive({
    input$transformation
  })
  
  forecastingmethod <- shiny::reactive({
    input$forecastingmethod
  })
  
  blocksize <- shiny::reactive({
    input$blocksize
  })

  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- shiny::renderPlot({
    obj1 <- split_dataset(dataset(), transformation())
    obj2 <- forecast_function(obj_ts = obj1, 
                              method = forecastingmethod(), 
                              block_size = blocksize(),
                              B = 100, 
                              level = 95, 
                              seed=123)
    plot_results(obj2)
  })
}

# Create Shiny app ----
shiny::shinyApp(ui = shiny::htmlTemplate("www/index.html"), server)
