# Exercise 3: Shiny widgets

# Load the shiny, ggplot2, and dplyr libraries
library(shiny)
library(ggplot2)
library(dplyr)

# You will once again be working with the `diamonds` data set provided by ggplot2
# Use dplyr's `sample_n()` function to get a random 3000 rows from the data set
# Store this sample in a variable `diamonds_sample`

diamonds_sample <- sample_n(diamonds, 3000)

# For convenience store the `range()` of values for the `price` and `carat` 
# columns (of the ENTIRE diamonds dataset)

price_range <- range(diamonds$price)
carat_range <- range(diamonds$carat)


# Define a UI using a fluidPage layout

ui <- fluidPage(
  
  # Include a `titlePanel` with the title "Diamond Viewer"
  
  titlePanel("Diamond Viewer"),
  
  # Include a `sidebarLayout()`
  
  # The layout's `siderbarPanel()` should have the following control widgets:
  
  # A sliderInput labeled "Price (in dollars)". This slider should let the 
  # user pick a range between the minimum and maximum price of the entire 
  # diamond data set (e.g., the ranges you stored earlier)
  
  # A sliderInput labeled "Carats". This slider should let the user pick a 
  # range between the minimum and maximum carats of the entire data set
  
  # A checkboxInput labeled "Show Trendline". It's default value is TRUE
  
  # A slectInput labeled "Facet By", with choices "cut", "clarity" and "color"
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("price_input", label = "Price (in dollars)", min = price_range[1], max = price_range[2], value = price_range),
      sliderInput("carat_input", label = "Carats", min = carat_range[1], max = carat_range[2], value = carat_range),
      checkboxInput("trendline", label = "Show Trendline", value=TRUE),
      selectInput("facet_by", label="Facet By", choices = c("cut", "clarity", "color"))
    ),
    
    # The layout's `mainPanel()` should have the following reactive outputs:
    # A plotOutput showing the 'plot' output (based on the user specifications)
    
    mainPanel(
      plotOutput('plot'),
      dataTableOutput('table')
    )
  )
)

# Define a Server function for the app

server <- function(input, output) {
  
  # reactive variable for shared data
  
  filtered <- reactive({
    data <- diamonds_sample %>%
      filter(price > input$price_choice[1] & price < input$price_choice[2]) %>%
      filter(carat > input$carat_choice[1] & carat < input$carat_choice[2])
    
    data
  })
  
  # Assign a reactive `renderPlot()` function to the outputted 'plot' value
  
  output$plot <- renderPlot({
    
    # This function should take the `diamonds_sample` data set and filter it by 
    # the input price and carat ranges.
    # Hint: use dplyr and multiple `filter()` operations
    
    # Use the filtered data set to create a ggplot2 scatter plot with the carat 
    # on the x-axis, the price on the y-axis, and color based on the clarity. 
    # Facet the plot based on which feature the user selected to "facet by"
    #   (hint: you can just pass that string directly to `facet_wrap()`)
    # Save your plot as a variable.
    
    p <- ggplot(data = filtered(), mapping = aes(x = carat, y = price, color = cut)) +
      geom_point() +
      facet_wrap(input$facet_by)
    
    # Finally, if the "trendline" checkbox is selected, you should also include 
    # a geom_smooth geometry (with `se=FALSE`)
    # Hint: use an if statement to see if you need to add more geoms to the plot
    # Be sure and return the completed plot!
    
    if(input$smooth) {
      p <- p + geom_smooth(se = FALSE)
    }
    
    return(p)
  })
  
  # Bonus: Assign a reactive `renderDataTable()` function to the outputted table
  # You may want to use a `reactive()` variable to avoid needing to filter the 
  # data twice!
  
  output$table <- renderDataTable({
    return(filtered())
  })
}

# Create a new `shinyApp()` using the above ui and server

shinyApp(ui = ui, server = server)

## Double Bonus: For fun, can you make a similar browser for the `mpg` data set?
## it makes the bonus data table a lot more useful!