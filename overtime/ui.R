library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bank Size and Risk over time"),
  
  # Sidebar with a slider input for the number of bars
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'vars', 'variables',
        choices=sort(c("numemp","asset","RWAJT","RBCT2","rbc1rwaj","rbcrwaj"))
      ),
      selectizeInput(
        'blue', 'blue',
        choices=c("Min",sort(c("Median","Mean","Max","Sum")))
      ),
      selectizeInput(
        'red', 'red',
        choices=c("Median",sort(c("Min","Mean","Max","Sum")))
      ),
      selectizeInput(
        'green', 'green',
        choices=c("Mean",sort(c("Median","Min","Max","Sum")))
      ),
      sliderInput("range", "Range:",
                  min = 1992,
                  max = 2015, 
                  value = c(1992,2015),
                  sep = "")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

)