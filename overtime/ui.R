library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("overtime size and risk"),
  
  # Sidebar with a slider input for the number of bars
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'vars', 'variables',
        choices=c("numemp","asset","RWAJT","RBCT2","rbc1rwaj","rbcrwaj")
      ),
      selectizeInput(
        'metric1', 'red',
        choices=c("Sum","Max","Median","Mean","Min")
      ),
      selectizeInput(
        'metric2', 'blue',
        choices=c("Sum","Max","Median","Mean","Min")
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
  )
)