library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bank Size and Risk over time"),
  
  # Sidebar with a slider input for the number of bars
  sidebarLayout(
    sidebarPanel(
      sliderInput("range", "Range:",
                  min = 1992,
                  max = 2015, 
                  value = c(1992,2015),
                  sep = ""),
      selectizeInput(
        'var1', 'variable',
        choices=sort(c("numemp","asset","RWAJT","RBCT2","rbc1rwaj","rbcrwaj"))
      ),
      selectizeInput(
        'blue1', 'blue 1',
        choices=c("Min",sort(c("Median","Mean","Max","Sum")))
      ),
      selectizeInput(
        'red1', 'red 1',
        choices=c("Median",sort(c("Min","Mean","Max","Sum")))
      ),
      selectizeInput(
        'green1', 'green 1',
        choices=c("Mean",sort(c("Median","Min","Max","Sum")))
      ),
      selectizeInput(
        'blue2', 'blue 2',
        choices=c("Min",sort(c("Median","Mean","Max","Sum")))
      ),
      selectizeInput(
        'red2', 'red 2',
        choices=c("Median",sort(c("Min","Mean","Max","Sum")))
      ),
      selectizeInput(
        'green2', 'green 2',
        choices=c("Mean",sort(c("Median","Min","Max","Sum")))
      )
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      verbatimTextOutput('out1')
    )
  )
)
)
