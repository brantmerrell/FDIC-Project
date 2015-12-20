library(shiny)

quarters<-expand.grid(1992:2015,1:4)

quarters<-sort(paste(quarters[,1],quarters[,2],sep="_"))

quarters<-quarters["1992_4"<=quarters]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bank Size & Risk"),
  
  # Sidebar with a slider input for the number of bars
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'vars', 'variables',
        # choices = colnames(DF[,-ncol(DF)])
        choices = c("numemp","asset","RWAJT","RBCT2","rbc1rwaj","rbcrwaj")
      ),
      selectizeInput(
        'quar', 'quarter',
        choices = quarters
      ),
      selectizeInput(
        'view', 'view',
        choices = c("bar","dot")
      ),
      sliderInput("bars",
                  "Number of bars:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
