library(shiny)
load(".RData")
shinyServer(function(input, output) {
#   qPattern<-strsplit(qPattern,"_")[[1]]
#   qPattern[2]
  output$distPlot <- renderPlot({
    qPattern<-strsplit(input$quar,"_")[[1]]
    qPattern[2]<-c("0331","0630","0930","1231")[as.numeric(qPattern[2])]
    qPattern<-paste(qPattern,collapse = "")
    Data<-quarterData(qPattern)
    if(input$vars %in% colnames(Data)){
      x    <- Data[, input$vars]  # bank risk variable
      MAIN <- input$vars
      SUB <- ""
    }else{
      x <- Data[, "RBCT2"]
      MAIN <- paste(input$vars,
                    "does not exist in for quarter",
                    input$quar,
                    "- defaulting to RBCT2")
      SUB <- ""
    }
    if(input$view=="bar"){
      x<-x[!is.na(x)]
      bars <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out = input$bars + 1)
      barplot(quantile(x, seq(0,1,length.out=length(bars))), 
              # breaks = bars, 
              col = sample(colors(),1), border = sample(colors(),1),
              main = MAIN, sub = SUB)
    }
    if(input$view=="dot"){
      plot(Data$asset,x, col = sample(colors(),1), xlab = input$vars,
           main = MAIN, sub = SUB)
    }
  })
})
