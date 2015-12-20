library(shiny)

shinyServer(function(input, output) {
#   subDF<-Data[,grepl("asset",colnames(Data))]
  subDF<-Data[,grepl("rbcrwaj",colnames(Data))]
#   plot(row.names(subDF),subDF[,grepl("Sum",colnames(subDF))])
  output$distPlot <- renderPlot({
    subDF<-Data[,grepl(input$vars,colnames(Data))]
    First<-subDF[,grepl(input$metric1,colnames(subDF))]
    Second<-subDF[,grepl(input$metric2,colnames(subDF))]
    plot(as.ts(First),col="red",
         ylim=c(0,max(c(First,Second))),
         ylab=input$vars,xlab=row.names(subDF)[c(1,nrow(subDF))])
    lines(as.ts(Second),col="blue")
  })
})
colnames(subDF)
length(row.names(subDF))
length(subDF[,grepl("Sum",colnames(subDF))])
plot(subDF[,grepl("Sum",colnames(subDF))])
?as.ts
