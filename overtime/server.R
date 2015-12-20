library(shiny)

shinyServer(function(input, output) {
  # subDF<-Data[,grepl("RWAJT",colnames(Data))]
  output$distPlot <- renderPlot({
    subDF<-Data[,grepl(input$vars,colnames(Data))]
    years<-as.numeric(substr(row.names(subDF), 1, 4))
    rangeVec<-years<=max(input$range) & min(input$range)<=years
    # rangeVec<-years<=max(2010) & min(2000)<=years
    subDF<-subDF[rangeVec,]
    years<-years[rangeVec]
    naVec<-unlist(lapply(subDF[,1],is.na))
    subDF<-subDF[!naVec,]
    years<-years[!naVec]
    red<-as.numeric(subDF[,grepl(input$red,colnames(subDF))])
    # red<-as.numeric(subDF[,grepl("Median",colnames(subDF))])
    blue<-as.numeric(subDF[,grepl(input$blue,colnames(subDF))])
    # blue<-as.numeric(subDF[,grepl("Min",colnames(subDF))])
    green<-as.numeric(subDF[,grepl(input$green,colnames(subDF))])
    # green<-as.numeric(subDF[,grepl("Mean",colnames(subDF))])
    plot(as.ts(red),col="red",
         ylim=c(min(c(red,blue,green)),
                max(c(red,blue,green))),
         xaxt="n",
         # ylab=input$vars,
         xlab="Years")
    lines(as.ts(blue),col="blue")
    lines(as.ts(green),col="green")
    axis(side = 1, 
         at = seq(1,nrow(subDF), length.out = 5),
         labels = years[seq(1,nrow(subDF),length.out = 5)])
  })
})

