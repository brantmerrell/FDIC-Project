library(shiny)
load(".RData")

shinyServer(function(input, output) {
  output$out1 <- renderPrint({
    definition<-as.vector(glossary[input$var1==glossary[,"VarName"],])
    gsub("<br>","",as.vector(definition[,"LongDescription"]))
    })
  output$plot1 <- renderPlot({
    definition<-as.vector(glossary[(input$var1==glossary[,"VarName"]),])
    # subDF<-Data[,grepl("RWAJT",colnames(Data))]
    subDF<-Data[,grepl(input$var1,colnames(Data))]
    years<-as.numeric(substr(row.names(subDF), 1, 4))
    rangeVec<-years<=max(input$range) & min(input$range)<=years
    # rangeVec<-years<=max(2010) & min(2000)<=years
    subDF<-subDF[rangeVec,]
    years<-years[rangeVec]
    naVec<-unlist(lapply(subDF[,1],is.na))
    subDF<-subDF[!naVec,]
    years<-years[!naVec]
    red<-as.numeric(subDF[,grepl(input$red1,colnames(subDF))])
    # red<-as.numeric(subDF[,grepl("Median",colnames(subDF))])
    blue<-as.numeric(subDF[,grepl(input$blue1,colnames(subDF))])
    # blue<-as.numeric(subDF[,grepl("Min",colnames(subDF))])
    green<-as.numeric(subDF[,grepl(input$green1,colnames(subDF))])
    # green<-as.numeric(subDF[,grepl("Mean",colnames(subDF))])
    MAIN<-as.vector(definition[,"ShortDescription"])
    noColor<-"yellow|pale|violet|sea|lime"
    reds<-sample(colors()[grepl("red",colors()) & !grepl(noColor,colors())],1)
    blues<-sample(colors()[grepl("blue",colors()) & !grepl(noColor,colors())],1)
    greens<-sample(colors()[grepl("green",colors()) & !grepl(noColor,colors())],1)
    plot(as.ts(red),col=reds,
         ylim=c(min(c(red,blue,green)),
                max(c(red,blue,green))),
         xaxt="n",
         ylab=input$var1,
         # ylab = "RWAJT",
         xlab="Years",
         main = MAIN
    )
    lines(as.ts(blue),col=blues)
    lines(as.ts(green),col=greens)
    print(c(reds,blues,greens))
    axis(side = 1, 
         at = seq(1,nrow(subDF), length.out = 5),
         labels = years[seq(1,nrow(subDF),length.out = 5)])
  })
#   output$plot2 <- renderPlot({
#     definition<-as.vector(glossary[(input$var1==glossary[,"VarName"]),])
#     # subDF<-Data[,grepl("RWAJT",colnames(Data))]
#     subDF<-Data[,grepl(input$var1,colnames(Data))]
#     years<-as.numeric(substr(row.names(subDF), 1, 4))
#     rangeVec<-years<=max(input$range) & min(input$range)<=years
#     # rangeVec<-years<=max(2010) & min(2000)<=years
#     subDF<-subDF[rangeVec,]
#     years<-years[rangeVec]
#     naVec<-unlist(lapply(subDF[,1],is.na))
#     subDF<-subDF[!naVec,]
#     years<-years[!naVec]
#     red<-as.numeric(subDF[,grepl(input$red2,colnames(subDF))])
#     # red<-as.numeric(subDF[,grepl("Median",colnames(subDF))])
#     blue<-as.numeric(subDF[,grepl(input$blue2,colnames(subDF))])
#     # blue<-as.numeric(subDF[,grepl("Min",colnames(subDF))])
#     green<-as.numeric(subDF[,grepl(input$green2,colnames(subDF))])
#     # green<-as.numeric(subDF[,grepl("Mean",colnames(subDF))])
#     # MAIN<-as.vector(definition[,"ShortDescription"])
#     plot(as.ts(red),col="red",
#          ylim=c(min(c(red,blue,green)),
#                 max(c(red,blue,green))),
#          xaxt="n",
#          ylab=input$var1,
#          # ylab = "RWAJT",
#          xlab="Years"#,
#          # main = MAIN
#     )
#     lines(as.ts(blue),col="blue")
#     lines(as.ts(green),col="green")
#     axis(side = 1, 
#          at = seq(1,nrow(subDF), length.out = 5),
#          labels = years[seq(1,nrow(subDF),length.out = 5)])
  # })
})

