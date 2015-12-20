rm(list=ls())
setwd("..")
setwd("./FDIC-project")
list.files(getwd(), all.files = T)
load('.RData')

# list files:
Files <- list.files(path = "modified", 
                       pattern = "\\.csv$", 
                       full.names = T)

quarterData<-function(quarterPattern="19950331"){
  Files <- list.files(path = "modified", 
                      pattern = paste(quarterPattern,".+\\.csv$",sep = ""),
                      full.names = T)
  # if(exists("DF")){rm(DF)}
  DF1<-read.csv(Files[1])
  DF2<-read.csv(Files[2])
  if(!identical(DF1$X,DF2$X)){
    DF1<-DF1[order(DF1$X),]
    DF2<-DF2[order(DF2$X),]
  }
  ifelse(identical(DF1$X,DF2$X),
         DF<-cbind(DF1, DF2),
         stop("certs not identical"))
  rm(DF1,DF2,Files)
  row.names(DF)<-DF$X
  DF<-DF[,-grep("^X$",colnames(DF))]
  DF<-cbind(DF,quarter = quarterPattern)
  DF
}

quarterPlotBase<-function(quarterPattern="19930331",
                       x=c("asset","numemp"),
                       y=c("RBCT2","RWAJT","rbc1rwaj","rbcrwaj")){
  plotData <- quarterData(quarterPattern)
  # plot(plotData$numemp,plotData$asset)
  reg1 <- lm(plotData[,x[1]]~plotData[,y[1]])
  plot(plotData[,x[1]], plotData[,y[1]], xlab = x[1], ylab = y[1],
       sub = quarterPattern, main = "size and risk")
  par(cex=.8)
  abline(reg1)
}
unlink("Asset RBCT2",recursive = T)
dir.create("Asset RBCT2")
for(year in 1993:2015){
  # plot<-quarterPlotBase(paste(year,"0331", sep=""))
  png(file.path("Asset RBCT2",paste(year,"0331.png",sep="")))
  print(quarterPlotBase(paste(year,"0331",sep="")))
  dev.off()
}
for(year in 1993:2015){
  png(file.path("Asset RBCT2",paste(year,"0630.png",sep="")))
  print(quarterPlotBase(paste(year,"0630",sep="")))
  dev.off()
}
for(year in 1993:2014){
  png(file.path("Asset RBCT2",paste(year,"0630.png",sep="")))
  print(quarterPlotBase(paste(year,"0630",sep="")))
  dev.off()
}
for(year in 1993:2014){
  png(file.path("Asset RBCT2",paste(year,"0930.png",sep="")))
  print(quarterPlotBase(paste(year,"0930",sep="")))
  dev.off()
}
for(year in 1993:2014){
  png(file.path("Asset RBCT2",paste(year,"1231.png",sep="")))
  print(quarterPlotBase(paste(year,"1231",sep="")))
  dev.off()
}

