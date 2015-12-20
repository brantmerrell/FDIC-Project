
setwd("../FDIC-Project")
ls()
workDF<-quarterData("20000331")
colnames(workDF)
workVar<-"RBCT2"
varVec<-workDF[,workVar]
colSummary_fdic<-function(varVec){
  Row<-data.frame(matrix(summary(varVec)[c("Min.","Median","Mean","Max.")],nrow=1))
  Row<-cbind(Row,sum(as.numeric(varVec),na.rm = T))
  colnames(Row)<-c("Min.","Median","Mean","Max.","Sum")
  Row
}
dfSummary_fdic<-function(workDF){
  quarter<-levels(workDF$quarter)
  for(workVar in colnames(workDF)){
    if(workVar!="quarter"){
      newRows<-colSummary_fdic(workDF[,workVar])
      colnames(newRows)<-paste(workVar,colnames(newRows))
      ifelse(workVar==colnames(workDF)[1],
             newRow<-newRows,newRow<-cbind(newRow,newRows))
    }
  }
  row.names(newRow)<-quarter
  newRow
}
quarter<-quarters[1]
for(quarter in quarters){
  print(paste("start",quarter))
  qPattern<-strsplit(quarter,"_")[[1]]
  qPattern[2]<-c("0331","0630","0930","1231")[as.numeric(qPattern[2])]
  qPattern<-paste(qPattern,collapse = "")
  Data<-quarterData(qPattern)
  if(!"RWAJT" %in% colnames(Data)){
    Data<-cbind(Data[,c("numemp","asset")],RWAJT=NA,Data[,c("RBCT2","rbc1rwaj","rbcrwaj","quarter")])
  }
  newSummary<-dfSummary_fdic(Data)
  ifelse(quarter==quarters[1],
         projSummary<-newSummary,
         projSummary<-rbind(projSummary,newSummary))
  print(paste("complete",quarter))
}
Data<-projSummary
rm(projSummary,newSummary,qPattern,newRows,Row,workDF)
rm(quarter,reviewColumn,varName)
rm(varVec,workVar)
write.csv(Data,"time summary data.csv")
