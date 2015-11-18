rm(list=ls()) # clear workspace
setwd("Josh/FDIC-Project")
load('fdic.RData') # load FDIC functions
zipFolder<-"C:/Users/Kent/Documents/Josh/Zips" # define folder for FDIC data
tq<-c(year=1993, quarter=3) # choose a test quarter for testing

# select the corresponding zipFile
zipFile<-list.files(zipFolder,pattern=quarterID(tq['year'],tq['quarter']),
                    full.names = T)
# unzip 
unzip(zipFile, exdir = "./rawFiles")

# define the variables to assess:
projectVars<-c("cert","asset","numemp","rbc1rwaj","RBCT2","rbcrwaj","RWAJT")

# remove all files without at least two of the project's variables (cert + one other):
fdicFiles <- list.files("./rawFiles", 
                        pattern = paste(quarterID(tq['year'],tq['quarter']),".+\\.csv$", sep = ""), 
                        full.names = T)
for(File in fdicFiles){
  ColNames<-colnames(read.csv(File,nrows = 3))
  ColNames<-ColNames[which(ColNames %in% projectVars)]
  if(length(ColNames)<2){
    file.remove(File)
  }
}
filePattern<-"\\d{8}_Assets and Liabilities.csv|\\d{8}_Performance and Condition Ratios.csv"

zipFiles<-list.files(zipFolder,"All_Reports",full.names=T)
for(ZIP in zipFiles){
  # Files<-unzip(zipFiles[1],list=T)$Name
  Files<-unzip(ZIP,list=T)$Name
  Files<-Files[grepl(filePattern,Files)]
  unzip(ZIP, files = Files, exdir = "./rawFiles")
  print(Files)
}
dir.create("modified")
File = list.files("rawFiles",pattern="\\.csv$",full.names = T)[1]
for(File in list.files("rawFiles",pattern="\\.csv$",full.names = T)){
  DF<-read.csv(File)
  ColNames <- which(colnames(DF) %in% projectVars)
  DF<-DF[,ColNames]
  write.csv(DF,gsub("rawFiles","modified",File))
}
for(File in list.files("rawFiles",pattern="\\.csv$",full.names = T)){
  Message<-"This file has been cleared for space"
  writeLines(Message,File)
}

fdicFiles<-list.files("modified",full.names = T)

DF1<-read.csv(fdicFiles[1],row.names="cert")
DF1<-DF1[,-grep("^X$",colnames(DF1))]
DF2<-read.csv(fdicFiles[2],row.names="cert")
DF2<-DF2[,-grep("^X$",colnames(DF2))]

DF<-data.frame(matrix(nrow=nrow(DF1),ncol=ncol(DF1)+ncol(DF2)),
               row.names=row.names(DF1))
colnames(DF)<-c(colnames(DF1),colnames(DF2))
for(cert in rownames(DF)){
  DF[cert,colnames(DF1)]<-DF1[cert,colnames(DF1)]
  DF[cert,colnames(DF2)]<-DF2[cert,colnames(DF2)]
  if(cert %in% rownames(DF)[seq(1,nrow(DF),length.out = 20)]){
    print(DF[cert,])
  }
}
quarterPatterns<-unlist(strsplit(fdicFiles,"_"))
quarterPatterns<-quarterPatterns[grepl("^\\d{8}$",quarterPatterns)]
qP = quarterPatterns[1]
for(qP in quarterPatterns){
  File<-fdicFiles[grepl(qP,fdicFiles)]
  DF1<-read.csv(File[1],row.names="cert")
  DF1<-DF1[,-grep("^X$",colnames(DF1))]
  DF2<-read.csv(File[2],row.names="cert")
  DF2<-DF2[,-grep("^X$",colnames(DF2))]
  DF<-data.frame(matrix(nrow=nrow(DF1),ncol=ncol(DF1)+ncol(DF2)),
                 row.names=row.names(DF1))
  colnames(DF)<-c(colnames(DF1),colnames(DF2))
  for(cert in rownames(DF)){
    DF[cert,colnames(DF1)]<-DF1[cert,colnames(DF1)]
    DF[cert,colnames(DF2)]<-DF2[cert,colnames(DF2)]
    if(cert %in% rownames(DF)[seq(1,nrow(DF),length.out = 20)]){
      print(cbind(DF[cert,],quarter=qP))
    }
  }
  newFile<-paste("modified/",qP,".csv",sep="")
  write.csv(DF,newFile)
}

