rm(list=ls()) # clear workspace
setwd("Josh/FDIC-Project")
load('fdic.RData') # load FDIC functions
fdicFolder<-"C:/Users/Kent/Documents/FDIC" # define folder for FDIC data
download.FDIC(1993,3,fdicFolder) # download a quarter for testing

# get the list of csv files to scan:
fdicFiles<-list.files(fdicFolder,"199309.+\\.csv$",recursive = T, full.names = T)

# define the variables to retrieve:
projectVars<-c("cert","asset","numemp","rbc1rwaj","RBCT2","rbcrwaj","RWAJT")

# get all files with more than two of the variables (cert + one other):
for(File in fdicFiles){
  ColNames<-colnames(read.csv(File,nrows = 3))
  ColNames<-ColNames[which(ColNames %in% projectVars)]
  if(1<length(ColNames)){
    ifelse(exists("projectFiles"), 
           projectFiles<-c(projectFiles,File),
           projectFiles<-File)
  }
}
rm(fdicFiles) # clean workspace
for(File in projectFiles){
  DF<-read.csv(File)
  projectColumns<-which(colnames(DF) %in% projectVars)
  ifelse(exists("projectFrames"),
         projectFrames<-list(projectFrames,DF[,projectColumns]),
         projectFrames<-DF[,projectColumns])
  
}
projectDF<-cbind(projectFrames[[1]],projectFrames[[2]])
colnames(projectDF)
head(projectDF[,c(1,5)])
identical(projectDF[,1],projectDF[,5])
projectQuarterFrame<-function(year,
                              quarter,
                              projectVars=c("cert","asset","numemp",
                                            "rbc1rwaj","RBCT2","rbcrwaj","RWAJT"),
                              destfolder = "C:/Users/Kent/Documents/FDIC"){
  fdicFolder<-destfolder
  download.FDIC(year,quarter,fdicFolder)
  quarterMonths<-c("03","06","09","12")
  quarterID<-paste(year,quarterMonths[quarter],sep="")
  quarterPattern<-paste(year,quarterMonths[quarter],".+\\.csv$",sep="")
  fdicFiles<-list.files(fdicFolder,quarterPattern,recursive = T, full.names = T)
  rm(projectFiles)
  for(File in fdicFiles){
    ColNames<-colnames(read.csv(File,nrows = 3))
    ColNames<-ColNames[which(ColNames %in% projectVars)]
    if(1<length(ColNames)){
      ifelse(exists("projectFiles"), 
             projectFiles<-c(projectFiles,File),
             projectFiles<-File)
    }
  }
  DF1<-read.csv(projectFiles[1])
  DFColumns<-which(colnames(DF1) %in% projectVars)
  DF1<-DF1[,DFColumns]
  DF2<-read.csv(projectFiles[2])
  DFColumns<-which(colnames(DF2) %in% projectVars)
  DF2<-DF2[,DFColumns]
  DF<-data.frame(cbind(DF1,DF2))
  DF<-DF[,-grep("^cert.1$",colnames(DF))]
  row.names(DF)<-paste(quarterID,as.character(DF$cert),sep="_")
  DF<-cbind(DF,quarter=quarterID)
  return(DF)
}
DF<-projectQuarterFrame(1993,3)
summary(DF)
quarterID<-expand.grid(year=1994:2014,quar=1:4)
for(n in 1:nrow(quarterID)){
  DF<-rbind(DF,
            projectQuarterFrame(quarterID[n,"year"],
                                quarterID[n,"quar"]))
  print(data.frame(Time=paste(Sys.time()),
                   Year=quarterID[n,"year"],
                   Q=quarterID[n,"quar"]))
} # error after 2001.1

rownames(DF)[seq(1,nrow(DF),length.out=20)]
as.vector(DF[seq(1,nrow(DF),length.out=20),"quarter"])
DF2<-projectQuarterFrame(2002,1)
head(DF2)
head(DF)
DF1<-DF
rm(DF)
for(n in 9:nrow(quarterID)){
  DF<-projectQuarterFrame(quarterID[n,"year"],quarterID[n,"quar"])
  if(!ncol(DF) %in% c(7,8)){stop("new width of data frame")}
  if(ncol(DF)==7){DF1<-rbind(DF1,DF)}
  if(ncol(DF)==8){DF2<-rbind(DF2,DF)}
  print(data.frame(Time=paste(Sys.time()),Year=quarterID[n,"year"],Q=quarterID[n,"quar"]))
}
rownames(DF1)[seq(1,nrow(DF1),length.out=20)] # 1993.3 to 2001.2
rownames(DF2)[seq(1,nrow(DF2),length.out=20)] # 2002.1 to 2009.2
rownames(DF)[seq(1,nrow(DF),length.out=20)] # 2009.2
head(DF1)
head(DF2)
head(DF)
for(n in 38:nrow(quarterID)){
  DF<-projectQuarterFrame(quarterID[n,"year"],quarterID[n,"quar"])
  if(!ncol(DF) %in% c(7,8)){stop("new width of data frame")}
  if(ncol(DF)==7){DF1<-rbind(DF1,DF)}
  if(ncol(DF)==8){DF2<-rbind(DF2,DF)}
  print(data.frame(Time=paste(Sys.time()),Year=quarterID[n,"year"],Q=quarterID[n,"quar"]))
}
rownames(DF1)[seq(1,nrow(DF1),length.out=20)] # 1993.1 to 2001.3
rownames(DF2)[seq(1,nrow(DF2),length.out=20)] # 2002.1 to 2014.4
allQuarters<-unique(as.character(DF1$quarter))
allQuarters<-c(allQuarters,unique(as.character(DF2$quarter)))
allQuarters<-sort(allQuarters)
sum(grepl(pattern=1994,allQuarters))
quarterCount<-function(Pattern){sum(grepl(Pattern,allQuarters))}
Counts<-lapply(1993:2014,quarterCount)
unlist(Counts)
1+length(1994:2014)*4
file.remove(list.files(fdicFolder,full.names = T,recursive = T))
DF<-projectQuarterFrame(2014,1)
