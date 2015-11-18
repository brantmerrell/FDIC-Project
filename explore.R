rm(list=ls()) # clear workspace
setwd("Josh/FDIC-Project")
load('fdic.RData') # load FDIC functions
fdicFolder<-"C:/Users/Kent/Documents/FDIC" # define folder for FDIC data
download.FDIC(1993,3,fdicFolder) # download a quarter for testing

# get the list of csv files to scan:
fdicFiles<-list.files(fdicFolder,"199309.+\\.csv$",recursive = T, full.names = T)

# define the variables to retrieve:
projectVars<-c("cert","asset","numemp","rbc1rwaj","RBCT2","rbcrwaj","RWAJT")

# get all files with more than two of the project's variables (cert + one other):
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
} # error after 1994.3
save.image()
file.remove(list.files(fdicFolder,full.names=T,recursive=T,pattern="\\.csv$"))
rownames(DF)[seq(1,nrow(DF),length.out=20)]
as.vector(DF[seq(1,nrow(DF),length.out=20),"quarter"])
for(n in 44:nrow(quarterID)){
  DF<-rbind(DF,
            projectQuarterFrame(quarterID[n,"year"],
                                quarterID[n,"quar"]))
  print(data.frame(Time=paste(Sys.time()),
                   Year=quarterID[n,"year"],
                   Q=quarterID[n,"quar"]))
} # error after 1994.3
DF1<-DF
rm(DF)
write.csv(DF1,"data1.csv")
rm(DF1)

DF<-projectQuarterFrame(quarterID[44,"year"],quarterID[44,"quar"])
for(n in 44:nrow(quarterID)){
  DF<-projectQuarterFrame(quarterID[n,"year"],quarterID[n,"quar"])
  if(!ncol(DF) %in% c(7,8)){stop("new width of data frame")}
  if(ncol(DF)==7){DF1<-rbind(DF1,DF)}
  if(ncol(DF)==8){DF2<-rbind(DF2,DF)}
  print(data.frame(Time=paste(Sys.time()),Year=quarterID[n,"year"],Q=quarterID[n,"quar"]))
} # error immediate
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
