rm(list=ls()) # clear workspace
# setwd("Josh/FDIC-Project")
setwd("FDIC-Project")
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
# Remove irrelevant files:
for(File in fdicFiles){
  
  # read column names of each file:
  ColNames<-colnames(read.csv(File,nrows = 3))
  
  # remove off-topic column names:
  ColNames<-ColNames[which(ColNames %in% projectVars)]
  
  # remove files without the project's variables (other than "cert")
  if(length(ColNames)<2){
    file.remove(File)
  }
}

# create a pattern that selects the two relevant files
filePattern<-"\\d{8}_Assets and Liabilities.csv|\\d{8}_Performance and Condition Ratios.csv"

# store the list of zip files from which to extract the files:
zipFiles<-list.files(zipFolder,"All_Reports",full.names=T)

# extract relevant files from zips:
for(ZIP in zipFiles){
  
  # get a list of all files within zip:
  Files<-unzip(ZIP,list=T)$Name
  
  # store the exact names of files relevant to project:
  Files<-Files[grepl(filePattern,Files)]
  
  # unzip the project's relevant files from the zipfile:
  unzip(ZIP, files = Files, exdir = "./rawFiles")
  
  print(Files) 
}

# create a folder to store modified files:
if(!file.exists("modified")){dir.create("modified")}

# extract the relevant columns from each raw file:

for(File in list.files("rawFiles",pattern="\\.csv$",full.names = T)){
  
  # read the raw file:
  DF<-read.csv(File, row.names = "cert")
  
  # identify which columns are relevant:
  ColNames <- which(colnames(DF) %in% projectVars)
  
  # trim the data frame to include only relevant files:
  DF<-DF[,ColNames]
  
  # write the data frame to the same file name, but in the "modified" folder
  write.csv(DF,gsub("rawFiles","modified",File))
  print(File)
}

# # keep the raw files so the coding pipeline can be understood/replicated
# # but erase the data so the files fit on github:
# for(File in list.files("rawFiles",pattern="\\.csv$",full.names = T)){
#   Message<-"This file has been cleared for space"
#   writeLines(Message,File)
# }
rm(add,all,DF,DF1,DF2,fileDF,test)

for(File in list.files("modified","\\.csv$",full.names = T)){
  
  DF<-read.csv(File,row.names = "X")
  DF<-DF[order(rownames(DF)),]
  file.remove(File)
  write.csv(DF,File)
  print(File)
}

certs<-rownames(read.csv(list.files("modified","\\.csv$",full.names = T)[1], row.names = "X"))
length(unique(certs))
certs2<-rownames(read.csv(list.files("modified","\\.csv$",full.names = T)[2], row.names = "X"))
identical(certs,certs2)
certs3<-rownames(read.csv(list.files("modified","\\.csv$",full.names = T)[3], row.names = "X"))
identical(certs,certs3)
certs4<-unique(certs,certs3)
sum(!certs3 %in% certs4)
rm(certs,certs2,certs3,certs4)

for(File in list.files("modified",))

rows <- lapply(list.files("modified",".csv",full.names = T), read.csv, row.names="X")
rows <- lapply(rows, nrow)
rows <- unlist(rows)
which(max(rows)==rows)
certs<-rownames(read.csv(list.files("modified","\\.csv$",full.names = T)[1], row.names = "X"))
certTest<-function(File){
  print(File)
  sum(!row.names(read.csv(File,row.names = "X")) %in% certs)==0
}
certTest(list.files("modified",".csv",full.names = T)[1])
Tests<-lapply(list.files("modified",".csv",full.names = T),certTest)
FALSE %in% unlist(Tests)

certFill<-function(File){
  DF<-read.csv(File,row.names = "X")
  missing<-certs[!certs %in% row.names(DF)]
  if(0<length(missing)){
    all<-matrix(nrow = length(missing), ncol = ncol(DF))
    all<-data.frame(all, row.names = missing)
    colnames(all)<-colnames(DF)
    DF <- rbind(DF,all)
    DF <- DF[order(as.numeric(row.names(DF))),]
  }
  DF
}

for(File in (list.files("modified","\\.csv$",full.names = T))){
  DF1<-certFill(File)
  qP<-strsplit(File,"_")[[1]]
  qP<-qP[grepl("\\d{8}",qP)]
  colnames(DF1)<-paste(colnames(DF1),qP,sep = "_")
  ifelse(File==list.files("modified","\\.csv$",full.names = T)[1],
         DF<-certFill(File),
         DF<-cbind(DF))
  File<-list.files("modified",
                   pattern=paste("_",qp,sep=""),
                   full.names = T)
}

DF<-certFill(File=list.files("modified",full.names = T)[1])
DF[seq(1,nrow(DF),length.out = 20),]
DF<-certFill(File=list.files("modified",full.names = T)[2])
DF[seq(1,nrow(DF),length.out = 20),]
DF<-certFill(File=list.files("modified",full.names = T)[30])
DF[seq(1,nrow(DF),length.out = 20),]
DF<-certFill(File=list.files("modified",full.names = T)[61])
DF[seq(1,nrow(DF),length.out = 20),]
DF<-certFill(File=list.files("modified",full.names = T)[90])
DF[seq(1,nrow(DF),length.out = 20),]
DF<-certFill(File=list.files("modified",full.names = T)[121])
DF[seq(1,nrow(DF),length.out = 20),]

for(File in list.files("modified",pattern = "\\.csv$", full.names = T)){
  DF<-certFill(File)
  file.remove(File)
  write.csv(DF,File)
  print(File)
}
DF<-certFill(list.files("modified",full.names=T)[4])
# test the column-binding of the files:

  # read two files:
  DF1<-read.csv(list.files("modified",pattern = "\\.csv", full.names = T)[1],row.names="X")
  DF2<-read.csv(list.files("modified",pattern = "\\.csv", full.names = T)[4],row.names="X")

  head(cbind(DF1,DF2))
  # remove column "X" from each if it exists:
  DF1<-DF1[,-grep("^X$",colnames(DF1))]
  DF2<-DF2[,-grep("^X$",colnames(DF2))]

  # create an empty data frame with the target rownames (cert):
  DF<-data.frame(matrix(nrow=nrow(DF1), # same number of rows as either data frame
                        
                        ncol=ncol(DF1)+ncol(DF2)), # width of both data frames combined
                 
                 row.names=row.names(DF1)) # use the rownames of either of the data frames
  
  # name the columns after both data frames together:
  colnames(DF)<-c(colnames(DF1),colnames(DF2))

  # find the DF1 and DF2 rows for each cert and combine them into a DF row:
  for(cert in rownames(DF)){
    
    # find the cert on DF, add the corresponding DF1 data to the row:
    DF[cert,colnames(DF1)]<-DF1[cert,colnames(DF1)]
    
    # find the cert on DF, add the corresponding DF2 data to the row:
    DF[cert,colnames(DF2)]<-DF2[cert,colnames(DF2)]

    # print just enough of results to visually follow the loop:
    if(cert %in% rownames(DF)[seq(1,nrow(DF),length.out = 20)]){
      print(DF[cert,])
    }
  }

# create quarterPatterns to match File pairs for merging:

  # splitting FDIC files by underscore will break the quarterPatterns from their strings:
  quarterPatterns<-unlist(strsplit(fdicFiles,"_"))

  # only the quarterPatterns consist solely of eight digits, so filter by regex:
  quarterPatterns<-quarterPatterns[grepl("^\\d{8}$",quarterPatterns)]

  # there are two files for each quarterPatterns, so redefine quarterPattern results to be unique:
  quarterPatterns<-unique(quarterPatterns)

# merge each quarterPattern's two files into a third csv file named "quarterPattern"
for(qP in quarterPatterns){
  
  # avoid duplicating work if the third file already exists:
  if(!file.exists(paste("modified/",qP,".csv",sep=""))){
    
    # filter fdicFiles by quarterPattern:
    File<-fdicFiles[grepl(qP,fdicFiles)]
    
    # load the data from each file into two separate data frames:
    DF1<-read.csv(File[1],row.names="cert")
    DF2<-read.csv(File[2],row.names="cert")
    
    # remove the "x" column from each data frame (if it exists):
    DF1<-DF1[,-grep("^X$",colnames(DF1))]
    DF2<-DF2[,-grep("^X$",colnames(DF2))]
    
    # create an empty data frame with the target size / shape:
    
    DF<-data.frame(matrix(nrow=nrow(DF1), # target length equal to either data frame
    
                                                ncol=ncol(DF1)+ncol(DF2)), # target width equal to both data frames 
                   
                   row.names=row.names(DF1)) # target rownames equal to either data frame
    
    # name the target data frames columns after both data frames:
    colnames(DF)<-c(colnames(DF1),colnames(DF2))
    
    # find the DF1 and DF2 rows for each cert and combine them into a DF row:
    for(cert in rownames(DF)){
      
      # find the cert on DF, add the corresponding DF1 data to the row:
      DF[cert,colnames(DF1)]<-DF1[cert,colnames(DF1)]
      
      # find the cert on DF, add the corresponding DF2 data to the row:
      DF[cert,colnames(DF2)]<-DF2[cert,colnames(DF2)]
      
      # print just enough of results to visually follow the loop:
      if(cert %in% rownames(DF)[seq(1,nrow(DF),length.out = 20)]){
        print(cbind(DF[cert,],quarter=qP))
      }
      
    }
    
    # name the new file after the quarterPattern:
    newFile<-paste("modified/",qP,".csv",sep="")
    
    # write the data frame into the new file:
    write.csv(DF,newFile)
  }
}
  
# erase earlier stages of the data:
file.remove(list.files("modified",pattern="_",full.names = T))

# modify files to include all certs:
  # aggregate all certs:

  for(n in 1:length(quarterPatterns)){
    File<-paste("modified/",quarterPatterns[n],".csv",sep="")
    newCerts <- rownames(read.csv(File))
    ifelse(n==1, certs <- newCerts, certs <- unique(c(certs, newCerts)))
    print(File)
  }

  for(qP in quarterPatterns){
    File<-list.files("modified",pattern=qP,full.names = T)
    DF<-read.csv(File, row.names="X")
    missing<-certs[!certs %in% row.names(DF)]
    
    if(0<length(missing)){
      add<-data.frame(matrix(ncol=ncol(DF),nrow=length(missing)), row.names = missing)
      colnames(add)<-colnames(DF)
      track<-c(quarter=qP,target=length(certs), start=nrow(DF), add=nrow(add))
      DF<-rbind(add,DF)
      DF<-DF[order(as.numeric(rownames(DF))),]
      write.csv(DF, File)
      print(track)
    }else{print(paste(qP,"is already full size"))}
  }
  for(File in list.files("modified",full.names = T)){
    print(nrow(read.csv(File)))
  }
  for(n in 1:length(quarterPatterns)){
    workFrame<-read.csv(list.files("modified", pattern = quarterPatterns[n], full.names = T),
                        row.names = "X")
    
  }
  list.files("modified",full.names=T)
  for(n in 1:length(quarterPatterns)){
    workFrame<-read.csv(list.files("modified",
                                   pattern = quarterPatterns[n],
                                   full.names = T),
                        row.names = "X")
    colnames(workFrame)<-paste(colnames(workFrame),qP,sep = "_")
    ifelse(n==1, DF<-workFrame, DF<-cbind(DF, workFrame))
    print(colnames(workFrame))
  }
  