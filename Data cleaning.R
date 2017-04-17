library(Hmisc)
library(stringr)
library(openxlsx)
library(xlsx)
library(readxl)
library(plotrix)
library(plyr)

source("Functions.R")



filelist<-list.files("Data")
input<-filelist[1]
#==Pull aquaculture data
PullData<-function(input)
{
inDir<-paste('Data/',input,sep="")
temp<-loadWorkbook(inDir)
Years<-names(getSheets(temp))
  
data<-list(list())
for(x in 1:length(Years))
{
  try(data[[x]]   <-read_excel(inDir,sheet=x),silent=T)
  try(data[[x]]   <-cbind(data[[x]],Year=rep(Years[x],nrow(data[[x]]))),silent=T)
}
return(data)
}

outs<-PullData(filelist[1])

test<-ldply(outs)


