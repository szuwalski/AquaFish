library(Hmisc)
library(stringr)
library(openxlsx)
library(xlsx)
library(readxl)
library(plotrix)
library(plyr)

source("Functions.R")

filelist<-list.files("Data")

#==Function to pulls data from xlsx into lists
#==(awkwardly)...this would be easier if the workbooks were in the same formats
PullData<-function(input,cutInd=NA)
{
inDir<-paste('Data/',input,sep="")
temp<-loadWorkbook(inDir)
Years<-names(getSheets(temp))
  
data<-list(list())
indRange<-length(Years)
if(!is.na(cutInd))
  indRange<-cutInd
for(x in 1:indRange)
{
  try(data[[x]]   <-read_excel(inDir,sheet=x),silent=T)
  try(data[[x]]   <-cbind(data[[x]],Year=rep(Years[x],nrow(data[[x]]))),silent=T)
  try(colnames(data[[x]])[1]<-"Province",silent=T)
}
return(data)
}

#==Applying function to fishing data
outs <-PullData(filelist[1])

#==zero clue why this is happening, but this fixes it
outs[[19]]<-outs[[19]][,-1]
colnames(outs[[19]])[1]<-"Province"
outs[[20]]<-outs[[20]][,-1]
colnames(outs[[20]])[1]<-"Province"

outs2<-PullData(filelist[2])
outs3<-PullData(filelist[3])
outs4<-PullData(filelist[4],cutInd=13)

#==apply black magic
test <-ldply(outs)
test2<-ldply(outs2)
test3<-ldply(outs3)
test4<-ldply(outs4)

#==workbooks had repeated columns
#==this combines them into one
#==this sucks
temp<-join(join(join(test,test2),test3,by=c("Province","Year")),test4,by=c("Province","Year"))

InitNames<-colnames(temp)
DeleteThese<-NULL
for(x in 1:length(InitNames))
{
  nums<-which(!is.na(match(InitNames,InitNames[x])))
  if(length(nums)>1 & all(is.na(match(DeleteThese,nums))))
  {
    for(y in 2:length(nums))
    {
      # find values in other columns that are filled
     takeInd<- which(!is.na(temp[,nums[y]]))
     temp[takeInd,nums[1]]<-temp[takeInd,nums[y]]
    }
   DeleteThese<-append(DeleteThese,nums[2:length(nums)])
  }
}


just<-temp[,c(1,12,nums)]

#==deletes the repeated columns after the data were transplanted
fishing_yield_by_province<-temp[,-DeleteThese]

#==explore species
CoastalProv<-c("Liaoning Province","Hebei","Tianjin","Shandong","Jiangsu Province","Zhejiang","Fujian Province","Guangdong","Hainan","Guangxi")
#inputName<-colnames(fishing_yield_by_province)
pdf("C:/Users/Cody/Desktop/china.pdf")
for(z in 2:length(colnames(fishing_yield_by_province)))
{
inputName<-colnames(fishing_yield_by_province)[z]
short <-fishing_yield_by_province[,c(1,12,which(colnames(fishing_yield_by_province)==inputName))]
short <-short[which(!is.na(match(short$Province,CoastalProv))),]
short$Year <-as.numeric(as.character(short$Year))
bleh <-gsub(",","",as.character(short[,3]))
srsly <-gsub(" ","",as.character(bleh))
short[,3]<-as.numeric(srsly)

print(qplot(x=short$Year,y=short[,3],color=short$Province,ylab=inputName,geom=))

}

dev.off()
