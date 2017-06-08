#====================================================
# Data cleaning for Chinese fisheries and aquaculture
# Cody Szuwalski
# April 1, 2017
#====================================================
library(Hmisc)
library(stringr)
library(openxlsx)
library(xlsx)
library(readxl)
library(plotrix)
library(plyr)
library(dplyr)

source("Functions.R")

filelist<-list.files("Data")

#==================================================================
#==Function for data cleaning
#==(awkwardly)...this would be easier if the workbooks were in the same formats
#==================================================================

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

for(y in 1:length(data))
{
  temp<-data[[y]]
  if(length(temp)>0)
  {
  for(z in 2:ncol(temp))
  {
    bleh <-gsub(",","",as.character(temp[,z]))
    srsly <-gsub(" ","",as.character(bleh)) 
    temp[,z]<-as.numeric(as.character(srsly))
  }
  data[[y]]<-temp
  }
}
return(data)
}

#====================================

CleanData<-function(input)
{
  InitNames<-colnames(input)
  DeleteThese<-NULL
  for(x in 1:length(InitNames))
  {
    nums<-which(!is.na(match(InitNames,InitNames[x])))
    if(length(nums)>1 & all(is.na(match(DeleteThese,nums))))
    {
      for(y in 2:length(nums))
      {
        # find values in other columns that are filled
        takeInd<- which(!is.na(input[,nums[y]]))
        input[takeInd,nums[1]]<-input[takeInd,nums[y]]
      }
      DeleteThese<-append(DeleteThese,nums[2:length(nums)])
    }
  }
  if(length(DeleteThese)>0)
    input<-input[,-DeleteThese]
  
  return(input)
}

#====================================================

PlotData<-function(input,filename)
{
  pdf(paste(filename,".pdf",sep=""))
  for(z in 2:length(colnames(input)))
  {
    inputName<-colnames(input)[z]
    yearVar<-which(colnames(input)=="Year")
    short <-input[,c(1,yearVar,which(colnames(input)==inputName))]
    short <-short[which(!is.na(match(short$Province,CoastalProv))),]
    short$Year <-as.numeric(as.character(short$Year))
    bleh <-gsub(",","",as.character(short[,3]))
    srsly <-gsub(" ","",as.character(bleh))
    short[,3]<-as.numeric(srsly)
    print(qplot(x=short$Year,y=short[,3],color=short$Province,ylab=inputName,geom='line'))
  }
  dev.off()  
}


#===================================
#==PULL FISHERIES PRODUCTION DATA
#===================================

outs <-PullData(input=filelist[1])
outs2<-PullData(input=filelist[2])
outs3<-PullData(input=filelist[3])
outs4<-PullData(input=filelist[4])
outs5<-PullData(input=filelist[5])

#==apply black magic
test <-ldply(outs)
test2<-ldply(outs2)
test3<-ldply(outs3)
test4<-ldply(outs4)
test5<-ldply(outs5)

#==join 'em up
temp<-join(test,test2,by=c('Province','Year'))
temp<-join(temp,test3,by=c('Province','Year'))
temp<-join(temp,test4,by=c('Province','Year'))
temp<-join(temp,test5,by=c('Province','Year'))

#==deletes the repeated columns after the data were transplanted
fishing_yield_by_province<-CleanData(temp)

#==explore species
CoastalProv<-c("Liaoning Province","Hebei","Tianjin","Shandong","Jiangsu Province","Zhejiang","Fujian Province","Guangdong","Hainan","Guangxi")

#==plot the data
PlotData(input=fishing_yield_by_province,filename="fish_prod")


#===================================
#==PULL AQUACULTURE PRODUCTION DATA
#===================================
outs <-PullData(input=filelist[9])
outs2<-PullData(input=filelist[10])
outs3<-PullData(input=filelist[11])
outs4<-PullData(input=filelist[12])
outs5<-PullData(input=filelist[13])

#==apply black magic
test <-ldply(outs)
test2<-ldply(outs2)
test3<-ldply(outs3)
test4<-ldply(outs4)
test5<-ldply(outs5)

#==join 'em up
temp<-join(test,test2,by=c('Province','Year'))
temp<-join(temp,test3,by=c('Province','Year'))
temp<-join(temp,test4,by=c('Province','Year'))
temp<-join(temp,test5,by=c('Province','Year'))

aqua_prod_by_province<-CleanData(temp)
PlotData(input=aqua_prod_by_province,filename="aqua_prod")

#===================================
#==PULL AQUACULTURE AREA DATA
#===================================
outs <-PullData(input=filelist[6])
outs2<-PullData(input=filelist[7])
outs3<-PullData(input=filelist[8])

#==apply black magic
test <-ldply(outs)
test2<-ldply(outs2)
test3<-ldply(outs3)

temp<-join(test,test2,by=c('Province','Year'))
temp<-join(temp,test3,by=c('Province','Year'))

#==one year was in hectares instead of 1000s...wtf China
takeind<-seq(1,ncol(temp))[-which(colnames(temp)=="Year")]
takeind<-takeind[-1]
temp[temp$Year<1996,takeind]<-temp[temp$Year<1996,takeind]*1000

aqua_area_by_province<-CleanData(temp)
PlotData(input=aqua_area_by_province,filename="aqua_area")

#===================================
#==PULL FISHERIES VESSEL DATA====
#===================================
# 1 kilowatt = 1.34102 horsepower
effort <-PullData(input=filelist[15])
temp<-ldply(effort)

#==early years were in horsepower
temp[is.na(temp[,4]),4]<- round(as.numeric(temp[is.na(temp[,4]),7]) / 1.34102)

fish_effort_by_province<-CleanData(temp)
PlotData(input=fish_effort_by_province,filename="fish_effort")


#====================================================
#== Make data for use in analysis
#== Exclude variables with less than 20 years of data
#====================================================

TakeFunc<-function(input,cutoff=20)
{
kickback<-NULL
for(x in 1:ncol(input))
{
years_included<-unique(input$Year[!is.na(input[,x])])
if(length(years_included)>cutoff)
 kickback<-append(kickback,x)
}
return(kickback)
}

ChangeName<-function(input,name)
{
for(x in 2:length(colnames(input)))
{
  if(colnames(input)[x]!="Year")
    if(colnames(input)[x]!="Province")
     colnames(input)[x]<-paste(colnames(input)[x],name,sep="")
}
  return(input)
}


t1<-ChangeName(fishing_yield_by_province[,TakeFunc(input=fishing_yield_by_province)],"_catch")
t2<-fish_effort_by_province[,TakeFunc(input=fish_effort_by_province)]
t3<-ChangeName(aqua_area_by_province[,TakeFunc(input=aqua_area_by_province)],"_area")
t4<-ChangeName(aqua_prod_by_province[,TakeFunc(input=aqua_prod_by_province)],"_aqua")

Biggie<-join(t1,t2,by=c('Province','Year'))
Biggie<-join(Biggie,t3,by=c('Province','Year'))
Biggie<-join(Biggie,t4,by=c('Province','Year'))

PlotData(input=Biggie,filename="data_for_analysis")

#=======================================
# exploratory analysis
#=======================================
colnames(Biggie)
Biggie$CPUE<-Biggie$`all fish_catch`/Biggie$Kilowatts
boxplot(CPUE~Province,data=Biggie,ylim=c(0,2),las=2)

useCPUE<-CPUE[CPUE<=2]
useProv<-Biggie$Province[CPUE<=2]

mod<-lm(useCPUE~useProv)
summary(mod)
aov(mod)

nrow(Biggie)

# Trim data set to include only reasonable CPUE
# make columns for total CPUE, catch of 'other' fish
# Change names to reflect origin
# Think about arrangement of analysis
# time series clustering?
# fish = vessel + vessel^2 + aqua_area + aqua_area^2 + stock_ehance
# do it by totals by group first?
# then break out into species interactions and such?

#====================================================
#== Apply production models to all data?
#====================================================


#==do production models
yields<-fishing_yield_by_province[fishing_yield_by_province$Province=="Zhejiang",]
for(x in 1:ncol(yields))
{
  yields[,x] <-gsub(",","",as.character(yields[,x]))
  yields[,x] <-gsub(" ","",as.character(yields[,x]))
}
plot(as.numeric(yields$`small yellow croaker`)~as.numeric(yields$Year),type='l')

for(x in 1:nrow(assessMat))
 assessMat[x,3]<-yields$'small yellow croaker'[match(assessMat$Year[x],yields$Year)]  

plot(as.numeric(assessMat[,3])/1000~assessMat[,1],las=1)
assessMat[,4]<-as.numeric(assessMat[,1])/as.numeric(assessMat[,3])

plot(assessMat[,4]~assessMat[,2],ylim=c(0,1000))
useMat<-assessMat[-c(26,31),]


ProdMod<-function(x,CatchData,IndexData)
{
  K<-abs(x[1])
  r<-abs(x[2])
  q<-abs(x[3])
  predBio<-rep(0,length(IndexData))
  predBio[1]<-K
  for(i in 2:length(CatchData))
  {
    predBio[i]<-predBio[i-1]+r*predBio[i-1]*(1-predBio[i-1]/K)-CatchData[i]
  }
  SSQ<-sum((q*predBio-IndexData)^2,na.rm=T)
  return(SSQ)
}

ProdModPlot<-function(x,CatchData,IndexData,plots=0)
{
  K<-abs(x[1])
  r<-abs(x[2])
  q<-abs(x[3])
  predBio<-rep(0,length(IndexData)+1)
  predBio[1]<-K
  CatchData<-c(CatchData,0)
  for(i in 2:length(CatchData))
  {
    predBio[i]<-predBio[i-1]+r*predBio[i-1]*(1-predBio[i-1]/K)-CatchData[i]
  }
  if(plots==1)
  {
    plot(IndexData[1:(length(IndexData)-1)],ylim=c(0,max(IndexData,na.rm=T)),las=1,ylab="")
    lines(q*predBio[1:(length(predBio)-1)])
    lines(CatchData[1:(length(CatchData)-1)],lty=2,col=2)
  }
  return(predBio)
}

inCatch	<-as.numeric(useMat[order(useMat$Year),3])
inCPUE	<-as.numeric(useMat[order(useMat$Year),4])
inYear<-as.numeric(useMat[order(useMat$Year),2])
plot(inCatch)
par(new=TRUE)
plot(inCPUE,col=2)
x<-c(100000,.2,.00)
outs		<-nlminb(start=x,objective=ProdMod,CatchData=inCatch,IndexData=inCPUE)

ProdModPlot(x=outs$par,CatchData=inCatch,IndexData=inCPUE,plots=1)
#write.csv(cbind(inCatch,inCPUE,inYear),"C:/Users/Cody/Desktop/syc.csv")
