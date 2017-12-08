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
library(lmtest)
library(xts)
library(FSA)
library(ggplot2)
source("Functions.R")

filelist<-list.files("Data")

#==================================================================
#==Function for data cleaning
#==(awkwardly)...this would be easier if the workbooks were in the same formats
#==================================================================

PullData<-function(input,cutInd=NA,which_dat=1)
{
if(which_dat==1)
inDir<-paste('Data/',input,sep="")
if(which_dat==2)
inDir<-input
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
  pdf(paste("Plots/",filename,".pdf",sep=""))
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
CoastalProv_name<-c("Liaoning","Hebei","Tianjin","Shandong","Jiangsu","Zhejiang","Fujian","Guangdong","Hainan","Guangxi")

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

#for(x in 1:length(CoastalProv))
#     aqua_area_by_province$Province[aqua_area_by_province$Province==CoastalProv[x]]<-CoastalProv_name[x]
aqua_area_by_province<-CleanData(temp)
PlotData(input=aqua_area_by_province,filename="aqua_area")

#===================================
#==PULL FISHERIES VESSEL DATA====
#===================================
# 1 kilowatt = 1.34102 horsepower
effort <-PullData(input=filelist[17])
temp<-ldply(effort)

#==early years were in horsepower
temp[is.na(temp[,4]),4]<- round(as.numeric(temp[is.na(temp[,4]),7]) / 1.34102)

temp[temp$Year==1984,]
fish_effort_by_province<-CleanData(temp)

#for(x in 1:length(CoastalProv))
# fish_effort_by_province$Province[fish_effort_by_province$Province==CoastalProv[x]]<-CoastalProv_name[x]

PlotData(input=fish_effort_by_province,filename="fish_effort")

#==fishing type by gear
fishtype<-PullData(input=filelist[16])
temp<-ldply(fishtype)
fish_type_by_province<-CleanData(temp)
PlotData(input=fish_type_by_province,filename="fish_type")

#==sum over province
TotalEffort<- fish_effort_by_province%>%
  group_by(Year) %>%
  summarise(TotShips = sum(Ships,na.rm=T),TotPower=sum(Kilowatts,na.rm=T),TotTon=sum(Tons,na.rm=T))
log10(TotalEffort$TotTon)
colnames(fish_effort_by_province)

#write.csv(TotalEffort,"TotalEffort.csv")
GlobalEffort<-read.csv("Data_aux/Global fleet size.csv")
colnames(GlobalEffort)
print(qplot(x=Year,y=log10(Value),color=Country,data=GlobalEffort[GlobalEffort$UNIT=="NU",],geom='line'))
print(qplot(x=Year,y=log10(Value),color=Country,data=GlobalEffort[GlobalEffort$UNIT=="GT",],geom='line'))

print(qplot(x=Year,y=(Value),color=Country,data=GlobalEffort[GlobalEffort$UNIT=="NU",],geom='line',ylab="numbers"))
print(qplot(x=Year,y=(Value),color=Country,data=GlobalEffort[GlobalEffort$UNIT=="GT",],geom='line',ylab="tonnage"))

#===================================
#==PULL STOCK ENHANCEMENT DATA====
#===================================
stock_enhance <-PullData(input=filelist[14])
temp<-ldply(stock_enhance)
stock_enhance_by_province<-CleanData(temp)
names(stock_enhance_by_province)[2]<-"All fish enhance"
names(stock_enhance_by_province)[5]<-"All shrimp enhance"
PlotData(input=stock_enhance_by_province,filename="stock_enhancement")



#===================================
#==PULL DISTANT WATER DATA====
#===================================
distant_water <-PullData(input="Data_2/distant_water_fisheries.xlsx",which_dat=2)
temp<-ldply(distant_water)
distant_water_by_province<-CleanData(temp)
PlotData(input=distant_water_by_province,filename="distant_water")

#===================================
#==PULL ECONOMIC LOSS DATA====
#===================================
econ_loss <-PullData(input="Data_2/economic_loss.xlsx",which_dat=2)
temp<-ldply(econ_loss)
econ_loss_by_province<-CleanData(temp)
PlotData(input=stock_enhance_by_province,filename="stock_enhancement")

#===================================
#==PULL ECONOMIC OUTPUT DATA====
#===================================
econ_output <-PullData(input="Data_2/Fisheries_economic_output_province.xlsx",which_dat=2)
temp<-ldply(econ_output)
econ_output_by_province<-CleanData(temp)
PlotData(input=econ_output_by_province,filename="econ_output")




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
t5<-ChangeName(stock_enhance_by_province[,TakeFunc(input=stock_enhance_by_province,cutoff=10)],"_enhance")

Biggie<-join(t1,t2,by=c('Province','Year'))
Biggie<-join(Biggie,t3,by=c('Province','Year'))
Biggie<-join(Biggie,t4,by=c('Province','Year'))
Biggie<-join(Biggie,t5,by=c('Province','Year'))
for(x in 1:length(colnames(Biggie)))
  colnames(Biggie)[x]<-gsub(" ","_",colnames(Biggie)[x]) 

Biggie$Province<-as.factor(Biggie$Province)
Biggie$CPUE<-Biggie$`all_fish_catch`/Biggie$Kilowatts

Biggie$all_fisheries_catch<-apply(cbind(Biggie$all_algae_catch,Biggie$all_crustaceans_catch,
                                Biggie$all_fish_catch,Biggie$all_other_catch,
                                Biggie$all_shellfish_catch),1,sum,na.rm=T)

plot(Biggie$all_fish_catch~Biggie$Year)
allCatch<- Biggie%>%
     group_by(Year) %>%
     summarise(allfisheries = sum(all_fisheries_catch,na.rm=T))
allfishCatch<- Biggie%>%
     group_by(Year) %>%
     summarise(allfisheries = sum(all_fish_catch,na.rm=T))
plot(allCatch)
lines(allfishCatch)

allfishCatch/allCatch

PlotData(input=Biggie,filename="data_for_analysis")
#write.csv(Biggie,"Biggie.csv")
#==put in approximate coastlines as proxy for area fished
CoastLine<-read.csv("Data_aux/China_coastline.csv")
addCoast<-rep(NA,length(Biggie$Province))
for(x in 1:length(CoastalProv))
  addCoast[  !is.na(match(Biggie$Province,CoastalProv[x]))]<-CoastLine[match(CoastalProv_name[x],CoastLine[,1]),2]

#==multiply by 200 to get area of EEZ approximately
Biggie$Fishing_diqu<-addCoast*200
#======================================================
# For each province, pairs plot for different variables
#======================================================
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#===============================================================
# Aquaculture analysis
#===============================================================
pdf("Plots/Aqua_area_vs_prod.pdf")
par(mar=c(.1,.1,.1,.1),oma=c(0,8,8,0))
incol<-rainbow(length(unique(Biggie$Province)))
pairs(Biggie[,c(34,38,39,41,48,51,54,55)],gap=.01,las=2,col=incol[Biggie$Province],pch=16,upper.panel=panel.smooth,lower.panel=panel.cor)
dev.off()

AquaInds<-c(34,38,39,41,48,51,54,55)
pdf("Plots/Aqua_Pairs.pdf")
pairs(Biggie[,AquaInds],gap=.01,cex.labels=.6,lower.panel =panel.smooth,upper.panel=panel.cor,col=incol[Biggie$Province],pch=16)
for(x in 1:length(CoastalProv))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  for(y in 1:length(AquaInds))
  {
    if(sum(!is.na(tempDat[,AquaInds[y]]))<10)
      tempDat[1:10,AquaInds[y]]<-rnorm(10,1,10)
  }
  inDat<-data.frame(tempDat[,AquaInds])
  pairs(inDat,gap=.01,cex.labels=.6,lower.panel=panel.smooth,upper.panel=panel.cor,pch=16)
  #pairs(inDat,gap=.01,cex.labels=.6,lower.panel=panel.smooth,upper.panel=panel.cor)
  mtext(side=3,CoastalProv[x],line=3)
}
dev.off()



#==what is the relationship between area and production by province?
library(mgcv)
Aqua_dat<-Biggie[,c(1,8,AquaInds)]
aqua_types<-c("algae","all_fish","crustacean","shellfish")

pdf("Plots/GAMs_aqua.pdf")
for(x in 1:length(aqua_types))
 {
  temp<-grep(aqua_types[x],colnames(Aqua_dat))
  for(y in 1:length(CoastalProv))
  {
   temp2<-Aqua_dat[Aqua_dat$Province==CoastalProv[y],temp]
   if(sum(!is.na(temp2))>25)
    {
   mod<-gam(temp2[,2]~s(temp2[,1],k=3))
   plot(mod,residuals=TRUE,cex=4,ylab=colnames(Aqua_dat)[temp[2]],xlab=colnames(Aqua_dat)[temp[1]],shade.col=x+1,shade=T)
   mtext(side=3,CoastalProv[y])
    }
  }
}
dev.off()


#==are there interactions between varieties of aquaculture?
# make models, aggregate parameters, make plots


#=============================================================
# Fishery yields analysis
#=============================================================
#=pairs plot of all yields vs inputs
maybeInds<-c(9,2,27,28,29,30,33,34,38,39,41,48,51,53,54,55)
pdf("Plots/Pairs_aggregate.pdf")
pairs(Biggie[,maybeInds],gap=.01,cex.labels=.6,upper.panel=panel.smooth,lower.panel=panel.cor)
for(x in 1:length(CoastalProv))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  for(y in 1:length(maybeInds))
  {
    if(sum(!is.na(tempDat[,maybeInds[y]]))<10)
      tempDat[1:10,maybeInds[y]]<-rnorm(10,1,10)
  }
  inDat<-data.frame(tempDat[,maybeInds])
  pairs(inDat,gap=.01,cex.labels=.6,upper.panel=panel.smooth,lower.panel=panel.cor)
  #pairs(inDat,gap=.01,cex.labels=.6,lower.panel=panel.smooth,upper.panel=panel.cor)
  mtext(side=3,CoastalProv[x],line=3)
}
dev.off()

#==just total fishery yields against all other vars
pdf("Plots/Yields_vs_vars.pdf")
par(mar=c(4,5,1,1))
for(x in 2:length(maybeInds))
{
   plot(Biggie[,maybeInds[1]]~Biggie[,maybeInds[x]],ylab='',xlab='',las=1,pch=16,col='grey')
   mtext(side=2,"Fishery yields",line=4)
   mtext(side=1,colnames(Biggie)[maybeInds[x]],line=3)
}

for(x in 2:length(maybeInds))
{
  plot(Biggie[,maybeInds[1]]~Biggie[,maybeInds[x]],ylab='',xlab='',las=1,pch=16,col=incol[Biggie$Province])
  mtext(side=2,"Fishery yields",line=4)
  mtext(side=1,colnames(Biggie)[maybeInds[x]],line=3)
}
dev.off()

#==province level inputs
#==aquaculture, stock enhancement, vessels (by gear type?)
par(mfcol=c(3,11),mar=c(.1,.1,.1,.1),oma=c(4,6,4,3))
AquaInd<-c(8,34,38,39,41)
Aqua_names<-c("Algae","Fish","Crustaceans","Shellfish")
EffortInd<-c(8,31,33)
StockEnc<-c(8,61,64,66)

AquaCol<-seq(1,5)


SelectPlot<-function(data,indices,ylimIn,xlimIn,log_dat=0,incol)
{
  starter<-1
  if(log_dat==0)
   plot(-1000,xaxt='n',yaxt='n',ylim=c(0,ylimIn),xlim=xlimIn,bty='n')
  if(log_dat==1)
    plot(-1000,xaxt='n',yaxt='n',ylim=c(0,log(ylimIn)),xlim=xlimIn,bty='n')
  for(y in 2:length(indices))
  {
    if(!is.na(data[,indices[y]])&log_dat==0)
      lines(data[,indices[y]]~data$Year,col=incol[y])
    if(!is.na(data[,indices[y]])&log_dat==1)
      lines(log(data[,indices[y]])~data$Year,col=incol[y])
  } 
}

ylimAqua<-max(Biggie[,AquaInd[-1]],na.rm=T)
ylimAqua<-200000
ylimVess<-max(Biggie[,EffortInd[-c(1,3)]],na.rm=T)
ylimPower<-max(Biggie[,EffortInd[-c(1,2)]],na.rm=T)/1000000
ylimStockEnh<-max(Biggie[,StockEnc[-c(1)]],na.rm=T)
xlimAll<-c(min(Biggie$Year,na.rm=T),max(Biggie$Year,na.rm=T))

for(x in 1:length(CoastalProv))
{
 tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
 SelectPlot(data=tempDat,indices=AquaInd,ylimIn=ylimAqua,xlimIn=xlimAll,log_dat=0,incol=AquaCol) 
 mtext(side=3,CoastalProv_name[x],cex=.7)
 if(x==1)
   axis(side=2,las=1,line=1)
 
 
 
 plot(tempDat[,31]~tempDat$Year,xaxt='n',yaxt='n',type='l',ylim=c(0,ylimVess),bty='n')
 if(x==1)
 {
   axis(side=2,las=1,line=1)
   legend("topleft",bty='n',lty=c(1,2),c("Vessels","Power"))
 }
 par(new=T)
 plot(tempDat[,33]/1000000~tempDat$Year,xaxt='n',yaxt='n',type='l',ylim=c(0,ylimPower),lty=2,bty='n')

  if(x==10)
   axis(side=4,las=1,line=1)
 SelectPlot(data=tempDat,indices=StockEnc,ylimIn=ylimStockEnh,xlimIn=xlimAll,log_dat=0,incol=AquaCol) 
 if(x==1)
   axis(side=2,las=1,line=1)
 axis(side=1)
}

plot.new()
legend('center',bty='n',col=AquaCol+1,legend=Aqua_names,pch=15)
plot.new()
plot.new()

 #==fishery CPUE differences and trends
Biggie$CPUE[Biggie$Provinc=="Guangxi" & Biggie$Year==1991]<-NA
boxplot(CPUE~Province,data=Biggie[!is.na(match(Biggie$Province,CoastalProv)),],ylim=c(0,2),las=2)
#cbind(Biggie$CPUE,Biggie$Year,Biggie$Province)
print(qplot(x=Biggie$Year,y=Biggie$CPUE,color=Biggie$Province,geom='line'))
print(qplot(x=Year,y=CPUE,color=Province,geom='line',data=Biggie[Biggie$Year>1984 & !is.na(match(Biggie$Province,CoastalProv)),]))
print(qplot(x=Year,y=CPUE,color=Province,geom='line',data=Biggie[Biggie$Year>1978 & !is.na(match(Biggie$Province,CoastalProv)) & Biggie$Year!=1989,]))
catch_indices<-unlist(lapply(list(colnames(Biggie)),function(x) grep("catch",x)))

mod<-lm(CPUE~all_crustaceans_area+all_shellfish_area+all_algae_area+all_fish_area+Province,data=Biggie[Biggie$Year!=1989,])
mod<-lm(CPUE~all_crustaceans_area+all_shellfish_area+all_algae_area+all_fish_area,data=Biggie)
summary(mod)
inCPUE<-scale(Biggie$CPUE)
inCrus<-scale(Biggie$all_crustaceans_area)
inShel<-scale(Biggie$all_shellfish_area)
inAlga<-scale(Biggie$all_algae_area)
inFish<-scale(Biggie$all_fish_area)

mod<-lm(inCPUE~inCrus+inShel+inAlga+inFish)
summary(mod)

inCPUE<-(Biggie$CPUE)
for(x in 1:length(CoastalProv))
 inCPUE[Biggie$Province==CoastalProv[x]]<-scale(inCPUE[Biggie$Province==CoastalProv[x]])[,1]

inCrus<-scale(Biggie$all_crustaceans_area)
inShel<-scale(Biggie$all_shellfish_area)
inAlga<-scale(Biggie$all_algae_area)
inFish<-scale(Biggie$all_fish_area)

mod<-lm(inCPUE~inCrus+inShel+inAlga+inFish)
summary(mod)

#==zhejiang catch for a given effort is higher than tianjin
#==is the aquaculture in the area related to this difference?
#==need to get gear and species type in here...
#==how to summarize species type? is gear good enough for that?
#==predict CPUE in fisheries by stuff?
#==predict catch by stuff
mod<-lm(all_fish_catch~Kilowatts+I(Kilowatts^2)+all_crustaceans_area+all_shellfish_area+all_algae_area+all_fish_area,data=Biggie)
summary(mod)
mod<-lm(all_fish_catch~Kilowatts+I(Kilowatts^2)+all_crustaceans_area+all_shellfish_area+all_algae_area+all_fish_area+Province,data=Biggie)
summary(mod)
inKil<-Biggie$Kilowatts/100000
mod<-lm(Biggie$all_fish_catch~-1+inKil+I(inKil^2))
summary(mod)

#==too simple linear model, all data
dummy<-na.omit(cbind(Biggie$Year[Biggie$Province=="Zhejiang"],Biggie$all_fish_catch[Biggie$Province=="Zhejiang"],Biggie$all_algae_area[Biggie$Province=="Zhejiang"]))
xmax<-max(inKil,na.rm=T)
par(mfrow=c(1,1))
plot(all_fish_catch~Kilowatts,data=Biggie,cex=.01,ylim=c(0,3000000),xlim=c(0,xmax))
#plot(all_fish_catch~Kilowatts,data=Biggie,cex=.01,ylim=c(0,1000000),xlim=c(0,2000000))
text(Biggie$Province,x=Biggie$Kilowatts,y=Biggie$all_fish_catch,cex=.5,col=as.numeric(Biggie$Province))
preds<-mod$coeff[1] + mod$coeff[2]*Biggie$Kilowatts + mod$coeff[3]*I(Biggie$Kilowatts^2)
preds<- mod$coeff[1]*Biggie$Kilowatts + mod$coeff[1]*I(Biggie$Kilowatts^2)
points(preds~Biggie$Kilowatts,col=2)
dummy<-seq(0,xmax,10000)
preds<-mod$coeff[1] + mod$coeff[2]*dummy + mod$coeff[3]*I(dummy^2)
lines(preds~dummy)

invar<-Biggie$Kilowatts/Biggie$Fishing_diqu
plot(Biggie$all_fish_catch~invar,cex=.01,ylim=c(0,3000000))
#plot(all_fish_catch~Kilowatts,data=Biggie,cex=.01,ylim=c(0,1000000),xlim=c(0,2000000))
text(Biggie$Province,x=invar,y=Biggie$all_fish_catch,cex=.5,col=as.numeric(Biggie$Province))

#===============================================
# Plot yield curves by province
#==============================================
layout(matrix(c(1,1,1,2),nrow=1))
par(mfrow=c(1,1),mar=c(.1,.1,.1,.1),oma=c(4,6,1,4))



#============================================
# plot different types of catch
#============================================
scaleEFf<-100000
library(RColorBrewer)
tempCol<-brewer.pal(10,"Set3")
inCol<-rep(NA,length(Biggie$Province))
for(x in 1:length(inCol))
     inCol[!is.na(match(Biggie$Province,CoastalProv[x]))]<-tempCol[x]

par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,1,4))
plot(Biggie$all_fish_catch~Biggie$Kilowatts)
plot(Biggie$all_shellfish_catch~Biggie$Kilowatts)
plot(Biggie$all_crustaceans_catch~Biggie$Kilowatts)
plot(Biggie$all_algae_catch~Biggie$Kilowatts)

#=====================================================
#
# FIGURE 3 OF THE PAPER???
# need to figure out starting values for quadratic
#======================================================
mat<-matrix(c(1,2,5,6,7,
              3,4,5,8,9),ncol=5,byrow=T)
layout(mat)
par(mar=c(.1,.1,.1,.1),oma=c(4,4,1,4))

inEff<-Biggie$Kilowatts/1000
cat_mod<-1000
dummymax<-4000
plot(Biggie$all_fish_catch/cat_mod~inEff,xaxt='n',las=1,col=inCol,pch=16,)
PlotCurves(y=Biggie$all_fish_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
PlotCurves(y=Biggie$all_fish_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Fish")

plot(Biggie$all_shellfish_catch/cat_mod~inEff,xaxt='n',las=1,col=inCol,pch=16,yaxt='n')
axis(side=4,las=1)
PlotCurves(y=Biggie$all_shellfish_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
PlotCurves(y=Biggie$all_shellfish_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Shellfish")

plot(Biggie$all_crustaceans_catch/cat_mod~inEff,las=1,col=inCol,pch=16,)
PlotCurves(y=Biggie$all_crustaceans_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
PlotCurves(y=Biggie$all_crustaceans_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Crustaceans")

plot(Biggie$all_algae_catch/cat_mod~inEff,las=1,col=inCol,pch=16,yaxt='n')
axis(side=4,las=1)
PlotCurves(y=Biggie$all_algae_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
PlotCurves(y=Biggie$all_algae_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Algae")

legend("right",CoastalProv_name,col=tempCol,pch=16,bty='n',cex=.7)
Aqua_dat<-Biggie[,c(1,8,AquaInds)]
aqua_types<-c("algae","all_fish","crustacean","shellfish")

tempCol<-brewer.pal(10,"Set3")
inCol<-rep(NA,length(Aqua_dat$Province))
for(x in 1:length(inCol))
     inCol[!is.na(match(Aqua_dat$Province,CoastalProv[x]))]<-tempCol[x]
plot.new()

plot(Aqua_dat[,7]~Aqua_dat[,3],ylim=c(0,800000),xlim=c(0,60000),xaxt='n',las=1,col=inCol,pch=16,)
PlotCurves(y=Aqua_dat[,7],x=Aqua_dat[,3],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,7],x=Aqua_dat[,3],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Algae")

plot(Aqua_dat[,10]~Aqua_dat[,6],ylim=c(0,800000),xlim=c(0,130000),yaxt='n',xaxt='n',las=1,col=inCol,pch=16,)
PlotCurves(y=Aqua_dat[,10],x=Aqua_dat[,6],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,10],x=Aqua_dat[,6],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Crustaceans")

plot(Aqua_dat[,8]~Aqua_dat[,4],ylim=c(0,300000),xlim=c(0,60000),col=inCol,pch=16,las=1)
PlotCurves(y=Aqua_dat[,8],x=Aqua_dat[,4],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,8],x=Aqua_dat[,4],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Fish")

plot(Aqua_dat[,9]~Aqua_dat[,5],ylim=c(0,300000),xlim=c(0,130000),yaxt='n',col=inCol,pch=16,)
PlotCurves(y=Aqua_dat[,9],x=Aqua_dat[,5],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,9],x=Aqua_dat[,5],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Shellfish")

#=====================================================
# FIGURE 3 OF THE PAPER???
#don't plot all curves
#======================================================
pdf("Plots/output_vs_input.pdf",height=4,width=8)
mat<-matrix(c(1,2,5,6,7,
              3,4,5,8,9),ncol=5,byrow=T)
layout(mat)
par(mar=c(.1,.1,.1,.1),oma=c(4,4,2,4))

inEff<-Biggie$Kilowatts/1000
cat_mod<-1000
dummymax<-4000
plot(Biggie$all_fish_catch/cat_mod~inEff,xaxt='n',las=1,col=inCol,pch=16)
#PlotCurves(y=Biggie$all_fish_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
#PlotCurves(y=Biggie$all_fish_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Fish")

plot(Biggie$all_shellfish_catch/cat_mod~inEff,xaxt='n',las=1,col=inCol,pch=16,yaxt='n')
axis(side=4,las=1)
#PlotCurves(y=Biggie$all_shellfish_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
#PlotCurves(y=Biggie$all_shellfish_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Shellfish")

plot(Biggie$all_crustaceans_catch/cat_mod~inEff,las=1,col=inCol,pch=16,)
#PlotCurves(y=Biggie$all_crustaceans_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
#PlotCurves(y=Biggie$all_crustaceans_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Crustaceans")

plot(Biggie$all_algae_catch/cat_mod~inEff,las=1,col=inCol,pch=16,yaxt='n')
axis(side=4,las=1)
#PlotCurves(y=Biggie$all_algae_catch/cat_mod,x=inEff,col=inCol,group=Biggie$Province,modtype="quad",dummymax=dummymax,inlty=2)
#PlotCurves(y=Biggie$all_algae_catch/cat_mod,x=inEff,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=dummymax,inlty=2)
legend("topleft",bty='n',"Algae")


Aqua_dat<-Biggie[,c(1,8,AquaInds)]
aqua_types<-c("algae","all_fish","crustacean","shellfish")

tempCol<-brewer.pal(10,"Set3")
inCol<-rep(NA,length(Aqua_dat$Province))
for(x in 1:length(inCol))
     inCol[!is.na(match(Aqua_dat$Province,CoastalProv[x]))]<-tempCol[x]
plot.new()
legend("center",CoastalProv_name,col=tempCol,pch=16,bty='n')

adjaqua<-1000
plot(Aqua_dat[,7]/adjaqua~Aqua_dat[,3],ylim=c(0,800000/adjaqua),xlim=c(0,60000),yaxt='n',xaxt='n',las=1,col=inCol,pch=16)
#PlotCurves(y=Aqua_dat[,7],x=Aqua_dat[,3],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
#PlotCurves(y=Aqua_dat[,7]/adjaqua,x=Aqua_dat[,3],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Algae")

plot(Aqua_dat[,10]/adjaqua~Aqua_dat[,6],ylim=c(0,800000/adjaqua),xlim=c(0,130000),xaxt='n',yaxt='n',las=1,col=inCol,pch=16,)
axis(side=4,las=1)
#PlotCurves(y=Aqua_dat[,10],x=Aqua_dat[,6],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
#PlotCurves(y=Aqua_dat[,10]/adjaqua,x=Aqua_dat[,6],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Crustaceans")

plot(Aqua_dat[,8]/adjaqua~Aqua_dat[,4],ylim=c(0,300000/adjaqua),xlim=c(0,60000),col=inCol,pch=16,las=1,yaxt='n')
#PlotCurves(y=Aqua_dat[,8],x=Aqua_dat[,4],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
#PlotCurves(y=Aqua_dat[,8]/adjaqua,x=Aqua_dat[,4],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Fish")

plot(Aqua_dat[,9]/adjaqua~Aqua_dat[,5],ylim=c(0,300000/adjaqua),xlim=c(0,130000),col=inCol,pch=16,yaxt='n')
axis(side=4,las=1)
#PlotCurves(y=Aqua_dat[,9],x=Aqua_dat[,5],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
#PlotCurves(y=Aqua_dat[,9]/adjaqua,x=Aqua_dat[,5],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Shellfish")

mtext(side=3,adj=.18,"Capture",outer=T,cex=.85)
mtext(side=3,adj=.82,"Culture",outer=T,cex=.85)
mtext(side=1,adj=.85,"Area (hectares)",outer=T,line=2,cex=.85)
mtext(side=1,adj=.15,"Effort (1000 kilowatts)",outer=T,line=2,cex=.85)
mtext(side=2,outer=T,line=2.8,"Catch (1000t)",cex=.85)
mtext(side=4,outer=T,line=2.8,"Production (1000t)",cex=.85)
dev.off()

#=====================================================
# FIGURE 3 OF THE PAPER???
#don't plot all curves
#======================================================
pdf("Plots/output_vs_input.pdf",height=4,width=8)
mat<-matrix(c(1,2,5,6,7,
              3,4,5,8,9),ncol=5,byrow=T)
layout(mat)
par(mar=c(.1,.1,.1,.1),oma=c(4,4,2,4))

inEff<-Biggie$Kilowatts/1000
cat_mod<-1000
dummymax<-4000
plot(Biggie$all_fish_catch/cat_mod~inEff,xaxt='n',las=1,col=inCol,pch=16,bty='n')
axis(side=2,las=1)
legend("topleft",bty='n',"Fish")

plot(Biggie$all_shellfish_catch/cat_mod~inEff,xaxt='n',las=1,col=inCol,pch=16,yaxt='n',bty='n')
axis(side=4,las=1)
legend("topleft",bty='n',"Shellfish")

plot(Biggie$all_crustaceans_catch/cat_mod~inEff,las=1,col=inCol,pch=16,bty='n')
legend("topleft",bty='n',"Crustaceans")

plot(Biggie$all_algae_catch/cat_mod~inEff,las=1,col=inCol,pch=16,yaxt='n',bty='n')
axis(side=4,las=1)
legend("topleft",bty='n',"Algae")

Aqua_dat<-Biggie[,c(1,8,AquaInds)]
aqua_types<-c("algae","all_fish","crustacean","shellfish")

tempCol<-brewer.pal(10,"Set3")
inCol<-rep(NA,length(Aqua_dat$Province))
for(x in 1:length(inCol))
     inCol[!is.na(match(Aqua_dat$Province,CoastalProv[x]))]<-tempCol[x]
plot.new()
legend("center",CoastalProv_name,col=tempCol,pch=16,bty='n')

adjaqua<-1000
plot(Aqua_dat[,7]/adjaqua~Aqua_dat[,3],ylim=c(0,800000/adjaqua),xlim=c(0,60000),yaxt='n',xaxt='n',las=1,col=inCol,pch=16,bty='n')
legend("topleft",bty='n',"Algae")

plot(Aqua_dat[,10]/adjaqua~Aqua_dat[,6],ylim=c(0,800000/adjaqua),xlim=c(0,130000),xaxt='n',yaxt='n',las=1,col=inCol,pch=16,bty='n')
axis(side=4,las=1)
legend("topleft",bty='n',"Crustaceans")

plot(Aqua_dat[,8]/adjaqua~Aqua_dat[,4],ylim=c(0,300000/adjaqua),xlim=c(0,60000),col=inCol,pch=16,las=1,yaxt='n',bty='n')
legend("topleft",bty='n',"Fish")

plot(Aqua_dat[,9]/adjaqua~Aqua_dat[,5],ylim=c(0,300000/adjaqua),xlim=c(0,130000),col=inCol,pch=16,yaxt='n',bty='n')
axis(side=4,las=1)
legend("topleft",bty='n',"Shellfish")

mtext(side=3,adj=.18,"Capture",outer=T,cex=.85)
mtext(side=3,adj=.82,"Culture",outer=T,cex=.85)
mtext(side=1,adj=.85,"Area (hectares)",outer=T,line=2,cex=.85)
mtext(side=1,adj=.15,"Effort (1000 kilowatts)",outer=T,line=2,cex=.85)
mtext(side=2,outer=T,line=2.8,"Catch (1000t)",cex=.85)
mtext(side=4,outer=T,line=2.8,"Production (1000t)",cex=.85)
dev.off()
#==========================================================================
scatter.smooth.col <-
     function (x, y = NULL, span = 2/3, degree = 1, 
               family = c("symmetric","gaussian"), xlab = NULL, ylab = NULL, ylim = range(y, pred$y,na.rm = TRUE), evaluation = 50, ..., lcol=1, llty=1, llwd=1)
     {
          xlabel <- if (!missing(x))
               deparse(substitute(x))
          ylabel <- if (!missing(y))
               deparse(substitute(y))
          xy <- xy.coords(x, y, xlabel, ylabel)
          x <- xy$x
          y <- xy$y
          xlab <- if (is.null(xlab))
               xy$xlab
          else xlab
          ylab <- if (is.null(ylab))
               xy$ylab
          else ylab
          pred <- loess.smooth(x, y, span, degree, family, evaluation)
          plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, ...)
          lines(pred$x, pred$y, col=lcol, lty=llty, lwd=llwd)
          invisible()
     }

addalpha <- function(colors, alpha=1.0) {
     r <- col2rgb(colors, alpha=T)
     # Apply alpha
     r[4,] <- alpha*255
     r <- r/255.0
     return(rgb(r[1,], r[2,], r[3,], r[4,]))
}
#pdf("Plots/prod_vs_effort.pdf",height=9,width=6)
png("Plots/prod_vs_effort.png",height=9,width=6,res=1200,units='in')
mid_wid<-3
inmat<-matrix(c(seq(1,10),rep(seq(11,20),mid_wid),rep(seq(21,30),mid_wid),seq(31,40)),nrow=10)
layout(inmat)
par(mar=c(.1,.1,.1,.3),oma=c(3,1,3,1))

bigX<-4500

incol<-addalpha(brewer.pal(5,"Set2"),alpha=.5)
line_col<-brewer.pal(5,"Set2")
in_lwd<-2

for(x in 1:length(CoastalProv))
{
     tempCatch<-Biggie[Biggie$Province==CoastalProv[x],]
     temp<-c(max(tempCatch$all_fish_catch,na.rm=T)/cat_mod,
                 max(tempCatch$all_shellfish_catch,na.rm=T)/cat_mod,
                 max(tempCatch$all_crustaceans_catch,na.rm=T)/cat_mod,
                 max(tempCatch$all_algae_catch,na.rm=T)/cat_mod)
     temp[temp=="-Inf"]<-0
     if(x==10)
     barplot(-temp,col=incol[1:4],las=1,xlim=c(-bigX,0),names.arg=c("Fish","Shellfish","Crustacean","Algae"),horiz=T,xaxt='n') else
     barplot(-temp,col=incol[1:4],xaxt='n',las=1,xlim=c(-bigX,0),horiz=T)  
     
     if(x==1)
     {
          incex<-.9
      text(y=4,x=-bigX,"Algae",pos=4,cex=incex)     
      text(y=3,x=-bigX,"Crustacean",pos=4,cex=incex)  
      text(y=2,x=-bigX,"Shellfish",pos=4,cex=incex)  
      text(y=1,x=-bigX,"Fish",pos=4,cex=incex) 
     }

}
axis(side=1,at=c(-1000,-4000),labels=c(1000,4000))
mtext(side=1,line=2,"1000 t",cex=.8)
for(x in 1:length(CoastalProv))
{
 tempCatch<-Biggie[Biggie$Province==CoastalProv[x],]
 inEff<-tempCatch$Kilowatts/1000
 cat_mod<-1000
 par(new=F)
 plot.new()
 par(new=T)
 scatter.smooth.col(scale(tempCatch$all_fish_catch/cat_mod)~inEff,yaxt='n',xaxt='n',col=incol[1],pch=16,bty='n',lcol=line_col[1],llwd=in_lwd)
 par(new=T)
 scatter.smooth.col(scale(tempCatch$all_shellfish_catch/cat_mod)~inEff,yaxt='n',xaxt='n',col=incol[2],pch=16,bty='n',lcol=line_col[2],llwd=in_lwd) 
 par(new=T)
 scatter.smooth.col(scale(tempCatch$all_crustaceans_catch/cat_mod)~inEff,yaxt='n',xaxt='n',col=incol[3],pch=16,bty='n',lcol=line_col[3],llwd=in_lwd)
 par(new=T)
 if(!is.na(tempCatch$all_algae_catch))
      scatter.smooth.col(scale(tempCatch$all_algae_catch/cat_mod)~inEff,yaxt='n',xaxt='n',col=incol[4],pch=16,bty='n',lcol=line_col[4],llwd=in_lwd)
 
 legend("topleft",bty='n',CoastalProv_name[x],cex=1.1)
}
axis(side=1)
mtext(side=1,line=2,"Effort (1000 kilowatts)",cex=.8)
 

for(x in 1:length(CoastalProv))
{
tempAqua<-Aqua_dat[Aqua_dat$Province==CoastalProv[x],]

par(new=F)
plot.new()
par(new=T) # algae
if(!is.na(tempAqua[,7]))   
 scatter.smooth.col(scale(tempAqua[,7])~tempAqua[,3],yaxt='n',xaxt='n',col=incol[1],pch=16,bty='n',lcol=line_col[1],llwd=in_lwd)
par(new=T) #crustaceans
if(!is.na(tempAqua[,10])) 
 scatter.smooth.col(scale(tempAqua[,10])~tempAqua[,6],yaxt='n',xaxt='n',col=incol[2],pch=16,bty='n',lcol=line_col[2],llwd=in_lwd) 
par(new=T) # fish
scatter.smooth.col(scale(tempAqua[,8])~tempAqua[,4],yaxt='n',xaxt='n',col=incol[3],pch=16,bty='n',lcol=line_col[3],llwd=in_lwd)
par(new=T) # shellfish
scatter.smooth.col(scale(tempAqua[,9])~tempAqua[,5],yaxt='n',xaxt='n',col=incol[4],pch=16,bty='n',lcol=line_col[4],llwd=in_lwd)   
}
axis(side=1)
mtext(side=1,line=2,"Area (hectares)",cex=.8)

for(x in 1:length(CoastalProv))
{
     tempCatch<-Aqua_dat[Aqua_dat$Province==CoastalProv[x],]
     temp<-c(max(tempCatch$all_fish_aqua,na.rm=T)/cat_mod,
             max(tempCatch$all_shellfish_aqua,na.rm=T)/cat_mod,
             max(tempCatch$all_crustaceans_aqua,na.rm=T)/cat_mod,
             max(tempCatch$all_algae_aqua,na.rm=T)/cat_mod)
     temp[temp=="-Inf"]<-0
     barplot(temp,col=incol[1:4],xaxt='n',las=1,xlim=c(0,bigX),horiz=T) 
     
     if(x==1)
     {
          # text(y=4,x=5000,"Algae",pos=2)     
          # text(y=3,x=5000,"Crustacean",pos=2)  
          # text(y=2,x=5000,"Shellfish",pos=2)  
          # text(y=1,x=5000,"Fish",pos=2)  
     }
}
axis(side=1,at=c(1000,4000),labels=c(1000,4000))
mtext(side=1,line=2,"1000 t",cex=.8)
mtext("Capture",side=3,outer=T,adj=.3)
mtext("Culture",side=3,outer=T,adj=.7)
dev.off()

pdf("Plots/province_production_curves.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col=inCol,pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
axis(side=1,las=1)
axis(side=2,las=1)

PlotCurves(x=scaledKilo,y=invar,col=inCol,group=Biggie$Province,modtype="quad",dummymax=42)
PlotCurves(x=scaledKilo,y=invar,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=42,inlty=2)
legend("topleft",CoastalProv_name,col=tempCol,pch=16,bty='n',cex=.7)

mtext(side=2,"Marine fisheries catch ",line=3.2,cex=0.85)
mtext(side=2," (1,000,000 t)",line=2.3,cex=0.85)
mtext(side=1,"Fleet power (100,000 kw)",line=2,cex=0.85)
dev.off()


pdf("Plots/total_production_curve.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col="grey",pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
PlotCurves(x=scaledKilo,y=invar,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=42,inlty=2)
axis(side=1,las=1)
axis(side=2,las=1)
mtext(side=2,"Marine fisheries catch ",line=3.2,cex=0.85)
mtext(side=2," (1,000,000 t)",line=2.3,cex=0.85)
mtext(side=1,"Fleet power (100,000 kw)",line=2,cex=0.85)
dev.off()

pdf("Plots/total_production_curve_province.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col=inCol,pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
PlotCurves(x=scaledKilo,y=invar,col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=42,inlty=2)
axis(side=1,las=1)
axis(side=2,las=1)
mtext(side=2,"Marine fisheries catch ",line=3.2,cex=0.85)
mtext(side=2," (1,000,000 t)",line=2.3,cex=0.85)
mtext(side=1,"Fleet power (100,000 kw)",line=2,cex=0.85)
dev.off()

#==============================================
# Plot yield per hectare
#===========================================
par(mfrow=c(10,4))
for(x in 1:length(CoastalProv))
{
     tempAqua<-Aqua_dat[Aqua_dat$Province==CoastalProv[x],]
     
     par(new=F)
     plot.new()
     par(new=T) # algae
     if(!is.na(tempAqua[,7]))   
          scatter.smooth.col(scale(tempAqua[,7])~tempAqua[,3],yaxt='n',xaxt='n',col=incol[1],pch=16,bty='n',lcol=line_col[1],llwd=in_lwd)
     par(new=F)
     plot.new() 
     par(new=T)#crustaceans
     if(!is.na(tempAqua[,10])) 
          scatter.smooth.col(scale(tempAqua[,10])~tempAqua[,6],yaxt='n',xaxt='n',col=incol[2],pch=16,bty='n',lcol=line_col[2],llwd=in_lwd) 
     par(new=F)
     plot.new() # fish
     par(new=T)
     scatter.smooth.col(scale(tempAqua[,8])~tempAqua[,4],yaxt='n',xaxt='n',col=incol[3],pch=16,bty='n',lcol=line_col[3],llwd=in_lwd)
     par(new=F)
     plot.new() # shellfish
     par(new=T)
     scatter.smooth.col(scale(tempAqua[,9])~tempAqua[,5],yaxt='n',xaxt='n',col=incol[4],pch=16,bty='n',lcol=line_col[4],llwd=in_lwd)   
}
axis(side=1)
mtext(side=1,line=2,"Area (hectares)",cex=.8)


allProd_vs_effort_algae<-Aqua_dat[,7]/Aqua_dat[,3]
allProd_vs_effort_crust<-Aqua_dat[,10]/Aqua_dat[,6]
allProd_vs_effort_fish<-Aqua_dat[,8]/Aqua_dat[,4]
allProd_vs_effort_shell<-Aqua_dat[,9]/Aqua_dat[,5]

pdf("Plots/Prod_per_hectare.pdf",height=7,width=7)
par(mfrow=c(10,4),mar=c(.1,3,.1,.1),oma=c(3,1,3,7))
incex<-.8
for(x in 1:length(CoastalProv))
{
     tempAqua<-Aqua_dat[Aqua_dat$Province==CoastalProv[x],]
     
     par(new=F)
     plot.new()
     par(new=T) # algae
     if(!is.na(tempAqua[,7]))   
          scatter.smooth.col((tempAqua[,7]/tempAqua[,3])~tempAqua$Year,xaxt='n',col=incol[1],
                             pch=16,bty='n',lcol=line_col[1],llwd=in_lwd,
                             ylim=c(0,max(allProd_vs_effort_algae,na.rm=T)),ylab='',las=1,cex.axis=incex)
     if(x==1)
          mtext(side=3,"Algae")

     par(new=F)
     plot.new() 
     par(new=T)#crustaceans
     if(!is.na(tempAqua[,10])) 
          scatter.smooth.col((tempAqua[,10]/tempAqua[,6])~tempAqua$Year,xaxt='n',
                             col=incol[2],pch=16,bty='n',lcol=line_col[2],llwd=in_lwd,
                             ylim=c(0,max(allProd_vs_effort_crust,na.rm=T)),ylab='',las=1,cex.axis=incex) 
     if(x==1)
          mtext(side=3,"Crustaceans")
     if(x==10)
          axis(side=1)
     par(new=F)
     plot.new() # fish
     par(new=T)
     scatter.smooth.col((tempAqua[,8]/tempAqua[,4])~tempAqua$Year,xaxt='n',
                        col=incol[3],pch=16,bty='n',lcol=line_col[3],llwd=in_lwd,
                        ylim=c(0,100),ylab='',las=1,cex.axis=incex)
     if(x==1)
          mtext(side=3,"Fish")
     if(x==10)
          axis(side=1)
     par(new=F)
     plot.new() # shellfish
     par(new=T)
     scatter.smooth.col((tempAqua[,9]/tempAqua[,5])~tempAqua$Year,xaxt='n',
                        col=incol[4],pch=16,bty='n',lcol=line_col[4],llwd=in_lwd,
                        ylim=c(0,max(allProd_vs_effort_shell,na.rm=T)),ylab='',las=1,cex.axis=incex)  
     if(x==1)
          mtext(side=3,"Shellfish")
     if(x==10)
          axis(side=1)
     mtext(side=4,CoastalProv_name[x],line=.5,las=1)
     }
mtext(outer=T,side=2,adj=.5,line=-.5,"Production per hectare")
dev.off()



names(Aqua_dat)

all_aqua_CPUE<- Aqua_dat%>%
     group_by(Year,Province) %>%
     summarise(area=sum(all_algae_area,all_fish_area,all_crustaceans_area,all_shellfish_area,na.rm=T),
               prod=sum(all_algae_aqua,all_crustacean_aqua,all_fish_aqua,all_shellfish_aqua,na.rm=T))

all_aqua_CPUE$CPUE<-all_aqua_CPUE$prod/all_aqua_CPUE$area
print(qplot(x=all_aqua_CPUE$Year,y=all_aqua_CPUE$CPUE,color=all_aqua_CPUE$Province,geom='line'))


#===================================================
# Plot aquaculture yields vs. aquaculture area
#==================================================
Aqua_dat<-Biggie[,c(1,8,AquaInds)]
aqua_types<-c("algae","all_fish","crustacean","shellfish")

tempCol<-brewer.pal(10,"Set3")
inCol<-rep(NA,length(Aqua_dat$Province))
for(x in 1:length(inCol))
     inCol[!is.na(match(Aqua_dat$Province,CoastalProv[x]))]<-tempCol[x]

plot(Aqua_dat[,7]~Aqua_dat[,3],las=1,col="grey",pch=16,ylim=c(0,800000),xlim=c(0,60000))
legend("topleft",bty='n',"Algae")
points(Aqua_dat[,8]~Aqua_dat[,4],col="grey",pch=16)
legend("topleft",bty='n',"Fish")
points(Aqua_dat[,10]~Aqua_dat[,6],las=1,col="grey",pch=16)
legend("topleft",bty='n',"Crustaceans")
points(Aqua_dat[,9]~Aqua_dat[,5],col="grey",pch=16)
points("topleft",bty='n',"Shellfish")

colnames(Aqua_dat)
par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,1,4))
plot(Aqua_dat[,7]~Aqua_dat[,3],ylim=c(0,800000),xlim=c(0,60000),xaxt='n',las=1,col="grey",pch=16,)
legend("topleft",bty='n',"Algae")
plot(Aqua_dat[,10]~Aqua_dat[,6],ylim=c(0,800000),xlim=c(0,130000),yaxt='n',xaxt='n',las=1,col="grey",pch=16,)
legend("topleft",bty='n',"Crustaceans")
plot(Aqua_dat[,8]~Aqua_dat[,4],ylim=c(0,300000),xlim=c(0,60000),col="grey",pch=16,las=1)
legend("topleft",bty='n',"Fish")
plot(Aqua_dat[,9]~Aqua_dat[,5],ylim=c(0,300000),xlim=c(0,130000),yaxt='n',col="grey",pch=16,)
legend("topleft",bty='n',"Shellfish")

par(mfrow=c(2,2),mar=c(.1,.1,.1,.1),oma=c(4,4,1,4))
plot(Aqua_dat[,7]~Aqua_dat[,3],ylim=c(0,800000),xlim=c(0,60000),xaxt='n',las=1,col=inCol,pch=16,)
PlotCurves(y=Aqua_dat[,7],x=Aqua_dat[,3],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,7],x=Aqua_dat[,3],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Algae")

plot(Aqua_dat[,10]~Aqua_dat[,6],ylim=c(0,800000),xlim=c(0,130000),yaxt='n',xaxt='n',las=1,col=inCol,pch=16,)
PlotCurves(y=Aqua_dat[,10],x=Aqua_dat[,6],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,10],x=Aqua_dat[,6],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Crustaceans")

plot(Aqua_dat[,8]~Aqua_dat[,4],ylim=c(0,300000),xlim=c(0,60000),col=inCol,pch=16,las=1)
PlotCurves(y=Aqua_dat[,8],x=Aqua_dat[,4],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,8],x=Aqua_dat[,4],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Fish")

plot(Aqua_dat[,9]~Aqua_dat[,5],ylim=c(0,300000),xlim=c(0,130000),yaxt='n',col=inCol,pch=16,)
PlotCurves(y=Aqua_dat[,9],x=Aqua_dat[,5],col=inCol,group=Aqua_dat[,1],modtype="quad",dummymax=130000,inlty=2)
PlotCurves(y=Aqua_dat[,9],x=Aqua_dat[,5],col=1,group=rep(1,length(scaledKilo)),modtype="quad",dummymax=130000,inlty=2)
legend("topleft",bty='n',"Shellfish")



pdf("Plots/total_aqua_production_curve.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col="grey",pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
invar<-Biggie$all_fish_catch/Biggie$Fishing_diqu
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf

axis(side=1,las=1)
axis(side=2,las=1)
mtext(side=2,"Marine fisheries catch ",line=3.2,cex=0.85)
mtext(side=2," (1,000,000 t)",line=2.3,cex=0.85)
mtext(side=1,"Fleet power (100,000 kw)",line=2,cex=0.85)
dev.off()





pdf("Plots/Modelfits.pdf",height=4,width=8)
for(x in 1:length(CoastalProv))
{
 par(mfrow=c(1,2),mar=c(.1,.1,.1,.1),oma=c(4,4,2,4))
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  plot(all_fish_catch~Kilowatts,data=tempDat,pch=16,ylim=c(0,max(all_fish_catch,na.rm=T)),xlim=c(0,max(Kilowatts,na.rm=T)),las=1)
  mod<-lm(all_fish_catch~-1 + Kilowatts+I(Kilowatts^2),data=tempDat)
  summary(mod)
  dummy<-seq(0,max(tempDat$Kilowatts,na.rm=T),10000)
  preds<- mod$coeff[1]*dummy + mod$coeff[2]*I(dummy^2)
  lines(preds~dummy)

  plot(resid(mod), yaxt='n')
  abline(h=0)
  mtext(outer=T,side=3,CoastalProv[x])
  
  #==find all the catch
  catchDat<-tempDat[,grep("catch",colnames(tempDat))]
  keepers<-rep(0,length(catchDat))
  for(y in 1:ncol(catchDat))
  {
    if(sum(!is.na(catchDat[,y]))>20)
      keepers[y]<-1
  }
  #==calculate 'other' fish
  #==sum between 'all_fish_catch" and "all_shellfish_catch"
  par(mfrow=c(1,2),mar=c(.1,.1,.1,.1),oma=c(4,4,2,4))
  plotDat<-cbind(tempDat$Year,catchDat[,keepers!=0])
  plotDat[,1]-apply(plotDat[,2:ncol(plotDat)],1,sum,na.rm=T)
  plot(-100,xlim=c(min(plotDat[,1],na.rm=T),max(plotDat[,1],na.rm=T)),ylim=c(0,max(plotDat,na.rm=T)),las=1,yaxt='n',xaxt='n')
  for(y in 2:ncol(plotDat))
    lines(plotDat[,y]~plotDat[,1],col=y)
  plot.new()
  legend()
  }
dev.off()

# for each model, test a line, dome, GAM
par(mfrow=c(1,4),mar=c(.1,.6,.1,.1),oma=c(4,6,1,4))
for(x in c(6,4,8,7))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
 # plot(tempDat$all_fish_catch~tempDat$Kilowatts,ylim=c(0,2500000),xlim=c(0,4000000),las=1,
  #     yaxt='n',bty='n')
  
  mod<-scatter.smooth(tempDat$all_fish_catch~tempDat$Kilowatts,ylim=c(0,2500000),xlim=c(0,4000000),las=1,
                 yaxt='n',bty='n')
  legend('bottomright',CoastalProv[x],bty='n')
  if(x==6)
    axis(side=2,las=1)
}

par(mfrow=c(1,4),mar=c(.1,.6,.1,.1),oma=c(4,6,1,4))
for(x in c(1,9,10,5))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  #plot(tempDat$all_fish_catch~tempDat$Kilowatts,,ylim=c(0,1200000),xlim=c(0,1500000),las=1,yaxt='n',bty='n')
  scatter.smooth(tempDat$all_fish_catch~tempDat$Kilowatts,ylim=c(0,1200000),xlim=c(0,1500000),las=1,
                 yaxt='n',bty='n')
  legend('bottomright',CoastalProv[x],bty='n')
  if(x==1)
    axis(side=2,las=1)
}

par(mfrow=c(1,1),mar=c(.1,.6,.1,.1),oma=c(4,6,1,4))
for(x in c(2))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  #plot(tempDat$all_fish_catch~tempDat$Kilowatts,,ylim=c(0,1200000),xlim=c(0,1500000),las=1,yaxt='n',bty='n')
  scatter.smooth(tempDat$all_fish_catch~tempDat$Kilowatts,ylim=c(0,200000),xlim=c(0,500000),las=1,
                 yaxt='n',bty='n')
  legend('bottomright',CoastalProv[x],bty='n')
  if(x==2)
    axis(side=2,las=1)
}

par(mfrow=c(1,1),mar=c(.1,.6,.1,.1),oma=c(4,6,1,4))
for(x in c(3))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  #plot(tempDat$all_fish_catch~tempDat$Kilowatts,,ylim=c(0,1200000),xlim=c(0,1500000),las=1,yaxt='n',bty='n')
  scatter.smooth(tempDat$all_fish_catch~tempDat$Kilowatts,ylim=c(0,50000),xlim=c(0,100000),las=1,
                 yaxt='n',bty='n')
  legend('bottomright',CoastalProv[x],bty='n')
  if(x==2)
    axis(side=2,las=1)
}



c(6,4,8,7,1,9,2,3,5,10)
Big_Fish<-c("Shandong","Zhejiang","Guangdong")
Med_Fish<-c("Liaoning Province","Hainan","Fujian Province")
Small_Fish<-c("Guangxi","Jiangsu Province","Hebei","Tianjin")


#===============================================================
# Fit models to the data
# Identify strategies for increased production for each province
# there needs to be an interaction term in here
# don't predict catch and effort--cpue gets around interaction between effort and area
# CPUE depends on two things--biomass and effort
# Fit ricker to the catch data with 
#================================================================
# for each model, test a line, dome, GAM
library(visreg)
par(mfrow=c(1,4),mar=c(.1,.6,.1,.1),oma=c(4,6,1,4))
gam_outs<-list(list())
gam_mod<-list(list())
var_N<-c(4,5,4,6,4,6,4,6,6,5)
pdf("Plots/Gamtests.pdf",width=9,height=4)
Biggie$All_fish_enhance_enhance[is.na(Biggie$All_fish_enhance_enhance)]<-0
Biggie$All_shrimp_enhance_enhance[is.na(Biggie$All_shrimp_enhance_enhance)]<-0
#names(Biggie)
Biggie$Year[Biggie$Province=="Jiangsu Province" & Biggie$Year==1991]<-NA

for(x in 1:length(CoastalProv))
{
 tempDat<-Biggie[Biggie$Province==CoastalProv[x]&Biggie$Year!=1989,]
 tempDat<-tempDat[!is.na(tempDat$Year),]
 #tempDat[,c(8,38,34,39,41,62,65,73)]
 for(y in c(38,34,39,41,62,65))
  for(z in 2:(nrow(tempDat)-1))
  {
     if(is.na(tempDat[z,y]))
          try(tempDat[z,y]<-mean(tempDat[z-1,y],tempDat[z+1,y]),TRUE)
  }     
 
 #if(x==2)
 #     tempDat<-tempDat[-c(27),]
 if(any(!is.na(match(x,c(3)))))
 {
      mod<-gam(CPUE~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(All_fish_enhance_enhance,k=3)+s(All_shrimp_enhance_enhance,k=3),data=tempDat,select=T)         
 }
 
 # hebei, guangxi #5
 if(any(!is.na(match(x,c(2,10)))))
 {
      mod<-gam(CPUE~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(all_shellfish_area,k=3)+s(All_fish_enhance_enhance,k=3)+s(All_shrimp_enhance_enhance,k=3),data=tempDat,select=T)         
 }
 
 # shandong, zhejiang, guangdong, hainan #6
 if(any(!is.na(match(x,c(4,6,8,9)))))
 {
 mod<-gam(CPUE~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(all_shellfish_area,k=3)+s(all_algae_area,k=3)+s(All_fish_enhance_enhance,k=3)+s(All_shrimp_enhance_enhance,k=3),data=tempDat,select=T)         
 }

 # liaoning, fujian, jiangsu #4
 if(any(!is.na(match(x,c(1,7,5)))))
 {
      mod<-gam(CPUE~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(all_shellfish_area,k=3)+s(all_algae_area,k=3),data=tempDat,select=T)         
 }
 
 par(mfrow=c(1,var_N[x]),mar=c(.1,.6,.1,.1),oma=c(4,6,1,4))
  gam_outs[[x]]<-summary(mod)  
  gam_mod[[x]]<-mod  
  gamplot<-plot(mod,residuals=TRUE,cex=4,shade.col=x+1,shade=T,yaxt='n')
    
  #visreg(mod)
  mtext(side=3,CoastalProv[x])
}
dev.off()  

terms<-NULL
for(x in 1:length(gam_outs))
 terms<-c(terms,rownames(gam_outs[[x]]$s.table))

unq_terms<-unique(terms)

pvals<-matrix(nrow=length(unq_terms),ncol=10)
rownames(pvals)<-unq_terms
for(x in 1:length(gam_outs))
 pvals[na.omit(match(rownames(gam_outs[[x]]$s.table),unq_terms)),x]<-gam_outs[[x]]$s.table[,4]   

boxplot(t(pvals),las=2)

devexpl<-NULL
for(x in 1:length(gam_outs))
     devexpl<-c(devexpl,gam_outs[[x]]$dev.expl)
hist(devexpl)

titles<-c("Crustacean","Fish","Shellfish","Algae","Fish","Crustacean")
par(mfrow=c(10,7),mar=c(.1,.1,.1,.1),oma=c(3,3,3,1))

#pdf("Plots/CPUE_aqua.pdf",height=5,width=8)
png("Plots/CPUE_aqua.png",height=5,width=8,res=1200,units='in')
inmat<-cbind(seq(1,64,7),seq(1,64,7),matrix(seq(1,70),nrow=10,byrow=T))
inmat<-cbind(seq(1,64,7),matrix(seq(1,70),nrow=10,byrow=T))
layout(inmat)
par(mar=c(.1,.1,.1,.1),oma=c(3,4,3,1))
incols<-brewer.pal(6,"Set3")
legend_ps<-c()

for(x in 1:length(CoastalProv))
{
     tempDat<-Biggie[Biggie$Province==CoastalProv[x]&Biggie$Year!=1989,]
     tempDat<-tempDat[!is.na(tempDat$Year),]
     # if(x==2)
     #      tempDat<-tempDat[-c(27),]
     # #tempDat[,c(8,38,34,39,41,62,65)]
     for(y in c(38,34,39,41,62,65))
          for(z in 2:(nrow(tempDat)-1))
          {
               if(is.na(tempDat[z,y]))
                    try(tempDat[z,y]<-mean(tempDat[z-1,y],tempDat[z+1,y]),TRUE)
          } 

     
          
     plot(tempDat$CPUE~tempDat$Year,xaxt='n',las=1,pch=16,col='grey',bty='n',xlim=c(1983,2016),cex.axis=.5)
     par(xpd=NA)
     if(x==1)
          legend(x=2008,y=1.8,pch=c(16,NA),lty=c(NA,1),col=c("grey",1),bty='n',legend=c("Obs","Pred"),cex=.8)
     par(xpd=TRUE)
     inYr<-unique(na.omit(tempDat$Year))
     if(x>=8)
          inYr<-inYr[-length(inYr)]
     lines(predict(gam_mod[[x]])~inYr)
     # if(any(!is.na(match(x,c(2,3,4,7,9)))))
     # legend('topleft',bty='n',CoastalProv_name[x],cex=.8) else
     # legend('bottomleft',bty='n',CoastalProv_name[x],cex=.8)
     mtext(side=2,CoastalProv_name[x],cex=.4,line=2)
     
     if(x==10)
      axis(side=1,line=.5)
      # takers<-which(pvals[,x]<=0.05)
      # for(w in takers)
     for(w in 1:nrow(pvals))
     {
     in_ind<-which(!is.na(pvals[,x]))
      if(!is.na(pvals[w,x]))
      {      
      if(pvals[w,x]<=0.05)
       gamplot<-plot(gam_mod[[x]],residuals=TRUE,shade.col=incols[w],shade=T,yaxt='n',select=match(w,in_ind),bty='n',xaxt='n',cex=1.5)     
      if(pvals[w,x]>0.05)
       gamplot<-plot(gam_mod[[x]],residuals=TRUE,shade.col='grey',shade=T,yaxt='n',select=match(w,in_ind),bty='n',xaxt='n',cex=1.5) 
      #plot.new()
      
      }
     if(is.na(pvals[w,x]))
      plot.new()   
     if(x==1)
      mtext(side=3,titles[w],cex=.8)
     }
      # if(length(takers)<4)
      # {
      # for(g in 1:(4-length(takers)))
      #   plot.new()
      # }   
}
#mtext(side=2,outer=T, "Catch per unit effort",line=2.3,cex=.8)
mtext(side=3,outer=T,line=1.5,adj=.5,"Aquaculture area")
mtext(side=3,outer=T,line=.5,adj=.1,"CPUE")
mtext(side=3,outer=T,line=1.5,adj=.97,"Stock enhancement")
dev.off()

  # for(x in 1:length(CoastalProv))
  # {
  # tempDat<-Biggie[Biggie$Province==CoastalProv[x],]    
  # if(sum(!is.na(tempDat$All_shrimp_enhance_enhance))>15) 
  #      print(CoastalProv[x])
  # }
  
  # crust = 10
  # shell = 9 (no tianjin)
  # fish = 10
  # algae = 7 (hebei, tianjin, guangxi)
  # fishE = 7 (liaoning, fujian, jiangsu )
  # shrimpE = 7 (liaoning, fujian, jiangsu )
  
     

#=====================================
# time series clustering
#=====================================

#==THIS ALL SEEMS PRETTY RIDICULOUS, BUT I DON'T FEEL LIKE FIXING IT
#==take colnames that have 'catch' or 'aqua' in them
#==remove those that have 'all' in them, except small yellow croaker
catchInd<-grep("catch",colnames(Biggie))
temp<-Biggie[,catchInd]
Biggie$other_crustaceans_catch<-temp[,1]-apply(temp[,2:6],1,sum,na.rm=T)
Biggie$other_fish_catch<-temp[,7]-apply(temp[,8:24],1,sum,na.rm=T)

aquaInd<-grep("aqua",colnames(Biggie))
temp<-Biggie[,aquaInd]
Biggie$other_algae_aqua<-temp[,1]-apply(temp[,2:3],1,sum,na.rm=T)
Biggie$other_crustacean_aqua<-temp[,4]-temp[,5]
Biggie$other_shellfish_aqua<-temp[,8]-apply(temp[,9:13],1,sum,na.rm=T)
Biggie$other_fish_aqua<-Biggie$all_fish_aqua

allInd<-grep("all_",colnames(Biggie))
allInd<-allInd[-c(3)]
colnames(Biggie)[allInd]

temp<-Biggie[,-c(allInd)]
catchInd<-grep("catch",colnames(temp))
aquaInd<-grep("aqua",colnames(temp))
tsDat<-temp[,c(1,7,unlist(catchInd),unlist(aquaInd))]

library(reshape)
library(abind)
asdf<-melt(tsDat,id=c('Province','Year'))
meh<-cast(asdf,variable~Year~Province,fun.aggregate=sum,na.rm=T)
hrm<-meh[,,c(3,5,6,8,9,16,19,22,27,33)]
useDat<-abind(hrm,apply(hrm,c(1,2),sum,na.rm=T))
dimnames(useDat)[[3]][11]<-"China"
dimnames(useDat)[[1]]
#==make colors
colrange       <-seq(-3,3,length.out=1000)
cols 		<-colorRampPalette(brewer.pal(11,"Spectral"))(length(colrange))

#=======================================
# plot all coastal provinces for supplementary materials
#=================================================
orig_col<-c(rep(1,25),rep('red',13))
species_names<-read.csv("species_names.csv")
par(xpd=NA)
pdf("Plots/clusters.pdf",height=7,width=8)
scale_dat<-useDat

for(x in 1:dim(useDat)[3])
{
 temp<-useDat[,,x]
 maxes<-(apply(temp,1,max,na.rm=T))/10000
 for(y in 1:nrow(temp)) 
 {
  for(z in 2:(ncol(temp)-1))
  {
   if(temp[y,z]==0)
    temp[y,z]<-(temp[y,z-1]+temp[y,z+1])/2
  }
  scale_dat[y,,x]<-scale(temp[y,])
  if(any(scale_dat[y,,x]=="NaN"))
       scale_dat[y,,x]<-0
 }
  
 dissim<-dist(scale_dat[,,x],method="euclidean")
 
 #==how are these found
 clusts<-hclust(dissim)

 mat<-matrix(c(2,2,2,1,1,
               2,2,2,1,1,
               2,2,2,1,1,
               2,2,2,1,1),ncol=5,byrow=T)
 layout(mat)
 par(mar=c(.1,.1,.1,.1),oma=c(4,4,1,12))
 plot(-100,xlim=c(1983,2016),ylim=c(-3,4*nrow(scale_dat[,,x])+3),axes=F,ylab='')
 mtext(side=3,dimnames(useDat)[[3]][x],line=-1)
 axis(side=1)
 markers<-seq(0,4*nrow(scale_dat[,,x])+3,4)

 for(q in 1:length(clusts$order))
 {
  incols<-scale_dat[clusts$order[q],,x]  
  for(w in 1:length(scale_dat[clusts$order[q],,x]))      
   incols[w]<-cols[which(abs(colrange-scale_dat[clusts$order[q],w,x])==min(abs(colrange-scale_dat[clusts$order[q],w,x])))]
  
  points(scale_dat[clusts$order[q],,x]+markers[q]~as.numeric(colnames(scale_dat)),pch=16,col=incols,cex=2)
  lines(scale_dat[clusts$order[q],,x]+markers[q]~as.numeric(colnames(scale_dat)))
  par(xpd=NA)
  #text(x=2016,y=markers[q],rownames(scale_dat[,,x])[clusts$order[q]],las=1,pos=4)
  text(x=2016,y=markers[q],paste(species_names[clusts$order[q],2]," (",signif(maxes[clusts$order[q]],2),")",sep=""),las=1,pos=4,col=orig_col[clusts$order[q]],cex=.7)
 }
  plot(clusts,yaxt='n')
}
dev.off()

#=======================================
# plot only China totals for 
#=================================================
species_names<-read.csv("species_names.csv")
scale_dat<-useDat[,,11]
temp<-scale_dat
for(y in 1:nrow(temp)) 
{
 for(z in 2:(ncol(temp)-1))
 {
  if(temp[y,z]==0)
    temp[y,z]<-(temp[y,z-1]+temp[y,z+1])/2
  }
  scale_dat[y,]<-scale(temp[y,])
}
     


#==clustering
dissim<-dist(scale_dat,method="euclidean")
clusts<-hclust(dissim)
#plot(clusts)
#pdf("Plots/clusters_all_China.pdf",height=6,width=4)
png("Plots/clusters_all_China.png",height=6,width=4,res=1200,units='in')
#==plotting     
par(mfrow=c(1,1),mar=c(.1,.1,.1,.1),oma=c(3,1.5,0,8))
maxes<-(apply(useDat[,,11],1,max,na.rm=T))/10000

#barpos<-barplot(-maxes[clusts$order],axes=FALSE,horiz=TRUE) 

plot(-100,xlim=c(1983,2016),ylim=c(0,4*nrow(scale_dat)+0),axes=F,ylab='',
     main="")
axis(side=1,cex.axis=.7)
markers<-seq(0,4*nrow(scale_dat)+3,4)
     
for(q in 1:length(clusts$order))
{
 incols<-scale_dat[clusts$order[q],]  
 for(w in 1:length(scale_dat[clusts$order[q],]))      
   incols[w]<-cols[which(abs(colrange-scale_dat[clusts$order[q],w])==min(abs(colrange-scale_dat[clusts$order[q],w])))]
          
 points(scale_dat[clusts$order[q],]+markers[q]~as.numeric(colnames(scale_dat)),pch=16,col=incols,cex=1.5)
 lines(scale_dat[clusts$order[q],]+markers[q]~as.numeric(colnames(scale_dat)))
 par(xpd=NA)
 text(x=2016,y=markers[q],paste(species_names[clusts$order[q],2]," (",signif(maxes[clusts$order[q]],2),")",sep=""),las=1,pos=4,col=orig_col[clusts$order[q]],cex=.7)
 #text(x=1983,y=markers[q],signif(maxes[clusts$order[q]],2),las=1,col=orig_col[clusts$order[q]],pos=2,cex=.7)
 }
x1<-1981.5
x2<-1981
gon_col<-'darkgrey'
polygon(x=c(x2,x2,x1,x1),y=c(149,136.5,136.5,149),border='NA',col=gon_col)
mtext(side=2,adj=.93,"Crash",line=.25,cex=.6)
polygon(x=c(x2,x2,x1,x1),y=c(93.5,135.5,135.5,93.5),border='NA',col=gon_col)
mtext(side=2,adj=.76,"Plateau",line=.25,cex=.6)
polygon(x=c(x2,x2,x1,x1),y=c(92.5,73.5,73.5,92.5),border='NA',col=gon_col)
mtext(side=2,adj=.54,"Dome",line=.25,cex=.6)
polygon(x=c(x2,x2,x1,x1),y=c(-3,72.5,72.5,-3),border='NA',col=gon_col)
mtext(side=2,adj=.25,"Increase",line=.25,cex=.6)
mtext(side=1,"Year",cex=.8,line=2)
dev.off()


#====================================
# plot sclaed CUPE
#====================================
PDO<-read.csv("C:/Users/Cody/Desktop/data.csv")
plot(-100,xlim=c(1983,2016),ylim=c(-3,3))
for(x in 1:length(CoastalProv))
{
tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
inCPUE	<-tempDat[order(tempDat$Year),]$CPUE
inYear    <-tempDat[order(tempDat$Year),]$Year

lines(scale(inCPUE)~inYear,col=x)

}

lines(scale(PDO$PDOa)~PDO$Year,lty=2,lwd=2)


plot(-100,xlim=c(-3,3),ylim=c(-3,3))
for(x in 1:length(CoastalProv))
{
     tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
     inCPUE	<-tempDat[order(tempDat$Year),]$CPUE
     inCPUE<-inCPUE[!is.na(inCPUE)]
     inYear    <-tempDat[order(tempDat$Year),]$Year
     points(scale(inCPUE)[,1]~PDO$PDOa,col=x)
}

lines(PDO$PDOw~PDO$Year,lty=2,lwd=2)
# Benford's law
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


ProdMod<-function(z,CatchData,IndexData)
{
  K<-exp(abs(z[1]))
  r<-abs(z[2])
  q<-log(abs(z[3]))
  B1<-z[4]
  predBio<-rep(0,length(IndexData))
  predBio[1]<-K*B1
  for(i in 2:length(CatchData))
  {
    predBio[i]<-predBio[i-1]+r*predBio[i-1]*(1-predBio[i-1]/K)-CatchData[i]
  }
  SSQ<-sum((q*predBio-IndexData)^2,na.rm=T)
  return(SSQ)
}

ProdModPlot<-function(z,CatchData,IndexData,plots=0)
{
  K<-exp(abs(z[1]))
  r<-abs(z[2])
  q<-log(abs(z[3]))
  B1<-z[4]
  predBio<-rep(0,length(IndexData)+1)
  predBio[1]<-K*B1
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


for(x in 1:length(CoastalProv))
{
 tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
     
inCatch	<-tempDat[order(tempDat$Year),]$all_fish_catch
inCPUE	<-tempDat[order(tempDat$Year),]$CPUE
inYear    <-tempDat[order(tempDat$Year),]$Year
plot(inCatch,type='l')
par(new=TRUE)
plot(inCPUE,col=2,type='l')
z<-c(17,.6,1.01,.8)
outs		<-nlminb(start=z,objective=ProdMod,CatchData=inCatch,IndexData=inCPUE)
}
ProdModPlot(z=outs$par,CatchData=inCatch,IndexData=inCPUE,plots=1)
write.csv(cbind(inYear,inCatch,inCPUE),"C:/Users/Cody/Desktop/data.csv")

temp<-data.frame(Year=Biggie$Year,Province=Biggie$Province,other_fish_catch=Biggie$other_fish_catch)

#==numbers for paper
othercatch<- temp%>%
     group_by(Year) %>%
     summarise(catches=sum(other_fish_catch,na.rm=T))
plot(othercatch)
othercatch$Year[which(othercatch$catches==max(othercatch$catches))]
(othercatch$catches[which(othercatch$catches==max(othercatch$catches))]-othercatch$catches[nrow(othercatch)-1])/othercatch$catches[which(othercatch$catches==max(othercatch$catches))]

#==total aqua area
temp<-data.frame(Year=Biggie$Year,
                 Province=Biggie$Province,
                 fish=Biggie$all_fish_area,
                 algae=Biggie$all_algae_area,
                 crust=Biggie$all_crustaceans_area,
                 shell=Biggie$all_shellfish_area)

all_area<- temp%>%
     group_by(Province,Year) %>%
     summarise(all=sum(fish,algae,crust,shell,na.rm=T))

all_area[all_area$Year==2016,]
sum(all_area[all_area$Year==2016,]$all,na.rm=T)

all_area[all_area$Year==1983,]
sum(all_area[all_area$Year==1983,]$all,na.rm=T)

biggest<-max(all_area$all,na.rm=T)
all_area$Province[which(all_area$all==biggest)]

#==total aqua prod
temp<-data.frame(Year=Biggie$Year,
                 Province=Biggie$Province,
                 fish=Biggie$all_fish_aqua,
                 algae=Biggie$all_algae_aqua,
                 crust=Biggie$all_crustacean_aqua,
                 shell=Biggie$all_shellfish_aqua)

all_aqua<- temp%>%
     group_by(Province,Year) %>%
     summarise(all=sum(fish,algae,crust,shell,na.rm=T))

all_aqua_prod<- temp%>%
     group_by(Year) %>%
     summarise(fish=sum(fish,na.rm=T),
               algae=sum(algae,na.rm=T),
               crust=sum(crust,na.rm=T),
               shell=sum(shell,na.rm=T),)

all_aqua_prod[all_aqua_prod$Year==2016,]

all_aqua_prod[all_aqua_prod$Year==2016,5]/sum(all_aqua_prod[all_aqua_prod$Year==2016,2:5],na.rm=T)
all_aqua_prod[all_aqua_prod$Year==2016,4]/sum(all_aqua_prod[all_aqua_prod$Year==2016,2:5],na.rm=T)
all_aqua_prod[all_aqua_prod$Year==2016,2]/sum(all_aqua_prod[all_aqua_prod$Year==2016,2:5],na.rm=T)
all_aqua_prod[all_aqua_prod$Year==2016,3]/sum(all_aqua_prod[all_aqua_prod$Year==2016,2:5],na.rm=T)

all_aqua[all_aqua$Year==2016,]
sum(all_aqua[all_aqua$Year==2016,]$all,na.rm=T)

write.csv(subset(Biggie,Province=="Zhejiang",select=c("Year","small_yellow_croaker_catch","Kilowatts")),"SYCdata.csv")

#=======================================================
# Make a GAM for all the species in catch vs. Aqua
#=======================================================
# why did you do this
# do you have no shame?!

plotters<-c(2:7,9:30)
pdf(paste("Plots/CPUE_aqua_species.pdf",sep=""),height=5,width=8)

for(w in plotters)
{
gam_outs<-list(list())
gam_mod<-list(list())
var_N<-c(4,5,4,6,4,6,4,6,6,5)
Biggie$All_fish_enhance_enhance[is.na(Biggie$All_fish_enhance_enhance)]<-0
Biggie$All_shrimp_enhance_enhance[is.na(Biggie$All_shrimp_enhance_enhance)]<-0
Biggie$Year[Biggie$Province=="Jiangsu Province" & Biggie$Year==1991]<-NA

for(x in 1:length(CoastalProv))
{
     tempDat<-Biggie[Biggie$Province==CoastalProv[x]&Biggie$Year!=1989,]
     tempDat<-tempDat[!is.na(tempDat$Year),]
     #tempDat[,c(8,38,34,39,41,62,65,73)]
     for(y in c(38,34,39,41,62,65))
          for(z in 2:(nrow(tempDat)-1))
          {
               if(is.na(tempDat[z,y]))
                    try(tempDat[z,y]<-mean(tempDat[z-1,y],tempDat[z+1,y]),TRUE)
          }     
     
     if(sum(!is.na(tempDat[,w]))>14)
     {
     if(any(!is.na(match(x,c(3)))))
     {
          mod<-gam(tempDat[,w]/tempDat$Kilowatts~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(All_fish_enhance_enhance,k=3)+s(All_shrimp_enhance_enhance,k=3),data=tempDat,select=T)         
     }
     
     # hebei, guangxi #5
     if(any(!is.na(match(x,c(2,10)))))
     {
          mod<-gam(tempDat[,w]/tempDat$Kilowatts~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(all_shellfish_area,k=3)+s(All_fish_enhance_enhance,k=3)+s(All_shrimp_enhance_enhance,k=3),data=tempDat,select=T)         
     }
     
     # shandong, zhejiang, guangdong, hainan #6
     if(any(!is.na(match(x,c(4,6,8,9)))))
     {
          mod<-gam(tempDat[,w]/tempDat$Kilowatts~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(all_shellfish_area,k=3)+s(all_algae_area,k=3)+s(All_fish_enhance_enhance,k=3)+s(All_shrimp_enhance_enhance,k=3),data=tempDat,select=T)         
     }
     
     # liaoning, fujian, jiangsu #4
     if(any(!is.na(match(x,c(1,7,5)))))
     {
          mod<-gam(tempDat[,w]/tempDat$Kilowatts~s(all_crustaceans_area,k=3)+s(all_fish_area,k=3)+s(all_shellfish_area,k=3)+s(all_algae_area,k=3),data=tempDat,select=T)         
     }
     
     gam_outs[[x]]<-summary(mod)  
     gam_mod[[x]]<-mod  
     #gamplot<-plot(mod,residuals=TRUE,cex=4,shade.col=x+1,shade=T,yaxt='n')
     }  
 }

terms<-NULL
for(x in 1:length(gam_outs))
     terms<-c(terms,rownames(gam_outs[[x]]$s.table))

unq_terms<-unique(terms)

pvals<-matrix(nrow=length(unq_terms),ncol=10)
rownames(pvals)<-unq_terms
for(x in 1:length(gam_outs))
     pvals[na.omit(match(rownames(gam_outs[[x]]$s.table),unq_terms)),x]<-gam_outs[[x]]$s.table[,4]   

titles<-c("Crustacean","Fish","Shellfish","Algae","Fish","Crustacean")

   
    inmat<-cbind(seq(1,64,7),matrix(seq(1,70),nrow=10,byrow=T))
    layout(inmat)
    par(mar=c(.1,.1,.1,.1),oma=c(3,4,3,1))
    incols<-brewer.pal(6,"Set3")

for(x in 1:length(CoastalProv))
{
     tempDat<-Biggie[Biggie$Province==CoastalProv[x]&Biggie$Year!=1989,]
     tempDat<-tempDat[!is.na(tempDat$Year),]
     # if(x==2)
     #      tempDat<-tempDat[-c(27),]
     # #tempDat[,c(8,38,34,39,41,62,65)]
     for(y in c(38,34,39,41,62,65))
          for(z in 2:(nrow(tempDat)-1))
          {
               if(is.na(tempDat[z,y]))
                    try(tempDat[z,y]<-mean(tempDat[z-1,y],tempDat[z+1,y]),TRUE)
          } 
     
     
     if(sum(!is.na(tempDat[,w]))>5)
      {plot(tempDat[,w]/tempDat$Kilowatts~tempDat$Year,xaxt='n',las=1,pch=16,col='grey',bty='n',xlim=c(1983,2016),cex.axis=.5)
      }else
      plot.new()
     mtext(side=2,CoastalProv_name[x],cex=.4,line=2)
     if(x==10)
          axis(side=1,line=.5)
     
     par(xpd=NA)
     if(x==1)
          legend(x=2008,y=1.8,pch=c(16,NA),lty=c(NA,1),col=c("grey",1),bty='n',legend=c("Obs","Pred"),cex=.8)
     par(xpd=TRUE)
     inYr<-unique(na.omit(tempDat$Year))
     
     if(sum(!is.na(tempDat[,w]))>14)
     {     
      in_preds<-predict(gam_mod[[x]])
      if(length(inYr)>length(in_preds))
      inYr<-inYr[1:length(in_preds)]
      lines(in_preds~inYr)

     for(q in 1:nrow(pvals))
     {
          in_ind<-which(!is.na(pvals[,x]))
          if(!is.na(pvals[q,x]))
          {      
               if(pvals[q,x]<=0.05)
                    gamplot<-plot(gam_mod[[x]],residuals=TRUE,shade.col=incols[q],shade=T,yaxt='n',select=match(q,in_ind),bty='n',xaxt='n')     
               if(pvals[q,x]>0.05)
                    gamplot<-plot(gam_mod[[x]],residuals=TRUE,shade.col='grey',shade=T,yaxt='n',select=match(q,in_ind),bty='n',xaxt='n') 
          }
          if(is.na(pvals[q,x]))
               plot.new()   
          if(x==1)
               mtext(side=3,titles[q],cex=.8)
     }
     }else
     for(x in 1:6)
          plot.new()
}
mtext(side=3,outer=T,line=1.5,adj=.5,"Aquaculture area")
mtext(side=3,outer=T,line=.5,adj=.1,"CPUE")
mtext(side=3,outer=T,line=1.75,adj=.05,colnames(Biggie)[w])
mtext(side=3,outer=T,line=1.5,adj=.97,"Stock enhancement")
}
}
dev.off()
