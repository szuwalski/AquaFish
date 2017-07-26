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

fish_effort_by_province<-CleanData(temp)
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

GlobalEffort<-read.csv("Data_aux/Global fleet size.csv")
colnames(GlobalEffort)
print(qplot(x=Year,y=log10(Value),color=Country,data=GlobalEffort[GlobalEffort$UNIT=="NU",],geom='line'))
print(qplot(x=Year,y=log10(Value),color=Country,data=GlobalEffort[GlobalEffort$UNIT=="GT",],geom='line'))

#===================================
#==PULL STOCK ENHANCEMENT DATA====
#===================================
stock_enhance <-PullData(input=filelist[14])
temp<-ldply(stock_enhance)
stock_enhance_by_province<-CleanData(temp)
PlotData(input=stock_enhance_by_province,filename="stock_enhancement")



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
Biggie<-join(Biggie,t4,by=c('Province','Year'))
for(x in 1:length(colnames(Biggie)))
  colnames(Biggie)[x]<-gsub(" ","_",colnames(Biggie)[x]) 

Biggie$Province<-as.factor(Biggie$Province)
Biggie$CPUE<-Biggie$`all_fish_catch`/Biggie$Kilowatts

PlotData(input=Biggie,filename="data_for_analysis")

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
boxplot(CPUE~Province,data=Biggie[!is.na(match(Biggie$Province,CoastalProv)),],ylim=c(0,2),las=2)
cbind(Biggie$CPUE,Biggie$Year,Biggie$Province)
print(qplot(x=Biggie$Year,y=Biggie$CPUE,color=Biggie$Province,geom='line'))
print(qplot(x=Year,y=CPUE,color=Province,geom='line',data=Biggie[Biggie$Year>1984 & !is.na(match(Biggie$Province,CoastalProv)),]))
catch_indices<-unlist(lapply(list(colnames(Biggie)),function(x) grep("catch",x)))

#==zhejiang catch for a given effort is higher than tianjin
#==is the aquaculture in the area related to this difference?
#==need to get gear and species type in here...
#==how to summarize species type? is gear good enough for that?
#==predict CPUE in fisheries by stuff?
#==predict catch by stuff
mod<-lm(all_fish_catch~Kilowatts+I(Kilowatts^2)+all_crustaceans_area+all_shellfish_area+all_algae_area+all_fish_area,data=Biggie)
summary(mod)
inKil<-Biggie$Kilowatts/100000
mod<-lm(Biggie$all_fish_catch~-1+inKil+I(inKil^2))
summary(mod)

#==too simple linear model, all data
dummy<-na.omit(cbind(Biggie$Year[Biggie$Province=="Zhejiang"],Biggie$all_fish_catch[Biggie$Province=="Zhejiang"],Biggie$all_algae_area[Biggie$Province=="Zhejiang"]))
xmax<-6000000
par(mfrow=c(1,1))
plot(all_fish_catch~Kilowatts,data=Biggie,cex=.01,ylim=c(0,3000000),xlim=c(0,xmax))
#plot(all_fish_catch~Kilowatts,data=Biggie,cex=.01,ylim=c(0,1000000),xlim=c(0,2000000))
text(Biggie$Province,x=Biggie$Kilowatts,y=Biggie$all_fish_catch,cex=.5,col=as.numeric(Biggie$Province))
preds<-mod$coeff[1] + mod$coeff[2]*Biggie$Kilowatts + mod$coeff[3]*I(Biggie$Kilowatts^2)
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

Ricker<-function(x,inY,inX)
{
  alpha		<-x[1]
  beta		<-x[2]
  sigma		<-abs(x[3])
  Pred		<-inX*exp(alpha-beta*inX)
  loglike	<-log(1/(sqrt(2*3.141598*sigma^2))) - ((log(Pred)-log(inY))^2)/(2*sigma^2) 
  return(sum(-1*loglike,na.rm=TRUE))
}

BevHolt<-function(x,inY,inX)
{
  #x<-x1
  alpha		<-abs(x[1])
  beta		<-abs(x[2])
  sigma		<-abs(x[3])
  Pred		<-alpha*inX/(1+(inX/beta))
  loglike	<-log(1/(sqrt(2*3.141598*sigma^2))) - ((log(Pred)-log(inY))^2)/(2*sigma^2) 
  SSQ<-(((Pred)-(inY))^2)
  return(sum(SSQ,na.rm=T))
}

Quadratic<-function(x,inY,inX)
{
  alpha		<-abs(x[1])
  beta		<-abs(x[2])
  Pred		<- -1*alpha*inX^2 + inX*beta
  SSQ<-(((Pred)-(inY))^2)
  return(sum(SSQ,na.rm=T))
}

scaleEFf<-100000
library(RColorBrewer)
tempCol<-brewer.pal(10,"Set3")
inCol<-rep(NA,length(Biggie$Province))
for(x in 1:length(inCol))
  inCol[!is.na(match(Biggie$Province,CoastalProv[x]))]<-tempCol[x]

pdf("Plots/province_production_curves.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
invar<-Biggie$all_fish_catch/Biggie$Fishing_diqu
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col=inCol,pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
axis(side=1,las=1)
axis(side=2,las=1)
modtype<-"quad"

for(x in 1:length(CoastalProv))
{
  tempDat<-Biggie[Biggie$Province==CoastalProv[x],]
  tempDat$Kilowatts<-tempDat$Kilowatts/scaleEFf
  invar<-tempDat$all_fish_catch/tempDat$Fishing_diqu
  invar<-tempDat$all_fish_catch/1000000
  dummy<-seq(0,max(Biggie$Kilowatts/scaleEFf,na.rm=T),max(Biggie$Kilowatts/scaleEFf,na.rm=T)/20000)
  dummy<-seq(0,24+max(Biggie$Kilowatts/scaleEFf,na.rm=T),max(Biggie$Kilowatts/scaleEFf,na.rm=T)/20000)
  
  #==quadratic model
  if(modtype=="quad")
  { 
  #mod<-lm(invar~-1 + tempDat$Kilowatts + I(tempDat$Kilowatts^2))
  #preds<- mod$coeff[1]*dummy + mod$coeff[2]*I(dummy^2)
  #preds[preds<0]<-NA
  #lines(preds~dummy,col=tempCol[x],lwd=2)
  #summary(mod)
  
  x1		<-c(1,1)
  outs		<-optim(x1,Quadratic,inY=invar,inX=tempDat$Kilowatts)
  x1		<-outs$par
  outs		<-optim(x1,Quadratic,inY=invar,inX=tempDat$Kilowatts)
  x1		<-outs$par
  outs		<-optim(x1,Quadratic,inY=invar,inX=tempDat$Kilowatts)
  alpha   <-abs(outs$par[1])
  beta    <-abs(outs$par[2])
  preds   <- -1*alpha*dummy^2 + dummy*beta 
  preds[preds<0]<-NA
  lines(preds~dummy,col=tempCol[x],lwd=2)
  }

  #==Michalis menten
  if(modtype=="MM")
  {  
    #plot(invar~tempDat$Kilowatts,col=tempCol[x],ylim=c(0,3))
    #Stx		<-srStarts(invar~tempDat$Kilowatts,type="BevertonHolt",param=2)
    #x1		<-c(Stx$a,Stx$p,1)
    x1		<-c(.25,200,1)
    outs		<-optim(x1,BevHolt,inY=invar,inX=tempDat$Kilowatts)
    x1		<-outs$par
    outs		<-optim(x1,BevHolt,inY=invar,inX=tempDat$Kilowatts)
    x1		<-outs$par
    outs		<-optim(x1,BevHolt,inY=invar,inX=tempDat$Kilowatts)
    alpha   <-outs$par[1]
    beta    <-outs$par[2]
    #alpha<-.25
    #beta<-200
    preds   <-(alpha*dummy)/(1+(dummy/beta)) 
    preds[preds<0]<-NA
    lines(preds~dummy,col=tempCol[x],lwd=2)
  }  
  
  #==Ricker
  if(modtype=="Rick")
  {
    Stx1<-srStarts(invar~tempDat$Kilowatts,type="Ricker",param=2)
    x1		<-c(Stx1$a,Stx1$b,1)
    outs1		<-optim(x1,Ricker,inY=invar,inX=tempDat$Kilowatts)
    x1		<-outs1$par
    outs1		<-optim(x1,Ricker,inY=invar,inX=tempDat$Kilowatts)
    x1		<-outs1$par
    outs1		<-optim(x1,Ricker,inY=invar,inX=tempDat$Kilowatts)

    alpha<-outs1$par[1]
    beta<-outs1$par[2]
    preds<- dummy*exp(alpha-beta*dummy)
    preds[preds<0]<-NA
    lines(preds~dummy,col=tempCol[x],lwd=2)
  }
}
#plot.new()
legend("topleft",CoastalProv_name,col=tempCol,pch=16,bty='n',cex=.7)

invar<-Biggie$all_fish_catch/Biggie$Fishing_diqu
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
x1		<-c(1,1)
x1		<-c(10,100000)
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
x1		<-outs$par
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
x1		<-outs$par
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
alpha   <-abs(outs$par[1])
beta    <-abs(outs$par[2])
preds   <- -1*alpha*(dummy^2) + dummy*beta 
preds[preds<0]<-NA
lines(preds~dummy,col=1,lwd=2,lty=2)

axis(side=1,las=1)
axis(side=2,las=1)
mtext(side=2,"Marine fisheries catch ",line=3.2,cex=0.85)
mtext(side=2," (1,000,000 t)",line=2.3,cex=0.85)
mtext(side=1,"Fleet power (100,000 kw)",line=2,cex=0.85)
dev.off()


pdf("Plots/total_production_curve.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col="grey",pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
invar<-Biggie$all_fish_catch/Biggie$Fishing_diqu
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
x1		<-c(1,1)
x1		<-c(10,100000)
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
x1		<-outs$par
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
x1		<-outs$par
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
alpha   <-abs(outs$par[1])
beta    <-abs(outs$par[2])
preds   <- -1*alpha*(dummy^2) + dummy*beta 
preds[preds<0]<-NA
lines(preds~dummy,col=1,lwd=2,lty=2)
axis(side=1,las=1)
axis(side=2,las=1)
mtext(side=2,"Marine fisheries catch ",line=3.2,cex=0.85)
mtext(side=2," (1,000,000 t)",line=2.3,cex=0.85)
mtext(side=1,"Fleet power (100,000 kw)",line=2,cex=0.85)
dev.off()

pdf("Plots/total_production_curve_province.pdf",height=4,width=6.5)
par(mar=c(3.5,5,1,1))
plot(invar~scaledKilo,ylim=c(0,max(invar,na.rm=T)),col=inCol,pch=16,las=1,xlim=c(0,45),yaxt='n',xaxt='n',bty='n',ylab='',xlab='')
invar<-Biggie$all_fish_catch/Biggie$Fishing_diqu
invar<-Biggie$all_fish_catch/1000000
scaledKilo<-(Biggie$Kilowatts)/scaleEFf
x1		<-c(1,1)
x1		<-c(10,100000)
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
x1		<-outs$par
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
x1		<-outs$par
outs		<-optim(x1,Quadratic,inY=invar,inX=scaledKilo)
alpha   <-abs(outs$par[1])
beta    <-abs(outs$par[2])
preds   <- -1*alpha*(dummy^2) + dummy*beta 
preds[preds<0]<-NA
lines(preds~dummy,col=1,lwd=2,lty=2)
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



# time series clustering
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
