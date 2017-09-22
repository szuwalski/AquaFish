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
Biggie<-join(Biggie,t5,by=c('Province','Year'))
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


#==========================================================================


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

allInd<-grep("all",colnames(Biggie))
allInd<-allInd[-c(3,11,17)]
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

#==make colors
colrange       <-seq(-3,3,length.out=1000)
cols 		<-colorRampPalette(brewer.pal(11,"Spectral"))(length(colrange))

#=======================================
# plot all coastal provinces for supplementary materials
#=================================================
#==split aquaculture and fisheries
orig_col<-c(rep(1,25),rep('red',11))
species_names<-read.csv("species_names.csv")

pdf("Plots/clusters_plain.pdf",height=9,width=6)
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

 # mat<-matrix(c(2,2,2,1,1,
 #               2,2,2,1,1,
 #               2,2,2,1,1,
 #               2,2,2,1,1),ncol=5,byrow=T)
 # layout(mat)
 par(mar=c(.1,.1,.1,.1),oma=c(4,4,1,12))
 plot(-100,xlim=c(1983,2016),ylim=c(-3,4*nrow(scale_dat[,,x])+3),axes=F,ylab='',
      main=dimnames(useDat)[[3]][x])
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
  #plot(clusts,yaxt='n')
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
pdf("Plots/clusters_all_China.pdf",height=6,width=4)
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
polygon(x=c(x2,x2,x1,x1),y=c(142,134.5,134.5,142),border='NA',col=gon_col)
mtext(side=2,adj=.965,"Decrease",line=.25,cex=.6)
polygon(x=c(x2,x2,x1,x1),y=c(109.5,132.5,132.5,109.5),border='NA',col=gon_col)
mtext(side=2,adj=.84,"Dome",line=.25,cex=.6)
polygon(x=c(x2,x2,x1,x1),y=c(107.5,70.5,70.5,107.5),border='NA',col=gon_col)
mtext(side=2,adj=.65,"Plateau",line=.25,cex=.6)
polygon(x=c(x2,x2,x1,x1),y=c(-3,68.5,68.5,-3),border='NA',col=gon_col)
mtext(side=2,adj=.27,"Increase",line=.25,cex=.6)
mtext(side=1,"Year",cex=.8,line=2)
dev.off()



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
