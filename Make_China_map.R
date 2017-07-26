
library(maps)
library(mapdata)
library(maptools)
library(ggplot2)
library(Hmisc)
library(stringr)
library(RColorBrewer)
library(openxlsx)
library(xlsx)
library(readxl)
library(plotrix)

source("Functions.R")

getSheetNames('Data/mariculture_fish_province.xlsx')
temp<-loadWorkbook('Data/mariculture_fish_province.xlsx')
Years<-names(getSheets(temp))

#==Pull aquaculture data
data<-list(list())
AquaFish<-list(list())
for(x in 1:length(Years))
{
  data[[x]]   <-read_excel('Data/mariculture_fish_province.xlsx',sheet=x)
  data[[x]]   <-cbind(data[[x]],Year=rep(Years[x],nrow(data[[x]])))
  AquaFish[[x]]<-data[[x]][,match(c("Province","all fish","Year"),names(data[[x]]))]
}

FlatAquaFish<-do.call(rbind.data.frame, AquaFish)

#==ugh, names are different with different data sets, spaces added...bleh
OrigName<-c("Liaoning Province","Jiangsu Province","Shanxi Province","Sichuan Province","Jiangxi Province","Gansu Province","Fujian Province")
ChangeName<-c("Liaoning","Jiangsu","Shanxi","Sichuan","Jiangxi","Gansu","Fujian")
for(x in 1:length(ChangeName))
  FlatAquaFish$Province[FlatAquaFish$Province==OrigName[x]]<-ChangeName[x]

FlatAquaFish$'all fish'<-gsub(" ","",FlatAquaFish$'all fish')
FlatAquaFish$Year<-gsub(" ","",FlatAquaFish$Year)
FlatAquaFish$'all fish'<-gsub(",","",FlatAquaFish$'all fish')
FlatAquaFish$Year<-gsub(",","",FlatAquaFish$Year)

#==============================
#==pull fishery data. sloppily. BUT IT'S PULLED.
#=============================
#==this just looks at 'all fish', to look species by species, 
#==other data files will need to be pulled
temp1<-loadWorkbook('Data/fishing_fish1_province.xlsx')
Years<-names(getSheets(temp1))

data1<-list(list())
FishFish<-list(list())

for(x in 1:length(Years))
{
  data1[[x]]   <-read_excel('Data/fishing_fish1_province.xlsx',sheet=x)
  data1[[x]]   <-cbind(data1[[x]],Year=rep(Years[x],nrow(data1[[x]])))
  FishFish[[x]]<-data1[[x]][,match(c("Province","all fish","Year"),names(data1[[x]]))] 
}

FlatFishFish<-do.call(rbind.data.frame, FishFish)

#==ugh, names are different with different data sets, spaces added...bleh
OrigName<-c("Liaoning Province","Jiangsu Province","Shanxi Province","Sichuan Province","Jiangxi Province","Gansu Province","Fujian Province")
ChangeName<-c("Liaoning","Jiangsu","Shanxi","Sichuan","Jiangxi","Gansu","Fujian")
for(x in 1:length(ChangeName))
  FlatFishFish$Province[FlatFishFish$Province==OrigName[x]]<-ChangeName[x]

FlatFishFish$'all fish'<-gsub(" ","",FlatFishFish$'all fish')
FlatFishFish$Year<-gsub(" ","",FlatFishFish$Year)
FlatFishFish$'all fish'<-gsub(",","",FlatFishFish$'all fish')
FlatFishFish$Year<-gsub(",","",FlatFishFish$Year)


#==pull China shape files
china_map1 <- readShapePoly("CHN_adm_shp/CHN_adm1.shp")
china_map2 <- china_map1@data
china_map3 <- fortify(china_map1) ## fortify() is a function in the package of ggplot2, we will talk about it later
china_map2$NAME <- iconv(china_map2$NAME, from = 'GBK')

TotalProdProvince2016<-cbind(as.numeric(FlatAquaFish$'all fish'[FlatAquaFish$Year=='2016'])+
  as.numeric(FlatFishFish$'all fish'[FlatFishFish$Year=='2016'],na.rm=T),
  FlatFishFish[FlatFishFish$Year=='2016',]$Province)

#FlatFishFish[FlatFishFish$Year=='2016',]
#FlatAquaFish[FlatAquaFish$Year=='2016',]

#==set up color schemes==
CoastalID<-c(4,6,7,9,10,15,18,23,27,31)
LabelX<-rep(0,length(CoastalID))
LabelY<-rep(0,length(CoastalID))
Label<-rep(0,length(CoastalID))

for(x in 1:length(CoastalID))
{
  LabelX[x]<-china_map1@polygons[[CoastalID[x]]]@labpt[1]
  LabelY[x]<-china_map1@polygons[[CoastalID[x]]]@labpt[2]
  Label[x]<-as.character(china_map1@data$NAME_1[CoastalID[x]])
}

#==make colors
colrange<-seq(min(log(as.numeric(FlatAquaFish$'all fish')),na.rm=T),
              max(log(as.numeric(TotalProdProvince2016)),na.rm=T),
              length.out=1000)
#cols 		<-colorRampPalette(c("purple","blue","green","yellow","orange","red"))(length(colrange))
cols 		<-colorRampPalette(brewer.pal(11,"Spectral"))(length(colrange))

inCols<-rep('lightgrey',31)


for(i in 1:length(CoastalID))
{
  PassInd<-match(Label[i],TotalProdProvince2016[,2])
  try(inCols[CoastalID[i]] <- cols[which(abs(colrange-log(as.numeric(TotalProdProvince2016[PassInd,1]))) == 
                                  min(abs(colrange-log(as.numeric(TotalProdProvince2016[PassInd,1])))))] )
}





#dev.new(width=8,height=5)
pdf("ChinaFishProduction.pdf",height=6,width=8)
layout(matrix(c(1,2,2,3),ncol=4))
xIn<-c(seq(1,length(CoastalID)),rep(rep(length(CoastalID)+1,length(CoastalID)),2),seq(length(CoastalID)+2,length(CoastalID)+1+length(CoastalID)))
layout(matrix(xIn,ncol=4))
par(mar=c(.3,.3,.3,.3),oma=c(2,1,3,1))

OrderedID<-c(7,5,9,8,6,10,1,2,4,3)

Years<-seq(1986,2016)

#==plot aquaculture here

for(y in 1:length(CoastalID))
{
  temp<-which(FlatAquaFish$Province==as.character(Label[OrderedID[y]]))
  
  #  why do you do this to me R??? I thought we were friends.
  useCol<-rep(1,sum(temp,na.rm=T))
  makePretty<-1
  if(makePretty>0)
  {

  for(i in 1:length(useCol))
    try(useCol[i] <- cols[which(abs(colrange-log(as.numeric(FlatAquaFish$'all fish'[temp][i]))) == 
                                  min(abs(colrange-log(as.numeric(FlatAquaFish$'all fish'[temp][i])))))] ,silent=T)
  }
  plot(FlatAquaFish$'all fish'[temp]~FlatAquaFish$Year[temp],type='l',bty='n',xaxt='n',yaxt='n',
       ylim=c(0,1.1*max(as.numeric(FlatAquaFish$'all fish'[temp]),na.rm=T)),lwd=1,col=useCol)
  points(FlatAquaFish$'all fish'[temp]~FlatAquaFish$Year[temp],pch=16,cex=1.5,col=useCol)

  #cbind(FlatAquaFish$'all fish'[temp],FlatAquaFish$Year[temp],useCol)
  
  legend("topleft",Label[OrderedID[y]],bty='n')
  if(y==length(CoastalID))
    axis(side=1)
}

plot(china_map1, col=inCols,xlim=c(108,123),ylim=c(14,42))

#==pie charts for aquaculture
# province,algae,other,total,fish,crustacean,shrimp,crab,shellfish
all_area_ind<-c(1,2,11,19,20,21,22,27,31)
plot_aqua<-aqua_area_by_province[aqua_area_by_province$Year==2016,all_area_ind]
plot_aqua<-plot_aqua[-4,] # get rid of shanghai
in_radius<-sqrt(plot_aqua$`total area`/3.141596)

plot_aqua$Province[3]<-"Liaoning"
plot_aqua$Province[4]<-"Jiangsu"
plot_aqua$Province[6]<-"Fujian"

Offset<-c( 0,0,  #Fujian
           .5,.2,  #Guangdong
           0,0,  #Guangxi
           0,0,  #Hainan
           -1,-1,  #Hebei
           .35,.5,  #Jiangsu
           0,0,	#Liaoning
           0,0,	#Shandong
           0,0,	#Tianjin
           0,0)	#Zhejiang
inOff<-matrix(Offset,ncol=2,byrow=T)

for(z in 1:length(Offset))
{     
temp<-plot_aqua[which(plot_aqua$Province==as.character(Label[OrderedID[z]])),]
floating.pie(xpos=LabelX[1],ypos=LabelY[1],radius=1,x=plot_aqua[z,])

}


Offset<-c( 0,0,  #Fujian
           .5,.2,  #Guangdong
           0,0,  #Guangxi
           0,0,  #Hainan
           -1,-1,  #Hebei
           .35,.5,  #Jiangsu
           0,0,	#Liaoning
           0,0,	#Shandong
           0,0,	#Tianjin
           0,0)	#Zhejiang
inOff<-matrix(Offset,ncol=2,byrow=T)

text(x=LabelX+inOff[,1],y=LabelY+inOff[,2],Label,cex=1)
par(xpd=NA)
text(x=116,y=16,"Marine fish production (t)")
color.legend2(109,13,123,14.5,rect.col=cols,
              #legend=round(exp(colrange[seq(1,length(colrange),length.out=6)])))
              legend=c(10,100,1000,15000,150000,2000000))


#==plot fisheries here
for(y in 1:length(CoastalID))
{
  temp<-which(FlatFishFish$Province==as.character(Label[OrderedID[y]]))
  
  #  why do you do this to me R??? I thought we were friends.
  useCol<-rep(1,sum(temp,na.rm=T))
  makePretty<-1
  if(makePretty>0)
  {
    
    for(i in 1:length(useCol))
      try(useCol[i] <- cols[which(abs(colrange-log(as.numeric(FlatFishFish$'all fish'[temp][i]))) == 
                                    min(abs(colrange-log(as.numeric(FlatFishFish$'all fish'[temp][i])))))] ,silent=T)
  }
  plot(FlatFishFish$'all fish'[temp]~FlatFishFish$Year[temp],type='l',bty='n',xaxt='n',yaxt='n',
       ylim=c(0,max(as.numeric(FlatFishFish$'all fish'[temp]),na.rm=T)*1.1),lwd=1,col=useCol)
  points(FlatFishFish$'all fish'[temp]~FlatFishFish$Year[temp],pch=16,cex=1.5,col=useCol)
  
  #cbind(FlatAquaFish$'all fish'[temp],FlatAquaFish$Year[temp],useCol)
  
  #legend("topleft",Label[OrderedID[y]],bty='n')
  if(y==length(CoastalID))
    axis(side=1)
}

mtext(outer=T,adj=.1,"Aquaculture",side=3)
mtext(outer=T,adj=.9,"Fisheries",side=3)
mtext(outer=T,adj=.5,"China's coastal provinces",side=3,cex=1.1)
dev.off()
