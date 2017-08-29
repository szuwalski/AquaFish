#==Have to run 'data cleaning.R' first
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

dimnames(useDat)[[3]][1]<-"Fujian"
dimnames(useDat)[[3]][6]<-"Jiangsu"
dimnames(useDat)[[3]][7]<-"Liaoning"

#==pull China shape files
china_map1 <- readShapePoly("CHN_adm_shp/CHN_adm1.shp")
china_map2 <- china_map1@data
china_map3 <- fortify(china_map1) ## fortify() is a function in the package of ggplot2, we will talk about it later
china_map2$NAME <- iconv(china_map2$NAME, from = 'GBK')

TotalProdProvince2016<-apply(useDat[,dimnames(useDat)[[2]]=="2016",],2,sum)

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
colrange  <-seq(0,max(log(as.numeric(TotalProdProvince2016[1:10])),na.rm=T),
              length.out=1000)
cols 	<-colorRampPalette(brewer.pal(11,"Spectral"))(length(colrange))

inCols<-rep('lightgrey',31)

for(i in 1:length(CoastalID))
{
     PassInd<-match(Label[i],names(TotalProdProvince2016))
     try(inCols[CoastalID[i]] <- cols[which(abs(colrange-log(as.numeric(TotalProdProvince2016[PassInd]))) == 
                                                 min(abs(colrange-log(as.numeric(TotalProdProvince2016[PassInd])))))] )
}



#dev.new(width=8,height=5)
pdf("Plots/ChinaSeafoodProduction.pdf",height=5,width=8)
layout(matrix(c(1,2,2,3),ncol=4))
xIn<-c(seq(1,length(CoastalID)),rep(rep(length(CoastalID)+1,length(CoastalID)),2),seq(length(CoastalID)+2,length(CoastalID)+1+length(CoastalID)))
layout(matrix(xIn,ncol=4))
par(mar=c(.3,.3,.3,.3),oma=c(2,1,3,1),xpd=TRUE,bg=NA)

OrderedID<-c(7,5,9,8,6,10,1,2,4,3)

Years<-seq(1986,2016)

#==plot aquaculture here
for(y in 1:length(CoastalID))
{

     temp<-useDat[,,dimnames(useDat)[[3]]==as.character(Label[OrderedID[y]])]
     temp<-temp[grep("aqua",rownames(temp)),]
     temp<-apply(temp,2,sum)
     ind<-which(names(temp)=="2007")
     temp[ind]<-(temp[ind-1]+temp[ind+1])/2
     #  why do you do this to me R??? I thought we were friends.
     useCol<-rep(1,length(temp))
     makePretty<-1
     if(makePretty>0)
     {
          for(i in 1:length(useCol))
               try(useCol[i] <- cols[which(abs(colrange-log(temp[i])) == min(abs(colrange-log(temp[i]))))] ,silent=T)
     }
     plot(temp~as.numeric(names(temp)),type='l',bty='n',xaxt='n',yaxt='n',
          ylim=c(-1,1.1*max(temp,na.rm=T)),lwd=1,col=useCol)
     points(temp~as.numeric(names(temp)),pch=16,cex=1.5,col=useCol)
     
     #cbind(FlatAquaFish$'all fish'[temp],FlatAquaFish$Year[temp],useCol)
     
     legend("topleft",Label[OrderedID[y]],bty='n')
     if(y==length(CoastalID))
          axis(side=1,line=.25)
}

plot(china_map1, col=inCols,xlim=c(108,123),ylim=c(14,42))

#==pie charts for aquaculture
# province,algae,other,total,fish,crustacean,shrimp,crab,shellfish
all_area_ind<-c(1,2,11,19,20,21,22,27,31)
plot_aqua<-aqua_area_by_province[aqua_area_by_province$Year==2016,all_area_ind]
plot_aqua<-plot_aqua[-4,] # get rid of shanghai
in_radius<-sqrt(plot_aqua$`total area`/3.141596)
in_radius<-in_radius/400

plot_aqua$Province[3]<-"Liaoning"
plot_aqua$Province[4]<-"Jiangsu"
plot_aqua$Province[6]<-"Fujian"

plot_aqua[is.na(plot_aqua)]<-0

Offset<-c( 1.4,-1.8,  #Fujian
           .7,-2.2,  #Guangdong
           -1.6,-2.7,  #Guangxi
           0,-1.5,  #Hainan
           3.5,-.7,  #Hebei
           2.3,.2,  #Jiangsu
           1.55,-3,	#Liaoning
           4,-.750,	#Shandong
           .6,-.4,	#Tianjin
           2.7,-.40)	#Zhejiang
inOff<-matrix(Offset,ncol=2,byrow=T)
aquacols 	<-colorRampPalette(brewer.pal(7,"Set3"))(7)

for(z in 1:nrow(inOff))
{     
     temp<-plot_aqua[which(plot_aqua$Province==as.character(Label[z])),]
     floating.pie(xpos=LabelX[z]+inOff[z,1],ypos=LabelY[z]+inOff[z,2],
                  radius=in_radius[which(plot_aqua$Province==as.character(Label[z]))],x=as.numeric(temp[c(2,3,5:ncol(temp))]),col=aquacols)
}

inNames<-c("Algae","Other","Fish","Crustaceans","Shrimp","Crab","Shellfish")
legend(x=108,y=35.6,legend=c("Aquaculture",inNames),col=c(NA,aquacols),pch=15,box.col='lightgrey',bg='lightgrey')

midLeg<-123
latLeg<-15
symbols(x=midLeg,y=latLeg,circles=in_radius[3],add=TRUE,inches=FALSE)
symbols(x=midLeg,y=latLeg-.7,circles=in_radius[2],add=TRUE,inches=FALSE)
text(x=midLeg,y=latLeg-.7,1.2,cex=.8)
text(x=midLeg,y=latLeg,9.3,cex=.8)
text(x=midLeg,y=latLeg-1.7,"10000 hectares")
text(x=midLeg,y=latLeg+1.7,"Aquaculture area")

Offset<-c( 0,0,  #Fujian
           .5,.2,  #Guangdong
           0,0,  #Guangxi
           0,0,  #Hainan
           -1,-1,  #Hebei
           .15,.5,  #Jiangsu
           0,0,	#Liaoning
           0,0,	#Shandong
           0,0,	#Tianjin
           0,0)	#Zhejiang
inOff<-matrix(Offset,ncol=2,byrow=T)

text(x=LabelX+inOff[,1],y=LabelY+inOff[,2],Label,cex=1)
text(x=112.6,y=16,"Marine seafood production (t)")
color.legend2(106,13,119.4,14.5,rect.col=cols,
              #legend=round(exp(colrange[seq(1,length(colrange),length.out=6)])))
              legend=c(1,25,500,15000,350000,6500000),cex=.2)


#==plot fisheries here
for(y in 1:length(CoastalID))
{
     temp<-useDat[,,dimnames(useDat)[[3]]==as.character(Label[OrderedID[y]])]
     temp<-temp[grep("catch",rownames(temp)),]
     temp<-apply(temp,2,sum)
     #  why do you do this to me R??? I thought we were friends.
     useCol<-rep(1,length(temp))
     makePretty<-1
     if(makePretty>0)
     {
          for(i in 1:length(useCol))
               try(useCol[i] <- cols[which(abs(colrange-log(temp[i])) == min(abs(colrange-log(temp[i]))))] ,silent=T)
     }
     plot(temp~as.numeric(names(temp)),type='l',bty='n',xaxt='n',yaxt='n',
          ylim=c(-10000,1.1*max(temp,na.rm=T)),lwd=1,col=useCol)
     points(temp~as.numeric(names(temp)),pch=16,cex=1.5,col=useCol)
     
     #cbind(FlatAquaFish$'all fish'[temp],FlatAquaFish$Year[temp],useCol)
     
     #legend("topleft",Label[OrderedID[y]],bty='n')
     if(y==length(CoastalID))
          axis(side=1,line=.25)
}

mtext(outer=T,adj=.1,"Culture",side=3)
mtext(outer=T,adj=.9,"Capture",side=3)
mtext(outer=T,adj=.5,"China's seafood production by province",side=3,cex=1.1)


dev.off()



