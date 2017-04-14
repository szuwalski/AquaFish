
library(maps)
library(mapdata)
library(maptools)
library(ggplot2)
library(Hmisc)

#map("china",xlim=c(105,145),col=seq(1,200),fill=T)
#map("china", fill = T, col = rainbow(15), mar = c(0, 0, 0, 0))

china_map1 <- readShapePoly("C:/Users/Cody/Desktop/ChinaMap/CHN_adm_shp/CHN_adm1.shp")
china_map2 <- china_map1@data
china_map3 <- fortify(china_map1) ## fortify() is a function in the package of ggplot2, we will talk about it later
china_map2$NAME <- iconv(china_map2$NAME, from = 'GBK')

china_map1@polygons[[1]]@labpt
inCols<-rep('lightgrey',31)
CoastalID<-c(4,6,7,9,10,15,18,23,24,27,30,31)
inCols[CoastalID]<-"lightblue"

LabelX<-rep(0,length(CoastalID))
LabelY<-rep(0,length(CoastalID))
Label<-rep(0,length(CoastalID))

for(x in 1:length(CoastalID))
{
LabelX[x]<-china_map1@polygons[[CoastalID[x]]]@labpt[1]
LabelY[x]<-china_map1@polygons[[CoastalID[x]]]@labpt[2]
Label[x]<-as.character(china_map1@data$NAME_1[CoastalID[x]])
}


dev.new(width=8,height=5)
layout(matrix(c(1,2,2,3),ncol=4))
xIn<-c(seq(1,length(CoastalID)),rep(rep(length(CoastalID)+1,length(CoastalID)),2),seq(length(CoastalID)+2,length(CoastalID)+1+length(CoastalID)))
layout(matrix(xIn,ncol=4))
par(mar=c(.3,.3,.3,.3),oma=c(2,1,3,1))

OrderedID<-c(7,5,10,8,6,9,12,1,2,4,3,11)
Years<-seq(1986,2016)

#==plot aquaculture here
for(y in 1:length(CoastalID))
{
 plot(arima.sim(list(order = c(1,0,0), ar = 0.95), n = 31)@.Data ~ Years,type='l',bty='n',xaxt='n',yaxt='n')
 legend("topleft",Label[OrderedID[y]],bty='n',cex=.7)
 if(y==length(CoastalID))
  axis(side=1)
}

plot(china_map1, col=inCols,xlim=c(97,125),ylim=c(14,40))
offSetLab<-c()
text(x=LabelX,y=LabelY,Label,cex=1)

TotalYield<-matrix(ncol=length(Years),nrow=2)
TotalYield[1,]<-c(seq(1,10),seq(10,60,10),seq(60,80,length.out=15))
TotalYield[2,]<-c(seq(1,12,length.out=20),rep(12,11))


dev.new()
plot(china_map1, col=inCols,xlim=c(105,115),ylim=c(30,31))

#subplot(
# barplot(TotalYield,type="l",yaxt='n',xaxt='n',ylab='',xlab='',
# 	,bty='n'),
# x=c(97,125),
# y=c(3,20))

#==plot fisheries here
for(y in 1:length(CoastalID))
{
 plot(arima.sim(list(order = c(1,0,0), ar = 0.95), n = 31)@.Data ~ Years,type='l',bty='n',xaxt='n',yaxt='n')
 legend("topright",Label[OrderedID[y]],bty='n',cex=.7)
 if(y==length(CoastalID))
  axis(side=1)
}

mtext(outer=T,adj=.1,"Aquaculture",side=3)
mtext(outer=T,adj=.9,"Fisheries",side=3)


Offset<-c(	2,-6,  #Fujian
		0,-7,  #Guangdong
		-12,-13,  #Guangxi
		0,-8,  #Hainan
		0,0,  #Hebei
		3,-2,  #Jiangsu
		0,0,	#Liaoning
		17,-1,	#Shandong
		12,-3,	#Shanghai
		0,0,	#Tianjin
		-8,-8,	#Yunnan
		2,-4)	#Zhejiang
inOff<-matrix(Offset,ncol=2,byrow=T)		


for(x in 1:length(CoastalID))
{
subplot(
 plot(rnorm(20,1,1),type="l",yaxt='n',xaxt='n',ylab='',xlab='',
 	,bty='n'),
 x=c(LabelX[x]+inOff[x,1],LabelX[x]+inOff[x,1]+10),
 y=c(LabelY[x]+inOff[x,2],LabelY[x]+inOff[x,2]+5))
}



# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
if(require(mapproj)) {	# mapproj is used for  projection="polyconic"
  # color US county map by 2009 unemployment rate
  # match counties to map using FIPS county codes
  # Based on J's solution to the "Choropleth Challenge"
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

  # load data
  # unemp includes data for some counties not on the "lower 48 states" county
  # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
  #  cities
  data(unemp)
  data(county.fips)

  # define color buckets
  colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
  unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
  leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

  # align data with map definitions by (partial) matching state,county
  # names, which include multiple polygons for some counties
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
    county.fips$polyname)]
  colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]

  # draw map
  map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")
  title("unemployment by county, 2009")
  legend("topright", leg.txt, horiz = TRUE, fill = colors)

  # Choropleth Challenge example, based on J's solution, see:
  # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
  # To see the faint county boundaries, use RGui menu:  File/SaveAs/PDF
}
