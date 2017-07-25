library(mizer)


# smaller k0 means that fishing impacts recruitment harder
# smaller k0 also means that M2 scales up

params_knife <- set_trait_model(no_sp = 10, min_w_inf = 10, max_w_inf = 1e5, knife_edge_size = 1000,k0=50000000,kappa = 10000)
Vir_sim <- project(params_knife, t_max=75, effort = 0)
Fish_sim <- project(params_knife, t_max=75, effort = 0.75)
lt_Fish_sim <- project(params_knife, t_max=75, effort = 0.25)
plot(Vir_sim)
plot(Fish_sim)
plot(lt_Fish_sim)

total_abund_vir <- apply(Vir_sim@n[76,,],2,sum)
total_abund_fish <- apply(Fish_sim@n[76,,],2,sum)
relative_abundance <- total_abund_fish / total_abund_vir

plot(x=params_knife@w, y=relative_abundance, log="xy", type="n", xlab = "Size (g)",
     ylab="Relative abundance", ylim = c(0.1,10))
lines(x=params_knife@w, y=relative_abundance)
lines(x=c(min(params_knife@w),max(params_knife@w)), y=c(1,1),lty=2)

#==if I use just a production model, the non-linearities could be attributed to trophic changes
#==if I use size spectrum models, the non-linearities dueto trophic changes won't matter, but there are more parameters

Biggie$all_fish_catch
x<-6
tempDat<-Biggie[Biggie$Province==CoastalProv[x],]

#==first get the scale right by estimating kappa
#==then pull in K0 (what exactly does it do?)

#==parameters to estimate: effort (time series), selectivity, K0, kappa:
params_knife <- set_trait_model(no_sp = 10, min_w_inf = 10, max_w_inf = 1e5, knife_edge_size = 1000,k0=50000000,kappa = 10000)
Vir_sim <- project(params_knife, t_max=75, effort = 0)
Fish_sim <- project(params_knife, t_max=75, effort = seq(0,0.75,length.out=75))




#======================================================================
# Estimate recruitment for scale
#=======================================================================
if(estRec==1)
{
  
  EstParsECS<-function(x,SppInd,SelPars)
  {
    #==Estimate effort
    top				<-maxEffort
    yr50				<-SelPars[1]
    yr95				<-SelPars[2]
    predEff			<-(top-bottom)/(1+exp(-1*log(19)*(Years-yr50)/(yr95-yr50))) + bottom
    useDat$r_max		<-inRmax
    useDat$r_max[SppInd]	<-exp(x[1])
    gear_names 			<- c("Trawl")
    times 			<- seq(from = 1, to = length(Years), by = 1)
    effort_array 		<- array(NA, dim = c(length(times), length(gear_names)),
                            dimnames = list(time = times, gear = gear_names))
    effort_array[,"Trawl"]	<- predEff
    
    params			<-MizerParams(useDat,kappa=kappaIN)
    sim1				<-project(params, effort = effort_array, t_max = length(Years), dt = 0.1, t_save = 1)
    YieldIn			<-getYield(sim1)
    indFit			<-match(seq(1986,2014),Years)
    SSQ				<-log(sum((obsIn[,y]-YieldIn[indFit,y])^2,na.rm=T))
    
    #==plot the fits to species catches
    Yields	<-getYield(sim1)
    par(mfrow=c(floor(sqrt(ncol(Yields))),ceiling(sqrt(ncol(Yields)))),mar=c(1,.1,.1,.1),oma=c(1,3,1,3))
    for(y in 1:(ncol(Yields)))
    {
      obs	<-catch[,which(grepl(colnames(Yields)[y],colnames(catch)))]
      plot(Yields[,y]~seq(startYear,endYear),yaxt='n',xaxt='n',type="l",col=2,xlim=c(PlotYearIn,PlotYearOut),ylim=c(0,max(obs,na.rm=T)))
      #legend(bty='n',"center",colnames(Yields)[y])
      mtext(colnames(Yields)[y],cex=.75)
      points(obs~catch[,1],yaxt='n',xaxt='n',xlim=c(startYear,endYear),type='b')
    }
    legend("center",legend=c("Observed","Predicted"),col=c(1,2),lty=c(1,1),pch=c(16,NA),bty='n')
    
    print(x)
    print(SSQ)
    return(SSQ)
  }
  
  
  #==================================
  #== Optimize recruitment
  #===================================
  SaveRmax<-inRmax
  Conv<-inRmax
  
  pdf(paste(CurDir,"/all the plots_newdata.pdf",sep=""))
  for(y in 1:(length(rMax)-1))
  {
    intime<-Sys.time()
    x		<-log(inRmax[y])
    outs		<-nlminb(x,EstParsECS,SppInd=y,SelPars=SelPars,lower=rep(0.01,length(x)), upper=rep(10e20,length(x)))
    SaveRmax[y]<-outs$par
    inRmax[y]  <-exp(outs$par)
    Conv[y]	<-(outs$convergence)
    print(y)
    print(Sys.time()-intime)
  }
  dev.off()
  
  x<-outs$par
  write.csv(exp(SaveRmax),paste(CurDir,"/ChinaSS_estRmax_newdata.csv",sep=""),row.names=F)
}