PlotCurves<-function(x,y,col,group,modtype="quad",dummymax,inlty=1,inlwd=2)
{
     dummy<-seq(0,dummymax,dummymax/10000)
     unq_group<-unique(group)
     for(z in 1:length(unq_group))
     {
         invarY<-y[group==unq_group[z]] 
         invarX<-x[group==unq_group[z]] 
         tempCol<-col[group==unq_group[z]]
         #==quadratic model
          if(modtype=="quad")
          { 
               x1		<-c(1,1)
               outs		<-optim(x1,Quadratic,inY=invarY,inX=invarX)
               x1		<-outs$par
               outs		<-optim(x1,Quadratic,inY=invarY,inX=invarX)
               x1		<-outs$par
               outs		<-optim(x1,Quadratic,inY=invarY,inX=invarX)
               alpha   <-abs(outs$par[1])
               beta    <-abs(outs$par[2])
               preds   <- -1*alpha*dummy^2 + dummy*beta 
               preds[preds<0]<-NA
               lines(preds~dummy,col=tempCol,lwd=inlwd,lty=inlty)
          }
          
          #==Michalis menten
          if(modtype=="MM")
          {  
               x1		<-c(.25,200,1)
               outs		<-optim(x1,BevHolt,inY=invarY,inX=invarX)
               x1		<-outs$par
               outs		<-optim(x1,BevHolt,inY=invarY,inX=invarX)
               x1		<-outs$par
               outs		<-optim(x1,BevHolt,inY=invarY,inX=invarX)
               alpha   <-outs$par[1]
               beta    <-outs$par[2]
               preds   <-(alpha*dummy)/(1+(dummy/beta)) 
               preds[preds<0]<-NA
               lines(preds~dummy,col=tempCol,lwd=inlwd,lty=inlty)
          }  
          
          #==Ricker
          if(modtype=="Rick")
          {
               Stx1<-srStarts(invarY~invarX,type="Ricker",param=2)
               x1		<-c(Stx1$a,Stx1$b,1)
               outs1		<-optim(x1,Ricker,inY=invarY,inX=invarX)
               x1		<-outs1$par
               outs1		<-optim(x1,Ricker,inY=invarY,inX=invarX)
               x1		<-outs1$par
               outs1		<-optim(x1,Ricker,inY=invarY,inX=invarX)
               
               alpha<-outs1$par[1]
               beta<-outs1$par[2]
               preds<- dummy*exp(alpha-beta*dummy)
               preds[preds<0]<-NA
               lines(preds~dummy,col=tempCol,lwd=inlwd,lty=inlty)
          }
     }     
}

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














color.legend2<-function (xl, yb, xr, yt, legend, rect.col, cex = 1, align = "lt", 
                         gradient = "x", ...) 
{
  oldcex <- par("cex")
  gradient.rect(xl, yb, xr, yt, col = rect.col, nslices = length(rect.col), 
                gradient = gradient)
  if (gradient == "x") {
    xsqueeze <- (xr - xl)/(2 * length(rect.col))
    textx <- seq(xl + xsqueeze, xr - xsqueeze, length.out = length(legend))
    if (match(align, "rb", 0)) {
      texty <- yb - 0.2 * strheight("O")
      textadj <- c(0.5, 1)
    }
    else {
      texty <- yt + 0.2 * strheight("O")
      textadj <- c(0.5, 0)
    }
  }
  else {
    ysqueeze <- (yt - yb)/(2 * length(rect.col))
    texty <- seq(yb + ysqueeze, yt - ysqueeze, length.out = length(legend))
    if (match(align, "rb", 0)) {
      textx <- xr + 0.2 * strwidth("O")
      textadj <- c(0, 0.5)
    }
    else {
      textx <- xl - 0.2 * strwidth("O")
      textadj <- c(1, 0.5)
    }
  }
  text(textx, texty, labels = legend, adj = textadj, ...)
  par(xpd = FALSE, cex = oldcex)
}