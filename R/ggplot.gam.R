
ggplot.gam=function(mod,part=T, conf=T, ylabel=NULL){
  
  mm=mean(predict.gam(mod))
  
  pg=plot.gam (mod, resid=T, pages=1)
  
  for (i in 1:length(pg)){
    xp=pg[[i]]$raw
    r=pg[[i]]$p.resid
    
    if (part==T) {
      df=data.frame(x=xp,y=r+mm)
      p=ggplot(data=df,aes(x=x,y=y,color="red4"))+geom_point(size=1)
      x=pg[[i]]$x
      y=mm+pg[[i]]$fit
      df=data.frame(x=x,y=y)
      p=p+geom_line(data=df,aes(x=x,y=y),color="black",linewidth=1.5)+theme_lsp(22)
      
    } else {
      
      x=pg[[i]]$x
      y=mm+pg[[i]]$fit
      df=data.frame(x=x,y=y)
      p=ggplot(data=df,aes(x=x,y=y))+geom_line(color="black",linewidth=1.5)+theme_lsp(22)
      
    }
    
    if (conf==T) {
      df=data.frame (x=pg[[i]]$x,fit=pg[[i]]$fit,se=pg[[i]]$se)
      p=p+geom_ribbon(data=df,aes(ymin=fit+mm-se*1.96,ymax=fit+mm+se*1.96),fill="purple",alpha=.4)
    } 
    
    p=p+xlab(pg[[i]]$xlab)
    
    if (pg[[i]]$xlab=="yearday") {
      p=p+scale_x_continuous(breaks=seq(0,365,by=91.25),labels=c("Jan","Apr","Jul","Oct","Jan"))
      p=p+xlab("Time of Year")
    }
    
    if (pg[[i]]$xlab=="daytime") {
      p=p+scale_x_continuous(breaks=seq(0,24,by=6),labels=c("00","06","12","18","00"))
      p=p+xlab("Time of Day")
    }
    p=p+ylab(pg[[i]]$ylab)
    if (!is.null(ylabel))p=p+ylab(ylabel)
    p=p + theme(legend.position = "none")
    print(p)
  }
}
