require(visreg)
require(ggplot2)



pplot=function(model,xvar,by=NULL,breaks=NULL, partial=FAlSE,leg.name=NULL,leg.labs=NULL,cols=c("red","blue","green","purple"), transp=0.5, rsize=2,rcols=FALSE, rshapes=TRUE){

  if (is.null(by)){
    p=visreg(model,xvar,plot=FALSE)
  }else{
    p=visreg(model,xvar,by,breaks,plot=FALSE)
  }

  ix=which(names(p$fit)==p$meta$x)
  ib=which(names(p$fit)==p$meta$by)

  if (is.null(leg.name)) leg.name=waiver()
  if  (is.null(leg.labs)) leg.labs=waiver()

  if (!is.null(p$meta$by)){
    plt= ggplot(p$fit, aes(x=p$fit[,ix], y=visregFit,fill=as.factor(p$fit[,ib])))+
      geom_ribbon(aes(ymin=visregLwr,ymax=visregUpr),alpha=transp) +
      scale_fill_manual(values = cols,        name = leg.name, labels=leg.labs) +
      geom_line()+xlab(p$meta$x)+ylab(p$meta$y)

    if (partial==T){
      ix=which(names(p$res)==p$meta$x)
      ib=which(names(p$res)==p$meta$by)
      grp=as.factor(p$res[,ib])

      if (rcols==T) cl =grp else cl=as.factor(1)
      if ((rshapes==T)) sh=grp else sh=as.factor(1)

      plt=plt+geom_point(data=p$res,aes(x=p$res[,ix],y=visregRes,fill= grp,
                                        shape= sh,color=cl),
                         show.legend = FALSE,size=rsize) +
        scale_color_manual(values=c("gray35","gray50","gray65","gray80"))
    }
  } else {
    plt=ggplot(p$fit, aes(x=p$fit[,ix], y=visregFit))+
      geom_ribbon(aes(ymin=visregLwr,ymax=visregUpr),fill=cols[1],alpha=transp)+
      geom_line()+xlab(p$meta$x)+ylab(p$meta$y)

    if (partial==T){
      plt=plt+geom_point(data=p$res,aes(x=p$res[,ix],y=visregRes),colour="gray50",
                         size=rsize, shape="circle") }
  }

  plt
}

