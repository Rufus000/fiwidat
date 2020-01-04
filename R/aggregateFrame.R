
aggregateFrame=function(data,by,compute="mean", na.rm=T){

  by=as.character(by)
  dm=dim(data)[2]
  n=length(unique(by))
  aframe=data[1:n,]
  flag=0
  for (column in 1:dm){

   this=data[,column]
   if (is.numeric(this)) {
	   flag=0
	   agr=tapply(this,by,compute,na.rm=T)
	   ix=which(is.nan(agr))
	   agr[ix]=NA
	   agr=as.numeric(agr)
	   aframe[,column]=agr
   }else{
	   if (is.factor(this)) {
	   	  flag=1
	   	   this=as.character(this)
	   	   }
	   if (any(class(this)=="POSIXlt")) {
	   	  flag=2
	   	  this=difftime(this,"1900-01-01",units="secs")
	   	  this=as.numeric(this)
	   	  }
	   if (any(class(this)=="POSIXct")) {
	   	  flag=3
	   	  this=difftime(this,"1900-01-01",units="secs")
	   	  this=as.numeric(this)
	   	  }

	   agr=tapply(this,by,"min",na.rm=T)
	   if (class(agr)=="array" & flag==1) agr=as.character(agr)
       if (class(agr)=="array" & flag>1) agr=as.numeric(agr)

	   aframe[,column]=agr

	   if (flag==1) aframe[,column]=as.factor(aframe[,column])

	   if (flag==2) {
	      var=names(aframe)[column]
	   	  cmd=paste("aframe$",var,"=as.POSIXlt(aframe$",var,",origin='1900-01-01')",sep="")
	   	  eval(parse(text=cmd))
	    }

	   if (flag==3) {
	      var=names(aframe)[column]
	   	  cmd=paste("aframe$",var,"=as.POSIXct(aframe$",var,",origin='1900-01-01')",sep="")
	   	  eval(parse(text=cmd))
	    }

   }

  }

  data=aframe
  return(data)
}

