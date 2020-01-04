getEx<-function() {
	if (.Platform$OS.type=="windows"){
	d<-read.table(textConnection(readClipboard()),header=T,sep="\t")
	}else{
	d=read.table(pipe("pbpaste"), header=T)
	}
return (d)
}


putEx<-function(d) {
if (.Platform$OS.type=="windows"){
changed<-paste(names(d),collapse="\t")
dat<- as.matrix(d)

n<-dim(dat)[1]
  for (i in 1:n){
  line<-paste(dat[i,],collapse="\t")
  changed<-rbind(changed,line)
  }
  writeClipboard(changed)

  }else{

  zz <- pipe('pbcopy','w')
  write.table(d,file=zz,sep='\t',row.names=FALSE,quote=F)
  close(zz)
  }
}