sem=function(x, na.rm=T){
 if (na.rm==T) x=na.omit(x)
 l=length(x)
 return (sd(x)/sqrt(l))
 }