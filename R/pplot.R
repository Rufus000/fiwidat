

pplot=function(model,term=NULL,confint=FALSE){


	if (is.null(term)){
		plot(allEffects(model, partial.residuals=T),partial.residual=list(pch=16,col="gray60",smooth=F),lines=list(col="blue"),confint=confint)
	} else {
		plot(effect(term, model, partial.residuals=T), partial.residual=list(pch=16,col="gray60",smooth=F),lines=list(col="blue"), confint=confint)
	}


}


