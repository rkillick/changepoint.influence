inf.input.model=function(model){
  if(inherits(model,"cpt")){
    if(cpttype(model)!="mean"){stop("Currently only models generated from the cpt.mean function are supported.")}

    data=data.set(model)
    seglen=seg.len(model)
    cpts=c(0,model@cpts)
    cpttype=ifelse(cpttype(model)=="mean","mean",ifelse(cpttype(model)=="mean and variance","meanvar",
                ifelse(cpttype(model)=="variance","var","np"))) # note that regression is cpt.reg object, not cpt
    cptfuncname=paste0("cpt.",cpttype)
    cptfuncargs=paste0("penalty=",shQuote(pen.type(model),"cmd"),",pen.value=",pen.value(model),
                ",method=",shQuote(method(model),"cmd"),",Q=",ncpts.max(model),",test.stat=",shQuote(test.stat(model),"cmd"),
                ",minseglen=",minseglen(model))
    if(test.stat(model)=="Gamma"){cptfuncargs=paste0(cptfuncargs,",shape=",param.est(model)$shape)}
    if(test.stat(model)=="Binomial"){cptfuncargs=paste0(cptfuncargs,",size=",param.est(model)$size)}
    #if(test.stat(model)=="empirical_distribution"){cptfuncargs=paste0(cptfuncargs,",nquantiles=",param.est(model)$nquantiles} # needs adding to changepoint.np
    if(cpttype(model)=="mean"){param=param.est(model)$mean}
    else if(cpttype(model)=="mean and variance"){param=cbind(param.est(model)$mean,param.est(model)$variance)}
    else if(cpttype(model)=="variance"){param=param.est(model)$variance}
    else{param=NULL} # NULL as cpt.np doesn't return parameters
  }
  else if(inherits(model,"list")){
    if(!hasName(model,"data")){stop("Model should contain a named data list entry")}
    if(!hasName(model,"cpts")){stop("Model should contain a named cpts list entry")}
    if(!hasName(model,"seglen")){stop("Model should contain a named seglen list entry")}
    if(!hasName(model,"cptfuncname")){stop("Model should contain a named cptfuncname list entry")}
    if(!hasName(model,"cptfuncargs")){stop("Model should contain a named cptfuncargs list entry")}
    if(!hasName(model,"param")){
      warning("Model does not contain a named param list entry, replacing with NULL")
      model$param=NULL
    }
    
    n=length(model$data)
    if(model$cpts[1]!=0){model$cpts=c(0,model$cpts)}
    if(end(model$cpts)!=n){model$cpts=c(model$cpts,n)}
    if(!is.character(model$cptfuncargs)){stop("Model list entry cptfuncargs should be a character string of arguments to pass to cptfuncname")}
    if(!is.null(model$param)){ # there are parameters given
      if(!inherits(model$param,"matrix")){
        warning("model$param is not a matrix, trying to coerce")
        model$param=matrix(model$param,ncol=1)
      }
      if(dim(model$param)[1]==(length(model$cpts)-1)){stop(paste("Length/nrow of model$param is the number of segments.  Please change to be the length of the data",n))}
      else if(dim(model$param)[1]!=n){stop(paste('Length/nrow of model$param must be the same length as the data',n))}
    }
    
    return(model) # return here as no point extracting everything to return it again
  }
  else{stop("Model should be a 'cpt' object or list with required components, see man file")}
  
  return(list(data=data,cpts=cpts,seglen=seglen,cptfuncname=cptfuncname,cptfuncargs=cptfuncargs,param=param))
}