inf.output.model=function(output,...){
  # function to put the output from applying the changepoint approach to the new data
  # into the required format
  
  # output        The output from the changepoint call
  # ...           arguments to be passed to the changepoint method
  
  if(inherits(output,"cpt")){
    ncpts=ncpts(output)
    seg.len=seg.len(output)
    paramest=NULL
    for(i in 1:length(param.est(output))){ # for each parameter
      if(length(param.est(output)[[i]]==(ncpts+1))){ # a changing parameter
        paramest=cbind(paramest,param.est(output)[[i]]) # add it to the list
        # nseg x nparam
      }
    }
  }
  else if(inherits(output,"list")){
    if(!hasName(output,"seg.len")){stop("Output from cptfuncname should contain a named seg.len entry in the list")}
    if(!hasName(output,"ncpts")){output$ncpts=length(output$seg.len)-1}
    if(length(output$ncpts)!=(length(output$seg.len)-1)){
      warning("ncpts",output$ncpts,"and (length of seg.len)",length(output$seg.len)-1,"do not match, modifying ncpts to match")
      output$ncpts=length(output$seg.len)-1
    }
    if(!hasName(output,"param.est")){stop("Output from cptfuncname should contain a named param.est entry in the list")}
    if(!inherits(output$param.est,"matrix")){
      warning("The param.est output from cptfuncname is not a matrix, trying to coerce")
      output$param.est=matrix(output$param.est,ncol=1) # presumes that if not a matrix then it is a vector
    }
    return(output)
  }

  # return the new list output
  return(list(ncpts=ncpts,seg.len=seg.len,param.est=paramest))
}
