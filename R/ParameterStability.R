ParameterStability=function(infcpt,digits=6,ylab='',xlab='Index',cpt.col='red',
                            cpt.width=3,...){
  # Function to plot the parameter stability across the influence modification
  # Note that this function
  n=length(infcpt$org.data)

  names=NULL
  if(dim(infcpt$delete$segment)[2]!=1){names=c(names,"delete")}
  if(dim(infcpt$outlier$segment)[2]!=1){names=c(names,"outlier")}
  
  for(i in 1:length(names)){
    if(names[[i]]=="delete"){
      method="Deletion"
      if(is.null(infcpt$delete$param)){
        warning("ParameterStability plot for Deletion method needs estimated parameters.  infcpt$delete$param is NULL.")
        next
      }
      influence=infcpt$delete#influence=slot(infcpt,names[i]) # take a copy as we are going to modify it
      max=nrow(influence$segment)-infcpt$k # n-1
    }

    else if(names[i]=="outlier"){
      method="Outlier"
      if(is.null(infcpt$outlier$param)){
        warning("ParameterStability plot for Outlier method needs estimated parameters.  The original call to influence, that generated infcpt, did not include parameter estimates.")
        next
      }
      influence=infcpt$outlier
      max=nrow(influence$segment)-infcpt$k-1 # n-2

      # remove the known outlier points
      diag(influence$param[,infcpt$modified,1])=NA # RK: need to modify for multiple parameters!
    }
    counts=apply(matrix(1:n,ncol=1),1,FUN=function(x,influence,digits,i){
      counts=rle(sort(signif(influence$param[,x,1],digits=digits)))
      counts.df=data.frame(index=rep(x,length(counts$values)),values=counts$values,counts=counts$lengths)
      return(counts.df)
    },influence=influence,digits=digits,i=i)
    counts=rbindlist(counts)
    
    plot(counts$index,counts$values,pch=20,col=hsv(v=0,alpha=0.3*(counts$counts/n+1)),lwd=0,#bg=hsv(v=0,alpha=0.5*(counts$counts/n+1)),
         main=paste('Parameter Stability: ',method,"method"),xlab=xlab,ylab=ylab,...)

    for(j in 1:length(infcpt$org.param)){
      segments(infcpt$org.cpts[j]+1,infcpt$org.param[j],infcpt$org.cpts[j+1]-1,infcpt$org.param[j],col=cpt.col,lwd=cpt.width)
    }
  }
}