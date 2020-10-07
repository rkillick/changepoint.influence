parameter.stability=function(influence,original.mean=NULL,digits=6){
  # Function to plot the parameter stability across the influence modification
  # Note that this function
 
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # original.mean     If supplied, n length vector of the original mean is plotted on top of the influence values
  # digits            The number of significant digits to round to before counting unique parameter values, can be Inf to not round
  
  n=nrow(influence[[1]]$class)
  names=names(influence)
  
  for(i in 1:length(influence)){
    method="deletion"
    max=n-1
    if(names[i]=="out"){
      method="outlier"
      max=n
      # remove the known outlier points
      for(j in 1:n){
        influence$out$param.out[j,j]=NA
      }
    }
    counts=apply(matrix(1:n,ncol=1),1,FUN=function(x,influence,digits,i){
      counts=rle(sort(signif(influence[[i]]$param[,x],digits=digits)))
      counts.df=data.frame(index=rep(x,length(counts$values)),values=counts$values,counts=counts$lengths)
      return(counts.df)
    },influence=influence,digits=digits,i=i)
    counts=rbindlist(counts.list)
    
    plot(counts$index,counts$values,pch=19,col=hsv(v=0,alpha=0.5*(counts$counts/n+1)),bg=hsv(v=0,alpha=0.5*(counts$counts/n+1)),
         main=paste('Parameter Stability using',method,"method"),xlab='Time',ylab='')
    if(!is.null(original.mean)){
      if(length(original.mean)!=n){
        stop(paste('Length of original.mean must be',n))
      }
      points(1:n,original.mean,col='red',pch=20)
    }
  }
}