stability.overview=function(data, original.cpts, influence, expected=NULL,...){
  # plots the original changepoints with colours indicating whether they have moved within the modify/delete methods
  
  # data              Vector of original data
  # original.cpts     The cpts in the original data
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  
  n=length(data)
  ncpts=length(original.cpts)
  
  original.class=rep(1:(ncpts+1),times=diff(c(0,original.cpts,n)))
  names=names(influence)
  
  # if the expected isn't given it needs calculating
  if(is.null(expected)){expected=list()}
  
  for(i in 1:length(influence)){
    if(eval(parse(text=paste("is.null(expected$",names[i],")",sep="")))){
      eval(parse(text=paste("expected$",names[i],"=",names[i],".expected.mean(original.class)",sep=""))) # calculated the expected
    }
  }
  
  for(i in 1:length(influence)){
    method="outlier"
    max=n
    if(names[i]=="del"){
      method="deletion"
      max=n-1
    }
    plot(data,type='l',ylab='',xlab='Time',main=paste('Stability dashboard using',method,"method"),...) # plot the original time series


    resid=influence[[i]]$class-expected[[i]]
    ########### note the partial matching used here (class.del or class.out)
    
    col.cpts=rep("dark green",length(original.cpts))
    for(j in 1:ncpts){
      if(any(resid[,original.cpts[j]]!=0)){
        col.cpts[j]="orange2"
      }
    }
    col.cpts[which(diff(original.cpts)==1)]="red"
    col.cpts[which(diff(original.cpts)==1)+1]="red"
    
    abline(v=original.cpts,col=col.cpts,lty=2,lwd=2)
    # do we want a legend to specify the colours or just leave it to the documentation?
  }
}