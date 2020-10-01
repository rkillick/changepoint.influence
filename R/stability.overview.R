stability.overview=function(data, original.cpts, influence, expected=NULL,cpt.lty="dashed",cpt.lwd=2,...){
  # plots the original changepoints with colours indicating whether they have moved within the modify/delete methods
  
  # data              Vector of original data
  # original.cpts     The cpts in the original data
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # expected          The expected segmentation based on original.cpts (if NULL is calculated using *.expected.mean functions)
  # cpt.lty           Line type for the changepoint lines
  # cpt.lwd           Line width for the changepoint lines
  
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
    if(names[i]=="del"){
      method="deletion"
      index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
      influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-1] # replace NA with previous index
      influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
      
      # repeat for expected
      index.na=which(is.na(expected$del))[-1] # -1 as we will deal with the first instance separately
      expected$del[index.na]=expected$del[index.na-1] # replace NA with previous index
      expected$del[1,1]=1 # replace the first NA with 1
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
    
    abline(v=original.cpts,col=col.cpts,lty=cpt.lty,lwd=cpt.lwd)
    # do we want a legend to specify the colours or just leave it to the documentation?
  }
}