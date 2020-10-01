location.stability=function(original.cpts, influence, expected=NULL,hist.tcpt.delete=FALSE,cpt.lwd=2,...){
  # histograms the changepoint locations identified
  
  # original.cpts     The cpts in the original data
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # expected          The expected segmentation based on original.cpts (if NULL is calculated using *.expected.mean functions)
  # cpt.lty           Line type for the changepoint lines
  # cpt.lwd           Line width for the changepoint lines
  
  
  n=nrow(influence[[1]]$class)
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
      
      # Dealing with the NAs temporarily (not returned to user) so we can plot nicely
      index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
      influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-1] # replace NA with previous index
      influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
      
      # repeat for expected
      index.na=which(is.na(expected$del))[-1] # -1 as we will deal with the first instance separately
      expected$del[index.na]=expected$del[index.na-1] # replace NA with previous index
      expected$del[1,1]=1 # replace the first NA with 1
    }
    
    cpts=unlist(apply(influence[[i]]$class,1,FUN=function(x){which(diff(x)==1)}))
    tcpts=table(cpts)
    
    col.cpts=rep("dark green",length(original.cpts))
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(original.cpts[j]))]!=n){
        col.cpts[j]="orange2"
      }
    }
    col.cpts[which(diff(original.cpts)==1)]="red"
    col.cpts[which(diff(original.cpts)==1)+1]="red"
    
    hist.col=rep(1,n)
    hist.col[original.cpts]=col.cpts
    if(hist.tcpt.delete==TRUE){
      hist.col[original.cpts]=0
      for(j in 1:ncpts){
        tmp=which(cpts==original.cpts[j])
        if(length(tmp)!=0){
          cpts=cpts[-tmp]
        }
      }
    }
    hist(cpts,col=hist.col,border=hist.col,breaks=0:n,xlim=c(0,n),main=paste('Location Stability using',method,"method"))
    segments(x0=original.cpts-0.5,y0=-100,y1=0,col=col.cpts,lwd=cpt.lwd) # do -0.5 so in the middle of the bar
    # start breaks at 0 as define the boundaries thus 1:n is n-1 breaks, not n
    
    abline(h=n, col='grey')
  }
}