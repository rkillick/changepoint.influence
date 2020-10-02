location.stability=function(original.cpts, influence, data=NULL,include.data=FALSE,hist.tcpt.delete=FALSE,cpt.lty="dashed",cpt.lwd=2,...){
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
  
  for(i in 1:length(influence)){
    method="outlier"
    max=n-2
    if(names[i]=="del"){
      method="deletion"
      max=n-1
      
      # Dealing with the NAs temporarily (not returned to user) so we can plot nicely
      index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
      influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-n] # replace NA with previous index
      influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
    }
    
    cpts=unlist(apply(influence[[i]]$class,1,FUN=function(x){which(diff(x)==1)}))
    cpts=sort(cpts)
    
    if(names[i]=="del"){
      # create an index of cpts to delete as they are just a function of the deletion process
      del.correct.index=apply(matrix(original.cpts,ncol=1),1,FUN=function(x){return(which(cpts==(x+1))[1])})
      cpts=cpts[-del.correct.index]
    }
    else{
      # create an index of cpts to delete as they are just a function of the outlier process
      del.outlier.index=apply(matrix(1:(n-1),ncol=1),1,FUN=function(x){return(which(cpts==x)[1:2])})
      cpts=cpts[-del.outlier.index]
    }
    
    tcpts=table(cpts)
    
    col.cpts=rep("dark green",length(original.cpts))
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(original.cpts[j]))]!=max){
        col.cpts[j]="orange2"
      }
    }
    col.cpts[which(diff(original.cpts)==1)]="red"
    col.cpts[which(diff(original.cpts)==1)+1]="red"
    
    if(include.data==TRUE){
      if(is.null(data)){
        stop("data argument must be supplied if include.data=TRUE.")
      }
      par(mfrow=c(2,1))
      plot(data,type='l',ylab='',xlab='Time',main='',...) # plot the original time series
      abline(v=original.cpts,col=col.cpts,lty=cpt.lty,lwd=cpt.lwd)
    }
    
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
    
    abline(h=max, col='grey')
  }
}