LocationStability=function(original.cpts, influence, data=NULL,include.data=FALSE,hist.tcpt.delete=FALSE,cpt.lwd=4,cpt.col=c("#009E73", "#E69F00", "#D55E00"),cpt.lty=c("dashed","dotdash","dotted"),ylab='',xlab='Index',...){
  # histograms the changepoint locations identified
  
  # original.cpts     The cpts in the original data
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # data              Include if you want the data plotted
  # include.data      True if you want to plot the data too
  # hist.tcpt.delete  If true plots the histogram with the true changepoints deleted so you can see the moved changes more easily
  # cpt.lwd           Line width for the changepoint lines
  # cpt.col           Colour of the changepoint lines (length 3)
  # cpt.lty           Line type for the changepoint lines (length 3)
  # ylab              Label for the y-axis
  # ...               Additional graphical parameters

  col.cpts=list()
  lty.cpts = list()
  
  n=nrow(influence[[1]]$class)
  ncpts=length(original.cpts)

  original.class=rep(1:(ncpts+1),times=diff(c(0,original.cpts,n)))
  names=names(influence)
  
  for(i in 1:length(influence)){
    method="Outlier"
    max=n-2
    if(names[i]=="delete"){
      method="Deletion"
      max=n-1
      
      # Dealing with the NAs temporarily (not returned to user) so we can plot nicely
      index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
      influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-n] # replace NA with previous index
      influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
    }
    
    cpts=unlist(apply(influence[[i]]$class,1,FUN=function(x){which(diff(x)==1)}))
    cpts=sort(cpts)
    
    if(names[i]=="delete"){
      # create an index of cpts to delete as they are just a function of the deletion process
      del.correct.index=apply(matrix(original.cpts,ncol=1),1,FUN=function(x){return(which(cpts==(x+1))[1])})
      cpts=cpts[-del.correct.index]
    }
    else{
      # create an index of cpts to delete as they are just a function of the modify process
      del.outlier.index=apply(matrix(1:(n-1),ncol=1),1,FUN=function(x){return(which(cpts==x)[1:2])})
      cpts=cpts[-del.outlier.index]
    }
    
    tcpts=table(cpts)
    
    col.cpts[[i]]=rep(cpt.col[1],length(original.cpts)) # "dark green"
    lty.cpts[[i]]=rep(cpt.lty[1],length(original.cpts)) # "dashed" for "green"
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(original.cpts[j]))]!=max){
        col.cpts[[i]][j]=cpt.col[2] # "orange2"
        lty.cpts[[i]][j]=cpt.lty[2] # "dotdash" for "orange"
      }
    }
    col.cpts[[i]][which(diff(original.cpts)==1)]=cpt.col[3] # red
    col.cpts[[i]][which(diff(original.cpts)==1)+1]=cpt.col[3]
    lty.cpts[[i]][which(diff(original.cpts)==1)]=cpt.lty[3] # "dotted" for "red
    lty.cpts[[i]][which(diff(original.cpts)==1)+1]=cpt.lty[3]
    names(col.cpts)[i]=names[i]
    
    hist.col=rep(1,n)
    hist.col[original.cpts]=col.cpts[[i]]
    if(hist.tcpt.delete==TRUE){
      hist.col[original.cpts]=1
      for(j in 1:ncpts){
        tmp=which(cpts==original.cpts[j])
        if(length(tmp)!=0){
          cpts=cpts[-tmp]
        }
      }
    }

    if(include.data==TRUE){
      if(is.null(data)){
        stop("data argument must be supplied if include.data=TRUE.")
      }
      op <- par(no.readonly = TRUE) # read current parameters
      par(mfrow=c(2,1))
      plot(data,type='l',ylab=ylab,xlab=xlab,main=paste('Location Stability: ',method,"method"),...) # plot the original time series
      abline(v=original.cpts,col=col.cpts[[i]],lty=lty.cpts[[i]],lwd=cpt.lwd) # cpt.lty
      
      if(hist.tcpt.delete==TRUE){
        hist(cpts,col=hist.col,border=hist.col,breaks=0:n,xlim=c(0,n),xlab='Changepoint locations',main='')
        yaxplength=par("yaxp")[2]-par("yaxp")[1]
        segments(x0=original.cpts,y0=-yaxplength,y1=-0.02*yaxplength,col=col.cpts[[i]],lwd=cpt.lwd) # do -0.5 so in the middle of the bar
      }
      else{
        hist(cpts,col=hist.col,border=hist.col,yaxt='n',breaks=0:n,xlim=c(0,n),xlab='Changepoint locations',ylab="Proportion",main='')
        axis(side=2,at=round(c(0,max/4,max/2,3*max/4,max),2),labels=c(0,0.25,0.5,0.75,1))
      }
      # start breaks at 0 as define the boundaries thus 1:n is n-1 breaks, not n
      abline(h=max, col='grey')
      par(op) # reset previous parameters
    }
    else{ # same as above but title included on Histogram
      if(hist.tcpt.delete==TRUE){
        hist(cpts,col=hist.col,border=hist.col,breaks=0:n,xlim=c(0,n),main=paste('Location Stability: ',method,"method"),xlab='Changepoint locations')
        yaxplength=par("yaxp")[2]-par("yaxp")[1]
        segments(x0=original.cpts,y0=-yaxplength,y1=-0.02*yaxplength,col=col.cpts[[i]],lwd=cpt.lwd) # do -0.5 so in the middle of the bar
      }
      else{
        hist(cpts,col=hist.col,border=hist.col,yaxt='n',breaks=0:n,xlim=c(0,n),main=paste('Location Stability: ',method,"method"),ylab="Proportion",xlab='Changepoint locations')
        axis(side=2,at=round(c(0,max/4,max/2,3*max/4,max),2),labels=c(0,0.25,0.5,0.75,1))
      }
      # start breaks at 0 as define the boundaries thus 1:n is n-1 breaks, not n

      abline(h=max, col='grey')
    }
    # change colours to something meaningful to return to the user
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[1])] = "stable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[2])] = "unstable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[3])] = "outlier"
    
  }
  return(col.cpts)
}
