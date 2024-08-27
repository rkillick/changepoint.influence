LocationStability=function(infcpt, type=c("Difference","Global","Local"),
          include.data=FALSE,cpt.lwd=4,cpt.col=c("#009E73", "#E69F00", "#E41A1C"),
          cpt.lty=c("dashed","dotdash","dotted"),ylab='',xlab='Index',...){
  # histograms the changepoint locations identified
  
  if(!any(type==c("Difference","Global","Local"))){
    stop("type should be Difference, Global, or Local.")
  }
  
  col.cpts=list()
  lty.cpts = list()
  
  n=length(infcpt$org.data)
  ncpts=length(infcpt$org.cpts)-2 # remove 0 and n

  original.segment=rep(1:(ncpts+1),times=infcpt$org.seglen)
  names=NULL
  if(dim(infcpt$delete$segment)[2]!=1){names=c(names,"delete")}
  if(dim(infcpt$outlier$segment)[2]!=1){names=c(names,"outlier")}
  
  for(i in 1:length(names)){
    method="Outlier"
    influence=infcpt$outlier#influence=slot(infcpt,names[i]) # take a copy as we are going to modify it
    max=nrow(influence$segment)-infcpt$k-1 # n-2

    if(names[i]=="delete"){
      method="Deletion"
      influence=infcpt$delete
      max=nrow(influence$segment)-infcpt$k # n-1
      
      # Dealing with the NAs temporarily (not returned to user) so we can plot nicely
      influence$segment[,1]=1 # First replace any NAs in the first column with ones
      for(ik in 1:infcpt$k){
        index.na=which(is.na(influence$segment))   # delete last k-1 lines since these are NAs
        influence$segment[index.na]=influence$segment[index.na-nrow(influence$segment)] # consecutively replace NA with previous index
      }
      
      if((any(type=="Difference"))&(is.null(infcpt$delete$expected))){
        # if the expected isn't given it needs calculating
        infcpt$delete$expected=delete.expected.mean(original.segment) # calculated the expected

        # repeat correction for expected
        infcpt$delete$expected[,1]=1 # First replace any NAs in the first column with ones
        for(ik in 1:infcpt$k){
          index.na=which(is.na(infcpt$delete$expected))   # delete last k-1 lines since these are NAs
          infcpt$delete$expected[index.na]=infcpt$delete$expected[index.na-nrow(infcpt$delete$expected)] # consecutively replace NA with previous index
        }
      }
    }
    else{
      # if the expected isn't given it needs calculating
      if((any(type=="Difference"))&(is.null(infcpt$outlier$expected))){
        infcpt$outlier$expected=outlier.expected.mean(original.segment) # calculated the expected
      }
    }
    cpts=unlist(apply(influence$segment,1,FUN=function(x){which(diff(x)==1)}))
    cpts=sort(cpts)
    
    if(names[i]=="delete"){
      # create an index of cpts to delete as they are just a function of the deletion process
      del.correct.index=apply(matrix(infcpt$org.cpts,ncol=1),1,FUN=function(x){return(which(cpts==(x+1))[1])})
      # There are NAs in del.correct.index for k>1, so cant do following line
      # cpts=cpts[-del.correct.index]
      cpts=cpts[-del.correct.index[!is.na(del.correct.index)]]
    }
    else{
      # create an index of cpts to delete as they are just a function of the modify process
      del.outlier.index=apply(matrix(1:(n-1),ncol=1),1,FUN=function(x){return(which(cpts==x)[1:2])})
      # There are NAs in del.correct.index for k>1, so cant do following line
      # cpts=cpts[-del.outlier.index]
      cpts=cpts[-del.outlier.index[!is.na(del.outlier.index)]]
    }
    
    tcpts=table(cpts)
    original.cpts=infcpt$org.cpts[-c(1,length(infcpt$org.cpts))] # take off 0 and n
    
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
    
    if(any(type=="Difference")){
      # need to calculated the changepoints from the segmentss for both observed and expected
      cpts.observed=unlist(apply(influence$segment,MARGIN=1,FUN=function(x){
        return(which(diff(x)!=0))}))
      cpts.observed=factor(cpts.observed,levels=1:n)
      tcpts.observed=table(cpts.observed)
      cpts.expected=ifelse(names[i]=="delete",unlist(apply(infcpt$delete$expected,MARGIN=1,FUN=
          function(x){return(which(diff(x)!=0))})),
          unlist(apply(infcpt$outlier$expected,MARGIN=1,FUN=function(x){return(which(diff(x)!=0))})))
      
      cpts.expected=factor(cpts.expected,levels=1:n)
      tcpts.expected=table(cpts.expected)
      
      tresid=tcpts.observed-tcpts.expected
      tresid=as.vector(tresid)
    }

    hist.col=rep(1,n)
    hist.col[original.cpts]=col.cpts[[i]]

    if(include.data==TRUE){
      if(is.null(infcpt$org.data)){
        stop("infcpt$org.data argument must be supplied if include.data=TRUE.")
      }
      op <- par(no.readonly = TRUE) # read current parameters
      on.exit(par(op)) # returns options as user had them set on exit
      par(mfrow=c(2,1))
      plot(infcpt$org.data,type='l',ylab=ylab,xlab=xlab,main=paste('Location Stability: ',method,"method"),...) # plot the original time series
      abline(v=original.cpts,col=col.cpts[[i]],lty=lty.cpts[[i]],lwd=cpt.lwd) # cpt.lty
      
      if(any(type=="Global")){
        hist(cpts,col=hist.col,border=hist.col,yaxt='n',breaks=0:n,xlim=c(0,n),xlab='Changepoint locations',ylab="Gloabl Proportion",main='',...)
        axis(side=2,at=round(c(0,max/4,max/2,3*max/4,max),2),labels=c(0,0.25,0.5,0.75,1))
        abline(h=max, col='grey')
      }
      # start breaks at 0 as define the boundaries thus 1:n is n-1 breaks, not n
      if(any(type=="Local")){
        for(j in 1:ncpts){ # remove original changepoints from plotting
          tmp=which(cpts==original.cpts[j])
          if(length(tmp)!=0){
            cpts=cpts[-tmp]
          }
        }
        hist(cpts,col=1,breaks=0:n,xlim=c(0,n),xlab='Changepoint locations',ylab="Local Count",main='',...)
        yaxplength=par("yaxp")[2]-par("yaxp")[1]
        segments(x0=original.cpts,y0=-yaxplength,y1=-0.02*yaxplength,col=col.cpts[[i]],lwd=cpt.lwd) # do -0.5 so in the middle of the bar
        abline(h=max, col='grey')
      }
      if(any(type=="Difference")){
        plot(tresid,type='n',col=hist.col,xlab='Changepoint locations',ylab="Difference from expected",main='',...)
        abline(h=0,col=1)
        to.plot=which(tresid!=0) # locations which are not 0
        for(seg in 1:length(to.plot)){
          if(any(to.plot[seg]==original.cpts)){lty.seg=lty.cpts[[i]][which(to.plot[seg]==original.cpts)]}else{lty.seg="solid"}
          segments(to.plot[seg],0,to.plot[seg],tresid[to.plot[seg]],col=hist.col[to.plot[seg]],lty=lty.seg)
        }
      }
    }
    else{ # same as above but title included on Histogram
      if(any(type=="Global")){
        hist(cpts,col=hist.col,border=hist.col,yaxt='n',breaks=0:n,xlim=c(0,n),main=paste('Location Stability: ',method,"method"),ylab="Global Proportion",xlab='Changepoint locations',...)
        axis(side=2,at=round(c(0,max/4,max/2,3*max/4,max),2),labels=c(0,0.25,0.5,0.75,1), ...)
        abline(h=max, col='grey')
      }
      # start breaks at 0 as define the boundaries thus 1:n is n-1 breaks, not n
      if(any(type=="Local")){
        for(j in 1:ncpts){ # remove original changepoints from plotting
          tmp=which(cpts==original.cpts[j])
          if(length(tmp)!=0){
            cpts=cpts[-tmp]
          }
        }
        hist(cpts,col=1,breaks=0:n,xlim=c(0,n),main=paste('Location Stability: ',method,"method"),xlab='Changepoint locations',ylab="Local Count",...)
        yaxplength=par("yaxp")[2]-par("yaxp")[1]
        segments(x0=original.cpts,y0=-yaxplength,y1=-0.02*yaxplength,col=col.cpts[[i]],lwd=cpt.lwd) # do -0.5 so in the middle of the bar
        abline(h=max, col='grey')
      }
      if(any(type=="Difference")){
        plot(tresid,type='n',col=hist.col,main=paste('Location Stability: ',method,"method"),xlab='Changepoint locations',ylab="Difference from expected",...)
        abline(h=0,col=1)
        to.plot=which(tresid!=0) # locations which are not 0
        for(seg in 1:length(to.plot)){
          if(any(to.plot[seg]==original.cpts)){lty.seg=lty.cpts[[i]][which(to.plot[seg]==original.cpts)]}else{lty.seg="solid"}
          segments(to.plot[seg],0,to.plot[seg],tresid[to.plot[seg]],col=hist.col[to.plot[seg]],lty=lty.seg)
        }
      }
    }
    # change colours to something meaningful to return to the user
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[1])] = "stable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[2])] = "unstable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[3])] = "outlier"
    if(names[i]=="delete"){
      infcpt$col.cpts$delete=col.cpts[[i]]
    }
    else{
      infcpt$col.cpts$outlier=col.cpts[[i]]
    }
  }
  
  return(infcpt) # the modified object is returned.
}
