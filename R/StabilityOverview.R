StabilityOverview=function(infcpt, cpt.lwd=2,cpt.col=c("#009E73", "#E69F00", "#E41A1C"),
                          cpt.lty=c("dashed","dotdash","dotted"),ylab=' ',xlab='Index', 
                          legend.args=list(display=TRUE,x="top",y=NULL,cex = 1,bty="n",
                                          horiz=TRUE,xpd=FALSE), ...){
  # plots the original changepoints with colours indicating whether they have moved within the modify/delete methods
  
  col.cpts=list()
  lty.cpts = list()
  n=length(infcpt$org.data)
  ncpts=length(infcpt$org.cpts)-2 # disclude 0 and n
  
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

      influence$segment[,1]=1 # First replace any NAs in the first column with ones
      for(ik in 1:infcpt$k){
        index.na=which(is.na(influence$segment))   # delete last k-1 lines since these are NAs
        influence$segment[index.na]=influence$segment[index.na-nrow(influence$segment)] # consecutively replace NA with previous index
      }
    }

    plot(infcpt$org.data,type='l',ylab=ylab,xlab=xlab,main=paste("Stability Dashboard: ", method,"method"),...) # plot the original time series
    
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

    org.cpts=infcpt$org.cpts[-c(1,length(infcpt$org.cpts))] # take off 0 and n
    
    col.cpts[[i]]=rep(cpt.col[1],length(org.cpts)) # "dark green"
    lty.cpts[[i]]=rep(cpt.lty[1],length(org.cpts)) # "dashed" for "green"
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(org.cpts[j]))]!=max){
        col.cpts[[i]][j]=cpt.col[2] # orange2
        lty.cpts[[i]][j]=cpt.lty[2] # "dotdash" for "orange"
      }
    }
    col.cpts[[i]][which(diff(org.cpts)==1)]=cpt.col[3] # red or other option #661100
    col.cpts[[i]][which(diff(org.cpts)==1)+1]=cpt.col[3] 
    lty.cpts[[i]][which(diff(org.cpts)==1)]=cpt.lty[3] # "dotted" for "red
    lty.cpts[[i]][which(diff(org.cpts)==1)+1]=cpt.lty[3]
    names(col.cpts)[i]=names[i]

    abline(v=org.cpts,col=col.cpts[[i]],lty=lty.cpts[[i]],lwd=cpt.lwd) # cpt.lty
    # do we want a legend to specify the colours or just leave it to the documentation?
    
    # change colours to something meaningful to return to the user
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[1])] = "stable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[2])] = "unstable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[3])] = "outlier"
    
    if(legend.args$display){
      legend(x=legend.args$x,y=legend.args$y,
             legend=c("stable", "unstable", "outlier"),
             lty=cpt.lty, lwd = cpt.lwd,
             col = cpt.col,
             xpd=legend.args$xpd, horiz=legend.args$horiz, 
             bty=legend.args$bty, cex = legend.args$cex)
    }

  }
  return(col.cpts)
}