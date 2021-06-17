stability.overview=function(data, original.cpts, influence,cpt.lwd=2,ylab=' ',xlab='Index', display.legend = T, legend.inset = c(0,1.2), legend.cex = 1, ...){
  # plots the original changepoints with colours indicating whether they have moved within the modify/delete methods
  
  # data              Vector of original data
  # original.cpts     The cpts in the original data
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # cpt.lwd           Line width for the changepoint lines
  # ylab              Label for the y-axis
  # ...               Other graphical parameters

  col.cpts=list()
  lty.cpts = list()
  n=length(data)
  ncpts=length(original.cpts)
  
  original.class=rep(1:(ncpts+1),times=diff(c(0,original.cpts,n)))
  names=names(influence)
  
  for(i in 1:length(influence)){
    method="Outlier"
    max=n-2
    if(names[i]=="del"){
      method="Deletion"
      max=n-1
      index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
      influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-n] # replace NA with previous index
      influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
    }

    # plot(data,type='l',ylab=ylab,xlab=xlab,main='Stability dashboard',sub=paste(method,"method"),...) # plot the original time series

    plot(data,type='l',ylab=ylab,xlab=xlab,main=paste("Stability dashboard: ", method,"method"),...) # plot the original time series
    
    cpts=unlist(apply(influence[[i]]$class,1,FUN=function(x){which(diff(x)==1)}))
    cpts=sort(cpts)
    
    if(names[i]=="del"){
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

    col.cpts[[i]]=rep("#009E73",length(original.cpts)) # "dark green"
    lty.cpts[[i]]=rep("dashed",length(original.cpts)) # "dashed" for "green"
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(original.cpts[j]))]!=max){
        col.cpts[[i]][j]="#E69F00" # orange2
        lty.cpts[[i]][j]="dotdash" # "dotdash" for "orange"
      }
    }
    col.cpts[[i]][which(diff(original.cpts)==1)]="#D55E00" # red or other option #661100
    col.cpts[[i]][which(diff(original.cpts)==1)+1]="#D55E00" 
    lty.cpts[[i]][which(diff(original.cpts)==1)]="dotted" # "dotted" for "red
    lty.cpts[[i]][which(diff(original.cpts)==1)+1]="dotted" 
    names(col.cpts)[i]=names[i]

    abline(v=original.cpts,col=col.cpts[[i]],lty=lty.cpts[[i]],lwd=cpt.lwd) # cpt.lty
    # do we want a legend to specify the colours or just leave it to the documentation?
    
    col.cpts[[i]][which(col.cpts[[i]]=="#009E73")] = "green"
    col.cpts[[i]][which(col.cpts[[i]]=="#E69F00")] = "orange"
    col.cpts[[i]][which(col.cpts[[i]]=="#D55E00")] = "red"
    
    if(display.legend){
      legend("top", c("stable", "unstable", "outlier"), 
             lty=c(2,4,3), lwd = rep(3,3),
             col = c("#009E73", "#E69F00", "#D55E00"),
             inset=legend.inset, xpd=T, horiz=TRUE, bty="n", cex = legend.cex)
    }

  }
  

  return(col.cpts)
}