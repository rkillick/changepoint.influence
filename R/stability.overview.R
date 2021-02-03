stability.overview=function(data, original.cpts, influence, cpt.lty="dashed",cpt.lwd=2,ylab='',...){
  # plots the original changepoints with colours indicating whether they have moved within the modify/delete methods
  
  # data              Vector of original data
  # original.cpts     The cpts in the original data
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # cpt.lty           Line type for the changepoint lines
  # cpt.lwd           Line width for the changepoint lines
  # ylab              Label for the y-axis
  # ...               Other graphical parameters

  col.cpts=list()
  
  n=length(data)
  ncpts=length(original.cpts)
  
  original.class=rep(1:(ncpts+1),times=diff(c(0,original.cpts,n)))
  names=names(influence)
  
  for(i in 1:length(influence)){
    method="modify"
    max=n-2
    if(names[i]=="del"){
      method="delete"
      max=n-1
      index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
      influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-n] # replace NA with previous index
      influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
    }

    plot(data,type='l',ylab=ylab,xlab='Time',main=paste('Stability dashboard using',method,"method"),...) # plot the original time series


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

    col.cpts[[i]]=rep("dark green",length(original.cpts))
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(original.cpts[j]))]!=max){
        col.cpts[[i]][j]="orange2"
      }
    }
    col.cpts[[i]][which(diff(original.cpts)==1)]="red"
    col.cpts[[i]][which(diff(original.cpts)==1)+1]="red"
    names(col.cpts)[i]=names[i]
    
    abline(v=original.cpts,col=col.cpts[[i]],lty=cpt.lty,lwd=cpt.lwd)
    # do we want a legend to specify the colours or just leave it to the documentation?
  }
  return(col.cpts)
}