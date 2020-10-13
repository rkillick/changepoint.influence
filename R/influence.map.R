influence.map=function(original.cpts, influence, resid=NULL,data=NULL,include.data=FALSE,cpt.lty="dashed",cpt.lwd=2,...){
  # images the residuals of fit-expected for class
  
  # original.cpts     The cpts in the original data (not including 0 and n)
  # influence         The influenced cpts and parameters (output from influence.generate.** functions)
  # expected          The expected segmentation based on original.cpts (if NULL is calculated using *.expected.mean functions)
  # data              The original time series
  # include.data      Logical, whether to include a plot of the original time series with cpts
  # cpt.lty           Line type for the changepoint lines
  # cpt.lwd           Line width for the changepoint lines
  
  n=nrow(influence[[1]]$class)
  ncpts=length(original.cpts)
  
  original.class=rep(1:(ncpts+1),times=diff(c(0,original.cpts,n)))
  names=names(influence)
  
  return=0
  # if the resid isn't given it needs calculating
  if(is.null(resid)){
    return=1
    expected=list()
  
    for(i in 1:length(influence)){
      if(eval(parse(text=paste("is.null(expected$",names[i],")",sep="")))){
        eval(parse(text=paste("expected$",names[i],"=",names[i],".expected.mean(original.class)",sep=""))) # calculated the expected
      }
    }
  }
  
  if(include.data==TRUE){
    if(is.null(data)){
      stop("data argument must be supplied if include.data=TRUE.")
    }
    lay <- rbind(c(1,1,1,1,1),
                 c(2,2,2,2,2),
                 c(2,2,2,2,2),
                 c(2,2,2,2,2))
    
    data=data.frame(data=data,index=1:length(data))
  }
  
  for(i in 1:length(influence)){
    method="outlier"
    max=n-2
    if(names[i]=="del"){
      method="deletion"
      max=n-1
      
      if(is.null(resid)){
        # Dealing with the NAs temporarily (not returned to user) so we can plot nicely
        index.na=which(is.na(influence[[i]]$class.del))[-1] # -1 as we will deal with the first instance separately
        influence[[i]]$class.del[index.na]=influence[[i]]$class.del[index.na-n] # replace NA with previous index
        influence[[i]]$class.del[1,1]=1 # replace the first NA with 1
      
        # repeat for expected
        index.na=which(is.na(expected$del))[-1] # -1 as we will deal with the first instance separately
        expected$del[index.na]=expected$del[index.na-n] # replace NA with previous index
        expected$del[1,1]=1 # replace the first NA with 1
      }
    }
    
    if(is.null(resid)){
      resid=influence[[i]]$class-expected[[i]]
    ########### note the partial matching used here (class.del or class.out)
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
    
    ggimage=ggplot()+geom_raster(data=melt(t(resid)),aes(X1,X2,fill=value),show.legend=TRUE)+
      labs(x="Observation Index\n\nFewer Cpts                        More Cpts", y = "Altered Data Point")+
      scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0, name="")+
      theme(legend.position="bottom")
    ggimage=ggimage+geom_abline(slope=1,colour="grey")+geom_point(data = data.frame(x=original.cpts,y=original.cpts), aes(x, y),colour=col.cpts,alpha=0.8)
    
    if(include.data==TRUE){
      ggcpt=ggplot(data=data)+geom_line(aes(x=index,y=data))+ labs(x="Index", y = "")+
              geom_vline(xintercept = original.cpts, colour = col.cpts, linetype = cpt.lty) # add cpts
      ggcpt=ggplotGrob(ggcpt)
      
      maxWidth = grid::unit.pmax(ggimage$widths[2:5], ggcpt$widths[2:5])
      ggimage$widths[2:5] <- as.list(maxWidth)
      ggcpt$widths[2:5] <- as.list(maxWidth)
      grid.arrange(grobs = list(ggcpt, ggimage), layout_matrix=lay, top = textGrob(paste('Influence map using',method,"method"),gp=gpar(fontsize=14)))
    }
    else{
      ggimage=ggimage+ggtitle(paste('Influence map using',method,"method"))+
        theme(plot.title = element_text(hjust = 0.5))
      print(ggimage)
    }
  }
  if(return){
    return(resid)
  }
}