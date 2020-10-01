influence.map=function(original.cpts, influence, expected=NULL,data=NULL,include.data=FALSE,cpt.lty="dashed",cpt.lwd=2,...){
  # images the residuals of fit-expected for class
  
  # original.cpts     The cpts in the original data (not including 0 and n)
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
    
    ggimage=ggplot()+geom_raster(data=melt(t(resid)),aes(X1,X2,fill=value),show.legend=TRUE)+
      labs(x="Observation Index\n\nFewer Cpts                        More Cpts", y = "Outlier Position")+
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

}