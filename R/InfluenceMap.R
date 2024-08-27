InfluenceMap=function(infcpt, include.data=FALSE,
                      influence.col=c("#0C4479","white","#AB9783"),
                      cpt.col=c("#009E73", "#E69F00", "#E41A1C"),
                      cpt.lty=c("dashed","dotdash","dotted"),ylab='',ggops=NULL){
  # images the residuals of fit-expected for class
  
  ## Setting variables used by ggplot below to avoid no visible binding error in CRAN checks:
  Var1=Var2=value=index=NULL
  
  col.cpts=list()
  lty.cpts = list()
  
  n=length(infcpt$org.data)
  ncpts=length(infcpt$org.cpts)-2 # as includes 0 and n
  original.segment=rep(1:(ncpts+1),times=infcpt$org.seglen)
  original.cpts=infcpt$org.cpts[-c(1,length(infcpt$org.cpts))] # take off 0 and n
  
  names=NULL
  if(dim(infcpt$delete$segment)[2]!=1){names=c(names,"delete")}
  if(dim(infcpt$outlier$segment)[2]!=1){names=c(names,"outlier")}
  
  # if the expected isn't given it needs calculating
  for(i in 1:length(names)){
    if(eval(parse(text=paste("is.null(infcpt$",names[i],"$expected)",sep="")))){
      eval(parse(text=paste("infcpt$",names[i],"$expected=",names[i],".expected.mean(original.segment)",sep=""))) # calculated the expected
    }
  }
  
  if(include.data==TRUE){
    if(is.null(infcpt$org.data)){
      stop("infcpt$org.data argument must be supplied if include.data=TRUE.")
    }
    lay <- rbind(c(1,1,1,1,1),
                 c(2,2,2,2,2),
                 c(2,2,2,2,2),
                 c(2,2,2,2,2))
    
    data=data.frame(data=infcpt$org.data,index=1:length(infcpt$org.data))
  }
  
  for(i in 1:length(names)){
    method="Outlier"
    max=n-2
    if(names[i]=="delete"){
      method="Deletion"
      max=n-1
      
      # Dealing with the NAs temporarily (not returned to user) so we can plot nicely
      class=infcpt$delete$segment # take a copy to modify
      index.na=which(is.na(infcpt$delete$segment))
      if(index.na[1]==1){
        class[1,1]=1
        index.na=index.na[-1]
      }
      while(length(index.na)!=0){
        # sequential NAs so iterating
        # RK: look at a more efficient way to do this
        class[index.na]=class[index.na-n] # replace NA with previous index (which could be NA)
        index.na=which(is.na(class)) # update na list
      }
    
      # repeat for expected
      expected=infcpt$delete$expected # take a copy to modify
      index.na=which(is.na(infcpt$delete$expected))
      if(index.na[1]==1){
        expected[1,1]=1
        index.na=index.na[-1]
      }
      while(length(index.na)!=0){
        # sequential NAs so iterating
        # RK: look at a more efficient way to do this
        expected[index.na]=expected[index.na-n] # replace NA with previous index
        index.na=which(is.na(expected)) # update na list
      }
    }
    else{ # take copies here too to make later code work for both
      class=infcpt$outlier$segment
      expected=infcpt$outlier$expected
    }
    resid=class-expected
    
    cpts=unlist(apply(class,1,FUN=function(x){which(diff(x)==1)}))
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
    
    col.cpts[[i]]=rep(cpt.col[1],length(original.cpts))
    lty.cpts[[i]]=rep(cpt.lty[1],length(original.cpts))
    for(j in 1:ncpts){
      if(tcpts[which(names(tcpts)==as.character(original.cpts[j]))]!=max){
        col.cpts[[i]][j]=cpt.col[2]
        lty.cpts[[i]][j]=cpt.lty[2]
      }
    }
    col.cpts[[i]][which(diff(original.cpts)==1)]=cpt.col[3]
    col.cpts[[i]][which(diff(original.cpts)==1)+1]=cpt.col[3]
    lty.cpts[[i]][which(diff(original.cpts)==1)]=cpt.lty[3]
    lty.cpts[[i]][which(diff(original.cpts)==1)+1]=cpt.lty[3]
    names(col.cpts)[i]=names[i]
    
    ggimage=ggplot()+geom_raster(data=reshape2::melt(t(resid)),aes(Var1,Var2,fill=value),show.legend=TRUE)+
      labs(x="Index\nFewer Cpts                        More Cpts", y = "Altered Data Point")+
      scale_fill_gradient2(low=influence.col[1],mid=influence.col[2],high=influence.col[3],midpoint=0, name="")
    ggimage=ggimage+geom_abline(slope=1,colour="grey")+geom_point(data = data.frame(X1=original.cpts,X2=original.cpts), aes(X1, X2),colour=col.cpts[[i]],alpha=0.8)
    ggimage=ggimage+theme_classic()+theme(legend.position="bottom",legend.text=element_text(size=11))
    
    if(include.data==TRUE){
      ggcpt=ggplot(data=data.frame(infcpt$org.data))+geom_line(aes(x=1:length(data[,1]),y=data[,1]))+ labs(x="Index", y = ylab)+
              geom_vline(xintercept = original.cpts, colour = col.cpts[[i]], linetype = lty.cpts[[i]]) # add cpts
      ggcpt=ggcpt+theme_classic()
      ggcpt=ggcpt+ggops # add user options at the end so can override our defaults
      ggcpt=ggplotGrob(ggcpt)
      
      ggimage=ggimage+ggops # add user options at the end so can override our defaults
      
      maxWidth = grid::unit.pmax(ggimage$widths[2:5], ggcpt$widths[2:5])
      ggimage$widths[2:5] <- as.list(maxWidth)
      ggcpt$widths[2:5] <- as.list(maxWidth)
      
      gridtitleops=list()
      class(gridtitleops)="gpar"
      gridtitleops$fontsize=14
      if(any(names(ggops)=="plot.title")){
        gridtitleops$fontfamily=ggops$plot.title$family
        gridtitleops$fontface=ggops$plot.title$face
        gridtitleops$col=ggops$plot.title$colour
        gridtitleops$fontsize=ggops$plot.title$size
      }
      grid.arrange(grobs = list(ggcpt, ggimage), layout_matrix=lay, top = textGrob(paste('Influence map:',method,"method"),gp=gridtitleops))
    }
    else{
      ggimage=ggimage+ggtitle(label=paste('Influence map: ',method,"method"))+
        theme(plot.title = element_text(hjust = 0.5))
      ggimage=ggimage+ggops # add user options at the end so can override our defaults
      print(ggimage)
    }
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[1])] = "stable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[2])] = "unstable"
    col.cpts[[i]][which(col.cpts[[i]]==cpt.col[3])] = "outlier"
  }
  if(names[i]=="delete"){
    infcpt$col.cpts$delete=col.cpts[[i]]
  }
  else{
    infcpt$col.cpts$outlier=col.cpts[[i]]
  }
  
  return(infcpt)
}