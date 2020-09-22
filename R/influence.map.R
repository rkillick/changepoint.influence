influence.map=function(original.cpts, influence, expected=NULL,hist.tcpt.delete=FALSE,cpt.lty="dashed",cpt.lwd=2,...){
  # images the residuals of fit-expected for class
  
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
    
    ggimage=ggplot()+geom_raster(data=melt(t(resid)),aes(Var1,Var2,fill=value),show.legend=TRUE)+
      ggtitle(paste('Influence map using',method,"method"))+
      labs(x="Observation Number", y = "Outlier Position")+
      scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0,name="Change in segment index")+
      theme(legend.position="bottom")
    ggimage=ggimage+geom_abline(slope=1,colour="grey")+geom_point(data = data.frame(x=original.cpts,y=original.cpts), aes(x, y),colour=col.cpts,alpha=0.8)
    
    print(ggimage)
  }

}