influence.generate.PELT=function(cptobject,method=c("delete","outlier"),pos=pos,same=same,sd=sd){
  # function to calculate the influence of a given set of data using different methods
  
  # cptobject     cpt object output from the changepoint packages
  # method        method to calculate the influence according to
  # pos=TRUE      MOO: If true outlier is above the data, if false then below
  # same=FALSE    MOO: If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd=0.01       MOO: jitter to add to the outlier point
  
  
  if(class(cptobject)!="cpt"){stop("This function takes a cpt object as the data input")}
  
  n=length(data.set(cptobject))
  data=data.set(cptobject)
  
  ans=list()
  
  if(any(method=="delete")){
    # replicate the generation of data and application of changepoint method to the data
    ansclass.del=sapply(X=1:n,FUN=loo.ind.PELT,data=data,pen.value=pen.value(cptobject),test.stat=test.stat(cptobject),penalty=pen.type(cptobject),minseglen=minseglen(cptobject))

    # collate the output
    anscpts.del=matrix(NA,ncol=n,nrow=n)
    ansparam.del=matrix(NA,nrow=n,ncol=n)
    for(i in 1:n){
      # building segment vector
      class=rep(1:(ncpts(ansclass.del[[i]])+1),times=diff(c(0,cpts(ansclass.del[[i]]),n-1)))
      if(i==1){class=c(NA,class)}
      else if(i==n){class=c(class,NA)}
      else{class=c(class[1:(i-1)],NA,class[i:(n-1)])} # filling the i back in to align everything
      anscpts.del[i,]=class
      
      # building mean param vector
      param=rep(param.est(ansclass.del[[i]])$mean,times=diff(c(0,cpts(ansclass.del[[i]]),n-1)))
      if(i==1){param=c(NA,param)}
      else if(i==n){param=c(param,NA)}
      else{param=c(param[1:(i-1)],NA,param[i:(n-1)])} # filling the i back in to align everything
      ansparam.del[i,]=param
    }
    ans=c(ans,del=list(cpts.del=anscpts.del,param.del=ansparam.del))
  }
  else if(any(method=="outlier")){
    # replicate the generation of data and application of changepoint method to the data
    ansclass.out=sapply(X=1:n,FUN=moo.ind.PELT,data=data,pos=pos,same=same,sd=sd,pen.value=pen.value(cptobject),test.stat=test.stat(cptobject),penalty=pen.type(cptobject),minseglen=minseglen(cptobject))
  
    # collate the output
    anscpts.out=matrix(NA,ncol=n,nrow=n)
    ansparam.out=matrix(NA,nrow=n,ncol=n)
    for(i in 1:n){
      # building segment vector
      anscpts.out[i,]=rep(1:(ncpts(ansclass.out[[i]])+1),times=diff(c(0,cpts(ansclass.out[[i]]),n)))
      
      # building mean param vector
      ansparam.out[i,]=rep(param.est(ansclass.out[[i]])$mean,times=diff(c(0,cpts(ansclass.out[[i]]),n)))
    }
    ans=c(ans,out=list(cpts.out=anscpts.out,param.out=ansparam.out))
  }
  else(stop('method not recognized, must be "delete" or "outlier".'))
  
  # return
  return(ans)
}