setMethod("influence","cpt",function(model,method=c("delete","outlier"), nrep,k=1, pos=TRUE,same=FALSE,sd=0.01, random=FALSE){
  # function to calculate the influence of a given set of data using different methods
  
  # object        cpt object output from the changepoint packages
  # method        method to calculate the influence according to
  # k             Number of points to modify simultaneously
  # pos=TRUE      MOO: If true modification is above the data, if false then below
  # same=FALSE    MOO: If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd=0.01       MOO: jitter to add to the modify point
  # random=FALSE  logical: If true then randomly choose the indices to be modified, if false then all indices are consecutively modified
  # nrep          number of indices to be modified if random=TRUE
  
  if(!inherits(model,"cpt")){stop("This function takes a cpt object as the model input")}
  if(cpttype(model)!="mean"){stop("Currently only models generated from the cpt.mean function are supported.")}

  
  n=length(data.set(model))
  data=data.set(model)
  if(random){
    if(nrep>(n-k+1)){stop("Number of indices to be drawn, nrep, needs to be smaller than n-k+1")}
  }

  ans=list()
  
  # Settings vary for random=TRUE versus random=FALSE
  # note that in the matrices rows are the different manipulations and the columns are the time index (the way it should be!)
  if(random){
    nrep = nrep
    end_index = nrep
    random_indices = sample(1:(n-k+1), nrep) # Sample the random starting indices, draw them at start such that we use same indices for leave out or modify setting in case both options are asked for
  }else{
    nrep = n
    end_index = n-k+1
  }
  
  if(any(method=="delete")){
    
    if(random){
      ansobject.del=sapply(X=random_indices,FUN=lmo.ind.cpt, k=k, data=data,pen.value=pen.value(model),test.stat=test.stat(model),penalty=pen.type(model),minseglen=minseglen(model),method=method(model))
    }else{
      ansobject.del=sapply(X=1:(n-k+1),FUN=lmo.ind.cpt, k=k, data=data,pen.value=pen.value(model),test.stat=test.stat(model),penalty=pen.type(model),minseglen=minseglen(model),method=method(model))
    }
    # indices and set indices above, leave out if else part.

    # collate the output
    ansclass.del=matrix(NA,ncol=n,nrow=nrep)
    ansparam.del=matrix(NA,nrow=nrep,ncol=n)

    for(i in 1:end_index){ # Index indicating the number of points which we delete

      # We need a new index indicating the point that is deleted
      if(random){
        ii=random_indices[i]
      }else{
        ii=i
      }
      
      # building segment vector
      class=rep(1:(ncpts(ansobject.del[[i]])+1),times=diff(c(0,cpts(ansobject.del[[i]]),n-k)))
      if(ii==1){class=c(rep(NA,k),class)} # If first point is deleted
      else if(ii==(n-k+1)){class=c(class,rep(NA,k))} # If last point is deleted
      else{class=c(class[1:(ii-1)],rep(NA,k),class[ii:(n-k)])} # filling the deleted indices back in to align everything
      ansclass.del[i,]=class
      
      # building mean param vector
      param=rep(param.est(ansobject.del[[i]])$mean,times=diff(c(0,cpts(ansobject.del[[i]]),n-k)))
      if(ii==1){param=c(rep(NA,k),param)}
      else if(ii==(end_index)){param=c(param,rep(NA,k))}
      else{param=c(param[1:(ii-1)],rep(NA,k),param[ii:(n-k)])} # filling the deleted indices back in to align everything
      ansparam.del[i,]=param
    }
    ans$delete=list(class.del=ansclass.del,param.del=ansparam.del)
    method=method[-which(method=="delete")]
  }
  if(any(method=="outlier")){
    
    # If random=FALSE: replicate the generation of data and application of changepoint method to the data
    if(random){
      ansobject.out=sapply(X=random_indices,FUN=mmo.ind.cpt,k=k,data=data,range=diff(range(data)),pos=pos,same=same,sd=sd,pen.value=pen.value(model),test.stat=test.stat(model),penalty=pen.type(model),minseglen=minseglen(model),method=method(model))
    }else{
      ansobject.out=sapply(X=1:(n-k+1),FUN=mmo.ind.cpt,k=k,data=data,range=diff(range(data)),pos=pos,same=same,sd=sd,pen.value=pen.value(model),test.stat=test.stat(model),penalty=pen.type(model),minseglen=minseglen(model),method=method(model))
    }

    # collate the output
    ansclass.out=matrix(NA,ncol=n,nrow=nrep)
    ansparam.out=matrix(NA,nrow=nrep,ncol=n)
    
    # If random=FALSE
    for(i in 1:(end_index)){
      # building segment vector
      ansclass.out[i,]=rep(1:(ncpts(ansobject.out[[i]])+1),times=diff(c(0,cpts(ansobject.out[[i]]),n)))
      
      # building mean param vector
      ansparam.out[i,]=rep(param.est(ansobject.out[[i]])$mean,times=diff(c(0,cpts(ansobject.out[[i]]),n)))
    }
    ans$outlier=list(class.out=ansclass.out,param.out=ansparam.out)
    method=method[-which(method=="outlier")]
  }
  
  if(length(method)>0){
    warning('method contains elements that are not recognized, must be "delete" or "outlier".')
  }
  
  return(ans)
})