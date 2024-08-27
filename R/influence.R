influence=function(model,method=c("delete","outlier"), k=1, modify="all", n.modify, pos=TRUE,same=FALSE,sd=0.01){
  # function to calculate the influence of a given set of data using different methods
  # model is assumed to contain:
  #     - data
  #     - cpts        (note that we start at zero and end at n)
  #     - seglen
  #     - param
  #     - cptfuncname (see man file for specified form of output required)
  #     - cptfuncargs (character vector)
  
  # method        method to calculate the influence according to
  # k             Number of points to modify consecutively
  # modify        Index/method of choosing modification points "all", "random", "stratified", c()
  # n.modify      Number of points to modify, used in "random" and "stratified" only
  # pos=TRUE      MOO: If true modification is above the data, if false then below
  # same=FALSE    MOO: If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd=0.01       MOO: jitter to add to the modify point

  model=inf.input.model(model) # extract the things we need from the model object for uniformity

  n=length(model$data)
    
  if(k>(n-1) | k<1){stop(paste("k must be between 1 and",n-1))}
  if(any(modify==c("random","stratified"))){
    if(n.modify>(n-k+1)){stop(paste("Number of indices to be drawn, n.modify, needs to be smaller than",n-k+1))}
  }
  
  
  
  # creating start vector depending on modify strategy
  if(inherits(modify,"numeric")){
    if(any(modify>(n-k+1))){stop(paste("User supplied indices must be between 1 and",n-k+1))}
    if(any(modify<1)){stop(paste("User supplied indices must be between 1 and",n-k+1))}
    start=sort(unique(modify)) # take out any (potentially unintentional) duplicates
  }else if(modify=="all"){
    start=1:(n-k+1)
  }else if(modify=="random"){
    start = sort(sample(1:(n-k+1), n.modify)) # Sample the random starting indices, draw them at start such that we use same indices for leave out or modify setting in case both options are asked for
  }else if(modify=="stratified"){

    # removing last k from start options
    tmp.sum=model$seglen[end(model$seglen)[1]]
    inf.seglen=model$seglen
    removed=0
    while(tmp.sum<(k+1)){
      removed=removed+inf.seglen[end(inf.seglen)[1]]
      inf.seglen=inf.seglen[-end(inf.seglen)[1]] # remove last segment as still smaller than k
      tmp.sum=tmp.sum+inf.seglen[end(inf.seglen)[1]]
    }
    inf.seglen[end(inf.seglen)[1]]=inf.seglen[end(inf.seglen)[1]]-k+removed+1 # remove the last k data points as start options

    seg.sample=floor(inf.seglen*n.modify/n) # relative number to be drawn from each segment
    remainder=n.modify-sum(seg.sample)
    start=unlist(apply(matrix(1:length(seg.sample),ncol=1),1,FUN=function(ind){
      nsample=seg.sample[ind]
      index=(model$cpts[ind]+1):model$cpts[ind+1]
      index=index[index<(n-k+1)]
      return(sort(sample(index,nsample)))
    })) # stratified cpts from each segment
    if(any(seg.sample==0)){ # sample from those segments with zero representation so far
      index=unlist(apply(matrix(which(seg.sample==0),ncol=1),1,FUN=function(ind){
        return((model$cpts[ind]+1):model$cpts[ind+1])
      }))
      if(length(index)<remainder){ # if not enough short segments to cover the remainder
        start=sort(c(start,index)) # sample what there is
        remainder=remainder-length(index) # update remainder
      }
      else{ # just sample from the short segments uniformly
        start=sort(c(start,sample(index,remainder))) # append remainder
        remainder=0 # update as no more needed
      }
    }
    if(remainder>0){ # uniformly sample the rest
      index=1:(n-k+1)
      index=index[-start]
      start=sort(c(start,sample(index,remainder)))
    }
  }else{
    stop("The modify argument should be 'all', random', 'stratified', or a vector of indices.")
  }
  n.modify=length(start)
  
  
  
  # now calculate the segmentations and store
  ans=list()
  ans$org.data=model$data
  ans$org.cpts=model$cpts
  ans$org.seglen=model$seglen
  ans$org.param=model$param
  ans$modified=start
  ans$k=k
  
  ans$inputargs=list()
  ans$inputargs$cptfuncname=model$cptfuncname
  ans$inputargs$cptfuncargs=model$cptfuncargs
  ans$inputargs$method=method
  ans$inputargs$modify=modify
  ans$inputargs$n.modify=n.modify
  ans$inputargs$pos=pos
  ans$inputargs$same=same
  ans$inputargs$sd=sd
  
  ans$delete=list();ans$delete$segment=matrix(0,ncol=1,nrow=1)
  ans$outlier=list();ans$outlier$segment=matrix(0,ncol=1,nrow=1)
  
  # note that in the matrices rows are the different manipulations and the columns are the time index (the way it should be!)  
  if(any(method=="delete")){
    
    ansobject=lapply(X=start,FUN=loo.ind.cpt, k=k, model=model)
 
    nparam=ncol(ansobject[[1]]$param.est) # already been checked that this is a matrix

    # collate the output
    ansseg=matrix(NA,ncol=n,nrow=n.modify)
    ansparam=array(NA, dim=c(n.modify,n,nparam))# ncol=n,nrow=n.modify)

    for(i in 1:n.modify){ # Index indicating the number of points which we modify

      ii=start[i] # the point that is deleted (for convenience)

      # building segment vector
      segs=rep(1:(ansobject[[i]]$ncpts+1),times=ansobject[[i]]$seg.len)
      if(ii==1){segs=c(rep(NA,k),segs)} # If first point is deleted
      else if(ii==(n-k+1)){segs=c(segs,rep(NA,k))} # If last point is deleted
      else{segs=c(segs[1:(ii-1)],rep(NA,k),segs[ii:length(segs)])} # filling the deleted indices back in to align everything
      ansseg[i,]=segs
      
      # building mean param vector
      if(!is.null(model$param)){ # parameters are estimated by the changepoint model
        param=NULL
        for(p in 1:nparam){
          param=cbind(param,rep(ansobject[[i]]$param.est[,p],times=ansobject[[i]]$seg.len))
        }

        if(nparam==1){ # ruddy R making matrices vectors when selecting a single row or column
          if(ii==1){param=c(rep(NA,k),param)}
          else if(ii==(n-k+1)){param=c(param,rep(NA,k))}
          else{param=c(param[1:(ii-1)],rep(NA,k),param[ii:length(param)])} # filling the deleted indices back in to align everything
          ansparam[i,,]=param
        }
        else{
          if(ii==1){param=rbind(matrix(NA,nrow=k,ncol=nparam),param)}
          else if(ii==(n-k+1)){param=rbind(param,matrix(NA,nrow=k,ncol=nparam))}
          else{param=rbind(param[1:(ii-1),],matrix(NA,nrow=k,ncol=nparam),param[ii:nrow(param),])} # filling the deleted indices back in to align everything
          ansparam[i,,]=param
        }
      }
    }
    ans$delete=list(segment=ansseg,param=ansparam)
    method=method[-which(method=="delete")]
  }
  if(any(method=="outlier")){
    
    ansobject=lapply(X=start,FUN=moo.ind.cpt,k=k,model=model,range=diff(range(model$data)),pos=pos,same=same,sd=sd)

    nparam=ncol(ansobject[[1]]$param.est) # already been checked that this is a matrix

    # collate the output
    ansseg=matrix(NA,ncol=n,nrow=n.modify)
    ansparam=array(NA,dim=c(n.modify,n,nparam)) # ncol=n,nrow=n.modify)
    
    for(i in 1:n.modify){ # Index indicating the number of points which we modify
      # building segment vector
      ansseg[i,]=rep(1:(ansobject[[i]]$ncpts+1),times=ansobject[[i]]$seg.len)
      
      # building mean param vector
      param=NULL
      for(p in 1:nparam){
        param=cbind(param,rep(ansobject[[i]]$param.est[,p],times=ansobject[[i]]$seg.len))
      }
      ansparam[i,,]=param
    }
    ans$outlier=list(segment=ansseg,param=ansparam)
    method=method[-which(method=="outlier")]
  }
  
  if(length(method)>0){
    warning('method contains elements that are not recognized, must be "delete" or "outlier".')
  }
  
  return(ans)
}