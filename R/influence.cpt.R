setMethod("influence","cpt",function(model,method=c("delete","outlier"), k=1, modify="all", n.modify, pos=TRUE,same=FALSE,sd=0.01){
  # function to calculate the influence of a given set of data using different methods
  
  # object        cpt object output from the changepoint packages
  # method        method to calculate the influence according to
  # k             Number of points to modify consecutively
  # modify        Index/method of choosing modification points "all", "random", "stratified", c()
  # n.modify      Number of points to modify, used in "random" and "stratified" only
  # pos=TRUE      MOO: If true modification is above the data, if false then below
  # same=FALSE    MOO: If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd=0.01       MOO: jitter to add to the modify point

  if(!inherits(model,"cpt")){stop("This function takes a cpt object as the model input")}
  if(cpttype(model)!="mean"){stop("Currently only models generated from the cpt.mean function are supported.")}
  
  n=length(data.set(model))
  data=data.set(model)
  
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
    start = sample(1:(n-k+1), n.modify) # Sample the random starting indices, draw them at start such that we use same indices for leave out or modify setting in case both options are asked for
  }else if(modify=="stratified"){
    seglen=seg.len(model)

    # removing last k from start options
    tmp.sum=seglen[end(seglen)[1]]
    removed=0
    while(tmp.sum<(k+1)){
      removed=removed+seglen[end(seglen)[1]]
      seglen=seglen[-end(seglen)[1]] # remove last segment as still smaller than k
      tmp.sum=tmp.sum+seglen[end(seglen)[1]]
    }
    seglen[end(seglen)[1]]=seglen[end(seglen)[1]]-k+removed+1 # remove the last k data points as start options

    seg.sample=floor(seglen*n.modify/n) # relative number to be drawn from each segment
    remainder=n.modify-sum(seg.sample)
    cpts=c(0,model@cpts) # get the changepoints for segment starts (could also do cumsum(seglen) if wanting to remove dependence on cpts)
    start=unlist(apply(matrix(1:length(seg.sample),ncol=1),1,FUN=function(ind){
      nsample=seg.sample[ind]
      index=(cpts[ind]+1):cpts[ind+1]
      index=index[index<(n-k+1)]
      return(sort(sample(index,nsample)))
    })) # stratified cpts from each segment
    if(any(seg.sample==0)){ # sample from those segments with zero representation so far
      index=unlist(apply(matrix(which(seg.sample==0),ncol=1),1,FUN=function(ind){
        return((cpts[ind]+1):cpts[ind+1])
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
  
  # note that in the matrices rows are the different manipulations and the columns are the time index (the way it should be!)  
  if(any(method=="delete")){
    
    ansobject.del=sapply(X=start,FUN=loo.ind.cpt, k=k, data=data,pen.value=pen.value(model),test.stat=test.stat(model),penalty=pen.type(model),minseglen=minseglen(model),method=method(model))
 
    # collate the output
    ansclass.del=matrix(NA,ncol=n,nrow=n.modify)
    ansparam.del=matrix(NA,ncol=n,nrow=n.modify)

    for(i in 1:n.modify){ # Index indicating the number of points which we modify

      ii=start[i] # the point that is deleted (for convenience)

      
      # building segment vector
      class=rep(1:(ncpts(ansobject.del[[i]])+1),times=seg.len(ansobject.del[[i]]))
      if(ii==1){class=c(rep(NA,k),class)} # If first point is deleted
      else if(ii==(n-k+1)){class=c(class,rep(NA,k))} # If last point is deleted
      else{class=c(class[1:(ii-1)],rep(NA,k),class[ii:length(class)])} # filling the deleted indices back in to align everything
      ansclass.del[i,]=class
      
      # building mean param vector
      param=rep(param.est(ansobject.del[[i]])$mean,times=seg.len(ansobject.del[[i]]))
      if(ii==1){param=c(rep(NA,k),param)}
      else if(ii==(n-k+1)){param=c(param,rep(NA,k))}
      else{param=c(param[1:(ii-1)],rep(NA,k),param[ii:length(param)])} # filling the deleted indices back in to align everything
      ansparam.del[i,]=param
    }
    ans$delete=list(class.del=ansclass.del,param.del=ansparam.del)
    method=method[-which(method=="delete")]
  }
  if(any(method=="outlier")){
    
    ansobject.out=sapply(X=start,FUN=moo.ind.cpt,k=k,data=data,range=diff(range(data)),pos=pos,same=same,sd=sd,pen.value=pen.value(model),test.stat=test.stat(model),penalty=pen.type(model),minseglen=minseglen(model),method=method(model))

    # collate the output
    ansclass.out=matrix(NA,ncol=n,nrow=n.modify)
    ansparam.out=matrix(NA,ncol=n,nrow=n.modify)
    
    for(i in 1:n.modify){ # Index indicating the number of points which we modify
      # building segment vector
      ansclass.out[i,]=rep(1:(ncpts(ansobject.out[[i]])+1),times=seg.len(ansobject.out[[i]]))
      
      # building mean param vector
      ansparam.out[i,]=rep(param.est(ansobject.out[[i]])$mean,times=seg.len(ansobject.out[[i]]))
    }
    ans$outlier=list(class.out=ansclass.out,param.out=ansparam.out)
    method=method[-which(method=="outlier")]
  }
  
  if(length(method)>0){
    warning('method contains elements that are not recognized, must be "delete" or "outlier".')
  }
  
  return(ans)
})