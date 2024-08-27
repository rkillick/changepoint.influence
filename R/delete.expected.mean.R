delete.expected.mean=function(infcpt){
  # Calculates the expected segmentations for LOO based on an original segmentation
  
  # original.class    The class of the original segmentation
  
  n=length(infcpt$org.data)
  ncpts=length(infcpt$org.cpts)-2 # -2 as org.cpts includes 0 and n
  original.class=rep(1:(ncpts+1),times=infcpt$org.seglen)
  cpt=infcpt$org.cpts
  nmodified=length(infcpt$modified)
  
  orig.outlier=cpt[which(infcpt$org.seglen==1)+1] # segments of length 1 in the original data
  if(length(orig.outlier)!=0){
    if(orig.outlier[length(orig.outlier)]==n){orig.outlier=orig.outlier[-length(orig.outlier)]}
  }
  
  expected=matrix(rep(original.class,nmodified),nrow=nmodified,ncol=n,byrow=T)
  expected=t(apply(matrix(1:nmodified,ncol=1),MARGIN=1,FUN=function(i){
    expected[i,infcpt$modified[i]:(infcpt$modified[i]+infcpt$k-1)]=NA # removing the deleted points
    return(expected[i,])
  }))
  
  # need to clear up the segments that don't exist anymore due to the deletion
  expected=t(apply(matrix(1:nmodified,ncol=1),MARGIN=1,FUN=function(i){
    nas=which(is.na(expected[i,]))
    diffna=(expected[i,max(nas)+1]-expected[i,min(nas)-1])
    if(diffna>1){ # we have lost atleast one segment so need to correct numbering
      expected[i,(max(nas)+1):n]=expected[i,(max(nas)+1):n]-diffna+1
    }
    return(expected[i,])
  }))

  return(expected)
}