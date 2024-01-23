mmo.ind.cpt=function(out.point, k, data,range,pos=TRUE,same=FALSE,sd=0.01,...){
  # function to generate a single moo dataset and apply the PELT algorithm to the data
  
  # out.point     The start index of the point to modify
  # k             Number of points to modify simultaneously
  # data          The original data
  # range         Max-Min of data
  # pos.=TRUE      If true modification is above the data, if false then below
  # same.=FALSE    If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd.=0.01       jitter to add to the modified point
  # ...           arguments to be passed to the changepoint method
  
  # Indices to be changed
  out.ind=out.point:(out.point+k-1)
  
  # Call moo function 
  out = moo.ind.cpt(out.point=out.ind, data=data, range=range, pos=pos, same=same, sd=sd, ...)
  
  # return the output
  return(out)
}



moo.ind.cpt=function(out.point,data,range,pos=TRUE,same=FALSE,sd=0.01,...){
  # function to generate a single moo dataset and apply the PELT algorithm to the data
  
  # out.point     The index of the point to modify
  # data          The original data
  # range         Max-Min of data
  # pos.=TRUE      If true modification is above the data, if false then below
  # same.=FALSE    If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd.=0.01       jitter to add to the modified point
  # ...           arguments to be passed to the changepoint method
  
  n=length(data)
  if(any(out.point<1)){stop('out.point is less than 1')}
  else if(any(out.point>n)){stop('out.point is larger than the length of the data')}
  
  # generate the data
  data[out.point]=make.many.outlier(data[out.point],range.data=range,pos=pos,same=same,sd=sd)

  # apply the PELT approach
  outlier.ans=cpt.mean(data,...)
  
  # return the new cpt object
  return(outlier.ans)
}