lmo.ind.cpt=function(del.point, k=1, data,...){
  # function to generate a lmo dataset and apply the PELT algorithm to the data
  
  # del.point     The start index of the k points to delete
  # k             Number of points to modify simultaneously
  # data          The original data
  # ...           arguments to be passed to the changepoint method
  
  # Indices to be changed
  del.ind=del.point:(del.point+k-1)
  
  # Call loo function 
  out = loo.ind.cpt(del.point=del.ind,data=data,...)
  
  # return the output
  return(out)
  
}

loo.ind.cpt=function(del.point,data,...){
  # function to generate a single loo dataset and apply the PELT algorithm to the data
  
  # del.point     The index of the point to delete
  # data          The original data
  # ...           arguments to be passed to the changepoint method
  
  n=length(data)
  if(any(del.point<1)){stop('del.point is less than 1')}
  else if(any(del.point>n)){stop('del.point is larger than the length of the data')}
  
  # generate the data
  data=data[-del.point]
  
  # apply the changepoint approach to the reduced data
  del.ans=cpt.mean(data,...)
  
  # retrieve the relevant output
  # class vector
  
  # parameter values
  
  # return the output
  return(del.ans)
  
}