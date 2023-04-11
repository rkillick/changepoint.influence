loo.ind.cpt=function(del.point,k,data,...){
  # function to generate a single loo dataset and apply the PELT algorithm to the data
  
  # del.point     The start index of the k points to delete
  # k             number of points to modify simultaneously
  # data          The original data
  # ...           arguments to be passed to the changepoint method
  
  n=length(data)
  if(any(del.point<1)){stop('del.point is less than 1')}
  else if(any(del.point>n)){stop('del.point is larger than the length of the data')}
  
  # generate the data
  del.ind=del.point:(del.point+k-1)
  data=data[-del.ind]
  
  # apply the changepoint approach to the reduced data
  del.ans=cpt.mean(data,...)
  
  # retrieve the relevant output
  # class vector
  
  # parameter values
  
  # return the output
  return(del.ans)
  
}