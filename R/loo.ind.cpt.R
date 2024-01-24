loo.ind.cpt=function(del.point, k=1, data,...){
  # function to generate a lmo dataset and apply the PELT algorithm to the data
  
  # del.point     The start index of the k points to delete
  # k             Number of points to modify simultaneously
  # data          The original data
  # ...           arguments to be passed to the changepoint method
  
  # Indices to be changed
  del.ind=del.point:(del.point+k-1)
  
  n=length(data)
  if(any(del.ind<1)){stop('Generating negative indices: del.point is less than 1 or k is negative')}
  else if(any(del.ind>n)){stop('Generating indices larger than the length of the data: del.point or k is too large')}
  
  # generate the data
  data=data[-del.ind]

  # Call changepoint method on new data
  out = cpt.call(data=data,...)
  
  # return the output
  return(out)
}