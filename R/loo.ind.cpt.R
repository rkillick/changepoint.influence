loo.ind.cpt=function(del.point, k=1, model,...){
  # function to generate a lmo dataset and apply the PELT algorithm to the data
  
  # del.point     The start index of the k points to delete
  # k             Number of points to modify simultaneously
  # data          The original data
  # ...           arguments to be passed to the changepoint method
  
  # Indices to be changed
  del.ind=del.point:(del.point+k-1)
  
  n=length(model$data)
  if(any(del.ind<1)){stop('Generating negative indices: del.point is less than 1 or k is negative')}
  else if(any(del.ind>n)){stop('Generating indices larger than the length of the data: del.point or k is too large')}
  
  # generate the data
  data=model$data[-del.ind]

  # Call changepoint method on new data
  out=tryCatch(eval(parse(text=paste0(model$cptfuncname,"(data,",model$cptfuncargs,")"))))
  if(inherits(out,"try-error")){print(out);stop("The above error occured in calling cptfuncname with cptfuncargs on the new data.")}

  out=inf.output.model(out) # modify into the correct format across potential cpt calls
  
  # return the output
  return(out)
}