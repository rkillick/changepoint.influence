moo.ind.cpt=function(out.point, k, model,range,pos=TRUE,same=FALSE,sd=0.01,...){
  # function to generate a single moo dataset and apply the PELT algorithm to the data
  
  # out.point     The start index of the point to modify
  # k             Number of points to modify consecutively
  # model         The original model list
  # range         Max-Min of data
  # pos=TRUE      If true modification is above the data, if false then below
  # same=FALSE    If TRUE the original value doesn't matter the out.point is a new value, if true then range added to the original point
  # sd=0.01       jitter to add to the modified point
  # ...           arguments to be passed to the changepoint method
  
  # Create indices to be changed
  out.ind=out.point:(out.point+k-1)
  
  n=length(model$data)
  if(any(out.ind<1)){stop('Generating negative indices: out.point is less than 1 or k is negative')}
  else if(any(out.ind>n)){stop('Generating indices larger than the length of the data: out.point or k is too large')}
  
  # generate the data
  data=model$data
  data[out.point]=make.many.outlier(data[out.point],range.data=range,pos=pos,same=same,sd=sd)
  
  # Call changepoint method on new data
  out=tryCatch(eval(parse(text=paste0(model$cptfuncname,"(data,",model$cptfuncargs,")"))))
  if(inherits(out,"try-error")){print(out);stop("The above error occured in calling cptfuncname with cptfuncargs on the new data.")}
  
  out=inf.output.model(out) # modify into the correct format across potential cpt calls
  
  # return the output
  return(out)
}