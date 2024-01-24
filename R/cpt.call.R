cpt.call=function(data,...){
  # function to apply the required changepoint algorithm to the new data
  
  # data          The new data
  # ...           arguments to be passed to the changepoint method
  
  
  # apply the PELT approach
  ans=cpt.mean(data,...)
  
  # return the new cpt object
  return(ans)
}
