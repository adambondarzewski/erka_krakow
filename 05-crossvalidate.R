
cross_validation<-function(Data, fun_fit, fun_predict,fun_error,number_subsamples, size_subsample){
  #Crossvalidating function
  # takes  funtions to fit, predict and calculate error from the objects
  # 
  # number_subsamples is the number of the repetitions
  # size_subsample is subsample size
  
  #local variables

  dim=ifelse(is.null(dim(Data)[1]),length(Data),dim(Data)[1])
  errors<-list()
  subsamples<-list()
  
  for(i in 1:number_subsamples)
    subsamples[[i]]=sort(sample.int(dim,size_subsample))
  
  
  local_fun<-function(index){
    model<-fun_fit(Data[!index,])
    prediction<-fun_predict(model,Data[index,] )
    
    return(fun_error(prediction,Data[index,]))
  }
# main loop
 return(mean(sapply(subsamples,local_fun, simplify=TRUE), na.rm=TRUE))
  
}

#Testing the Function
Data<-DT
fun_fit<-function(x)( mean(x$avgScore, na.rm = TRUE) )
fun_predict<-function(model, x){model}
fun_error<-function(prediction, x){ mean((x$avgScore-prediction)^2,na.rm = TRUE) }

number_subsamples=1000
size_subsample=300
#
#calling the function
x<-cross_validation(Data,fun_fit,fun_predict,fun_error,number_subsamples,size_subsample)

stopifnot( x-var(DT$avgScore)<0)




