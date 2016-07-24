
#' @return list; list of 3-element lists with slots: pars - parameters chosen using training set, score - score on test set and Lambda used
cross_validation <- function(DT, iterations, training_set_size, penalty = function(x) abs(x)) {
  
  
  start<-rep(1/32, 64)
  start1<-rep(1/32, 64)
  lambda=0
  scores <- list(NULL)
  for (it in 1:iterations) {
    
    lambda=it/3
    rows_sample <- sample(nrow(DT), training_set_size)
    
    DT_train <- DT[rows_sample]
    DT_train_shrinked <- DT_shrinked[rows_sample]
    
    pars_chosen <- optim(par = start, calculate_score, DT_train=DT,DT_train_shrinked=DT_shrinked ,lambda=lambda,method="CG",score_fun =score,control=list(maxit=100) )
    pars_chosen1<-optim(par = start1, calculate_score, DT_train=DT,DT_train_shrinked=DT_shrinked ,lambda=lambda,method="CG",score_fun =score3,control=list(maxit=100) )
    
    start<-pars_chosen$par
    start1<-pars_chosen1$par
    # changing data set
    # WARNING: name train is inappropriate here, it shuld be DT_test or something like that
   
     DT_train <- DT[!rows_sample]
    DT_train_shrinked <- DT_shrinked[!rows_sample]
    score_test_set <- calculate_score(pars = pars_chosen$par
                                              , DT_train_shrinked = DT_train_shrinked
                                              , DT_train = DT_train
                                              , penalty = penalty
                                              , lambda=lambda
                                              , score_fun = score)
    score_test_set1 <- calculate_score(pars = pars_chosen1$par
                                              , DT_train_shrinked = DT_train_shrinked
                                              , DT_train = DT_train
                                              , penalty = penalty
                                              ,lambda=lambda
                                              ,score_fun = score3)
    
    
 
    
    scores[[it]] <- list(pars = pars_chosen, score = score_test_set, lambda=lambda, score1 = score_test_set)
  }
  
  return(scores)
}



