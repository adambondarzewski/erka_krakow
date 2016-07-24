
#' @return list; list of 2-element lists with slots: pars - parameters chosen using training set, score - score on test set
cross_validation <- function(DT, iterations, training_set_size) {
  
  scores <- list(NULL)
  for (it in 1:iterations) {
    rows_sample <- sample(nrow(DT), training_set_size)
    
    DT_train <- DT[rows_sample]
    DT_train_shrinked <- DT_shrinked[rows_sample]
    
    pars_chosen <- optim(par = rep(1/32, 64), calculate_score_wrapper, control = list(maxit = 10)
                         , DT_train_shrinked = DT_train_shrinked
                         , DT_train = DT_train)
    
    # changing data set
    # WARNING: name train is inappropriate here, it shuld be DT_test or something like that
    DT_train <- DT[!rows_sample]
    DT_train_shrinked <- DT_shrinked[!rows_sample]
    score_test_set <- calculate_score_wrapper(pars = pars_chosen$par
                                              , DT_train_shrinked = DT_train_shrinked
                                              , DT_train = DT_train)
    
    scores[[it]] <- list(pars = pars_chosen, score = score_test_set)
  }
  
  return(scores)
}



