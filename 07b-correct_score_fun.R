Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_corrected_cols <- c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))

DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with = FALSE]


# temporary
DT_train_shrinked <- copy(DT_shrinked)
DT_train <- copy(DT)

# produce 

score <- function(row, w, v, lambda, penalty = function(x) abs(x)) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w * row[Sc_cols] + v * row[Sd_corrected_cols] + lambda * penalty(w - 1/32)+lambda * penalty(v)
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}


score2 <- function(row, w, v, lambda, penalty = function(x) sqrt(abs(x))) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  # output <- w * row[Sc_cols] + v * row[Sd_corrected_cols] + lambda * penalty(w - 1/32)
  
  output <- w * row[Sc_cols]  + lambda * penalty(w)
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}

score3 <- function(row, w, v, lambda, penalty = function(x) sqrt(abs(x))){
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w * row[Sc_cols] + v * row[Sd_corrected_cols] + lambda * (penalty(w)+penalty(v))
  
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}




#' @param DT_train data.tabe; should be subset of DT_shrinked
#' 
calculate_score <- function(  w, v, lambda, score_fun = score
                            , DT_train = DT_train
                            , DT_train_shrinked = DT_train_shrinked
                            , penalty) {
    
    DT_train$score_measured <- apply(DT_train_shrinked, 1, function(x) {score_fun(x, w = w, v = v, lambda, penalty = penalty)})
    
    DT_groups <- DT_train[, .(score_measured = mean(score_measured), group_count = .N)
                    , by =.(Gender, SubGroup1)]
    
    DT_groups_counts <- DT_groups[, .(group_count = sum(group_count)), by = SubGroup1]
    
    DT_groups_casted <- dcast(DT_groups[ , .(SubGroup1, Gender, score_measured)], SubGroup1 ~ Gender)
    
    DT_groups_casted <- merge(DT_groups_casted, DT_groups_counts
                              , by ='SubGroup1')
    
    participants_sum <- DT_groups_casted[, sum(group_count)]
    DT_groups_casted[, group_part := group_count/participants_sum]
    DT_groups_casted[, gender_gap := abs(`F` - `M`) * group_count]
    
    # todo: ważyć ilością
  
  return(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
}

calculate_score_wrapper <- function( pars
                                    , DT_train = DT_train
                                    , DT_train_shrinked = DT_train_shrinked
                                    , penalty, lambda=1) {
  
  w <- exp(pars[1:32])/(1+exp(pars[1:32]))
  w<-w/sum(w)
  
  v <- exp(pars[33:64])/(1+exp(pars[33:64]))
  v<-v/sum(v)
    
  calculate_score(DT_train = DT_train, DT_train_shrinked = DT_train_shrinked, penalty = penalty, w = w, v = v, lambda)
}

answer <- optim(par = rep(1/32, 64), calculate_score_wrapper
                , DT_train_shrinked = DT_train_shrinked
                , DT_train = DT_train
                ,lambda==1
                ,method="CG"
                , penalty = function(x) abs(x)
                , control = list(maxit = 100))
