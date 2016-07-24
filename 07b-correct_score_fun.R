Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_corrected_cols <- c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))

DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with = FALSE]


# temporary
DT_train_shrinked <- copy(DT_shrinked)
DT_train <- copy(DT)

# produce 

score <- function(row, w, v, lambda, penalty = function(x) x^2) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w * row[Sc_cols] + v * row[Sd_corrected_cols] + lambda * penalty(w - 1/32)
  
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
    DT_groups_casted[, gender_gap := (`F` - `M`)^2 * group_part]
    
    # todo: ważyć ilością
  
  return(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
}

calculate_score_wrapper <- function( pars
                                    , DT_train = DT_train
                                    , DT_train_shrinked = DT_train_shrinked
                                    , penalty) {
  
  w <- pars[1:32]
  v <- pars[33:64]
    
  calculate_score(DT_train = DT_train, DT_train_shrinked = DT_train_shrinked, penalty = penalty, w = w, v = v, lambda = 1)
}

answer <- optim(par = rep(1/32, 64), calculate_score_wrapper
                , DT_train_shrinked = DT_train_shrinked
                , DT_train = DT_train
                , penalty = function(x) x^2
                , control = list(maxit = 10))
