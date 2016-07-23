Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_corrected_cols <- c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))

DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with = FALSE]

# produce 

score <- function(row, w, v, lambda, penalty = function(x) abs(x)) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
 # output <- w * row[Sc_cols] + v * row[Sd_corrected_cols] + lambda * penalty(w - 1/32)
  
  output <- w * row[Sc_cols]  + lambda * penalty(w - 1/32)
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}

calculate_score <- function(w, v, lambda, score_fun = score) {
  
    
    DT$score_measured <- apply(DT_shrinked, 1, function(x) {score(x, w = w, v = v, lambda)})
    
    DT_groups <- DT[, .(score_measured = mean(score_measured), group_count = .N)
                    , by =.(Gender, SubGroup1)]
    
    DT_groups_counts <- DT_groups[, .(group_count = sum(group_count)), by = SubGroup1]
    
    DT_groups_casted <- dcast(DT_groups[ , .(SubGroup1, Gender, score_measured)], SubGroup1 ~ Gender)
    
    DT_groups_casted <- merge(DT_groups_casted, DT_groups_counts
                              , by ='SubGroup1')
    
    participants_sum <- DT_groups_casted[, sum(ifelse(is.na(abs(`F` - `M`)),0, group_count))]
    DT_groups_casted[, group_part := group_count/participants_sum]
    DT_groups_casted[, gender_gap := abs(`F` - `M`) * group_count]
    
    print(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
    # todo: ważyć ilością
  
  return(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
}


calculate_score_wrapper <- function(pars) {
  
  w <- exp(pars[1:32])/(1+exp(pars[1:32]))
  w<-w/sum(w)
  
  #v <- exp(pars[33:64])/(1+exp(pars[33:64]))
  #v<-v/sum(v)
  v<-rep(1,32)
    
  calculate_score(DT, w = w, v = v, lambda = 1)
}
start.val<-rep(1, 32)
answer <- optim(par = start.val, calculate_score_wrapper, method="CG",control=list(maxit=1000) )

start.val<-answer$par




calculate_score_wrapper(start.val)

a <- exp(start.val)/(1+exp(start.val))
a<-a/sum(a)
