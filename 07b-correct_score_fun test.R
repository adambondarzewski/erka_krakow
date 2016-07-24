Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_corrected_cols <- c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))

#DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with = FALSE]
require(data.table)
# produce 

score <- function(row, w, v, lambda, penalty = function(x) abs(x)) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
 # output <- w * row[Sc_cols] + v * row[Sd_corrected_cols] + lambda * penalty(w - 1/32)
  
  output <- w * row[Sc_cols]  + lambda * penalty(w - 1/32)
  
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



calculate_score <- function(pars,DT, lambda=1, score_fun = score) {
  # removed wrappper and put it here for clarity
  w <- exp(pars[1:32])/(1+exp(pars[1:32]))
  w<-w/sum(w)
  
  v <- exp(pars[33:64])/(1+exp(pars[33:64]))
  v<-v/sum(v)
  
  Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
  Sd_corrected_cols <- c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))
  
    DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with= FALSE]

    map<-function(x){score_fun(x, w = w, v = v, lambda)}
    DT$score_measured <- apply(DT_shrinked, 1, map)
    
    DT_groups <- DT[, .(score_measured = mean(score_measured), group_count = .N), by =.(Gender, SubGroup1)]
    
    DT_groups_counts <- DT_groups[, .(group_count = sum(group_count)), by = SubGroup1]
    
    DT_groups_casted <- dcast(DT_groups[ , .(SubGroup1, Gender, score_measured)], SubGroup1 ~ Gender)
    
    DT_groups_casted <- merge(DT_groups_casted, DT_groups_counts, by ='SubGroup1')
    
    participants_sum <- DT_groups_casted[, sum(ifelse(is.na(abs(`F` - `M`)),0, group_count))]
    DT_groups_casted[, group_part := group_count/participants_sum]
    DT_groups_casted[, gender_gap := abs(`F` - `M`) * group_count]
    
    print(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
    # todo: ważyć ilością
  
  return(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
}


# calculate_score_wrapper <- function(pars, DT, lambda, score_fun = score) {
#   
#   w <- exp(pars[1:32])/(1+exp(pars[1:32]))
#   w<-w/sum(w)
#   
#   v <- exp(pars[33:64])/(1+exp(pars[33:64]))
#   v<-v/sum(v)
# 
#     
#   calculate_score(DT, w = w, v = v, lambda = 1,score_fun=score3)
# }

start.val<-rep(0, 64)

calculate_score(start.val,DT,1,score_fun = score)

pars<-start.val




answer <- optim(par = start.val, calculate_score, DT=DT, lambda=1,score_fun =score, method="CG",control=list(maxit=1000) )

start.val<-answer$par




calculate_score(start.val,DT)

a <- exp(start.val)/(1+exp(start.val))
a<-a/sum(a)

sum(abs(a/sum(a)-1/32))
lambda =1 =>
