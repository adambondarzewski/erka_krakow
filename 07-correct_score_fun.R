Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_corrected_cols <- c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))

# cuting the unnecessery columns
DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with = FALSE]

require(data.table)
# produce 


# temporary
#DT_train_shrinked <- copy(DT_shrinked)
#DT_train <- copy(DT)


score <- function(row, w, v, lambda, penalty = function(x) abs(x)) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w * row[Sc_cols]  + lambda * penalty(w - 1/32)
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}


score3 <- function(row, w, v, lambda, penalty = function(x) sqrt(abs(x))){
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w * row[Sc_cols] - v * row[Sd_corrected_cols] + lambda * (penalty(w)+penalty(v))
  
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}


#' @param DT_train data.table; should be subset of DT_shrinked
#' @param DT_train_shrinked is the DT_train but only with score and duration columns
#'  Function 
calculate_score <- function(pars, lambda, score_fun
                            , DT_train = DT_train
                            , DT_train_shrinked = DT_train_shrinked
                            , penalty = function(x) abs(x)){
                              
  # ensuring the weights are in 0-1 interval
  w <- exp(pars[1:32])/(1+exp(pars[1:32]))
  w<-w/sum(w)
  
  v <- exp(pars[33:64])/(1+exp(pars[33:64]))
  v<-v/sum(v)
  
  # applying the score function to teh individual
  DT_train$score_measured <- apply(DT_train_shrinked, 1, function(x) score_fun(x, w = w, v = v, lambda, penalty = penalty))
  
  #Scalling the score, to have mean of 100
  DT_train$score_measured<-100*DT_train$score_measured/mean(DT_train$score_measured)
  
  # aggregating into Gender subgroups
  DT_groups <- DT_train[, .(score_measured = mean(score_measured), group_count = .N)
                        , by =.(Gender, SubGroup1)]
  
  # aggregating into subgrups- counts of the subgrups
    DT_groups_counts <- DT_groups[, .(group_count = sum(group_count)), by = SubGroup1]
    
   #Spliting into Males and Females. Then joining on subgroups. 
    DT_groups_casted <- dcast(DT_groups[ , .(SubGroup1, Gender, score_measured)], SubGroup1 ~ Gender)
    DT_groups_casted <- merge(DT_groups_casted, DT_groups_counts, by ='SubGroup1')
    
    #calculating the absolute difference of genders in the non-empty groups
    DT_groups_casted[, gender_gap := abs(`F` - `M`) * group_count]
    
  # returning the sum of the deviations.
  return(DT_groups_casted[, sum(gender_gap, na.rm = TRUE)])
}
transform<-function(pars){
  # to ensure that parameters are in [0,1]
  w <- exp(pars[1:32])/(1+exp(pars[1:32]))
  w<-w/sum(w)
  
  v <- exp(pars[33:64])/(1+exp(pars[33:64]))
  v<-v/sum(v, na.rm=TRUE)
  return(c(w,v))
}


start.val<-seq(0,64)

calculate_score(start.val,DT ,DT_shrinked, lambda=1,score_fun = score ,penalty=function(x) abs(x))
calculate_score(start.val,DT ,DT_shrinked, lambda=1,score_fun = score3 ,penalty=function(x) abs(x))


answer <- optim(par = save, calculate_score, DT_train=DT,DT_train_shrinked=DT_shrinked ,lambda=1,method="CG",score_fun =score,control=list(maxit=100) )
answer1<-optim(par = save1, calculate_score, DT_train=DT,DT_train_shrinked=DT_shrinked ,lambda=1,method="CG",score_fun =score3,control=list(maxit=100) )

save<-answer$par
save1<-answer1$par

transform(save)

transform(save1)

a<-ifelse(transform(save)>0.01,transform(save),0)
a<-a/sum(a)
trans<-ifelse(is.na(transform(save1)),0,transform(save1))
a1<-ifelse(trans>0.01,trans,0)
a1<-a1/sum(a1)

#save(a, a1,DT_groups_casted,DT, file = "Final_results.RData")
