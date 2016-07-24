
require(data.table)
DT<-# here load the data

# use 01-data_load.R to create corrected durations
 # source("01-data_load.R")


#######################################################################
# correcting  durations for the model 
duration_correction =-0.5216398

cols <- c(paste0('Du0', 1:9), paste0('Du', 10:32))

DT_corrected_durations <- DT[, .SD[, cols, with = FALSE] + duration_correction * Band]

setnames(DT_corrected_durations, old = cols, new = paste0('corrected_', cols))
DT <- cbind(DT, DT_corrected_durations)

# correcting mean durations
DT[, corrected_mean_duration_person := mean_duration_person +  duration_correction * Band]

# load saved weights
load( file = "Final_results.RData")

w<- a[1:32]
w1<-a1[1:32]
v1<-a1[33:64]

final_score <- function(row) {
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w * row[Sc_cols]
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}
final_score1 <- function(row) {
  
  
  length_answers <- length(na.omit(row[Sc_cols]))
  
  output <- w1 * row[Sc_cols] - v1 * row[Sd_corrected_cols]
  
  
  output <- output/length_answers
  
  return(sum(output, na.rm = TRUE))
}


###########################################################
# now aply to each column of the data frame and standarize
DT_shrinked <- DT[, c(Sc_cols, Sd_corrected_cols), with = FALSE]

DT$score_measured_final <- apply(DT_train_shrinked, 1,final_score)

DT$score_measured_final1 <- apply(DT_train_shrinked, 1,final_score1)

# scaling the results to mean 100 
DT$score_measured_final<-100*(DT$score_measured_final/mean(DT$score_measured_final))

DT$score_measured_final1<-100*(DT$score_measured_final1/mean(DT$score_measured_final1))



#Now apply any scoring for the contest to both measures.  If only one can be accounted for in the rules  use score_measured_final1.

#####################################

# code to apply the aggregation and using our success measure
# aggregating into Gender subgroups
DT_groups <- DT[, .(score_measured = mean(score_measured_final),  group_count = .N)
                      , by =.(Gender, SubGroup1)]
DT_groups1 <- DT[, .(score_measured = mean(score_measured_final1),  group_count = .N)
                , by =.(Gender, SubGroup1)]
# aggregating into subgrups- counts of the subgrups
DT_groups_counts <- DT_groups[, .(group_count = sum(group_count)), by = SubGroup1]

#Spliting into Males and Females. Then joining on subgroups. 
DT_groups_casted <- dcast(DT_groups[ , .(SubGroup1, Gender, score_measured)], SubGroup1 ~ Gender)
DT_groups_casted1 <- dcast(DT_groups1[ , .(SubGroup1, Gender, score_measured)], SubGroup1 ~ Gender)

DT_groups_casted <- merge(DT_groups_casted, DT_groups_counts, by ='SubGroup1')
DT_groups_casted1 <- merge(DT_groups_casted1, DT_groups_counts, by ='SubGroup1')

#calculating the absolute difference of genders in the non-empty groups
DT_groups_casted[, gender_gap := abs(`F` - `M`) * group_count]
DT_groups_casted1[, gender_gap := abs(`F` - `M`) * group_count]

# returning the sum of the deviations.
DT_groups_casted[, sum(gender_gap, na.rm = TRUE)]
DT_groups_casted1[, sum(gender_gap, na.rm = TRUE)]

