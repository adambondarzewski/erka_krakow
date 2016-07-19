library(data.table)


DT <- as.data.table(read.csv('http://doitprofiler.com/wp-content/uploads/2016/06/RKrakowFinalData.csv', stringsAsFactors = F))

##data validatioon

######################################3
# rowing out the observations with less then half of answers, in order not to bias cross validation
DT<-DT[QuestionsCompleted> 16]
####################################33
#
#

comparison<-table(DT$Score, DT$Gender)
############################3333
# Comparing Score Distributions
library(sm)
# create value labels
cyl.f <- factor(DT$Gender, levels= c("F","M"),
                labels = c("Female",  "Male"))
sum(is.na(DT$Score))
sum(is.na(DT$Gender))
# plot densities
sm.density.compare(DT$Score/DT$QuestionsCompleted, cyl.f, xlab="Score")
title(main="Dstribution of Scores by gender")

Sa_cols <- c(paste0('An0', 1:9), paste0('An', 10:32))
Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_cols <- c(paste0('Du0', 1:9), paste0('Du', 10:32))


#  only Same or Not the Same  answers given , Same= 1 not the same =0
#
#
if (sum(apply(DT[, Sa_cols, with = FALSE]=="Same", 1,function (x) mean(x, na.rm=TRUE )%in%c(0,1)))>0){
  IND1<-which( apply(DT[, Sa_cols, with = FALSE]=="Same", 1,function (x) mean(x, na.rm=TRUE )%in%c(0,1)) )
  DT<-DT[!IND1,]
}

# checking duration
#
#
#calculating average durations
mean_duration_qestion<-apply(DT[, Sd_cols, with = FALSE], 2, mean, na.rm = TRUE) 
DT$mean_duration_person<-apply(DT[, Sd_cols, with = FALSE], 1, mean, na.rm = TRUE) 


#
#acceptable_duration - a feasible number for actually performing the task.
acceptable_duration=2

#throwing out the duration
if(sum(DT$mean_duration_person<acceptable_duration)>0)
  DT<-DT[mean_duration_person>acceptable_duration,]
#
#
DT$avgScore<-DT$Score/DT$QuestionsCompleted

