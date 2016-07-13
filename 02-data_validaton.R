library(data.table)
head(DT)
summary(DT)

######################################3
# rowing out the observations with less then half of answers, not to bias cross validation
DT<-DT[QuestionsCompleted< 16]
####################################33
#
#
table(DT$Score)
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
# some testing
DT[apply(DT[, Sa_cols, with = FALSE], 1, mean, na.rm = TRUE) ==1 ]
DT[apply(DT[, Sa_cols, with = FALSE], 1, mean, na.rm = TRUE) ==0 ]


