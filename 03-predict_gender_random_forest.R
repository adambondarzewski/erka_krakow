
# This script DT_trains a Random Forest model based on the data,
# saves a sample submission, and plots the relative importance
# of the variables in making predictions

# Download 1_random_forest_r_submission.csv from the output below
# and submit it through https://www.kaggle.com/c/titanic-gettingStarted/submissions/attach
# to enter this getting started competition!

library(ggplot2)
library(randomForest)

sample_size <- 500

set.seed(1)

#' Columns selection and data cleaning
#' @param data data.table
extractFeatures <- function(DT_in, features = c(  
                                                'Gender'
                                                , c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
                                                , c(paste0('Du0', 1:9), paste0('Du', 10:32))
                                              )) {
  
  DT_out <- DT_in[,features, with = FALSE]
  
  return(DT_out)
}

Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_cols <- c(paste0('Du0', 1:9), paste0('Du', 10:32))

DT_cleaned <- na.omit(extractFeatures(DT))

indexes_test <- sample(nrow(DT_cleaned), 100)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]

rf <- randomForest(extractFeatures(DT_train, features = c(Sd_cols, Sc_cols)
                                   )
                   , as.factor(DT_train[, Gender]), ntree=100, importance=TRUE,
                   maxnodes = 5)

real_genders <- data.table(real_gender = DT_test$Gender)

real_genders$prediction <- predict(rf, extractFeatures(DT_test)[, !'Gender', with = FALSE])

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)



# removing cols
new_cols <- c(Sc_cols, Sd_cols)[imp > 0]

set.seed(5)

indexes_test <- sample(nrow(DT_cleaned), sample_size)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]

rf <- randomForest(extractFeatures(DT_train, features = new_cols)
, as.factor(DT_train[, Gender]), ntree=100, importance=TRUE,
maxnodes = 5)

real_genders <- data.table(real_gender = DT_test$Gender)

real_genders$prediction <- predict(rf, extractFeatures(DT_test)[, !'Gender', with = FALSE])

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

# some testing
View(DT[apply(DT[, Sc_cols, with = FALSE], 1, mean, na.rm = TRUE) == 1])
