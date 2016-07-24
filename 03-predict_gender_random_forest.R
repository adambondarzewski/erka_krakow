
# This script trains a Random Forest model based on the data,
# of the variables in making predictions

library(ggplot2)
library(randomForest)

sample_size <- 500

set.seed(2)

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

# applying Feature extraction
DT_cleaned <- na.omit(extractFeatures(DT))

#test set
indexes_test <- sample(nrow(DT_cleaned), sample_size)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]

rf <- randomForest(x=extractFeatures(DT_train, features = c(Sd_cols, Sc_cols)) ,  y=as.factor(DT_train[, Gender]),  ntree=100, importance=TRUE,keep.forest=TRUE)

real_genders <- data.table(real_gender = DT_test$Gender)

real_genders$prediction <- predict(rf, extractFeatures(DT_test)[, !'Gender', with = FALSE])

sum(real_genders$real_gender==real_genders$prediction)

predict(rf, extractFeatures(DT_train)[, !'Gender', with = FALSE])

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



# removing cols with negative importance
new_cols <- c(Sc_cols, Sd_cols)[imp > 0]

set.seed(5)

indexes_test <- sample(nrow(DT_cleaned), sample_size)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]


rf <- randomForest(x=extractFeatures(DT_train, features = c(Sd_cols, Sc_cols)) ,  y=as.factor(DT_train[, Gender]),  ntree=100, importance=TRUE,keep.forest=TRUE)


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

sum(real_genders$real_gender==real_genders$prediction)/sample_size
