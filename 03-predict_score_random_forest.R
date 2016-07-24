
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
  , c(paste0('Du0', 1:9), paste0('Du', 10:32))
  ,"mean_duration_person", "AgeYears", "Institution","Group", "Region","DaysSince", "avgScore", "corrected_mean_duration_person",
  c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))
  
)){
  
  DT_out <- DT_in[,features, with = FALSE]
  
  return(DT_out)
}
extractFeatures1 <- function(DT_in, features = c(  
  'Gender'
  ,"mean_duration_person", "AgeYears", "Institution","Group", "Region","DaysSince", "avgScore", "corrected_mean_duration_person",
  c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32))
  
)){
  
  DT_out <- DT_in[,features, with = FALSE]
  
  return(DT_out)
}
extractFeatures2 <- function(DT_in, features = c(  
  'Gender',
 "avgScore",
  c(paste0('corrected_Du0', 1:9), paste0('corrected_Du', 10:32)), c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
  
)){
  
  DT_out <- DT_in[,features, with = FALSE]
  
  return(DT_out)
}



Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:32))
Sd_cols <- c(paste0('Du0', 1:9), paste0('Du', 10:32))

DT_cleaned <- na.omit(extractFeatures(DT))
DT_cleaned <- na.omit(extractFeatures1(DT))
DT_cleaned <- na.omit(extractFeatures2(DT))


DT_cleaned$Institution=as.factor(DT_cleaned[,Institution])
DT_cleaned$Gender=as.factor(DT_cleaned[,Gender])
DT_cleaned$Group=as.factor(DT_cleaned[,Group])
DT_cleaned$Region=as.factor(DT_cleaned[,Region])


indexes_test <- sample(nrow(DT_cleaned), 100)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]

rf <- randomForest(x=DT_train[, !'avgScore', with = FALSE], y=DT_train[, avgScore], ntree=100, importance=TRUE,maxnodes = 5, na.action=na.omit)


real_genders <- data.table(real_gender = DT_test$avgScore)

real_genders$prediction <- predict(rf, DT_test[, !'avgScore', with = FALSE])

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
p
ggsave("2_feature_importance1.png", p)



# removing cols
new_cols <-which(imp>0)

set.seed(5)

indexes_test <- sample(nrow(DT_cleaned), sample_size)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]

DT_train1<-DT_train[, new_cols, with=F]
DT_test1<-DT_test[, new_cols, with=F]

rf <- randomForest(x=DT_train[, new_cols, with=F], y=DT_train[, avgScore],
                   ntree=100, 
                   importance=TRUE,
                   maxnodes = 5,
                   na.action=na.omit)


real_genders <- data.table(real_gender =DT_test$avgScore)

real_genders$prediction <- predict(rf, DT_test1)

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



fun_fit<-function(DT_train){ randomForest(x=DT_train[, !'avgScore', with = FALSE], y=DT_train[, avgScore],
                                          ntree=100, importance=TRUE,maxnodes = 5, na.action=na.omit)}
fun_predict<-function(rf, x){ predict(rf, x[, !'avgScore', with = FALSE])}
fun_error<-function(prediction, x){ mean((x$avgScore-prediction)^2,na.rm = TRUE) }

Data<-DT_cleaned

number_subsamples=10
size_subsample=300
warnings()


#calling the function
x<-cross_validation(Data,fun_fit,fun_predict,fun_error,number_subsamples,size_subsample)

#
#local variables

dim=ifelse(is.null(dim(Data)[1]),length(Data),dim(Data)[1])
errors<-list()
subsamples<-list()

for(i in 1:number_subsamples)
  subsamples[[i]]=sort(sample.int(dim,size_subsample))


local_fun<-function(index){
  model<-fun_fit(Data[!index,])
  prediction<-fun_predict(model,Data[index,] )
  
  return(fun_error(prediction,Data[index,]))
}
# main loop
index<-subsamples[[1]]


model<-fun_fit(Data[!index,])
prediction<-fun_predict(model,Data[index,] )
local_fun(subsamples[[1]])
mean(sapply(subsamples,local_fun, simplify=TRUE), na.rm=TRUE)



