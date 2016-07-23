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


index_optim<-imp>1