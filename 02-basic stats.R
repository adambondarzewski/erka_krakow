

#data structure
str(DT_train)
str(DT_test)

# Survival rates in absolute numbers
table(DT_train$Survived)

# Survival rates in proportions

prop.table(table(DT_train$Survived))
# Two-way comparison: Sex and Survived

table(DT_train$Sex, DT_train$Survived)
# Two-way comparison: row-wise proportions
prop.table(table(DT_train$Sex, DT_train$Survived),1)

is_child=15
# Create the column child, and indicate whether child or no child
DT_train$Child <- NA
DT_train$Child[DT_train$Age < is_child] <-1
DT_train$Child[DT_train$Age >= is_child] <-0

DT_test$Child <- NA
DT_test$Child[DT_test$Age < is_child] <-1
DT_test$Child[DT_test$Age >= is_child] <-0
# Two-way comparison
prop.table(table(DT_train$Child, DT_train$Survived))



##############################################################
a1<-apply(DT[ Gender=='M', Sd_cols, with = FALSE], 1, mean, na.rm = TRUE) 

a2<-apply(DT[ Gender=='F', Sd_cols, with = FALSE], 2, mean, na.rm = TRUE) 
a1/a2



a1<-apply(DT[ Gender=='M', "Band", with = FALSE], 2, mean, na.rm = TRUE) 
a2<-apply(DT[ Gender=='F', "Band", with = FALSE], 2, mean, na.rm = TRUE) 

model1<-lm(data=DT,mean_duration_person~Band+Gender +AgeYears)
summary(model1)

model2<-lm(data=DT,avgScore~mean_duration_person+Band+Gender +AgeYears)
summary(model2)
