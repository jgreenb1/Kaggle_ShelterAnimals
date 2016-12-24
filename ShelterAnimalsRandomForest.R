###### Settings
library(rpart)
library(dplyr)
library(randomForest)
set.seed(500)

###### Loading data
setwd("C:/Kaggle/ShelterAnimals")
train<-read.csv("train.csv")
test<-read.csv("test.csv")

###### Formatting data
test$ID <- as.character(test$ID)
colnames(train)[1]<-"ID"

###### Combining dataset
full<-bind_rows(train, test)

###### Formatting
full$TimeValue <- sapply(full$AgeuponOutcome,  
                         function(x) strsplit(x, split = ' ')[[1]][1])
full$UnitofTime <- sapply(full$AgeuponOutcome,  
                          function(x) strsplit(x, split = ' ')[[1]][2])
full$UnitofTime <- gsub('s', '', full$UnitofTime)

full$TimeValue  <- as.numeric(full$TimeValue)
full$UnitofTime <- as.factor(full$UnitofTime)

multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30, # Close enough
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

full$AgeinDays <- full$TimeValue * multiplier
full$AgeinDays[is.na(full$AgeinDays)]<-median(na.omit(full$AgeinDays))

###### Factorize
full$SexuponOutcome<-as.factor(full$SexuponOutcome)
full$Breed<-as.factor(full$Breed)
full$Color<-as.factor(full$Color)

###### Split back into training and test sets
train1<-full[1:26729,]
test1<-full[26730:nrow(full),]

###### Train Random Forest Model
rf_model<-randomForest(OutcomeType~AnimalType+SexuponOutcome+AgeinDays,data=train1,ntree=600,importance=TRUE)

###### Importance
importance(rf_model)

###### Prediction
## Best to use type="vote" for multi-class prediction, will give probabilities instead of 1 and 0
prediction<-predict(rf_model,test1,type="vote")

###### Write solution
solution<-data.frame("ID"=test1$ID,prediction)
write.csv(solution,"rf_solution1.csv",row.names=F)


