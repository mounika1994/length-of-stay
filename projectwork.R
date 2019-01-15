# Decision Trees example


## Understanding Decision Trees ----
# calculate entropy of a two-class segment
#-0.60 * log2(0.60) - 0.40 * log2(0.40)

#curve(-x * log2(x) - (1 - x) * log2(1 - x),
# col="red", xlab = "x", ylab = "Entropy", lwd=4)

## Example: predicting the LOS of cardiac arrested people ----
## Step 2: Exploring and preparing the data ----
library(caTools)
library(ggplot2)
library(GGally)
#library(e1071)



cad<-read.csv("file:///D:/datascience with r/los_prject/fin .csv")

str(cad)
View(cad)




# look at the class variable
table(cad$los)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
cad_rand <- cad[order(runif(150)), ]

# compare the credit and credit_rand data frames
summary(cad_rand$hemoglobin)
summary(cad$hemoglobin)


# split the data frames
cad_train <- cad_rand[1:140, ]
cad_test  <- cad_rand[141:150, ]

# check the proportion of class variable
prop.table(table(cad_train$los))
prop.table(table(cad_test$los))

##Step 3:feature encoding 
cad_rand$gender=factor(cad_rand$gender,levels = c('0','1'),labels = c(0,1))
cad_rand$los=factor(cad_rand$los,levels = c('three','seven','eleven'),
                     labels =  c(3,7,11))

#Checkin the missing values and replacing 
#with the central tendency values like mean,mode,and median

sum(is.na(cad_rand))
sum(is.na(cad_rand$serumchlesterol))
sum(is.na(cad_rand$triglycerides))
sum(is.na(cad_rand$creatinine))

#data scaling of training set
cad_train[,3:4]=scale(cad_train[,3:4])
cad_train[,6:10]=scale(cad_train[,6:10])

#data prescaling of the test set

cad_test[,3:4]=scale(cad_test[,3:4])
cad_test[,6:10]=scale(cad_test[,6:10])




## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
cad_model <- C5.0(cad_train, cad_train$los)

# display simple facts about the tree
cad_model


# display detailed information about the tree
summary(cad_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
cad_pred <- predict(cad_model
                    , cad_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(cad_test$los, cad_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
cad_boost10 <- C5.0(cad_train, cad_train$los,
                       trials = 10)
cad_boost10
summary(cad_boost10)

cad_boost_pred10 <- predict(cad_boost10, cad_test)
CrossTable(cad_test$los, cad_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# boosted decision tree with 100 trials (not shown in text)
cad_boost100 <- C5.0(cad_train, cad_train$los,
                        trials = 100)
cad_boost_pred100 <- predict(cad_boost100, cad_test)
CrossTable(cad_test$los, cad_boost_pred100,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

##plotting the results

plot(cad_pred)
plot(cad_model)




#fitting the svm to the data
#install the package called e1071 for classification and type is 'C-Classification'
#install.packages("e1071")
ggpairs(cad_train, ggplot2::aes(colour = los, alpha = 0.4))

library(e1071)
svm_model=svm(los~.,
              data = cad_train,
              type = 'C-classification',
              kernel='radial')
svm_model
summary(svm_model)

#predicting and evolution
sv_pred = predict(svm_model, type = 'response', newdata = cad_test[-12])
CrossTable(cad_test$los, sv_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

cm=table(cad_test[,12],sv_pred)
cm

plot(svm_model)









# Visualising the Training set results
library(ElemStatLearn)
set =cad_train
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
#X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
#grid_set = expand.grid(cad_train)
colnames(cad_train) = c('chestpain', 'los')
#y_grid = predict(svm_model, newdata = cad_train)
plot(set,
     main = 'SVM (Training set)',
     xlab = 'age', ylab = 'los'
     )
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


### random forest model##
#now comparision of the two alg
#1.C50
#2.randomForest

library(caret)

##here two dependecies come one is that we need to install the "lattice" package
#another is dependencies is that we require the "ggplot2" also

library(rattle)  ## a free graphical interface for datamining with R

library(randomForest)

cad_data<-read.csv(file.choose(),na.strings=c("","NA"))

set.seed(2048)
randomdata<-createDataPartition(y=cad_data$los,p=0.75,list=FALSE)
train<-cad_data[randomdata,]
test<-cad_data[-randomdata,]
model<-randomForest(los~.,data=train)
model
plot(model)



confusionMatrix(predict(model,newdata=train),train$los)




confusionMatrix(predict(model,newdata=test),test$los)

varImpPlot(model)


##after do the C50 then we find the both accuracy results for that follow the syntax##


testpred<-c(factor(predict(modelC5a,newdata=cad_test)),
            factor(predict(modelFit,
                           newdata=test)) )
testact<-c(cad_test$los,test$los)

confusionMatrix(testpred,testact)


