#KNN and GLM for team stats
library("RWeka")
library("dplyr")
library("ROCR")

teamstatsKR <- read.csv('https://raw.githubusercontent.com/torontosj/League-of-Legends-Capstone/master/teamstatsKR.csv')

#List of Features
names(teamstatsKR)

#Remove Unique Match ID from Features
teamstatsKR$X_match_id <- NULL

#Correlation of Features
teamcor <- data.frame(cor(teamstatsKR))
teamcor

train <- slice(teamstatsKR,1:8560) 
test <- slice(teamstatsKR, 8561:10700)

## k=1
classifier <- IBk(as.factor(winner) ~., data = train,
                  control = Weka_control(K = 1))
evaluate_Weka_classifier(classifier, newdata = test)
#Correctly Classified Instances        1836               85.7944 %
#Incorrectly Classified Instances       304               14.2056 %

## k=2
classifier <- IBk(as.factor(winner) ~., data = train,
                  control = Weka_control(K = 2))
evaluate_Weka_classifier(classifier, newdata = test)
#Correctly Classified Instances        1838               85.8879 %
#Incorrectly Classified Instances       302               14.1121 %

## k=3
classifier <- IBk(as.factor(winner) ~., data = train,
                  control = Weka_control(K = 3))
evaluate_Weka_classifier(classifier, newdata = test)
#Correctly Classified Instances        1864               87.1028 %
#Incorrectly Classified Instances       276               12.8972 %

## k=4
classifier <- IBk(as.factor(winner) ~., data = train,
                  control = Weka_control(K = 4))
evaluate_Weka_classifier(classifier, newdata = test)
#Correctly Classified Instances        1862               87.0093 %
#Incorrectly Classified Instances       278               12.9907 %

## k=10
classifier <- IBk(as.factor(winner) ~., data = train,
                  control = Weka_control(K = 10))
evaluate_Weka_classifier(classifier, newdata = test)
#Correctly Classified Instances        1877               87.7103 %
#Incorrectly Classified Instances       263               12.2897 %

## k=20
classifier <- IBk(as.factor(winner) ~., data = train,
                  control = Weka_control(K = 15))
knnteam <- evaluate_Weka_classifier(classifier, newdata = test)
#Correctly Classified Instances        1886               88.1308 %
#Incorrectly Classified Instances       254               11.8692 %



#LR GLM
teamglm <- glm(winner ~., data = train,family = binomial)
teamglmpred <- predict.glm(teamglm, newdata = test, type = "response")
#Making Prediction by Winner
teampred <- prediction(teamglmpred,test$winner)
#Measuring Performance to by True Positive Rate and False Positive Rate
teamperf <- performance(teampred,measure="tpr",x.measure="fpr")
#Plotting ROC Curve
plot(teamperf)

#Measuring Performance by Area Under the ROC
teamAUC <- performance(teampred, measure = "auc")
#Printing Value of AUC
teamAUC