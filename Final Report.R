####Github Link####
#https://github.com/torontosj/League-of-Legends-Capstone

####Install and Load Packages####
install.packages("corrplot")
install.packages("ROCR")
install.packages("dplyr")

library("dplyr")
library("gplots")
library("corrplot")
library("ROCR")


####Loading Full Dataset of 65 Features####
playerstats_kr <- read.csv('https://raw.githubusercontent.com/torontosj/League-
of-Legends-Capstone/master/kr_challenger_top100players.csv')

####Checking for Missing Values####
table(is.na(playerstats_kr))

#All Cotained Features in Original Dataset
features <- cbind.data.frame(names(playerstats_kr))
features

#### Removal of Features ####
#Nondescript Ordinal Labels
playerstats_kr$X_participant_id <- NULL
playerstats_kr$X_id <- NULL

#Items
playerstats_kr$item0 <- NULL
playerstats_kr$item1 <- NULL
playerstats_kr$item2 <- NULL
playerstats_kr$item3 <- NULL
playerstats_kr$item4 <- NULL
playerstats_kr$item5 <- NULL
playerstats_kr$item6<- NULL

#Unrecorded Stats in Featured Gamemode
playerstats_kr$nodeCapture <- NULL
playerstats_kr$nodeCaptureAssist <- NULL
playerstats_kr$nodeNeutralize <- NULL
playerstats_kr$nodeNeutralizeAssist <- NULL
playerstats_kr$combatPlayerScore <- NULL
playerstats_kr$teamObjective <- NULL
playerstats_kr$objectivePlayerScore <- NULL
playerstats_kr$unrealKills <- NULL
playerstats_kr$totalPlayerScore <- NULL
playerstats_kr$totalScoreRank <- NULL


#Returns NA in predict.glm summary
summary(playerstats_kr$neutralMinionsKilledTeamJungle)
playerstats_kr$neutralMinionsKilledTeamJungle <- NULL

#Null Values
summary(playerstats_kr$firstBloodAssist)
playerstats_kr$firstBloodAssist <- NULL

####Loading Dataset from Github####
kr_50 <- read.csv('https://raw.githubusercontent.com/torontosj/League-of-Legends-Capstone/master/kr_50.csv')
### !!!RUN THIS LINE!!!
kr_50$X <- NULL
####Subsetting Dataset Into 5 Folds for Validation####
#Creating Training and Testing Folds
kr_f1 <- slice(kr_50,1:10000)
kr_f2 <- slice(kr_50,10001:20000)
kr_f3 <- slice(kr_50,20001:30000)
kr_f4 <- slice(kr_50,30001:40000)
kr_f5 <- slice(kr_50,40001:50000)
kr_trainf1 <- rbind(kr_f2,kr_f3,kr_f4,kr_f5)
kr_trainf2 <- rbind(kr_f1,kr_f3,kr_f4,kr_f5)
kr_trainf3 <- rbind(kr_f1,kr_f2,kr_f4,kr_f5)
kr_trainf4 <- rbind(kr_f1,kr_f2,kr_f3,kr_f5)
kr_trainf5 <- rbind(kr_f1,kr_f2,kr_f3,kr_f4)

###!!RUN BOTH FOLD LINES THEN RUN ALL CODE FOR MODEL!!###
#Fold 1
train <- kr_trainf1
test <- kr_f1
#Fold 2
train <- kr_trainf2
test <- kr_f2
#Fold 3
train <- kr_trainf3
test <- kr_f3
#Fold 4
train <- kr_trainf4
test <- kr_f4
#Fold 5
train <- kr_trainf5
test <- kr_f5

#### Model 1 All Features ####
#Creating General Linear Model with all features and winner as class
fullfeatureglm <- glm(winner ~., data = train,family = binomial)
summary(fullfeatureglm)
#Using Test Set to Create Prediction Object
fullfeatureglmpred <- predict.glm(fullfeatureglm, newdata = test, type = "response")
#Making Prediction by Winner
fullfeaturepred <- prediction(fullfeatureglmpred,test$winner)
#Measuring Performance by Area Under the ROC
fullfeatureauc <- performance(fullfeaturepred, measure = "auc")
#Printing Value of AUC
fullfeatureauc

#Measuring Performance to by True Positive Rate and False Positive Rate
fullfeatureperf <- performance(fullfeaturepred,measure="tpr",x.measure="fpr")
#Plotting ROC Curve
plot(fullfeatureperf, col="blue",lwd=.1,main="ROC Curve All Features")
par(new=TRUE)
plot(fullfeatureperf, col="purple", new=FALSE,lwd=.1)
plot(fullfeatureperf, col="red", new=FALSE,lwd=.1)
plot(fullfeatureperf, col="green", new=FALSE,lwd=.1)
plot(fullfeatureperf, col="black", new=FALSE,lwd=.1)
#Area Under the ROC by 5 folds
All_Features <- rbind(0.9665981,0.9633098,0.9657865,0.9648232,0.9674628)
                  
#### Model 2 Correlated 22 Features #### 
#Using Top 22 Highest/Negative Correlated Features
full_cor <- data.frame(cor(kr_50))
cor_rank <- data.frame(full_cor$winner,row.names(full_cor))
cor_rank

trn2glm <- glm(winner ~ totalDamageTaken+
              magicDamageTaken+
              deaths+
              firstInhibitorAssist+
              assists+
              largestKillingSpree+
              towerKills+
              kills+
              goldEarned+
              killingSprees+
              inhibitorKills+
              largestMultiKill+
              neutralMinionsKilledEnemyJungle+
              doubleKills+
              champLevel+
              firstInhibitorKill+
              goldSpent+
              totalDamageDealtToChampions+
              tripleKills+
              totalDamageDealt+
              totalHeal+
              firstTowerKill+
              physicalDamageDealtToChampions , data = train, family = binomial)
test2glm <- predict.glm(trn2glm, newdata = test, type = "response")
predtest2 <- prediction(test2glm,test$winner)
perftest2 <- performance(predtest2,measure="tpr",x.measure="fpr")
aucperftest2 <- performance(predtest2, measure = "auc")
aucperftest2
Correlated_22 <- rbind(0.963243,0.9603736,0.9625225,0.9610839,0.9642615)

plot(perftest2, col="blue",lwd=.1,main="ROC Curve 22 Largest Positive and Negative Correlation")
par(new=TRUE)
plot(perftest2, col="purple", new=FALSE,lwd=.1)
plot(perftest2, col="red", new=FALSE,lwd=.1)
plot(perftest2, col="green", new=FALSE,lwd=.1)
plot(perftest2, col="black", new=FALSE,lwd=.1)


#### Model 3 Correlated 10 Features #### 
#Using Top 10 Highest/Negative Correlated Features
trn3glm <- glm(winner ~ firstInhibitorAssist+
              assists+
              largestKillingSpree+
              towerKills+
              kills+
              goldEarned+
              killingSprees+
              inhibitorKills+
              largestMultiKill+
              deaths
               ,data = train, family = binomial)
test3glm <- predict.glm(trn3glm, newdata = test, type = "response")
predtest3 <- prediction(test3glm,test$winner)
perftest3 <- performance(predtest3,measure="tpr",x.measure="fpr")

aucperftest3 <- performance(predtest3, measure = "auc")
aucperftest3
Correlated_10 <- rbind(0.9497502,0.9464373,0.9485136,0.9471683,0.9497771)

#### Model 4 Stepwise ####
## Backward Stepwise Full Feature
backward_full_featured <- step(fullfeatureglm, direction = "backward")
backward_full_featured$formula
## Forward Stepwise Full Feature
forward_full_featured <- step(fullfeatureglm, direction = "forward")
forward_full_featured$formula

#Backward Stepwise
stepfullfeatureglm <- glm(backward_full_featured$formula, data = train,family = binomial)
stepfullfeatureglmpred <- predict.glm(stepfullfeatureglm, newdata = test, type = "response")
#Making Prediction by Winner
stepfullfeaturepred <- prediction(stepfullfeatureglmpred,test$winner)
#ROC
stepfullfeatureauc <- performance(stepfullfeaturepred, measure = "tpr", x.measure = "fpr")
#Measuring Performance by Area Under the ROC
stepfullfeatureauc <- performance(stepfullfeaturepred, measure = "auc")
#Printing Value of AUC
stepfullfeatureauc
Backward_Step_Formula <- rbind(0.9665789,0.9633706,0.9658008,0.9649632,0.9675379)

#Forward Stepwise !!!Returned Formula of Full Feature Set!!!
stepfullfeatureglm <- glm(forward_full_featured$formula, data = train,family = binomial)
stepfullfeatureglmpred <- predict.glm(stepfullfeatureglm, newdata = test, type = "response")
#Making Prediction by Winner
stepfullfeaturepred <- prediction(stepfullfeatureglmpred,test$winner)
#Measuring Performance by Area Under the ROC
stepfullfeatureauc <- performance(stepfullfeaturepred, measure = "auc")
#Printing Value of AUC
stepfullfeatureauc
###Same results as full features/formula is full feature###


#Boxplot for Performance of Models 1-4
AUCresults <- cbind.data.frame(All_Features,Correlated_22,Correlated_10,Backward_Step_Formula,
                               Kills_Deaths_Assists,By_Kills)
AUCfinalplot <- boxplot(AUCresults, main = "Model Performance Comparison by AUC")



#### Model 5 By Kills Deaths and Assists ####
##By Kills Deaths Assits
trn5glm <- glm(winner ~ deaths+
                 kills+
                 assists
               , data = train, family = binomial)
test5glm <- predict.glm(trn5glm, newdata = test, type = "response")
predtest5 <- prediction(test5glm,test$winner)
perftest5 <- performance(predtest5,measure="tpr",x.measure="fpr")

aucperftest5 <- performance(predtest5, measure = "auc")
aucperftest5
Kills_Deaths_Assists <- rbind(0.926741,0.9215555,0.9225956,0.9167266,0.9219676)


#### Model 6 By Kills Single Feature ####
##By Kills 
trn6glm <- glm(winner ~ 
                 kills
               , data = train, family = binomial)
test6glm <- predict.glm(trn6glm, newdata = test, type = "response")
predtest6 <- prediction(test6glm,test$winner)
perftest6 <- performance(predtest6,measure="tpr",x.measure="fpr")
AUCperftest6 <- performance(predtest6,measure="auc")
AUCperftest6
By_Kills <- rbind(0.6916908,0.6890947,0.690045,0.6812976,0.6957312)

#AUC Plot All Models
AUCresults2 <- cbind.data.frame(By_Kills,Kills_Deaths_Assists)
AUCplot2 <- boxplot(AUCresults2)

####Test for Overfit####
#GLM using Korean matches was tested using 43k matches of a separate dataset
#of North American region matches with the same feature set
#Load NA Player Stats from Github
na_player <- read.csv('')
na_player$X <- NULL
overfitglm <- glm(winner ~., data = kr_50,family = binomial)
overfitglmpred <- predict.glm(overfitglm, newdata = na_player, type = "response")
overfitpred <- prediction(overfitglmpred,na_player$winner)
overfitauc <- performance(overfitpred, measure = "auc")
overfitauc
0.956303

overfitROC <- performance(overfitpred, measure = "tpr", x.measure = "fpr")
plot(overfitROC, main = "KR Train with NA Test")