#title: "Predicting Customer Churn"

rm(list = ls())

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(pROC)
library(DataExplorer)
library(stringr)
library(rpart)
library(rpart.plot)
library(cowplot)
library(e1071)

#Read the dataset
churn <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

sink("Customer Churn Ouput.txt", append=FALSE, split=TRUE)
pdf("Plots.pdf")

set.seed(123) 
#Data overview
str(churn)

#sapply is used to check the missing values in each columns

sapply(churn, function(x) sum(is.na(x)))
plot_missing(churn)

churn <- churn[complete.cases(churn), ]


#Visualizing the dataset wrt churn
#CHURN columns tells us about the number of Customers who left within the last month.
#Around 26% of customers left the platform within the last month.

options(repr.plot.width = 6, repr.plot.height = 4)
churn %>% 
  dplyr::group_by(Churn) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(percent = prop.table(Count)*100) %>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#009E73", "#FC4E07"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+ 
  xlab("Churn") + 
  ylab("Percent(%)")+
  ggtitle("Churn Percentage")


#Change the values in column SeniorCitizen from 0 or 1 to No or Yes.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))


#Change No internet service to No for six columns: OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, streamingTV, streamingMovies.

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#Change No phone service to No for column MultipleLines

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))


#calculating Churn proportion w.r.t the variables(attributes)
options(repr.plot.width = 12, repr.plot.height = 8)

#gender,senior citizen,partner, dependents
plot_grid(ggplot(churn, aes(x=gender,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(), 
          ggplot(churn, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=Partner,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=Dependents,fill=Churn))+ geom_bar()+theme_minimal()+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), nrow = 2)

#PhoneService, multiple lines, internet service, online security
plot_grid(ggplot(churn, aes(x=PhoneService,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=MultipleLines,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=InternetService,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=OnlineSecurity,fill=Churn))+ geom_bar()+theme_minimal()+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),nrow = 2)

#online backup, device protection, TechSupport, StreamingTV
plot_grid(ggplot(churn, aes(x=OnlineBackup,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=DeviceProtection,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=TechSupport,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=StreamingTV,fill=Churn))+ geom_bar()+theme_minimal()+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),nrow = 2)

#StreamingMovies, Contract, PaperlessBilling, PaymentMethod
plot_grid(ggplot(churn, aes(x=StreamingMovies,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=Contract,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=PaymentMethod,fill=Churn))+ geom_bar(show.legend = FALSE)+theme_minimal(),
          ggplot(churn, aes(x=PaperlessBilling,fill=Churn))+ geom_bar()+theme_minimal()+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),nrow = 2)

#Gender count
churn %>%
  dplyr::group_by(gender,Churn) %>%
  dplyr::summarise(n=n())

#Senior Citizen Count
churn %>%
  dplyr::group_by(SeniorCitizen) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n))

#Senior Citizen churn Count
churn %>%
  dplyr::group_by(SeniorCitizen, Churn) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n))

#Partner count
churn %>%
  dplyr::group_by(Partner,Churn) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n))

#Dependents count
churn %>% 
  dplyr::group_by(Dependents, Churn) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(freq = n / sum(n))

#Plot tenure w.r.t to churn
churn_tenure <- churn%>% mutate(tenurefactor = as.factor(dplyr::if_else(tenure <= 12, "0-1 Year",
                                                        dplyr::if_else(tenure <= 24, "1-2 Year",
                                                        dplyr::if_else(tenure <= 36, "2-3 Year",
                                                        dplyr::if_else(tenure <= 48, "3-4 Year",
                                                        dplyr::if_else(tenure <= 60, "4-5 Year", "5-6 Year")))))))

ggplot(churn_tenure, aes(tenurefactor, fill = Churn))+ geom_bar()+coord_flip()+
  labs(y = "Frequency", x = "Tenure Category")+theme_minimal()


#Monthly Charges density split churn vs non churn
churn %>% ggplot(aes(x=MonthlyCharges,fill=Churn))+ geom_density(alpha=0.8)+labs(title='Monthly Charges density split churn vs non churn' )



#Data preparation

#The minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups: 0-12 Month, 12-24 Month, 24-48 Months, 48-60 Months, > 60 Months.
min(churn$tenure); max(churn$tenure)

#group tenure w.r.t months
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}


churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)


#Remove the columns we do not need for the analysis:
churn$customerID <- NULL
churn$tenure <- NULL


##Exploratory data analysis and feature selection

numeric.var <- sapply(churn, is.numeric) ## Find numerical variables
corr.matrix <- cor(churn[,numeric.var])  ## Calculate the correlation matrix
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numeric Variables", method="number")


#The Monthly Charges and Total Charges are correlated. So one of them will be removed from the model. We remove Total Charges.
churn$TotalCharges <- NULL

#Compare models for prediction
churn$Churn  <- factor(churn$Churn)

#split the dataset
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
training<- churn[intrain,]
testing<- churn[-intrain,]

#Confirm the splitting is correct.
dim(training); dim(testing)

#dotplot 3 models glm,lda,rpart
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

fit.glm <- train(Churn ~ ., data=training, method="glm", metric=metric, trControl=control,family=binomial(link="logit"))
fit.lda <- train(Churn ~ ., data=training, method="lda", metric=metric, trControl=control)
fit.rpart <- train(Churn ~ ., data=training, method="rpart", metric=metric, trControl=control)

results <- resamples(list(
  LR=fit.glm,
  LDA=fit.lda,
  rPART=fit.rpart
))

dotplot(results)

#Auc for different models
AccCalc <- function(TestFit, name) {
  predictedval <- predict(TestFit, newdata=testing)
  roc_obj <- roc(testing$Churn, as.numeric(predictedval))
  Auc <- round(auc(roc_obj),2)
  acc <- as.data.frame(Auc)
  acc$FitName <- name
  return(acc)
  
}

accAll <- AccCalc(fit.glm, "glm")
accAll <- rbind(accAll, AccCalc(fit.lda, "lda"))
accAll <- rbind(accAll, AccCalc(fit.rpart, "rpart"))

rownames(accAll) <- c()
arrange(accAll,desc(Auc))


#Selecting Logistic regression model
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

#Feature analysis
anova(LogModel, test="Chisq")

## Assessing the predictive ability of the model

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

## Confusion Matrix 
print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)

## Odds Ratio
exp(cbind(OR=coef(LogModel), confint(LogModel)))


### logistics regression for selected feature
fit <- glm(Churn~SeniorCitizen + tenure_group + MultipleLines + InternetService + StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, 
           data=training,family=binomial)
churn.probs <- predict(fit, testing, type="response")
head(churn.probs)

churn.probs <- ifelse(churn.probs > 0.5,1,0)
Error <- mean(churn.probs != testing$Churn)
print(paste('Logistic Regression Accuracy for selected features',1-Error))

##Decision Tree
testing$Churn[testing$Churn=="0"] <- "No"
testing$Churn[testing$Churn=="1"] <- "Yes"

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree, main="Decision tree",type='simple')

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)

print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

##Pruned tree

MyTree <- rpart(Churn~Contract+tenure_group+PaperlessBilling+MonthlyCharges, data=training, method="class", 
                control=rpart.control(minsplit=10, cp=0.005))

prunedTree <- prune(MyTree, cp=MyTree$cptable[which.min(MyTree$cptable[,"xerror"]),"CP"])

#Predict accuracy

predTraining <- predict(MyTree, training, type="class") 
predValidation <- predict(MyTree, testing, type="class")
predRateTraining <- mean(predTraining == training$Churn)
predRateValidation <- mean(predValidation == testing$Churn)

prp(prunedTree, main=paste("Decision Tree\n(Correct classification rate ",
                           round(predRateTraining,4)*100,
                           "% for the training set\n ",
                           round(predRateValidation,4)*100,
                           "% for the validation set)"), 
    type=4, extra=6, faclen=0, under=TRUE)

## Random Forest

rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

pred_rf <- predict(rfModel, testing)
caret::confusionMatrix(factor(pred_rf), factor(testing$Churn))

plot(rfModel)

t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)


## Fit the Random Forest Model again

rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

## Make Predictions and Confusion Matrix again


pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(factor(pred_rf_new), factor(testing$Churn))

## Random Forest Feature Importance
varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')

dev.off()
sink()
