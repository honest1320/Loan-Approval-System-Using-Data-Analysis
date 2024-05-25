## NAME: HONEST ALBERT 
## SURNAME: TEMU
##MATR. NO: 34538A

library(ggplot2)
library(caTools)
library(tidyverse)
library(dplyr)
library(factoextra)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(GGally) 
library(randomForest) 



loan_dataset <- read.csv("C:/Users/Honest/Downloads/Loan Approval Analysis/loan_1.csv")


head(loan_dataset)


str(loan_dataset)
#------------------------------HANDLING DATA------------------------------------
loan_dataset[loan_dataset==""]=NA

# Data imputation for categorical variables
loan_dataset$Gender[is.na(loan_dataset$Gender)] = "Male"
loan_dataset$Married[is.na(loan_dataset$Married)] = "Yes"
loan_dataset$Dependents[is.na(loan_dataset$Dependents)] = "0"
loan_dataset$Self_Employed[is.na(loan_dataset$Self_Employed)] = "No"
loan_dataset$Credit_History  <- as.factor(loan_dataset$Credit_History)
loan_dataset$Credit_Histor[is.na(loan_dataset$Credit_History)] = "1"
loan_dataset$Term <- as.factor(loan_dataset$Term)
loan_dataset$Term[is.na(loan_dataset$Term)] = "360"

# Data imputation for continuous variables
loan_dataset$LoanAmount[is.na(loan_dataset$LoanAmount)] = median(loan_dataset$LoanAmount, na.rm=TRUE)


#Final method to eliminate NA's
colSums(is.na(loan_dataset))
loan_dataset$Credit_History[is.na(loan_dataset$Credit_History)] = 1
loan_dataset$Term[is.na(loan_dataset$Term)] = 360
# -------------------------------CORRELATION PLOT----------------------------------

corr_data <- loan_dataset[ ,c(6,7,8,9,10)]
corrplot(cor(corr_data), method = 'color')



# -------------------------------PAIR PLOT----------------------------------

ggpairs(corr_data, main = "Pairs Plot of Loan Approval Data", col = iris$Species)

# -------------------------------PCA----------------------------------
# Calculate Principal Components
pca_result <- prcomp(data=loan_dataset, ~Applicant_Income + ~Coapplicant_Income + ~Loan_Amount, scale = TRUE)

summary(pca_result)

pca_result


pca_result$rotation <- -1*pca_result$rotation
# Display Principal Components
pca_result$rotation
# Reverse the sign of the scores
pca_result$x <- -1*pca_result$x
# Display the first six scores
head(pca_result$x)


summary(pca_result)


# Scree plot of Variance
fviz_eig(pca_result,
         addlabels = TRUE,
         ylim = c(0,100))


# Biplot with Labeled Variables
fviz_pca_biplot(pca_result, repel = TRUE)


# Calculate total variance explained by each principal component
var_explained = pca_result$sdev^2 / sum(pca_result$sdev^2)

# -------------------------------END OF PCA----------------------------------


# -------------------------------KMEANS----------------------------------
signf_cols <- loan_dataset[, c(6,8)]
head(signf_cols)

scaled_data <- scale(signf_cols)

plot(signf_cols$Applicant_Income, signf_cols$Loan_Amount, main="Observation of Cars", xlab="Applicant_Income ", ylab="Loan_Amount")

# Elbow method
fviz_nbclust(scaled_data, kmeans, method='wss')+
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")



KM <- kmeans(scaled_data, 3, 100)
KM$cluster


fviz_cluster(kmeans(scaled_data, 3, 100), data = scaled_data)



clustered_data <- cbind(signf_cols, KM$cluster)
clustered_data

# -------------------------------DECISION TREE----------------------------------

dt <- loan_dataset[, -c(9,11)]
str(dt)

dt <-mutate(dt, Dependents = factor(Dependents),
            Applicant_Income = as.numeric(Applicant_Income),Loan_Amount = as.numeric(Loan_Amount),
            Credit_History = factor(Credit_History))


set.seed(123)

sample = sample.split(dt$Status, SplitRatio = .70)
train = subset(dt, sample == TRUE)
test = subset(dt, sample == FALSE)

summary(test)
summary(train)


# Decision tree train

tree <- rpart(Status ~., data=train)

pred <-predict(tree, train, type='class')

confusionMatrix(pred, as.factor(train$Status))


rpart.plot(tree, type = 2, fallen.leaves = T, cex = .75)


# Decision tree test

tree_test <- rpart(Status ~., data=test)

pred_test <-predict(tree_test, test, type='class')


confusionMatrix(pred_test, as.factor(test$Status))


rpart.plot(tree_test, type = 2, fallen.leaves = T, cex = .75)


# -------------------------------LOGISTIC REGRESSION----------------------------------
df <- loan_dataset

str(df)

df <- mutate(df, Status = if_else(Status == "Y", 1,0))

str(df)

sample = sample.split(df$Status, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)



# Logistic regression involving ALL variables

# ---------------with train data-----------------------
model <- glm(Status ~., data = train, family = "binomial")

summary(model)


predic <- predict(model, train, type = "response")


# Check accuracy
confmatrix <- table(Actual_value=train$Status,Predicted_value= predic > 0.5)

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

# ---------------with test data-----------------------
  
model_test <- glm(Status ~., data = test, family = "binomial")
summary(model_test)

predic_test <- predict(model_test, test, type = "response")

confmatrix <- table(Actual_value=test$Status,Predicted_value= predic_test > 0.5)





# Logistic regression involving only selected key variables

# ---------------with train data-----------------------
model2 <- glm (Status ~ Credit_History+Education+Self_Employed+Area+Loan_Amount+
                    Applicant_Income,data = train, family = binomial)
summary(model2)

predic_2 <- predict(model2, train, type = "response")

confmatrix <- table(Actual_value=train$Status,Predicted_value= predic_2 > 0.5)
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)



# ---------------with test data-----------------------
model_test2 <- glm (Status ~ Credit_History+Education+Self_Employed+Area+Loan_Amount+
                 Applicant_Income,data = test, family = binomial)

summary(model_test2)

predic_2 <- predict(model_test2, test, type = "response")

confmatrix <- table(Actual_value=test$Status,Predicted_value= predic_2 > 0.5)
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)


# Characteristic Logistic Regression curve placed on top of the binary data
ggplot(train, aes(x=Credit_History, y= Status))+
 geom_jitter(height = .05, alpha = .1)+
  geom_smooth(method = 'glm', method.args = list(family='binomial'), se = FALSE)
