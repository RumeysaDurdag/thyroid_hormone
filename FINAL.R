##RÜMEYSA DURDAĞ 


getwd()
setwd("C:/Users/rmysa/OneDrive/Masaüstü/412")
library(dplyr)
library(naniar)
library(stringr)
library(ggplot2)
library(tidyverse)
library(readxl)

thyroid <- read.csv("thyroid.csv",na.strings = "?",stringsAsFactors = T)
head(thyroid)
str(thyroid)

#data cleaning
thyroid <- thyroid %>%
  mutate_at(vars(-c(referral_source,Class)), ~ gsub("t", "true", .))
thyroid <- thyroid %>%
  mutate_all(funs(str_replace(., "f", "false")))
thyroid$sex <- str_replace(thyroid$sex, fixed("M"), "male")
thyroid$sex <- str_replace(thyroid$sex, fixed("F"), "female")
thyroid <- thyroid %>%
  mutate_all(~str_replace(., "\\?", "NA"))
thyroid <- thyroid[,-28]
thyroid$age <- str_remove_all(thyroid$age,"455")
thyroid %>% count(age)
thyroid <- thyroid %>%
  mutate_at(vars(c(age,TSH,T3,TT4,T4U,FTI)), as.numeric)
thyroid <- thyroid %>%
  mutate_at(vars(!c(age,TSH,T3,TT4,T4U,FTI)), as.factor)
str(thyroid)
n_miss(thyroid)
#EDA with missing values
#Descriptive statistics
library(devtools)
library(visdat)
vis_miss(thyroid)
vis_dat(thyroid)
library(DataExplorer)





str(thyroid)


thyroid <- thyroid[, !(names(thyroid) %in% c("on_thyroxine", "query_on_thyroxine", 
                                             "thyroid_surgery", "lithium", "psych", "hypopituitary",
                                             "TBG_measured", "referral_source"))]

summary(thyroid)


#EDA with missing values
#Descriptive statistics

library(ggcorrplot)
library(ggthemes)
library(RColorBrewer)
library(hrbrthemes)
library(ggpubr)
library(viridis)
library(prismatic)
library(paletteer)


# Histogram of a numeric variable
hist(thyroid$age)



# Subset the data for hyperthyroid and hypothyroid cases
hyperthyroid_data <- subset(thyroid, query_hyperthyroid == "true")
hypothyroid_data <- subset(thyroid, query_hypothyroid == "true")

# Combine the two subsets into a single dataframe for box plot comparison
combined_data <- rbind(
  data.frame(type = "Hyperthyroid", age = hyperthyroid_data$age),
  data.frame(type = "Hypothyroid", age = hypothyroid_data$age)
)

# Compare the distribution of age between the conditions using box plots

ggplot(combined_data, aes(y =age,x = type,fill = type))+
  geom_boxplot()+xlab("Thyroid Type")+ylab("Age")+
  scale_fill_manual(values = c("#FFB6DB", "#B6DBFF"))+theme_bw()

#RESEARCH QUESTION 1: Is there a significant difference in thyroid hormone 
#levels between patients with hyperthyroidism and hypothyroidism?

combined_data2 <- rbind(
  data.frame(type = "Hyperthyroid", TSH = hyperthyroid_data$TSH),
  data.frame(type = "Hypothyroid", TSH = hypothyroid_data$TSH)
)


ggplot(combined_data2, aes(y =TSH,x = type,fill = Class))+
  geom_boxplot()+xlab("Thyroid Type")+ylab("TSH")+
  scale_fill_manual(values = c("#FFB6DB", "#B6DBFF"))+theme_bw()


#relation between two numerical variable (TT4 vs TSH)


ggplot(thyroid[thyroid$TSH_measured=="true"&thyroid$TT4_measured=="true",], aes(x=TT4, y=TSH, color = query_hypothyroid)) + 
  geom_point(shape=20, size=5)+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")+
  scale_y_continuous(limits = c(0,500))+theme_bw()



ggplot(thyroid[thyroid$TSH_measured=="true",], aes(x=age, y=TSH)) +
  geom_point(size=5, shape=20, color = "#9eb9f3") + geom_smooth(method=lm,linetype="dashed", se=FALSE)

#RESEARCH QUESTION 2: Are there any significant correlations between different thyroid hormone 
#levels in the dataset?

index<- unlist(lapply(thyroid, is.numeric)) 
ggcorrplot(cor(na.omit(thyroid[,index])))

# there is a strong correlation between TT4 and FTI

cor.test(thyroid$TT4,thyroid$FTI)
cor


##########RESEARCH QUESTION 3: Are there any significant difference between Class and TSH?######

negative <- subset(thyroid, Class == "negative")
positive <- subset(thyroid, Class == "sick")

combined_data3 <- rbind(
  data.frame(type = "Not Sick", age = negative$age),
  data.frame(type = "Sick", age = positive$age)
)

ggplot(combined_data3, aes(y =age,x = type,fill = type))+
  geom_boxplot()+xlab("Thyroid")+ylab("Age")+
  scale_fill_manual(values = c("#FFB6DB", "#B6DBFF"))+theme_bw()

t.test(age ~ Class, data = thyroid)

str(thyroid)
thyroid$age <- as.numeric(thyroid$age)
thyroid$Class <- as.character(thyroid$Class)

ggplot(thyroid, aes(x = Class,fill = Class)) +
  geom_bar()

ggplot(thyroid, aes(x = Class, fill = sex)) +
  geom_bar(aes(y = ..count..), stat = "count", position = position_dodge()) +
  ggtitle("Sick Or Not") +
  scale_fill_manual(values = c("#C29DFF", "#78E676","#403F34"))



ggplot(thyroid, aes(x = pregnant,fill = Class)) +
  geom_bar()+
  ggtitle("Sick Or Not")+ scale_fill_manual(values = c("#EE6C56", "#37AE79"))


#Imputation Methods
#MICE

library(mice)


# check na values
missing_values <- colSums(is.na(thyroid))
print(missing_values)

imputed_data <- mice(thyroid, m = 5, maxit = 50)

imputed_data <- complete(imputed_data, 1)

thyroid_imputed <- imputed_data

sum(is.na(thyroid_imputed))

write.csv(thyroid_imputed, file = "thyroid_imputed.csv", row.names = FALSE)


str(thyroid_imputed)

#ONE-HOT ENCODING

library(caret)
categoric <- thyroid_imputed[,c(2:11,13,15,17,19,21)]
str(thyroid_imputed)

dummy <- dummyVars("~ .", data = categoric)
encode_data <- data.frame(predict(dummy, newdata = categoric))
new_imp_data<- data.frame(thyroid_imputed[,c(1,12,14,16,18,20)],encode_data)
str(new_imp_data)
new_imp_data <- new_imp_data[,-c(8,9,11,13,15,17,19,21,23,25,27,29,31,33,35)]

str(new_imp_data)


### SVM ###
str(new_imp_data)
new_imp_data$Classsick  <- ifelse(encode_data$Classsick  == 0, "No","Yes")
new_imp_data$Classsick  <- as.factor(new_imp_data$Classsick )

library(e1071)

# Split the data into training and testing sets
set.seed(123)
training.samples <- new_imp_data$Classsick  %>%createDataPartition(p = 0.8, list = FALSE) #createDataPartition helps you define train set index
train.data  <- new_imp_data[training.samples, ]
test.data <- new_imp_data[-training.samples, ]
str(train.data)
# Train the SVM model

set.seed(1)
train.data$Classsick  <- as.factor(train.data$Classsick )
str(train.data)
svm_model <- svm(Classsick  ~ ., data = train.data, kernel = "radial")

svm_model


library(gridExtra)

train_prediction<-predict(svm_model,train.data,type = 'response')

train.data$Classsick  <- as.factor(train.data$Classsick )
confusionMatrix(data = train_prediction,reference = train.data$Classsick )


svm_test_predict<-predict(svm_model, newdata= test.data,type = 'response')

test.data$Classsick <- as.factor(test.data$Classsick )
str(test.data)
confusionMatrix(data = svm_test_predict,reference = test.data$Classsick)


#### ANN #####

library(nnet)
library(GGally)


train.data$Classsick <- as.character(train.data$Classsick)
train.data$Classsick <- ifelse(train.data$Classsick== "Yes",1,0)

for(i in 7:20){
  train.data[,i]=as.factor(train.data[,i])
}
str(train.data)
train.data$Classsick <- as.factor(train.data$Classsick)


test.data$Classsick <- as.character(test.data$Classsick)
test.data$Classsick <- ifelse(test.data$Classsick== "Yes",1,0)
for(i in 7:20){
  test.data[,i]=as.factor(test.data[,i])
}
str(test.data)
test.data$Classsick <- as.factor(test.data$Classsick)

NM <- nnet(Classsick ~ ., data = train.data, size = 10, maxit = 1000)
print(NM)
NM
NM_pred_train = predict(NM, train.data, type = 'class') 
NM_pred_test = predict(NM, test.data, type = 'class')

confusionMatrix(as.factor(NM_pred_train),as.factor(train.data$Classsick),mode = "everything")
confusionMatrix( as.factor(NM_pred_test),as.factor(test.data$Classsick),mode = "everything")

#### RANDOM FOREST ###
library(randomForest)

rf_model <- randomForest(Classsick ~ ., data = train.data, ntree = 500,importance=TRUE)

varImpPlot(rf_model)


model_rf_pred_train = predict(rf_model, train.data, type = 'response') 
model_rf_pred_test = predict(rf_model, test.data, type = 'response')

confusionMatrix(as.factor(model_rf_pred_train),as.factor(train.data$Classsick),mode = "everything")
confusionMatrix( as.factor(model_rf_pred_test),as.factor(test.data$Classsick),mode = "everything")

### XGBOOST ###
library("xgboost")
x.train=data.matrix(train.data[,c(-21)])
y.train=train.data[,21]

x.test=data.matrix(test.data[,c(-21)])
y.test=test.data[,21]

xgboost.train = xgb.DMatrix(data=x.train, label=y.train)
xgboost.test = xgb.DMatrix(data=x.test, label=y.test)
str(xgboost.test)


xg_model <- xgboost(data = xgboost.train,                   
                 max.depth=4,                       
                 nrounds=75)                              

pred_test = predict(xg_model, xgboost.test)
pred_train = predict(xg_model, xgboost.train)
pred_test
pred_train[(pred_train>4)]=3
pred_test[(pred_test>3)] = 3
pred_y = as.factor((levels(y.test))[round(pred_test)])
pred_y_train = as.factor((levels(y.train))[round(pred_train)])
print(pred_y)
confusionMatrix(y.test, pred_y,mode = "everything")
confusionMatrix(pred_y_train, pred_y,mode = "everything")


