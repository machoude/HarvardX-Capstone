if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

my_data=read.delim(file.choose(),header = FALSE)
attr=c("Age","Sex","Cp","Trestbps","Chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
my_data=separate(my_data,col = 1,sep = ",", remove = TRUE,
                  convert = FALSE, extra = "warn", fill = "warn",into = attr)

#Data clening
my_data=my_data%>%mutate(Age=as.numeric(Age),
                 Sex=as.numeric(Sex),
                 Cp=as.numeric(Cp),
                 Trestbps=as.numeric(Trestbps),
                 Chol=as.numeric(Chol),
                 fbs=as.numeric(fbs),
                 restecg=as.numeric(restecg),
                 thalach=as.numeric(thalach),
                 exang=as.numeric(exang),
                 oldpeak=as.numeric(oldpeak),
                 slope=as.numeric(slope),
                 ca=as.numeric(ca),
                 thal=as.numeric(thal),
                 num=as.numeric(num))

#removing the na's and replaciong them with zeros
my_data=my_data%>%mutate(thal=ifelse(is.na(thal),0,thal),
                         num=ifelse(is.na(num),0,num),
                         ca=ifelse(is.na(ca),0,ca))
#Data Visualitation
dim(my_data)
#how males and females
my_data%>%ggplot(aes(Sex))+geom_bar(fill="lightblue")+xlab("0=Female and 1=Male")
#the age range
my_data%>%ggplot(aes(Age))+geom_bar(fill="lightblue")
max(my_data$Age)
min(my_data$Age)
#chest pain 
my_data%>%ggplot(aes(Cp))+geom_histogram(fill="lightblue")+xlab("Chest pain")
#trestbps
my_data%>%filter(Sex==0)%>%ggplot(aes(Trestbps))+geom_histogram(fill="black")+xlab("resting blood pressure for females")
my_data%>%filter(Sex==1)%>%ggplot(aes(Trestbps))+geom_histogram(fill="lightgreen")+xlab("resting blood pressure for males")
#cholesteral
my_data%>%ggplot(aes(Chol))+geom_density(fill="lightblue")
my_data%>%filter(Sex==0)%>%ggplot(aes(Trestbps))+geom_histogram(fill="black")+xlab("serum cholestoral for females")
my_data%>%filter(Sex==1)%>%ggplot(aes(Trestbps))+geom_histogram(fill="lightgreen")+xlab("serum cholestoral for males")
#fbs
my_data%>%ggplot(aes(fbs))+geom_bar(fill="lightblue")+xlab("fasting blood sugar")
#resting electrocardiographic results
my_data%>%ggplot(aes(restecg))+geom_bar(fill="lightblue")+xlab("resting electrocardiographic results")
# maximum heart rate achieved
my_data%>%ggplot(aes(thalach))+geom_density(fill="lightblue")+xlab("maximum heart rate")
# exercise induced angina
my_data%>%ggplot(aes(exang))+geom_bar(fill="lightblue")+xlab("exercise induced angina")
#ST depression induced by exercise relative to rest
my_data%>%ggplot(aes(oldpeak))+geom_histogram(fill="lightblue")
#the slope of the peak exercise ST segment
my_data%>%ggplot(aes(slope))+geom_histogram(fill="lightblue")+xlab("slope of the peak exercise ST segment")
#number of major vessels (0-3) colored by flourosopy
my_data%>%ggplot(aes(ca))+geom_histogram(fill="lightblue")+xlab("major vessels colored by flourosopy")
# thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
my_data%>%ggplot(aes(thal))+geom_histogram(fill="lightblue")
#num: diagnosis of heart disease (angiographic disease status)
#Value 0: < 50% diameter narrowing
# Value 1: > 50% diameter narrowing
my_data%>%ggplot(aes(num))+geom_density(fill="lightblue")

#Relationship between the variables
#Sex vs Age
chisq.test(my_data$Sex,my_data$Age)$p.value
#Sex vs chest pain
chisq.test(my_data$Sex,my_data$Cp)$p.value
#Sex vs resting blood sugar
chisq.test(my_data$Sex,my_data$Trestbps)$p.value
#Sex vs serum cholestoral
chisq.test(my_data$Sex,my_data$Chol)$p.value
#Sex vs fasting blood sugar
chisq.test(my_data$Sex,my_data$fbs)$p.value
#sex vs resting electrocardiographic results
chisq.test(my_data$Sex,my_data$restecg)$p.value
#Sex vs maximum heart rate achieved
chisq.test(my_data$Sex,my_data$thalach)$p.value
#Sex vs ST depression induced by exercise relative to rest
chisq.test(my_data$Sex,my_data$oldpeak)$p.value
#Sex vs the slope of the peak exercise ST segment
chisq.test(my_data$Sex,my_data$slope)$p.value
#Sex vs number of major vessels (0-3) colored by flourosopy
chisq.test(my_data$Sex,my_data$ca)$p.value
#Sex vs thal
chisq.test(my_data$Sex,my_data$thal)$p.value
#Sex vs presence of heart disease
chisq.test(my_data$Sex,my_data$num)$p.value


#Machine learning
#test and train sets
set.seed(3,sample.kind="Rounding")
test_index=createDataPartition(my_data$Age,times=1,p=0.2,list=FALSE)
test_set=my_data[test_index,]
train_set=my_data[-test_index,]

#modeling
#model 1:linear regression
#fiting the model
model_lm=lm(Sex ~ .,data=train_set)
#predictions
y_hat_lm=predict(model_lm,test_set)
#confusion matrix
cm_lm=confusionMatrix(data=factor(round(y_hat_lm)),reference=factor(test_set$Sex))
#print the results of the confusion matrix
cm_lm
#store the results in a data frame
results <- data_frame(method = "Linear Regression",
                      Accuracy = cm_lm$overall[["Accuracy"]],
                      Sensitivity = cm_lm$byClass[["Sensitivity"]],
                      Specificity = cm_lm$byClass[["Specificity"]])
#print the results
results[1,]%>%knitr::kable()
#model 2:k-nn
#fiting the model
model_knn=knn3(Sex~.,data=train_set,k=5)
#predictions
y_hat_knn=predict(model_knn,test_set)
#store the predictions in the closets interger
p_hat_knn=ifelse(y_hat_knn[,1]>0.5,0,1)
#confusion matrix
cm_knn=confusionMatrix(data=factor(p_hat_knn),reference=factor(test_set$Sex))
#print the results of confusion matrix
cm_knn
#insert the new model to the data frame with the results
results <- bind_rows(results,
                       data_frame(method="Knn",
                                  Accuracy = cm_knn$overall[["Accuracy"]],
                                  Sensitivity = cm_knn$byClass[["Sensitivity"]],
                                  Specificity = cm_knn$byClass[["Specificity"]]))
#print the results
results[2,]%>%knitr::kable()
#model 3:decision tree 
#fiting the model
model_rpart=rpart(Sex~ .,data=train_set)
#predictions
y_hat_rpart=predict(model_rpart,test_set)
#confusion matrix
cm_rpart=confusionMatrix(data=factor(round(y_hat_rpart)),reference = factor(test_set$Sex))
#print the results of the confusion matrix
cm_rpart
#insert the new model to the data frame with the results
results <- bind_rows(results,
                     data_frame(method="Rpart",
                                Accuracy = cm_rpart$overall[["Accuracy"]],
                                Sensitivity = cm_rpart$byClass[["Sensitivity"]],
                                Specificity = cm_rpart$byClass[["Specificity"]]))
#print the results
results[3,]%>%knitr::kable()

#the decision tree
plot(model_rpart)
text(model_rpart)
