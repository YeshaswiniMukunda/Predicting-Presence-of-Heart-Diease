# load required libraries
library(e1071)


#load data
dat <- read.csv("cleveland.csv")
dat1 <- read.csv("hungarian.csv")
dat2 <- read.csv("switzerland.csv")
dat3<-read.csv("generated.csv")

#using same names so that combining datasets is easy 
names(dat1)<-names(dat)
names(dat2)<-names(dat)
names(dat3)<-names(dat)

#combine the three datasets
data<- rbind(dat,dat1,dat2,dat3)

#rename the cloumns
colnames(data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","target")

#replace ? with na
data[data=="?"]<- NA
dim(data)

# Set seed
set.seed(227)

# Remove variables having high missing percentage (50%)
data1 <- data[, colMeans(is.na(data)) <= .5]
dim(data1)

# Identifying numeric variables
data2 <-transform(data1,age=as.numeric(age),sex=as.numeric(sex),cp=as.numeric(cp),trestbps=as.numeric(trestbps),chol=as.numeric(chol)
                  ,fbs=as.numeric(fbs),restecg=as.numeric(restecg),thalach=as.numeric(thalach),
                  exang=as.numeric(exang),oldpeak=as.numeric(oldpeak),slope=as.numeric(slope),
                  thal=as.numeric(thal),target=as.numeric(target))

#Factorize
new_target = ifelse(data2$target<=0, 0, 1)
data2 = data.frame(data2, new_target)
data2$new_target<-factor(data2$new_target)

n_random<-round(0.7*nrow(data2))
index<-sample(1:nrow(data2),n_random)
train<-data2[index,]
test<-data2[-index,]

#training the model
nb_model<-naiveBayes (new_target ~.-target, data = train)

#summary of the model
summary(nb_model)

#prediction on test dataset
pred <-predict(nb_model, test,type = 'class')

#creating confusion matrix
confusion_matrix<-table(pred, test$new_target)
print(confusion_matrix)

#calculating miss classification rate
miss_class<-1-(sum(diag(confusion_matrix))/sum(confusion_matrix))
print(miss_class) 