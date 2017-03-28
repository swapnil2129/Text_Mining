#1.1
emails<-read.csv("emails.csv",stringsAsFactors = FALSE)
str(emails)
dim(emails)
View(emails)
#1.2
table(emails$spam)
#1.3
head(emails)
#1.4
max(nchar(emails$text))
#1.5
which.min(nchar(emails$text))
#2.1
corpus<-Corpus(VectorSource(emails$text))
corpus
dim(corpus)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, content_transformer(removePunctuation))
corpus = tm_map(corpus, removeWords, c( stopwords("english")))
corpus = tm_map(corpus, content_transformer(stemDocument))
dtm<-DocumentTermMatrix(corpus)
dtm
View(dtm)
dim(dtm)
#2.2
spdtm<-removeSparseTerms(dtm,0.95)
dim(spdtm)
str(spdtm)
emailsSparse<-as.data.frame(as.matrix(spdtm))
colnames(emailsSparse)<-make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
#2.4
emailsSparse$spam<-emails$spam
sort(colSums(subset(emailsSparse,spam==0)))
#2.5
sort(colSums(subset(emailsSparse,spam==1)))
#3.1
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
sample<-sample.split(emailsSparse$spam,0.75)
train<-emailsSparse[sample,]
test<-emailsSparse[-sample,]
#logistic
spamLog<-glm(spam~.,data = train,family = binomial)
predictLog<-predict(spamLog,data=train)
table(predictLog<0.00001)
table(predictLog>0.99999)
table(predictLog>=0.00001 & predictLog<=0.99999)

#Cart
spamCART<-rpart(spam~.,method = "class",data=train)
predictCART<-predict(spamCART,data=train )

#RF
set.seed(123)
spamRF<-randomForest(spam~.,data=train)
predictRf<-predict(spamRF,type="prob")

#3.2
summary(spamLog)
#3.3
prp(spamCART)
#3.4
table(train$spam,predictLog>0.5)
(3196+1011)/(nrow(train))
#3.5
predictionTrainLog = prediction(predictLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values)
#3.6
table(train$spam,predictCART[,2]>0.5)
(2885+894)/(2885+894+167+64)
#3.7
predictionTrainCART = prediction(predictCART[,2], train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values)
#3.8
table(train$spam,predictRf[,2]>0.5)
(3013+912)/nrow(train)
#3.9
predictionTrainRF = prediction(predictRf[,2], train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values)
#4.1
predictLogtest<-predict(spamLog,newdata=test,type="response")
table(test$spam,predictLogtest>0.5)
(1329+4309)/nrow(test)
#4.2
predictionTestLog = prediction(predictLogtest, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)
#4.3
predicttestCART<-predict(spamCART,newdata=test )
table(test$spam,predicttestCART[,2]>0.5)
(1279+4113)/nrow(test)
#4.4
predictionTestCART = prediction(predicttestCART[,2], test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)
#4.5
predictRftest<-predict(spamRF,type="prob",newdata = test)
table(test$spam,predictRftest[,2]>0.5)
(4336+1334)/nrow(test)
#4.6
predictionTestRF = prediction(predictRftest[,2], test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)
#6.1
wordCount = rowSums(as.matrix(dtm))
length(wordCount)
#6.2
hist(wordCount)
#6.3
hist(log(wordCount))
#6.4
emailsSparse$logWordCount=log(wordCount)
boxplot(emailsSparse$logWordCount~emailsSparse$spam)
#6.5
train2<-emailsSparse[sample,]
test2<-emailsSparse[-sample,]
spam2CART<-rpart(spam~.,data=train2,method="class")
prp(spam2CART)

set.seed(123)
spam2RF<-randomForest(spam~.,data=train2)
#6.6
predicttestCART2<-predict(spam2CART, type="prob",newdata=test2)
table(test2$spam,predicttestCART2[,2]>0.5)
(4089+1285)/nrow(test2)
#6.7
predictionTestCART2 = prediction(predicttestCART2[,2], test2$spam)
as.numeric(performance(predictionTestCART2, "auc")@y.values)
#6.8
predictRftest2<-predict(spam2RF,type="prob",newdata = test2)
table(test2$spam,predictRftest2[,2]>0.5)
(4347+1332)/(nrow(test2))
#6.9
predictionTestRF2 = prediction(predictRftest2[,2], test2$spam)
as.numeric(performance(predictionTestRF2, "auc")@y.values)
