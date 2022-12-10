library(e1071)
library(caTools)

df=read.csv(file.choose())
df

df$outlook=as.factor(df$outlook)
df$temp=as.factor(df$temp)
df$humidity=as.factor(df$humidity)
df$windy=as.factor(df$windy)
df$court=as.factor(df$court)
df$play=as.factor(df$play)

str(df)

naiveb=naiveBayes(play~. , data=df)
preds=predict(naiveb, df)
table(preds, df$play)
tot_accuracy=(51+55)/(51+55+18+26)
tot_accuracy

set.seed(2)

split<-sample.split(df$play, SplitRatio = 0.80)
train<-subset(df, split==TRUE)
test<-subset(df, split==FALSE)

model=naiveBayes(play~. , data=train)
train_predictions=predict(model, train)

table(train_predictions, train$play)
train_accuracy=(44+45)/(44+45+18+13)
train_accuracy

test_predictions=predict(model, test)
table(test_predictions, test$play)
test_accuracy=(18)/30
test_accuracy