library(caret)
library(randomForest)

d=read.csv("Seasons_Stats.csv")
e=read.csv("2018.csv")
for (i in 1:nrow(d)) {
  d$InNBA[i]=0
}


for (i in 1:nrow(d)) {
  if (is.na(d$Year[i])) {
    next()
  }
  if (d$Year[i]==2016) {
    from=i
    break
  }
}

for (i in from:nrow(d)) {
  if (is.na(d$Year[i])) {
    theend=i-1
    break
  }
}




data2016=d[from:theend,]
data2017=d[(theend+2):nrow(d),]
data2018=e[1:nrow(e),]
#print(data2016[1,2])
#print(data2017[1,2])

for (guy in 1:nrow(data2016)) {
  for (i in 1:nrow(data2017)) {
    if(grepl(data2016$Player[guy],data2017$Player[i])){
      data2016$InNBA[guy]=1
    }
  }
  
}

for (guy in 1:nrow(data2017)) {
  for (i in 1:nrow(data2018)) {
    if(grepl(data2017$Player[guy],data2018[i,2])){
      data2017$InNBA[guy]=1
    }
  }
}
for(i in 1:nrow(data2016)){
  for(j in 1:ncol(data2016)){
    if(is.na(data2016[i,j])){
      data2016[i,j]<-0
    }
  }
}

for(i in 1:nrow(data2017)){
  for(j in 1:ncol(data2017)){
    if(is.na(data2017[i,j])){
      data2017[i,j]<-0
    }
  }
}


rrr<-c(5,7:13,16:21,25,54)
data2016$InNBA <-as.factor(data2016$InNBA[])
data2017$InNBA <-as.factor(data2017$InNBA[])



sub2016<-data2016[,rrr]
sub2017<-data2017[,rrr]
#print(sub2016[1,])
#print(data2016[1:5,54])
#trainData <- sub2016[1:150,]

#testData <- sub2016[151:300,]

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("please enter your command", call.=FALSE)
}

i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    fold_num<-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    i<-i+1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }
  i<-i+1
}


#print(fold_num)
#print(out_f)
k<-as.numeric(fold_num)


folds<-createFolds(y=sub2016$InNBA,k)
#print(folds)
test<-{}
da<-{}
for(i in 1:k){
  testData<-sub2016[folds[[i]],]
  trainData<-sub2016[-folds[[i]],]

#print(trainData[1:5,])
#print(testData[1:5,])

nba_rf <- randomForest(InNBA~.,data=trainData,ntree=100,proximity=TRUE,importance=TRUE)
#test=c((nba_rf$confusion[2,2]+nba_rf$confusion[1,1])/as.numeric(sum(nba_rf$confusion)))
#test=c(test,length(sub2016$InNBA[which(predict(nba_rf,subset(testData,select = -InNBA))==sub2016$InNBA)])/length(sub2016$InNBA))

nba_pred<-predict(nba_rf,subset(testData,select = -InNBA))
confusion<-confusionMatrix(factor(testData$InNBA),nba_pred)
ta<-confusion$overall[1]
da<-c(da,ta)
print(da)

#nba_pred<-predict(nba_rf,newdata=testData)
#print(nba_pred)
#table(nba_pred,testData$InNBA)
}
da<-mean(da)
print(da)

nba_pred2<-predict(nba_rf,subset(sub2017,select = -InNBA))
confusion<-confusionMatrix(factor(sub2017$InNBA),nba_pred2)
print(confusion)
#test2=c(test,length(sub2017$InNBA[which(predict(nba_rf,subset(sub2017,select = -InNBA))==sub2017$InNBA)])/length(sub2017$InNBA))

#test<-mean(test)
#test2018<-test2[length(test2)]
#print(test)
#print(test2018)

table<-c("TN","FN","FP","TP")
ma<-c(confusion$table[1,1],confusion$table[1,2],confusion$table[2,1],confusion$table[2,2])
rb<-rbind(table,ma)
#ma<-confusion$table
mb<-confusion$overall[1]
mc<-confusion$byClass[1]
md<-confusion$byClass[2]
me<-confusion$byClass[5]
mf<-confusion$byClass[6]
mg<-confusion$byClass[7]
da<-c("Accuracy","Sensitivity","Specificity","Precision","Recall","F1")
data<-c(round(mb,4),round(mc,4),round(md,4),round(me,4),round(mf,4),round(mg,4))
rb2<-rbind(da,data)
cb<-cbind(rb,rb2)
print(cb)
write.table(cb, file = out_f, col.names = FALSE, row.names= FALSE,quote = FALSE,sep = ",")
print("done")
