#################################################
#################################################
### IMPORT LIBRARIES AND DATA SETS
#################################################
#################################################

library(ggplot2)
library(dplyr)
library(caTools)
library(stringr)
library(xgboost)
library(quanteda)
library(SnowballC)
library(tm)
library(gridExtra)
library(corrplot)
#library(data.table)

# SC Code
train <- fread("C:/Users/Steve Condylios/Documents/Mercari_Kaggle_ML/input/train.tsv")
test <- fread("C:/Users/Steve Condylios/Documents/Mercari_Kaggle_ML/input/test.tsv")
saveRDS(train, "trainfread.rds")
saveRDS(test, "testfread.rds")

train <-readRDS("trainfread.rds")
test <-readRDS("testfread.rds")



train = read.csv("C:/Users/Steve Condylios/Documents/Mercari_Kaggle_ML/input/train.tsv", sep='\t')
test = read.csv("C:/Users/Steve Condylios/Documents/Mercari_Kaggle_ML/input/test.tsv", sep='\t')
submission = read.csv('C:/Users/Steve Condylios/Documents/Mercari_Kaggle_ML/input/sample_submission.csv')
all = rbind(within(train,rm('train_id','price')),within(test,rm('test_id')))
summary(all)

#### In order to analyze the data more easily, you can split the set:

#split = sample.split(train, SplitRatio = 0.1)
#trainNew = subset(train,split == TRUE)

## If you do this, replace 'all' and 'train' with trainNew, and have yourself some fun.


summary(train$price)




options(repr.plot.width=5, repr.plot.height=5)
a = ggplot(train, aes(x=price))+ geom_histogram(binwidth=30)
train$price = with(train, ifelse(price > 0, log(price), price))
b = ggplot(train, aes(x=price))+ geom_histogram(binwidth=0.1)
grid.arrange(a,b,ncol=2)




##########################################
### brand_name
##########################################

all$brand_name = as.character(all$brand_name)
all$brand_name[all$brand_name == ""] = -1



all[1:5,'category_name']






###############################################
### category_name + cat1,cat2,cat3
###############################################

splitVar = str_split(all$category_name, "/")
cat2 = sapply(splitVar,'[',2)
cat3 = sapply(splitVar,'[',3)
all$cat2 = cat2
all$cat3 = cat3
all$cat2[is.na(all$cat2)] = -1
all$cat3[is.na(all$cat3)] = -1
all$cat1 = all$category_name
all$cat1[is.na(all$cat1)] = -1
all$category_name = NULL







all$descLength = str_length(all$item_description)
all$descLength[all$descLength == 2] = -1
all$descUP = sapply(gregexpr("[A-Z]", all$item_description), length)
all$descUP[all$item_description == -1] = -1
all$descNC = sapply(gregexpr("[0-9]", all$item_description), length)
all$descNC[all$item_description == -1] = -1
all$descUD = (all$descUP/all$descLength)
all$descND = (all$descNC/all$descLength)




corpus = Corpus(VectorSource(all$item_description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dataframe <- data.frame(text=sapply(corpus, identity),stringsAsFactors=F)
all$item_description = dataframe$text





all$item_description = ifelse(str_detect(all$item_description,'description yet'),-1,all$item_description)
all$descWC = sapply(gregexpr("\\W+", all$item_description), length) + 1
all$descWC[all$item_description == -1] = -1
all$descWD1 = (all$descLength/all$descWC)
all$descWD1[all$item_description == -1] = -1
all$descWD2 = tan(all$descWD1)
all$descWD2[all$item_description == -1] = -1

#######################################################################GOT TO HERE

all['nameLength'] = str_length(all$name)
all['nameUP'] = sapply(gregexpr("[A-Z]", all$name), length)
all['nameNC'] = sapply(gregexpr("[0-9]", all$name), length)
all['nameUD'] = (all$nameUP/all$nameLength)
all['nameND'] = (all$nameNC/all$nameLength)
corpus = Corpus(VectorSource(all$name))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dataframe1 <- data.frame(text=sapply(corpus, identity),stringsAsFactors=F)
all$name = dataframe1$text
all['nameLength'] = str_length(all$name)
all['nameWC'] = sapply(gregexpr("\\W+", all$name), length) + 1
all['nameWD1'] = (all$nameLength/all$nameWC)
all['nameWD2'] = tan(all$nameWD1)




train1 = subset(train, price <= 2.833)
train2 = subset(train, price > 2.833)

corpus1 = corpus(as.character(train1$item_description))
summary(corpus1)
dfm1 <- dfm(
  corpus1, 
  ngrams = 1, 
  remove = c("rm", stopwords("english")),
  remove_punct = TRUE,
  remove_numbers = TRUE,
  stem = TRUE)
tf1 <- topfeatures(dfm1, n = 100)

corpus2 = corpus(as.character(train2$item_description))
summary(corpus2)
dfm2 <- dfm(
  corpus2, 
  ngrams = 1, 
  remove = c("rm", stopwords("english")),
  remove_punct = TRUE,
  remove_numbers = TRUE,
  stem = TRUE)
tf2 <- topfeatures(dfm2, n = 100)

termFrame = data.frame(term = names(tf1), freq = unname(tf1))
termFrame1 = data.frame(term = names(tf2), freq = unname(tf2))
termFrame = termFrame %>%
  left_join(termFrame1, by = 'term')
termFrame$ratio = termFrame$freq.x/(termFrame$freq.x+termFrame$freq.y)





termFrame1 = subset(termFrame, ratio < 0.43)
options(repr.plot.width=8, repr.plot.height=5)
ggplot(termFrame1, aes(x=reorder(term,ratio), y=ratio))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits=c(0,1))+
  ggtitle('Term Frequency Ratio - Above Median Pricing - item_description')+
  geom_abline(intercept = 0.5, slope = 0,color="red")+
  xlab('Term')

termFrame2 = subset(termFrame, ratio > 0.53)
ggplot(termFrame2, aes(x=reorder(term,-ratio), y=ratio))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits=c(0,1))+
  ggtitle('Term Frequency Ratio - Below Median Pricing - item_description')+
  geom_abline(intercept = 0.5, slope = 0,color="red")+
  xlab('Term')






lMedian = subset(train, price <= 2.833)
hMedian = subset(train, price > 2.833)

corpusNL = corpus(as.character(lMedian$name))
summary(corpusNL)
dfmNL <- dfm(
  corpusNL, 
  ngrams = 1, 
  remove = c("rm", stopwords("english")),
  remove_punct = TRUE,
  remove_numbers = TRUE,
  stem = TRUE)
tfNL <- topfeatures(dfmNL, n = 100)

corpusNH = corpus(as.character(hMedian$name))
summary(corpusNH)
dfmNH <- dfm(
  corpusNH, 
  ngrams = 1, 
  remove = c("rm", stopwords("english")),
  remove_punct = TRUE,
  remove_numbers = TRUE,
  stem = TRUE)
tfNH <- topfeatures(dfmNH, n = 100)

termFrameNL = data.frame(term = names(tfNL), freq = unname(tfNL))
termFrameNH = data.frame(term = names(tfNH), freq = unname(tfNH))
termFrameN = termFrameNL %>%
  left_join(termFrameNH, by = 'term')
termFrameN$ratio = termFrameN$freq.x/(termFrameN$freq.x+termFrameN$freq.y)









##########################################
### name term frequency ratio
##########################################

termFrameNl = subset(termFrameN, ratio < 0.40)
ggplot(termFrameNl, aes(x=reorder(term,ratio), y=ratio))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits=c(0,0.5))+
  geom_abline(intercept = 0.5,slope=0,color="red")+
  ggtitle('Term Frequency Ratio - Below Median Pricing - name')+
  xlab('Term')

termFrameNh = subset(termFrameN, ratio > 0.60)
ggplot(termFrameNh, aes(x=reorder(term,-ratio), y=ratio))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits=c(0,1))+
  geom_abline(intercept = 0.5,slope=0,color="red")+
  ggtitle('Term Frequency Ratio - Below Median Pricing - name')+
  xlab('Term')









###############################################
### relevant words w.r.p. ratio of price sets
###############################################

##################################
### item_description
##################################

all['box'] = (str_detect(all$item_description, 'box'))*1
all['bag'] = (str_detect(all$item_description, 'bag'))*1
all['come'] = (str_detect(all$item_description, 'come'))*1
all['gold'] = (str_detect(all$item_description, 'gold'))*1
all['icase'] = (str_detect(all$item_description, 'case'))*1
all['condit'] = (str_detect(all$item_description, 'condit'))*1
all['authent'] = (str_detect(all$item_description, 'authent'))*1
all['origin'] = (str_detect(all$item_description, 'origin'))*1
all['leather'] = (str_detect(all$item_description, 'leather'))*1
all['includ'] = (str_detect(all$item_description, 'includ'))*1
all['ibundl'] = (str_detect(all$item_description, 'bundl'))*1
all['gb'] = (str_detect(all$item_description, 'gb '))*1

##################################
### name
##################################

all['nbundl'] = (str_detect(all$name, 'bundl'))*1
all['ncase'] = (str_detect(all$name, 'case'))*1
all['iphon'] = (str_detect(all$name, 'iphon'))*1
all['shirt'] = (str_detect(all$name, 'shirt'))*1
all['ship'] = (str_detect(all$name, 'ship'))*1
all['reserv'] = (str_detect(all$name,'reserv'))*1
all['ring'] = (str_detect(all$name,'ring'))*1
all['diamond'] = (str_detect(all$name,'diamond'))*1





all$brand_name[str_detect(all$name,'lularo')] = 'Lularoe'




funTime = all

features = names(funTime)
for(f in features){
  if(class(funTime[[f]])=="character"){
    levels=sort(unique(funTime[[f]]))
    funTime[[f]]=as.integer(factor(funTime[[f]],levels = levels))}
}




rmseEval=function(yTrain,yPred) {
  mseEval=sum((yTrain - yPred)^2)/length(yTrain)
  return(sqrt(mseEval)) }





ntrain = nrow(train)
nfun = nrow(funTime)
validation=funTime[1:ntrain,]
validation$price=train[,'price']
testXG = funTime[(ntrain+1):nfun,]
split=sample.split(validation,SplitRatio=0.99)
trainXG=subset(validation,split==TRUE)
validXG=subset(validation,split==FALSE)
yTrainXG=trainXG$price
yValidXG=validXG$price
trainXG = trainXG %>%
  select(-price)
validXG = validXG %>%
  select(-price)
trainXG[] = lapply(trainXG,as.numeric)
validXG[] = lapply(validXG,as.numeric)
testXG[] = lapply(testXG,as.numeric)
xgTrain = xgb.DMatrix(as.matrix(trainXG),label=yTrainXG)
xgValid = xgb.DMatrix(as.matrix(validXG),label=yValidXG)
xgTest = xgb.DMatrix(as.matrix(testXG))

xgPrm = list(boost='gbtree',objective='reg:linear',colsample_bytree=1,
             eta=0.11,max_depth=9,min_child_weight=1,alpha=0.3,
             lambda=0.4,gamma=0.2,subsample=0.8,seed=5,silent=TRUE)
xgbModel = xgb.train(xgPrm,xgTrain,nrounds=300,watchlist=list(train=xgTrain,test=xgValid))
ypredXgbTrain = predict(xgbModel,xgTrain)
rmseEval(yTrainXG,ypredXgbTrain)
ypredXgbValid = predict(xgbModel,xgValid)
rmseEval(yValidXG,ypredXgbValid)
ypredXgb = predict(xgbModel,xgTest)
submission$price = exp(ypredXgb)





rmseEval(yTrainXG,ypredXgbTrain)
rmseEval(yValidXG,ypredXgbValid)








predEval = as.data.frame(ypredXgbValid)
predEval$test = yValidXG
predEval$error = predEval$ypredXgbValid - predEval$test

big = ggplot(predEval, aes(x=test,y=ypredXgbValid)) +
  geom_point(aes(colour = error)) + scale_colour_gradient2()+
  expand_limits(x = 0, y = 0) +
  geom_abline(intercept = 0, slope = 1)+
  xlab('Price')+
  ylab('Prediction')+
  ggtitle('Prediction Accuracies')

subHigh = subset(predEval, abs(error) > 1.5)
small = ggplot(subHigh, aes(x=test,y=ypredXgbValid)) + 
  geom_point(aes(colour = error)) + scale_colour_gradient2()+
  expand_limits(x = 0, y = 0) +
  geom_abline(intercept = 0, slope = 1)+
  xlab('Price')+
  ylab('Prediction')+
  ggtitle('Prediction Outliers')
grid.arrange(big,small,ncol=2)






ntrain = nrow(train)
zeroPriceIN=funTime[1:ntrain,]
zeroPriceIN$price=train[,'price']

zeroPrice = zeroPriceIN %>%
  filter(price > 0) %>%
  group_by(brand_name,cat1) %>%
  summarize(sugPrice = median(price))

zeroPriceIN = zeroPriceIN %>%
  left_join(zeroPrice, by=c('brand_name','cat1')) %>%
  mutate(price = ifelse(price == 0,sugPrice,price)) %>%
  select(-sugPrice)

zeroPriceIN$price = as.numeric(zeroPriceIN$price)
train$price = zeroPriceIN$price







validation[] = lapply(validation,as.numeric)
correlations <- cor(validation)
corrPrice <- as.matrix(sort(correlations[,'price'], decreasing = TRUE))
corr.idx <- names(which(apply(corrPrice, 1, function(x) (x > 0.06 | x < -0.06))))
corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = 0.7,cl.cex = 0.7, number.cex=0.6)









names = names(trainXG)
importance_matrix <- xgb.importance(names, model = xgbModel)
options(repr.plot.width=8, repr.plot.height=8)
ggplot(importance_matrix,aes(x=reorder(Feature,Gain),y=Gain))+
  geom_bar(stat='identity',aes(fill = Gain > 0.003))+
  scale_fill_manual(values=c('red','grey'),guide=FALSE) +
  coord_flip()+
  xlab('Features')+
  ylab('Importance')+
  ggtitle('Feature Importance')





rmsePlot = data.frame(xgbModel$evaluation_log)
plot(rmsePlot$iter,rmsePlot$train_rmse,col="blue",ylim=c(0,2.5))
lines(rmsePlot$iter,rmsePlot$test_rmse,col="red")






#validationCR=funTime[1:ntrain,]
#validationCR$price=train[,'price']
#split=sample.split(validationCR,SplitRatio=0.1)
#trainX=subset(validationCR,split==TRUE)
#yTrain=trainX$price
#trainX = trainX %>%
#  select(-price)
#trainX[] = lapply(trainX,as.numeric)
#dTrain = xgb.DMatrix(as.matrix(trainX),label=yTrain)

#cvCtrl <- trainControl(method = "repeatedcv",number = 3,repeats = 2,allowParallel=T)
#xgbGrid <- expand.grid(nrounds=50,
#                       eta=c(0.08,0.09,0.1),
#                       max_depth=9,
#                       colsample_bytree=1,
#                       min_child_weight=1,
#                       subsample=0.4,
#                       gamma=0.2)
#set.seed(33)
#xgbTune <- train(as.matrix(trainX),yTrain,method="xgbTree",trControl=cvCtrl,
#          tuneGrid=xgbGrid,verbose=T,metric="RMSE")
#xgbTune
#plot(varImp(xgbTune))





write.csv(submission,'kernelSub.csv',row.names=FALSE)





