library(caret)
library(lubridate)

path <- 'your_path'

train.set <- read.csv(paste0(path,'/competition_data/train_set.csv'))
bill_of_materials <- read.csv(paste0(path,'/competition_data/bill_of_materials.csv'))
tube <- read.csv(paste0(path,'/competition_data/tube.csv'))
components <- read.csv(paste0(path,'/competition_data/components.csv',quote=""))
tube_end_form <- read.csv(paste0(path,'/competition_data/tube_end_form.csv'))
specs <- read.csv(paste0(path,'/competition_data/specs.csv'))

merge1 <- merge(train.set,bill_of_materials)
merge1 <- merge(merge1,components,by.x='component_id_1',by.y='component_id',all.x=TRUE)
merge1 <- merge(merge1,components,by.x='component_id_2',by.y='component_id',all.x=TRUE)
merge1 <- merge(merge1,tube,all.x=TRUE)
merge1 <- merge(merge1,specs,all.x=TRUE)
merge1 <- merge(merge1,tube_end_form, by.x='end_a',by.y='end_form_id',all.x=TRUE)
merged.data <- merge(merge1,tube_end_form, by.x='end_x',by.y='end_form_id',all.x=TRUE)


#check if variables are the right class
sapply(merged.data,class)
#check how many levels factor variables have
sapply(merged.data,nlevels)

#remove factor variables with too many factors (like component_ids)
varsToRemove <- c('tube_assembly_id')

merged.data <- merged.data[,!colnames(merged.data) %in% varsToRemove]

merged.data$quote_date <- as.Date(merged.data$quote_date)
merged.data$month <- month(merged.data$quote_date)
merged.data$year <- year(merged.data$quote_date)

merged.data$month_bins <- 0
merged.data[merged.data$month %in% c(1,2),'month_bins'] <-1
merged.data[merged.data$month %in% c(3,4),'month_bins'] <-2
merged.data[merged.data$month %in% c(5,6),'month_bins'] <-3
merged.data[merged.data$month %in% c(7,8),'month_bins'] <-4
merged.data[merged.data$month %in% c(9,10),'month_bins'] <-5
merged.data[merged.data$month %in% c(11,12),'month_bins'] <-6

merged.data$month < NULL
merged.data$quote_date < NULL
merged.data$month_bins<-as.factor(merged.data$month_bins)
merged.data$year <-as.factor(merged.data$year)
subset2009 <- subset(merged.data,quote_date > '2010-1-1')

#remove nzv
nzv <- nearZeroVar(merged.data)
merged.data.nzv <- merged.data[,-nzv]

#seperate data frame into numeric and non_numeric
numerics <- sapply(merged.data.nzv, is.numeric)
merged.data.numeric <- merged.data.nzv[,numerics]
merged.data.factors <- merged.data.nzv[,!numerics]

#missing value replacement (remove columns that are mostly empty and median impute the rest)
merged.data.numeric <- removeNACols(merged.data.numeric,.9)
merged.data.factors <- data.frame(removeNACols(merged.data.factors,.9),stringsAsFactors=FALSE)

medianImp <- preProcess(merged.data.numeric,
                        method='medianImpute')
merged.data.numeric.imp <- predict(medianImp,merged.data.numeric)
merged.data.factors.collapsed <- factor_collapser(merged.data.factors,7)

#remove highly correlated
cor_matrix <- cor(merged.data.numeric.imp)
highlyCor <- findCorrelation(cor_matrix,cutoff=0.75)
merged.data.numeric.cor <- merged.data.numeric.imp[,-highlyCor]
merged.final <- cbind(merged.data.numeric.cor,merged.data.factors.collapsed)



fitControl = trainControl(method='repeatedcv',               
                          number=5,                       
                          repeats=5)
fitNoControl <- trainControl(method='none')

# tuning grid 1
tuneGrid1 = expand.grid(interaction.depth = c(1,2,3,4)
                       			,n.trees = c(150,200,250,300)
                       			,shrinkage = c(0.1,0.05,0.01)
                       			,n.minobsinnode=10)
                       			
# tuning gride extreme (0)                       			
tuneGridExtreme = expand.grid(nrounds = c(1000,2000,3000,4000)
                       			,lambda = c(0,0.01,0.05,0.1)
                       			,alpha = c(0,0.01,0.05,0.1))
                       			
# tuning gride extreme (1)                       			
tuneGridXNoControl1 = expand.grid(nrounds = 4000
                        		,eta = 0.02
                         		,max_depth=8)
                         		
# tuning gride extreme (2)
tuneGridXNoControl2 = expand.grid(nrounds = 2000
                                ,eta = 0.02
                                ,max_depth=8)
                                        
# tuning gride extreme (3)
tuneGridXNoControl3 = expand.grid(nrounds = 3000
                                ,eta = 0.02
                                ,max_depth=8)

# fitted GBM (boosted model)
gbmFit1 <- train(cost ~ ., data = merged.final,           
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose=TRUE,
                 tuneGrid=tuneGrid1)

# fitted random forest
rfFit2 <- train(cost~.,data=merged.final
                 ,method = 'rf'
                 ,trControl = fitNoControl
                 ,tuneGrid = trainControl#expand.grid(mtry=20)
                )

dataPartition = createFolds(merged.final$cost,k=4)
merged.final1 <- merged.final[dataPartition[[1]],]
merged.final2 <- merged.final[dataPartition[[2]],]
merged.final3 <- merged.final[dataPartition[[3]],]
merged.final4 <- merged.final[dataPartition[[4]],]


xgBoost1 <- train(log(cost+1)~.,data=merged.final
                ,method = 'xgbTree'
                ,trControl = fitNoControl
                ,tuneGrid = tuneGridExtremeNoControl#expand.grid(nrounds=200,lambda=0.01,alpha=0.01)
                ,verbose=FALSE)

xgBoost2 <- train(cost^(1/16)~.,data=merged.final
                  ,method = 'xgbTree'
                  ,trControl = fitNoControl
                  ,tuneGrid = tuneGridExtremeNoControl2#expand.grid(nrounds=200,lambda=0.01,alpha=0.01)
                  ,verbose=FALSE)

rf3 <- train(log(cost+1)~.,data=merged.final
                  ,method = 'rf'
                  ,trControl = fitNoControl
                  ,tuneGrid = expand.grid(mtry=20) #expand.grid(nrounds=200,lambda=0.01,alpha=0.01)
                  #,verbose=FALSE)

xgBoost4 <- train(cost~.,data=merged.final
                  ,method = 'xgbTree'
                  ,trControl = fitNoControl
                  ,tuneGrid = tuneGridExtremeNoControl3#expand.grid(nrounds=200,lambda=0.01,alpha=0.01)
                  ,verbose=FALSE)



train_pred1 <- predict(xgBoost1,merged.final,na.action=na.pass)
train_pred2 <- predict(xgBoost2,merged.final,na.action=na.pass)
train_pred3 <- predict(rf3,merged.final,na.action=na.pass)
train_pred4 <- predict(xgBoost4,merged.final,na.action=na.pass)

merged.final$train_pred1 = train_pred1
merged.final$train_pred2 = train_pred2
merged.final$train_pred3 = train_pred3
merged.final$train_pred4 = train_pred4

finalModel <- train(cost~train_pred1+train_pred2+train_pred4,data=merged.final
                  ,method = 'gam'
                  ,trControl = fitControl
                  #,tuneGrid = tuneGridExtremeNoControl3#expand.grid(nrounds=200,lambda=0.01,alpha=0.01)
                  #,verbose=FALSE)



#### PREPARE THE TEST SET
##
##
##

test.set <- read.csv('/Users/kaangunaydin/Downloads/competition_data/test_set.csv')
merge1.test <- merge(test.set,bill_of_materials)
merge1.test <- merge(merge1.test,components,by.x='component_id_1',by.y='component_id',all.x=TRUE)
merge1.test <- merge(merge1.test,components,by.x='component_id_2',by.y='component_id',all.x=TRUE)
merge1.test <- merge(merge1.test,tube,all.x=TRUE)
merge1.test <- merge(merge1.test,specs,all.x=TRUE)
merge1.test <- merge(merge1.test,tube_end_form, by.x='end_a',by.y='end_form_id',all.x=TRUE)

merged.data.test <- merge(merge1.test,tube_end_form, by.x='end_x',by.y='end_form_id',all.x=TRUE)
merged.data.test$quote_date <- as.Date(merged.data.test$quote_date)
merged.data.test$month <- month(merged.data.test$quote_date)
merged.data.test$year <- year(merged.data.test$quote_date)
merged.data.test$month_bins <- 0

merged.data.test[merged.data.test$month %in% c(1,2),'month_bins'] <-1
merged.data.test[merged.data.test$month %in% c(3,4),'month_bins'] <-2
merged.data.test[merged.data.test$month %in% c(5,6),'month_bins'] <-3
merged.data.test[merged.data.test$month %in% c(7,8),'month_bins'] <-4
merged.data.test[merged.data.test$month %in% c(9,10),'month_bins'] <-5
merged.data.test[merged.data.test$month %in% c(11,12),'month_bins'] <-6

merged.data.test$month < NULL
merged.data.test$month_bins<-as.factor(merged.data.test$month_bins)
merged.data.test$year <-as.factor(merged.data.test$year)

numerics.test <- sapply(merged.data.test, is.numeric)
merged.data.numeric.test <- merged.data.test[,numerics.test]
merged.data.factors.test <- merged.data.test[,!numerics.test]

#missing value replacement (remove columns that are mostly empty and median impute the rest)
merged.data.numeric.test <- removeNACols(merged.data.numeric.test,.9)
merged.data.factors.test <- data.frame(removeNACols(merged.data.factors.test,.9),stringsAsFactors=FALSE)
medianImp.test <- preProcess(merged.data.numeric.test,
                        method='medianImpute')
merged.data.numeric.imp.test <- predict(medianImp.test,merged.data.numeric.test)
merged.data.factors.collapsed.test <- factor_collapser(merged.data.factors.test,7)
merged.final.test <- cbind(merged.data.numeric.imp.test,merged.data.factors.collapsed.test)

##### PREDICT ON THE TEST SET
##
##
##

merged.final.test = merged.final.test[,colnames(merged.final.test) %in% colnames(merged.final)]

# drop new levels here
table(merged.final.test$year)
merged.final.test[merged.final.test$component_id_3=='C-1637','component_id_3'] = 'missing'
merged.final.test[merged.final.test$spec1=='SP-0026','spec1'] = 'missing'
merged.final.test[merged.final.test$spec4=='SP-0069','spec4'] = 'missing'
merged.final.test[merged.final.test$year=='2007','year'] = 'OTHER_val'

preds1 = exp(predict(xgBoost1,merged.final.test,na.action=na.pass
                ))-1
        
pred1 <- predict(xgBoost1,merged.final.test,na.action=na.pass)
pred2 <- predict(xgBoost2,merged.final.test,na.action=na.pass)
pred3 <- predict(rf3,merged.final.test,na.action=na.pass)
pred4 <- predict(xgBoost4,merged.final.test,na.action=na.pass)

#these columns are only named train_pred because i am lazy  
merged.final.test$train_pred1 = pred1
merged.final.test$train_pred2 = pred2
merged.final.test$train_pred3 = pred3
merged.final.test$train_pred4 = pred4

preds <- predict(finalModel,merged.final.test)

preds[preds<0]=0

preds_train = predict(rfFit2,merged.final,na.action=na.pass)
my_r_squared(merged.final$cost,preds_train)

merged.final$preds = preds_train

submitDb = data.frame(id=merged.final.test$id,cost=preds)
submitDb2 = aggregate(data.frame(cost = submitDb$cost), by = list(id = submitDb$id), mean)

write.csv(submitDb, "submit7.csv", row.names = FALSE, quote = FALSE)

