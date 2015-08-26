library(caret)
my_r_squared <- function(actual_val,pred_val){
  mean_actual = mean(actual_val)
  sst = sum((actual_val-mean_actual)^2)
  ssr = sum((actual_val-pred_val)^2)  
  r_squared = 1- ssr/sst 
  return(r_squared)  
}



bill_of_materials <- read.csv('/tube-prediction/competition_data/bill_of_materials.csv')
tube <- read.csv('/tube-prediction/competition_data/tube.csv')
components <- read.csv('/tube-prediction/competition_data/components.csv',quote="")
tube_end_form <- read.csv('/tube-prediction/competition_data/tube_end_form.csv')
specs <- read.csv('/tube-prediction/competition_data/specs.csv')


train.set <- read.csv('/tube-prediction/competition_data/train_set.csv')
merge1.train <- merge(train.set,bill_of_materials)
merge2.train <- merge(merge1.train,components,by.x='component_id_1',by.y='component_id',all.x=TRUE)
merge3.train <- merge(merge2.train,components,by.x='component_id_2',by.y='component_id',all.x=TRUE)
merge4.train <- merge(merge3.train,tube,all.x=TRUE)
merge5.train <- merge(merge4.train,specs,all.x=TRUE)
merge6.train <- merge(merge5.train,tube_end_form, by.x='end_a',by.y='end_form_id',all.x=TRUE)
merged.data.train <- merge(merge6.train,tube_end_form, by.x='end_x',by.y='end_form_id',all.x=TRUE)

test.set <- read.csv('/tube-prediction/competition_data/test_set.csv')
merge1.test <- merge(test.set,bill_of_materials)
merge2.test <- merge(merge1.test,components,by.x='component_id_1',by.y='component_id',all.x=TRUE)
merge3.test <- merge(merge2.test,components,by.x='component_id_2',by.y='component_id',all.x=TRUE)
merge4.test <- merge(merge3.test,tube,all.x=TRUE)
merge5.test <- merge(merge4.test,specs,all.x=TRUE)
merge6.test <- merge(merge5.test,tube_end_form, by.x='end_a',by.y='end_form_id',all.x=TRUE)
merged.data.test <- merge(merge6.test,tube_end_form, by.x='end_x',by.y='end_form_id',all.x=TRUE)


merged_sets <- function(path) {
	path <- '/tube-prediction/competition_data/'
	fName <- read.csv((path + 'train_set.csv'))

	bill_of_materials <- read.csv(paste0(path,'bill_of_materials.csv')
	tube <- read.csv(paste0(path,'tube.csv'))
	tube_end_form <- read.csv(paste(path,'tube_end_form.csv'))
	specs <- read.csv(paste(path + 'specs.csv'))
}

# Function to clean training and test sets
drop_null <- function(df,cutoff){
	rows <- nrow(df)
	for (i in colnames(df)){
		if(sum(is.na(df[,i]))/rows>cutoff){
			df = df[,!colnames(df) %in% i]
		}
	}
	return(df)
}


factor_collapser <- function(factor_df,num_keep){
  for (i in names(factor_df)){
    factor_df[,i] = as.character(factor_df[,i])
    factor_df[is.na(factor_df[,i]),i] <- 'missing'
    factor_df[,i] = as.factor(factor_df[,i])
    freq_table = data.frame(table(factor_df[,i]))
    ordered_factors = freq_table[order(freq_table$Freq,decreasing=TRUE),]
    
    factor_df[,i] = as.character(factor_df[,i])
    
    #factor_df[is.na(merged.data.factors[,i]),i] <- 'missing'
    
    
    factor_df[!factor_df[,i] %in% ordered_factors[1:num_keep,'Var1'],i] = 'OTHER_val'
    
    factor_df[,i] = as.factor(factor_df[,i])
    
    
  }
  return(factor_df)
}


merged_nonnull <- drop_null(merged.data.train,0.8)
test_nonnull <- drop_null(merged.data.test,0.8)

col_types = lapply(merged_nonnull,class)
cat_df <- merged_nonnull[,col_types=="factor"]
cat_collapsed <- factor_collapser(cat_df,7)
num_df <- merged_nonnull[,!col_types=="factor"]
merged_nn <- cbind(num_df,cat_collapsed)

col_types = lapply(test_nonnull,class)
cat_test <- test_nonnull[,col_types=="factor"]
test_collapsed <- factor_collapser(cat_test,5)
num_test <- test_nonnull[,!col_types=="factor"]
test_nn <- cbind(num_test,test_collapsed)



#merged_nn= merged_nonnull[,!colnames(merged_nonnull) %in% varsToDrop]
nzv = nearZeroVar(merged_nn)
merged_nzv = merged_nn[,-nzv]
sapply(merged_nzv,nlevels)

test_nzv = test_nn[,-nzv]
sapply(test_nzv,nlevels)

#varsToDrop2 = c('tube_assembly_id','quote_date','component_id_5','component_id_6')
#merged_final= merged_nzv[,!colnames(merged_nzv) %in% varsToDrop2]


merged_final <- merged_nzv
test_final <- test_nzv
set.seed(346)

trainIndex = createDataPartition(merged_final$cost, p=0.8,list = FALSE,times = 1)

data.train= merged_final[trainIndex,]
data.test= merged_final[-trainIndex,]

grid_n <- expand.grid(size=(3:10),decay=seq(0.0,.15,.025))

grid <- expand.grid(interaction.depth=4,n.trees=300,shrinkage=.1)



fitControl2 = trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 5,
                           ## repeated ten times
                           repeats = 3)




gbmFit1 <- train(cost ~ ., data = data.train,
                 method = "gbm",
                 trControl = fitControl,
                 tuneGrid = grid,
                 verbose = TRUE
                 ## This last option is actually one
                 ## for gbm() that passes through
                 )

merged.test.final <- test_final

preds <- predict(gbmFit1,test_final)


fitControl2 = trainControl(method='repeatedcv',
							number=5,
							repeats=10)

gbmFit2 <- train(cost ~ ., data = data.train,
                 method = "gbm",
                 trControl = fitControl2,
                 verbose = TRUE
                 ## This last option is actually one
                 ## for gbm() that passes through
                 )

#nnetFit1 <- train(cost ~ ., data = data.train,
#                 method = "nnet",
#                 trControl = fitControl,
#                 tuneGrid = grid_n
#                 )
