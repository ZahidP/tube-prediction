library(caret)

merged = read.csv("/Users/zahidpanjwani/Desktop/Code/Kaggle/tube-prediction/competition_data/merged.csv")
data2 = read.csv("/Users/zahidpanjwani/Desktop/Code/Kaggle/tube-prediction/competition_data/tube_end_form.csv")

merged = merge(merged,data2,by.x='end_a',by.y='end_form_id',all.x=TRUE)
merged = merge(merged,data2,by.x='end_x',by.y='end_form_id',all.x=TRUE)


drop_null <- function(df,cutoff){
	rows <- nrow(df)
	for (i in colnames(df)){
		if(sum(is.na(df[,i]))/rows>cutoff){
			df = df[,!colnames(df) %in% i]
		}
		
	}
	return(df)
}

merged_nonnull <- drop_null(merged,0.8)

varsToDrop = c('component_id_4','component_id_5','component_id_6','component_id_7','component_id_8')

merged_nn= merged_nonnull[,!colnames(merged_nonnull) %in% varsToDrop]


nzv = nearZeroVar(merged_nn)
merged_nzv = merged_nn[,-nzv]

sapply(merged_nzv,nlevels)

varsToDrop2 = c('tube_assembly_id','quote_date','component_id_5','component_id_6')

merged_final= merged_nzv[,!colnames(merged_nzv) %in% varsToDrop2]


set.seed(346)

trainIndex = createDataPartition(merged_final$cost, p=0.8,list = FALSE,times = 1)


data.train= merged_final[trainIndex,]
data.test= merged_final[-trainIndex,]






fitControl = trainControl(method='repeatedcv',
							number=3,
							repeats=2)
							
gbmFit1 <- train(cost ~ ., data = data.train,
                 method = "gbm",
                 trControl = fitControl
                 ## This last option is actually one
                 ## for gbm() that passes through
                 )











