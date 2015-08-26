test.set <- read.csv('/path/competition_data/test_set.csv')
merge1.test <- merge(test.set,bill_of_materials)
merge2.test <- merge(merge1.test,components,by.x='component_id_1',by.y='component_id',all.x=TRUE)
merge3.test <- merge(merge2.test,components,by.x='component_id_2',by.y='component_id',all.x=TRUE)

merge4.test <- merge(merge3.test,tube,all.x=TRUE)
merge5.test <- merge(merge4.test,specs,all.x=TRUE)

merge6.test <- merge(merge5.test,tube_end_form, by.x='end_a',by.y='end_form_id',all.x=TRUE)

merged.data.test <- merge(merge6.test,tube_end_form, by.x='end_x',by.y='end_form_id',all.x=TRUE)



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
