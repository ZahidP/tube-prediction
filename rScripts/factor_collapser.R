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

