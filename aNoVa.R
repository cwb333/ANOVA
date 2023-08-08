
aNoVa <- function (x, IV = NULL) {
  
  if (IV == 1) {
    master_aov <- anova(lm(as.numeric(x[,1])~x[,2], data = x))
   } else if (IV == 2) {
    master_aov <- anova(lm(as.numeric(x[,1])~x[,2] + x[,3], data = x))
   } else if (IV == 3) {
    master_aov <- anova(lm(as.numeric(x[,1])~x[,2] + x[,3] + x[,4], data = x))
  } else if (IV == 4) {
    master_aov <- anova(lm(as.numeric(x[,1])~x[,2] + x[,3] + x[,4] + x[,5], data = x))
  } else {
    master_aov <- anova(lm(as.numeric(x[,1])~x[,2] + x[,3] + x[,4] + x[,5] + x[,6], data = x))
  }

  master_aov_df <- data.frame(round(master_aov,3))
  
  for (i in 1:IV)
    if ((master_aov_df$Pr..F.[i] < 0.001) == T)
      master_aov_df$Pr..F.[i] = "< 0.001"
  
  for (i in 1:nrow(master_aov_df))
    for (j in 1:ncol(master_aov_df)) {
      if (is.na(master_aov_df[i, j]))
        master_aov_df[i, j] = c("")
      else
        master_aov_df[i, j] = master_aov_df[i,j]
      colnames(master_aov_df) <- c("DF", "Sum of Squares", "Mean Square", "F-value", "P-Value")
      rownames(master_aov_df) <- c(colnames(x[2:(IV+1)]), "Residuals")
      
      }
  
  return(master_aov_df)
}
