
tUkEy <- function (x, IV = NULL) {
  
  require(agricolae)
  
  master <- x
  
  num <- 1:ncol(master)
  letters <- rep("V", ncol(master))
  colnames(master) <- paste0(letters, num)
  
  if (IV == 1) {
    master_aov <- aov(V1~V2, data = master)
    master_tukey <- HSD.test(master_aov, "V2")
    master_tukey_df <- data.frame(master_tukey$groups)
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    master_tukey_df <- master_tukey_df[ order(row.names(master_tukey_df)), ]
    master_tukey_df <- data.frame(rownames(master_tukey_df), master_tukey_df[,1:2], row.names = NULL)
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    colnames(master_tukey_df) <- c("Independent Variable", "Mean", "")
    print(master_tukey_df)
  } else if (IV == 2) {
    master_aov <- aov(V1~V2 + V3, data = master)
    master_tukey <- HSD.test(master_aov, "V2")
    master_tukey_1 <- HSD.test(master_aov, "V3")
    master_tukey_df <- data.frame(rbind(master_tukey$groups, master_tukey_1$groups))
    master_tukey_df <- data.frame(rownames(master_tukey_df), master_tukey_df[,1:2], row.names = NULL)
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    colnames(master_tukey_df) <- c("Independent Variable", "Mean", "")
    print(master_tukey_df)
    } else if (IV == 3) {
    master_aov <- aov(V1~V2 + V3 + V4, data = master)
    master_tukey <- HSD.test(master_aov, "V2")
    master_tukey_1 <- HSD.test(master_aov, "V3")
    master_tukey_2 <- HSD.test(master_aov, "V4")
    master_tukey_df <- data.frame(rbind(master_tukey$groups, master_tukey_1$groups, 
                                        master_tukey_2$groups))
    master_tukey_df <- data.frame(rownames(master_tukey_df), master_tukey_df[,1:2], row.names = NULL)
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    colnames(master_tukey_df) <- c("Independent Variable", "Mean", "")
    print(master_tukey_df)
  } else if (IV == 4) {
    master_aov <- aov(V1~V2 + V3 + V4 + V5, data = master)
    master_tukey <- HSD.test(master_aov, "V2")
    master_tukey_1 <- HSD.test(master_aov, "V3")
    master_tukey_2 <- HSD.test(master_aov, "V4")
    master_tukey_3 <- HSD.test(master_aov, "V5")
    master_tukey_df <- data.frame(rbind(master_tukey$groups, master_tukey_1$groups, 
                                        master_tukey_2$groups, master_tukey_3$groups))
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    master_tukey_df <- data.frame(rownames(master_tukey_df), master_tukey_df[,1:2], row.names = NULL)
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    colnames(master_tukey_df) <- c("Independent Variable", "Mean", "")
    print(master_tukey_df)
  } else {
    master_aov <- aov(V1~V2 + V3 + V4 + V5 + V6, data = master)
    master_tukey <- HSD.test(master_aov, "V2")
    master_tukey_1 <- HSD.test(master_aov, "V3")
    master_tukey_2 <- HSD.test(master_aov, "V4")
    master_tukey_3 <- HSD.test(master_aov, "V5")
    master_tukey_4 <- HSD.test(master_aov, "V6")
    master_tukey_df <- data.frame(rbind(master_tukey$groups, master_tukey_1$groups, 
                                        master_tukey_2$groups, master_tukey_3$groups,
                                        master_tukey_4$groups))
    master_tukey_df <- data.frame(rownames(master_tukey_df), master_tukey_df[,1:2], row.names = NULL)
    master_tukey_df$V1 <- round(master_tukey_df$V1, 3)
    colnames(master_tukey_df) <- c("Independent Variable", "Mean", "")
    print(master_tukey_df)
    }
}
