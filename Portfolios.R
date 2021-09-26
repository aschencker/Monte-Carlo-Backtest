library(PortfolioAnalytics)
library(PerformanceAnalytics)


Number_of_Rand_Portfolios = 1000

setwd("/home/siu854220350/Backtest")
R.Sheet <- read.csv("Return.Sheet.csv",sep = ",")
S.Sheet <- read.csv("Sector.Sheet.csv")
W.Sheet <- read.csv("Weight.Sheet.csv",sep = ",")
ER.Sheet <- read.csv("Excess.Return.Sheet.csv",sep = ",")
I.Sheet <- read.csv("TR.Index.csv",sep = ",")
SM.Sheet <- read.csv("SM.Sheet.csv",sep = ",")
D.Sheet <- read.csv("Date.Sheet.csv",sep = ",")
N.Sheet <- read.table("Name.Sheet.csv",sep = ",", header = FALSE)
F.Sheet <- read.csv("Factor.Sheet.csv",sep = ",")


Random_SSIF_Port <- function(S.Sheet){
  
  ru_matrix <- replace(S.Sheet,S.Sheet==1,runif(sum(S.Sheet),0,1))
  COND <- ru_matrix[,1:118]
  CONS <- ru_matrix[,119:144]
  ENRS <- ru_matrix[,145:195]
  FINL <- ru_matrix[,196:324]
  HLTH <- ru_matrix[,325:398]
  INDU <- ru_matrix[,399:486]
  INFT <- ru_matrix[,487:594]
  MATR <- ru_matrix[,595:630]
  RLST <- ru_matrix[76:108,631:672]
  TELS <- ru_matrix[,673:685]
  UTIL <- ru_matrix[,686:709]
  Starting_Num <- c(5,2,3,8,4,6,7,3,4,1,3)
  Min_Value <- .4/((33*45+75*41)/108)
  
  #########################################################################################################
  
  COND1 <- min(sort(COND[1,],decreasing = TRUE)[1:Starting_Num[1]])
  COND2 <- replace(COND[1,],COND[1,]>=COND1,1)
  COND3 <- replace(COND2,COND2<1,0)
  COND4 <- rbind(COND3,COND[2:108,])
  COND5 <- COND4[,COND4[1,]==1]
  COND6 <- cumprod(replace(COND5,COND5<=Min_Value,0))
  COND_Holdings <- replace(COND6,COND6>0,1)
  COND_Holdings1 <- replace(COND6,COND6>0,1)
  COND_Choose <- COND4[, !(colnames(COND4) %in% colnames(COND_Holdings))]
  COND_SUM <- rowSums(COND_Holdings)
  COND_BUY_row <- colSums(COND_Holdings1)+1
  i=1
  
  while(i<=length(COND_BUY_row)){
    if (COND_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(COND_SUM))>1){
      COND_BUY1 <- COND_Choose[,COND_Choose[COND_BUY_row[i],]==max(COND_Choose[COND_BUY_row[i],]),drop =FALSE] 
      COND_BUY2 <- as.ts(c(rep(1,COND_BUY_row[i]-1),COND_BUY1[COND_BUY_row[i]:108,]))
      COND_BUY3 <- cumprod(replace(COND_BUY2,COND_BUY2<=Min_Value,0)) 
      COND_BUY4 <- replace(COND_BUY3,COND_BUY3==1,0) 
      COND_BUY5 <- as.data.frame(replace(COND_BUY4,COND_BUY4>0,1))
      names(COND_BUY5) <- names(COND_BUY1)
      COND_Holdings <- cbind(COND_Holdings,COND_BUY5) 
      COND_Choose <- COND_Choose[, !(colnames(COND_Choose) %in% colnames(COND_BUY1))]
      COND_SUM <- rowSums(as.ts(COND_Holdings)) 
      COND_BUY3.1 <- as.data.frame(replace(COND_BUY3,COND_BUY3>0,1)) 
      names(COND_BUY3.1) <- names(COND_BUY1)
      COND_Holdings1 <- cbind(COND_Holdings1,COND_BUY3.1)
      COND_BUY_row <- colSums(COND_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  COND[colnames(COND_Holdings)] <- COND_Holdings
  COND <-replace(COND,COND<1,0)
  
  #########################################################################################################
  
  CONS1 <- min(sort(CONS[1,],decreasing = TRUE)[1:Starting_Num[2]])
  CONS2 <- replace(CONS[1,],CONS[1,]>=CONS1,1)
  CONS3 <- replace(CONS2,CONS2<1,0)
  CONS4 <- rbind(CONS3,CONS[2:108,])
  CONS5 <- CONS4[,CONS4[1,]==1]
  CONS6 <- cumprod(replace(CONS5,CONS5<=Min_Value,0))
  CONS_Holdings <- replace(CONS6,CONS6>0,1)
  CONS_Holdings1 <- replace(CONS6,CONS6>0,1)
  CONS_Choose <- CONS4[, !(colnames(CONS4) %in% colnames(CONS_Holdings))]
  CONS_SUM <- rowSums(CONS_Holdings)
  CONS_BUY_row <- colSums(CONS_Holdings1)+1
  i=1
  
  while(i<=length(CONS_BUY_row)){
    if (CONS_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(CONS_SUM))>1){
      CONS_BUY1 <- CONS_Choose[,CONS_Choose[CONS_BUY_row[i],]==max(CONS_Choose[CONS_BUY_row[i],]),drop =FALSE] 
      CONS_BUY2 <- as.ts(c(rep(1,CONS_BUY_row[i]-1),CONS_BUY1[CONS_BUY_row[i]:108,]))
      CONS_BUY3 <- cumprod(replace(CONS_BUY2,CONS_BUY2<=Min_Value,0)) 
      CONS_BUY4 <- replace(CONS_BUY3,CONS_BUY3==1,0) 
      CONS_BUY5 <- as.data.frame(replace(CONS_BUY4,CONS_BUY4>0,1))
      names(CONS_BUY5) <- names(CONS_BUY1)
      CONS_Holdings <- cbind(CONS_Holdings,CONS_BUY5) 
      CONS_Choose <- CONS_Choose[, !(colnames(CONS_Choose) %in% colnames(CONS_BUY1))]
      CONS_SUM <- rowSums(as.ts(CONS_Holdings)) 
      CONS_BUY3.1 <- as.data.frame(replace(CONS_BUY3,CONS_BUY3>0,1)) 
      names(CONS_BUY3.1) <- names(CONS_BUY1)
      CONS_Holdings1 <- cbind(CONS_Holdings1,CONS_BUY3.1)
      CONS_BUY_row <- colSums(CONS_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  CONS[colnames(CONS_Holdings)] <- CONS_Holdings
  CONS <-replace(CONS,CONS<1,0)
  
  
  #########################################################################################################
  
  ENRS1 <- min(sort(ENRS[1,],decreasing = TRUE)[1:Starting_Num[3]])
  ENRS2 <- replace(ENRS[1,],ENRS[1,]>=ENRS1,1)
  ENRS3 <- replace(ENRS2,ENRS2<1,0)
  ENRS4 <- rbind(ENRS3,ENRS[2:108,])
  ENRS5 <- ENRS4[,ENRS4[1,]==1]
  ENRS6 <- cumprod(replace(ENRS5,ENRS5<=Min_Value,0))
  ENRS_Holdings <- replace(ENRS6,ENRS6>0,1)
  ENRS_Holdings1 <- replace(ENRS6,ENRS6>0,1)
  ENRS_Choose <- ENRS4[, !(colnames(ENRS4) %in% colnames(ENRS_Holdings))]
  ENRS_SUM <- rowSums(ENRS_Holdings)
  ENRS_BUY_row <- colSums(ENRS_Holdings1)+1
  i=1
  
  while(i<=length(ENRS_BUY_row)){
    if (ENRS_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(ENRS_SUM))>1){
      ENRS_BUY1 <- ENRS_Choose[,ENRS_Choose[ENRS_BUY_row[i],]==max(ENRS_Choose[ENRS_BUY_row[i],]),drop =FALSE] 
      ENRS_BUY2 <- as.ts(c(rep(1,ENRS_BUY_row[i]-1),ENRS_BUY1[ENRS_BUY_row[i]:108,]))
      ENRS_BUY3 <- cumprod(replace(ENRS_BUY2,ENRS_BUY2<=Min_Value,0)) 
      ENRS_BUY4 <- replace(ENRS_BUY3,ENRS_BUY3==1,0) 
      ENRS_BUY5 <- as.data.frame(replace(ENRS_BUY4,ENRS_BUY4>0,1))
      names(ENRS_BUY5) <- names(ENRS_BUY1)
      ENRS_Holdings <- cbind(ENRS_Holdings,ENRS_BUY5) 
      ENRS_Choose <- ENRS_Choose[, !(colnames(ENRS_Choose) %in% colnames(ENRS_BUY1))]
      ENRS_SUM <- rowSums(as.ts(ENRS_Holdings)) 
      ENRS_BUY3.1 <- as.data.frame(replace(ENRS_BUY3,ENRS_BUY3>0,1)) 
      names(ENRS_BUY3.1) <- names(ENRS_BUY1)
      ENRS_Holdings1 <- cbind(ENRS_Holdings1,ENRS_BUY3.1)
      ENRS_BUY_row <- colSums(ENRS_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  ENRS[colnames(ENRS_Holdings)] <- ENRS_Holdings
  ENRS <-replace(ENRS,ENRS<1,0)
  #########################################################################################################
  
  FINL1 <- min(sort(FINL[1,],decreasing = TRUE)[1:Starting_Num[4]])
  FINL2 <- replace(FINL[1,],FINL[1,]>=FINL1,1)
  FINL3 <- replace(FINL2,FINL2<1,0)
  FINL4 <- rbind(FINL3,FINL[2:108,])
  FINL5 <- FINL4[,FINL4[1,]==1]
  FINL6 <- cumprod(replace(FINL5,FINL5<=Min_Value,0))
  FINL_Holdings <- replace(FINL6,FINL6>0,1)
  FINL_Holdings1 <- replace(FINL6,FINL6>0,1)
  FINL_Choose <- FINL4[, !(colnames(FINL4) %in% colnames(FINL_Holdings))]
  FINL_SUM <- rowSums(FINL_Holdings)
  FINL_BUY_row <- colSums(FINL_Holdings1)+1
  i=1
  
  while(i<=length(FINL_BUY_row)){
    if (FINL_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(FINL_SUM))>1){
      FINL_BUY1 <- FINL_Choose[,FINL_Choose[FINL_BUY_row[i],]==max(FINL_Choose[FINL_BUY_row[i],]),drop =FALSE] 
      FINL_BUY2 <- as.ts(c(rep(1,FINL_BUY_row[i]-1),FINL_BUY1[FINL_BUY_row[i]:108,]))
      FINL_BUY3 <- cumprod(replace(FINL_BUY2,FINL_BUY2<=Min_Value,0)) 
      FINL_BUY4 <- replace(FINL_BUY3,FINL_BUY3==1,0) 
      FINL_BUY5 <- as.data.frame(replace(FINL_BUY4,FINL_BUY4>0,1))
      names(FINL_BUY5) <- names(FINL_BUY1)
      FINL_Holdings <- cbind(FINL_Holdings,FINL_BUY5) 
      FINL_Choose <- FINL_Choose[, !(colnames(FINL_Choose) %in% colnames(FINL_BUY1))]
      FINL_SUM <- rowSums(as.ts(FINL_Holdings)) 
      FINL_BUY3.1 <- as.data.frame(replace(FINL_BUY3,FINL_BUY3>0,1)) 
      names(FINL_BUY3.1) <- names(FINL_BUY1)
      FINL_Holdings1 <- cbind(FINL_Holdings1,FINL_BUY3.1)
      FINL_BUY_row <- colSums(FINL_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  FINL[colnames(FINL_Holdings)] <- FINL_Holdings
  FINL <-replace(FINL,FINL<1,0)
  
  #########################################################################################################
  
  HLTH1 <- min(sort(HLTH[1,],decreasing = TRUE)[1:Starting_Num[5]])
  HLTH2 <- replace(HLTH[1,],HLTH[1,]>=HLTH1,1)
  HLTH3 <- replace(HLTH2,HLTH2<1,0)
  HLTH4 <- rbind(HLTH3,HLTH[2:108,])
  HLTH5 <- HLTH4[,HLTH4[1,]==1]
  HLTH6 <- cumprod(replace(HLTH5,HLTH5<=Min_Value,0))
  HLTH_Holdings <- replace(HLTH6,HLTH6>0,1)
  HLTH_Holdings1 <- replace(HLTH6,HLTH6>0,1)
  HLTH_Choose <- HLTH4[, !(colnames(HLTH4) %in% colnames(HLTH_Holdings))]
  HLTH_SUM <- rowSums(HLTH_Holdings)
  HLTH_BUY_row <- colSums(HLTH_Holdings1)+1
  i=1
  
  while(i<=length(HLTH_BUY_row)){
    if (HLTH_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(HLTH_SUM))>1){
      HLTH_BUY1 <- HLTH_Choose[,HLTH_Choose[HLTH_BUY_row[i],]==max(HLTH_Choose[HLTH_BUY_row[i],]),drop =FALSE] 
      HLTH_BUY2 <- as.ts(c(rep(1,HLTH_BUY_row[i]-1),HLTH_BUY1[HLTH_BUY_row[i]:108,]))
      HLTH_BUY3 <- cumprod(replace(HLTH_BUY2,HLTH_BUY2<=Min_Value,0)) 
      HLTH_BUY4 <- replace(HLTH_BUY3,HLTH_BUY3==1,0) 
      HLTH_BUY5 <- as.data.frame(replace(HLTH_BUY4,HLTH_BUY4>0,1))
      names(HLTH_BUY5) <- names(HLTH_BUY1)
      HLTH_Holdings <- cbind(HLTH_Holdings,HLTH_BUY5) 
      HLTH_Choose <- HLTH_Choose[, !(colnames(HLTH_Choose) %in% colnames(HLTH_BUY1))]
      HLTH_SUM <- rowSums(as.ts(HLTH_Holdings)) 
      HLTH_BUY3.1 <- as.data.frame(replace(HLTH_BUY3,HLTH_BUY3>0,1)) 
      names(HLTH_BUY3.1) <- names(HLTH_BUY1)
      HLTH_Holdings1 <- cbind(HLTH_Holdings1,HLTH_BUY3.1)
      HLTH_BUY_row <- colSums(HLTH_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  HLTH[colnames(HLTH_Holdings)] <- HLTH_Holdings
  HLTH <-replace(HLTH,HLTH<1,0)
  
  #########################################################################################################
  
  INDU1 <- min(sort(INDU[1,],decreasing = TRUE)[1:Starting_Num[6]])
  INDU2 <- replace(INDU[1,],INDU[1,]>=INDU1,1)
  INDU3 <- replace(INDU2,INDU2<1,0)
  INDU4 <- rbind(INDU3,INDU[2:108,])
  INDU5 <- INDU4[,INDU4[1,]==1]
  INDU6 <- cumprod(replace(INDU5,INDU5<=Min_Value,0))
  INDU_Holdings <- replace(INDU6,INDU6>0,1)
  INDU_Holdings1 <- replace(INDU6,INDU6>0,1)
  INDU_Choose <- INDU4[, !(colnames(INDU4) %in% colnames(INDU_Holdings))]
  INDU_SUM <- rowSums(INDU_Holdings)
  INDU_BUY_row <- colSums(INDU_Holdings1)+1
  i=1
  
  while(i<=length(INDU_BUY_row)){
    if (INDU_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(INDU_SUM))>1){
      INDU_BUY1 <- INDU_Choose[,INDU_Choose[INDU_BUY_row[i],]==max(INDU_Choose[INDU_BUY_row[i],]),drop =FALSE] 
      INDU_BUY2 <- as.ts(c(rep(1,INDU_BUY_row[i]-1),INDU_BUY1[INDU_BUY_row[i]:108,]))
      INDU_BUY3 <- cumprod(replace(INDU_BUY2,INDU_BUY2<=Min_Value,0)) 
      INDU_BUY4 <- replace(INDU_BUY3,INDU_BUY3==1,0) 
      INDU_BUY5 <- as.data.frame(replace(INDU_BUY4,INDU_BUY4>0,1))
      names(INDU_BUY5) <- names(INDU_BUY1)
      INDU_Holdings <- cbind(INDU_Holdings,INDU_BUY5) 
      INDU_Choose <- INDU_Choose[, !(colnames(INDU_Choose) %in% colnames(INDU_BUY1))]
      INDU_SUM <- rowSums(as.ts(INDU_Holdings)) 
      INDU_BUY3.1 <- as.data.frame(replace(INDU_BUY3,INDU_BUY3>0,1)) 
      names(INDU_BUY3.1) <- names(INDU_BUY1)
      INDU_Holdings1 <- cbind(INDU_Holdings1,INDU_BUY3.1)
      INDU_BUY_row <- colSums(INDU_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  INDU[colnames(INDU_Holdings)] <- INDU_Holdings
  INDU <-replace(INDU,INDU<1,0)
  
  #########################################################################################################
  
  INFT1 <- min(sort(INFT[1,],decreasing = TRUE)[1:Starting_Num[7]])
  INFT2 <- replace(INFT[1,],INFT[1,]>=INFT1,1)
  INFT3 <- replace(INFT2,INFT2<1,0)
  INFT4 <- rbind(INFT3,INFT[2:108,])
  INFT5 <- INFT4[,INFT4[1,]==1]
  INFT6 <- cumprod(replace(INFT5,INFT5<=Min_Value,0))
  INFT_Holdings <- replace(INFT6,INFT6>0,1)
  INFT_Holdings1 <- replace(INFT6,INFT6>0,1)
  INFT_Choose <- INFT4[, !(colnames(INFT4) %in% colnames(INFT_Holdings))]
  INFT_SUM <- rowSums(INFT_Holdings)
  INFT_BUY_row <- colSums(INFT_Holdings1)+1
  i=1
  
  while(i<=length(INFT_BUY_row)){
    if (INFT_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(INFT_SUM))>1){
      INFT_BUY1 <- INFT_Choose[,INFT_Choose[INFT_BUY_row[i],]==max(INFT_Choose[INFT_BUY_row[i],]),drop =FALSE] 
      INFT_BUY2 <- as.ts(c(rep(1,INFT_BUY_row[i]-1),INFT_BUY1[INFT_BUY_row[i]:108,]))
      INFT_BUY3 <- cumprod(replace(INFT_BUY2,INFT_BUY2<=Min_Value,0)) 
      INFT_BUY4 <- replace(INFT_BUY3,INFT_BUY3==1,0) 
      INFT_BUY5 <- as.data.frame(replace(INFT_BUY4,INFT_BUY4>0,1))
      names(INFT_BUY5) <- names(INFT_BUY1)
      INFT_Holdings <- cbind(INFT_Holdings,INFT_BUY5) 
      INFT_Choose <- INFT_Choose[, !(colnames(INFT_Choose) %in% colnames(INFT_BUY1))]
      INFT_SUM <- rowSums(as.ts(INFT_Holdings)) 
      INFT_BUY3.1 <- as.data.frame(replace(INFT_BUY3,INFT_BUY3>0,1)) 
      names(INFT_BUY3.1) <- names(INFT_BUY1)
      INFT_Holdings1 <- cbind(INFT_Holdings1,INFT_BUY3.1)
      INFT_BUY_row <- colSums(INFT_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  INFT[colnames(INFT_Holdings)] <- INFT_Holdings
  INFT <-replace(INFT,INFT<1,0)
  
  
  #########################################################################################################
  
  MATR1 <- min(sort(MATR[1,],decreasing = TRUE)[1:Starting_Num[8]])
  MATR2 <- replace(MATR[1,],MATR[1,]>=MATR1,1)
  MATR3 <- replace(MATR2,MATR2<1,0)
  MATR4 <- rbind(MATR3,MATR[2:108,])
  MATR5 <- MATR4[,MATR4[1,]==1]
  MATR6 <- cumprod(replace(MATR5,MATR5<=Min_Value,0))
  MATR_Holdings <- replace(MATR6,MATR6>0,1)
  MATR_Holdings1 <- replace(MATR6,MATR6>0,1)
  MATR_Choose <- MATR4[, !(colnames(MATR4) %in% colnames(MATR_Holdings))]
  MATR_SUM <- rowSums(MATR_Holdings)
  MATR_BUY_row <- colSums(MATR_Holdings1)+1
  i=1
  
  while(i<=length(MATR_BUY_row)){
    if (MATR_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(MATR_SUM))>1){
      MATR_BUY1 <- MATR_Choose[,MATR_Choose[MATR_BUY_row[i],]==max(MATR_Choose[MATR_BUY_row[i],]),drop =FALSE] 
      MATR_BUY2 <- as.ts(c(rep(1,MATR_BUY_row[i]-1),MATR_BUY1[MATR_BUY_row[i]:108,]))
      MATR_BUY3 <- cumprod(replace(MATR_BUY2,MATR_BUY2<=Min_Value,0)) 
      MATR_BUY4 <- replace(MATR_BUY3,MATR_BUY3==1,0) 
      MATR_BUY5 <- as.data.frame(replace(MATR_BUY4,MATR_BUY4>0,1))
      names(MATR_BUY5) <- names(MATR_BUY1)
      MATR_Holdings <- cbind(MATR_Holdings,MATR_BUY5) 
      MATR_Choose <- MATR_Choose[, !(colnames(MATR_Choose) %in% colnames(MATR_BUY1))]
      MATR_SUM <- rowSums(as.ts(MATR_Holdings)) 
      MATR_BUY3.1 <- as.data.frame(replace(MATR_BUY3,MATR_BUY3>0,1)) 
      names(MATR_BUY3.1) <- names(MATR_BUY1)
      MATR_Holdings1 <- cbind(MATR_Holdings1,MATR_BUY3.1)
      MATR_BUY_row <- colSums(MATR_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  MATR[colnames(MATR_Holdings)] <- MATR_Holdings
  MATR <-replace(MATR,MATR<1,0)
  
  
  
  #########################################################################################################
  
  RLST1 <- min(sort(RLST[1,],decreasing = TRUE)[1:Starting_Num[9]])
  RLST2 <- replace(RLST[1,],RLST[1,]>=RLST1,1)
  RLST3 <- replace(RLST2,RLST2<1,0)
  RLST4 <- rbind(RLST3,RLST[2:33,])
  RLST5 <- RLST4[,RLST4[1,]==1]
  RLST6 <- cumprod(replace(RLST5,RLST5<=Min_Value,0))
  RLST_Holdings <- replace(RLST6,RLST6>0,1)
  RLST_Holdings1 <- replace(RLST6,RLST6>0,1)
  RLST_Choose <- RLST4[, !(colnames(RLST4) %in% colnames(RLST_Holdings))]
  RLST_SUM <- rowSums(RLST_Holdings)
  RLST_BUY_row <- colSums(RLST_Holdings1)+1
  i=1
  
  while(i<=length(RLST_BUY_row)){
    if (RLST_BUY_row[i]==34){
      i=i+1 
      next 
    }
    if(length(unique(RLST_SUM))>1){
      RLST_BUY1 <- RLST_Choose[,RLST_Choose[RLST_BUY_row[i],]==max(RLST_Choose[RLST_BUY_row[i],]),drop =FALSE] 
      RLST_BUY2 <- as.ts(c(rep(1,RLST_BUY_row[i]-1),RLST_BUY1[RLST_BUY_row[i]:33,]))
      RLST_BUY3 <- cumprod(replace(RLST_BUY2,RLST_BUY2<=Min_Value,0)) 
      RLST_BUY4 <- replace(RLST_BUY3,RLST_BUY3==1,0) 
      RLST_BUY5 <- as.data.frame(replace(RLST_BUY4,RLST_BUY4>0,1))
      names(RLST_BUY5) <- names(RLST_BUY1)
      RLST_Holdings <- cbind(RLST_Holdings,RLST_BUY5) 
      RLST_Choose <- RLST_Choose[, !(colnames(RLST_Choose) %in% colnames(RLST_BUY1))]
      RLST_SUM <- rowSums(as.ts(RLST_Holdings)) 
      RLST_BUY3.1 <- as.data.frame(replace(RLST_BUY3,RLST_BUY3>0,1)) 
      names(RLST_BUY3.1) <- names(RLST_BUY1)
      RLST_Holdings1 <- cbind(RLST_Holdings1,RLST_BUY3.1)
      RLST_BUY_row <- colSums(RLST_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  RLST[colnames(RLST_Holdings)] <- RLST_Holdings
  RLST <-replace(RLST,RLST<1,0)
  Matrix0 <- matrix(0,75,42)
  RLST1 <- rbind(as.ts(matrix(0,75,42)),as.ts(RLST))
  colnames(RLST1) <- c(names(RLST))
  RLST <- RLST1
  
  #########################################################################################################
  
  TELS1 <- min(sort(TELS[1,],decreasing = TRUE)[1])
  TELS2 <- replace(TELS[1,],TELS[1,]>=TELS1,1)
  TELS3 <- replace(TELS2,TELS2<1,0)
  TELS4 <- rbind(TELS3,TELS[2:108,])
  TELS5 <- as.matrix(TELS4[,TELS4[1,]==1,drop =FALSE])
  TELS_Holdings <- replace(TELS5,TELS5>0,1)
  TELS_Holdings1 <- replace(TELS5,TELS5>0,1)
  TELS_Choose <- TELS4[, !(colnames(TELS4) %in% colnames(TELS_Holdings))]
  TELS_SUM <- rowSums(TELS_Holdings)
  TELS_BUY_row <- colSums(TELS_Holdings1)+1
  i=1
  
  while(i<=length(TELS_BUY_row)){
    if (TELS_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(TELS_SUM))>1){
      TELS_BUY1 <- TELS_Choose[,TELS_Choose[TELS_BUY_row[i],]==max(TELS_Choose[TELS_BUY_row[i],]),drop =FALSE] 
      TELS_BUY2 <- as.ts(c(rep(1,TELS_BUY_row[i]-1),TELS_BUY1[TELS_BUY_row[i]:108,]))
      TELS_BUY4 <- replace(TELS_BUY2,TELS_BUY2==1,0) 
      TELS_BUY5 <- as.data.frame(replace(TELS_BUY4,TELS_BUY4>0,1))
      names(TELS_BUY5) <- names(TELS_BUY1)
      TELS_Holdings <- cbind(TELS_Holdings,TELS_BUY5) 
      TELS_Choose <- TELS_Choose[, !(colnames(TELS_Choose) %in% colnames(TELS_BUY1))]
      TELS_SUM <- rowSums(as.ts(TELS_Holdings)) 
      TELS_BUY3.1 <- as.data.frame(replace(TELS_BUY2,TELS_BUY2>0,1)) 
      names(TELS_BUY3.1) <- names(TELS_BUY1)
      TELS_Holdings1 <- cbind(TELS_Holdings1,TELS_BUY3.1)
      TELS_BUY_row <- colSums(TELS_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  TELS[colnames(TELS_Holdings)] <- TELS_Holdings
  TELS <-replace(TELS,TELS<1,0)
  
  #########################################################################################################
  
  UTIL1 <- min(sort(UTIL[1,],decreasing = TRUE)[1:Starting_Num[11]])
  UTIL2 <- replace(UTIL[1,],UTIL[1,]>=UTIL1,1)
  UTIL3 <- replace(UTIL2,UTIL2<1,0)
  UTIL4 <- rbind(UTIL3,UTIL[2:108,])
  UTIL5 <- UTIL4[,UTIL4[1,]==1]
  UTIL6 <- cumprod(replace(UTIL5,UTIL5<=Min_Value,0))
  UTIL_Holdings <- replace(UTIL6,UTIL6>0,1)
  UTIL_Holdings1 <- replace(UTIL6,UTIL6>0,1)
  UTIL_Choose <- UTIL4[, !(colnames(UTIL4) %in% colnames(UTIL_Holdings))]
  UTIL_SUM <- rowSums(UTIL_Holdings)
  UTIL_BUY_row <- colSums(UTIL_Holdings1)+1
  i=1
  
  while(i<=length(UTIL_BUY_row)){
    if (UTIL_BUY_row[i]==109){
      i=i+1 
      next 
    }
    if(length(unique(UTIL_SUM))>1){
      UTIL_BUY1 <- UTIL_Choose[,UTIL_Choose[UTIL_BUY_row[i],]==max(UTIL_Choose[UTIL_BUY_row[i],]),drop =FALSE] 
      UTIL_BUY2 <- as.ts(c(rep(1,UTIL_BUY_row[i]-1),UTIL_BUY1[UTIL_BUY_row[i]:108,]))
      UTIL_BUY3 <- cumprod(replace(UTIL_BUY2,UTIL_BUY2<=Min_Value,0)) 
      UTIL_BUY4 <- replace(UTIL_BUY3,UTIL_BUY3==1,0) 
      UTIL_BUY5 <- as.data.frame(replace(UTIL_BUY4,UTIL_BUY4>0,1))
      names(UTIL_BUY5) <- names(UTIL_BUY1)
      UTIL_Holdings <- cbind(UTIL_Holdings,UTIL_BUY5) 
      UTIL_Choose <- UTIL_Choose[, !(colnames(UTIL_Choose) %in% colnames(UTIL_BUY1))]
      UTIL_SUM <- rowSums(as.ts(UTIL_Holdings)) 
      UTIL_BUY3.1 <- as.data.frame(replace(UTIL_BUY3,UTIL_BUY3>0,1)) 
      names(UTIL_BUY3.1) <- names(UTIL_BUY1)
      UTIL_Holdings1 <- cbind(UTIL_Holdings1,UTIL_BUY3.1)
      UTIL_BUY_row <- colSums(UTIL_Holdings1)+1
      i=i+1
    }
    else{
      break
    }
  }
  
  UTIL[colnames(UTIL_Holdings)] <- UTIL_Holdings
  UTIL <-replace(UTIL,UTIL<1,0)
  
  #########################################################################################################
  
  Portfolio <- cbind(COND,CONS,ENRS,FINL,HLTH,INDU,INFT,MATR,RLST,TELS,UTIL)
  return(Portfolio)
}

#########################################################################################################
SSIF_Portfolio_Generator <- function(n,S.Sheet){
  i=1
  Port_List <- list()
  while(i<=n){
    Port <- Random_SSIF_Port(S.Sheet)
    Port_col <- Port*matrix(rep(1:709,each=108),nrow=108,ncol=709,byrow=FALSE)
    a <- unlist(apply(Port_col,1,unique, simplify = "matrix"))
    a <- as.matrix(a[!(a %in% 0)])
    b <- a[1:(75*42)]
    c <- a[3151:4668]
    d <- matrix(b,75,42, byrow =TRUE)
    d <- cbind(d,matrix(0,75,4))
    e <- matrix(c,33,46, byrow =TRUE)
    Col_Selection <- rbind(d,e)
    Port_List[[i]] <- Col_Selection
    i=i+1
  }
  return(Port_List)
}

#########################################################################################################

Ports <- SSIF_Portfolio_Generator(Number_of_Rand_Portfolios,S.Sheet)

setwd("/home/siu854220350/Backtest")
save(Ports,file = "Portfolios.rds")



