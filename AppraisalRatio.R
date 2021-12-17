library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(foreach)
library(parallel)
library(doParallel)


setwd("/Users/ari/Desktop/Projects/Monte-Carlo-Backtest/data")
load("Portfolios.rds")
load("RandomWeights.rds")
setwd("/Users/ari/Desktop/Projects/Monte-Carlo-Backtest/csv")
R.Sheet <- read.csv("Return.Sheet.csv",sep = ",")
S.Sheet <- read.csv("Sector.Sheet.csv")
W.Sheet <- read.csv("Weight.Sheet.csv",sep = ",")
ER.Sheet <- read.csv("Excess.Return.Sheet.csv",sep = ",")
I.Sheet <- read.csv("TR.Index.csv",sep = ",")
SM.Sheet <- read.csv("SM.Sheet.csv",sep = ",")
D.Sheet <- read.csv("Date.Sheet.csv",sep = ",")
N.Sheet <- read.table("Name.Sheet.csv",sep = ",", header = FALSE)
F.Sheet <- read.csv("Factor.Sheet.csv",sep = ",")

numCores <- detectCores()
registerDoParallel(cores = numCores-1)

len <- nrow(RandomWeights[[1]])
plen <- length(Portfolios) 

AR_MAX <- function(Portfolios,RandomWeights){
  foreach(i=(1:plen), .combine = "cbind") %dopar% {
    Return_Storage <- NULL
    for(j in 1:107){
      obj <- -1000
      indx <- 0
      Information <- matrix(as.numeric(unlist(R.Sheet[j:(j+35),c(Portfolios[[i]][j,])])),36)
      Returns <- as.numeric(unlist(R.Sheet[(j + 36),c(Portfolios[[i]][j,])]))
      Bench <- as.vector(I.Sheet[j:(j+35),1])
      for(k in 1:len){
        Port <- Information %*% as.numeric(RandomWeights[[j]][k,])
        beta <- as.numeric(cov(Port,Bench) / var(Bench))
        alpha <- mean(Port) - (beta * mean(Bench))
        epsilon <- Port - (alpha + (beta * Bench))
        trackingError <- sqrt(sum(epsilon^2) / (length(epsilon) - 2))
        ar <- alpha / trackingError
        if(ar > obj) {
          obj <- ar
          indx <- k
        } 
      }
      ossReturn <- Returns %*% as.numeric(RandomWeights[[j]][indx,])
      Return_Storage <- c(Return_Storage, ossReturn)
    }
    return(Return_Storage)
  }
}

AR <- AR_MAX(Portfolios,RandomWeights)

setwd("/Users/ari/Desktop/Projects/Monte-Carlo-Backtest/data")
save(AR,file = "AR.rds")


