library(PerformanceAnalytics) 
library(PortfolioAnalytics)
library(foreach)
library(parallel)
library(doParallel)

numCores <- detectCores()
registerDoParallel(cores = numCores-1)

randomWeightsCount = 20000

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

weights <- function(nWeights){
  RandWeights <- list()
  GroupListNoRlst <- list(groupA=c(1:5),groupB=c(6:7),groupC=c(8:10),
                          groupD=c(11:18),groupE=c(19:22),groupF=c(23:28),
                     groupG=c(29:35),groupH=c(36:38),groupI=c(39),groupJ=c(40:42))
  GroupListRlst <- list(groupA1=c(1:5),groupB1=c(6:7),groupC1=c(8:10),
                        groupD1=c(11:18),groupE1=c(19:22),groupF1=c(23:28),
                      groupG1=c(29:35),groupH1=c(36:38),groupI1=c(39:42),
                      groupJ1=c(43),groupK1=c(44:46))
  
  foreach(i=1:107) %dopar% {
    if(i <= 75) {
      MinWeights <- as.numeric(SM.Sheet[i,-(39:42)])
      SectorWeights <- round(W.Sheet[i,-9],4)
      portf <- portfolio.spec(c(1:42))
      portf <- add.constraint(portf, type="box", min=MinWeights, 
                              max=rep(.035,42))
      portf <- add.constraint(portf, type="group", groups = GroupListNoRlst, 
                              group_min=SectorWeights-.005,
                              group_max=SectorWeights+.005)
    } else {
      MinWeightsRlst <- as.numeric(SM.Sheet[i,])
      SectorWeightsRlst <- round(W.Sheet[i,],4)
      portf <- portfolio.spec(c(1:46))
      portf <- add.constraint(portf, type="box", min=MinWeightsRlst, 
                              max=rep(.035,46))
      portf <- add.constraint(portf, type="group", groups = GroupListRlst, 
                              group_min=SectorWeightsRlst-.005,
                              group_max=SectorWeightsRlst+.005)
      
    }
    portf <- add.constraint(portfolio=portf,type="weight_sum",
                            min = .99, max=1.01) 
    randomPortfolio <- random_portfolios(portf, permutations = nWeights, 
                                     rp_method = "sample",eliminate = TRUE)
    portf=1
    weights <- sweep(randomPortfolio,1,rowSums(randomPortfolio),"/")
    return(weights)
  }
}

RandomWeights <- weights(randomWeightsCount+2)

setwd("/Users/ari/Desktop/Projects/Monte-Carlo-Backtest/data")
save(RandomWeights,file = "RandomWeights.rds")