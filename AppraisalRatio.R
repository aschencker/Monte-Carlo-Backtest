library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(foreach)
library(parallel)
library(doParallel)


setwd("/home/siu854220350/Backtest")
load("Portfolios.rds")
load("RandomWeights.rds")
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

len <- nrow(Randweights[[1]])
plen <- length(Ports) 

AR_Max <- function(Ports,Randweights,S.Sheet,ER.Sheet,R.Sheet,W.Sheet,SM.Sheet){
  foreach(i=(1:plen)) %dopar% {
    Return_Storage <- matrix(0,107,1)
    for(j in 1:107){
      OBJ_storage <- matrix(0,nrow(Randweights[[j]]),1)
      Information <- as.ts(as.data.frame.matrix(R.Sheet[j:(j+35),c(Ports[[i]][j,])]))
      Returns <- as.data.frame(R.Sheet[(j+36),c(Ports[[i]][j,])])
      Bench <- as.matrix(I.Sheet[j:(j+35),1])
      for(k in 1:len){
        Port <- as.ts(rowSums(sweep(Information,2,Randweights[[j]][k,],"*")))
        OBJ_storage[k] <- (mean(Port)-((cov(Port,Bench)/var(Bench))*mean(Bench)))/(sqrt(((35/34)*(1-cor(Port,Bench)^2)))*sd(Port))
      }
      Return_Storage[j,1] <- sum(as.numeric(Randweights[[j]][match(max(OBJ_storage),OBJ_storage),])*as.numeric(as.character(t(Returns))))
    }
    return(Return_Storage)
  }
}

startt <- Sys.time()

AR <- matrix(unlist(AR_Max(Ports,Randweights,S.Sheet,ER.Sheet,R.Sheet,W.Sheet,SM.Sheet)),107,plen)

endd <- Sys.time()
endd-startt


setwd("/scratch/siu854220350/Backtest3")
save(AR,file = "AR.rds")

