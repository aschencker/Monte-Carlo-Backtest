library(PerformanceAnalytics) 
library(PortfolioAnalytics)

Number_of_Rand_Portfolios = 20000

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

Rand_weights <- function(nweights,SM.Sheet,W.Sheet,Info){
  Randweights <- list()
  Group_List <- list(groupA=c(1:5),groupB=c(6:7),groupC=c(8:10),groupD=c(11:18),groupE=c(19:22),groupF=c(23:28),
                     groupG=c(29:35),groupH=c(36:38),groupI=c(39),groupJ=c(40:42))
  x=1
  while(x<76){
    Min_weight <- as.numeric(SM.Sheet[x,-(39:42)])
    S_Weights <- round(W.Sheet[x,-9],4)
    portf <- portfolio.spec(c(1:42))
    portf <- add.constraint(portfolio=portf,type="weight_sum",min = .99, max=1.01) 
    portf <- add.constraint(portf, type="box", min=Min_weight, max=rep(.035,42))
    portf <- add.constraint(portf, type="group", groups = Group_List, group_min=S_Weights-.005,group_max=S_Weights+.005)
    random_Port <- random_portfolios(portf, permutations = nweights, rp_method = "sample",eliminate = TRUE)
    random_Port <- sweep(random_Port,1,rowSums(random_Port),"/")
    Randweights[[x]] <- random_Port
    portf=1
    x=x+1
  }
  
  Group_List1 <- list(groupA1=c(1:5),groupB1=c(6:7),groupC1=c(8:10),groupD1=c(11:18),groupE1=c(19:22),groupF1=c(23:28),
                      groupG1=c(29:35),groupH1=c(36:38),groupI1=c(39:42),groupJ1=c(43),groupK1=c(44:46))
  y=76
  while(y<108){
    Min_weight1 <- as.numeric(SM.Sheet[y,])
    S_Weights1 <- round(W.Sheet[y,],4)
    portf1 <- portfolio.spec(c(1:46))
    portf1 <- add.constraint(portfolio=portf1,type="weight_sum",min = .99, max=1.01) 
    portf1 <- add.constraint(portf1, type="box", min=Min_weight1, max=rep(.035,46))
    portf1 <- add.constraint(portf1, type="group", groups = Group_List1, group_min=S_Weights1-.005,group_max=S_Weights1+.005)
    random_Port1 <- random_portfolios(portf1, permutations = nweights, rp_method = "sample",eliminate = TRUE)
    random_Port1 <- sweep(random_Port1,1,rowSums(random_Port1),"/")
    Randweights[[y]] <- random_Port1
    portf1=1
    y=y+1
  }
  return(Randweights)
}


Randweights <- Rand_weights(Number_of_Rand_Weights_each_Period,SM.Sheet,W.Sheet)

setwd("/home/siu854220350/Backtest")
save(Randweights,file = "RandomWeights.rds")


