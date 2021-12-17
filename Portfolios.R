library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(foreach)
library(parallel)
library(doParallel)

Number_of_Rand_Portfolios = 1000
sellValue <- .4/((33*45+75*41)/108)

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

whichApply <- function(holding, holdings) {
  return(which(holding == holdings))
}

SectorHoldings <- function(Sector, size, sellValue, rlst) {
  columns <- which(N.Sheet[2,] == Sector)
  if(rlst == TRUE) {
    universe <- S.Sheet[76:108,columns]
    rows <- 33
  }else {
    universe <- S.Sheet[,columns]
    rows <- 108
  }
  randUnifMatrix <- replace(universe, universe == 1, runif(sum(universe),0,1))
  holdings <- as.matrix(cbind(rep(0, rows),rep(0, rows)))
  colnames(holdings) <- c("null1", "null2")
  i = 1
  while(!((length(unique(rowSums(holdings))) == 1) && (unique(rowSums(holdings)) == size))) {
    buyRow <- which(rowSums(holdings) < size)[1]
    buyColumn <- which(randUnifMatrix[buyRow,] == max(randUnifMatrix[buyRow,]))
    buy <- randUnifMatrix[, buyColumn]
    buy <- replace(buy, seq(1,rows) < buyRow, 1)
    buy <- cumprod(replace(buy, buy < sellValue, 0))
    buy <- as.matrix(replace(buy, buy > 0, 1))
    colnames(buy) <- names(randUnifMatrix)[buyColumn]
    buy <- replace(buy, seq(1,rows) < buyRow, 0)
    if(sum(buy) == 0) {
      buy[buyRow] <- 1
    }
    randUnifMatrix[,buyColumn] <- replace(randUnifMatrix[,buyColumn], buy == 1, 0)
    if(i == 1) {
      holdings <- buy
      i = i + 1
    } else {
      if(colnames(buy) %in% colnames(holdings)) {
        index <- which(colnames(buy) == colnames(holdings))
        holdings[, index] <- holdings[, index] + buy
      }else {
        holdings <- cbind(holdings, buy)
      }
    }
  }
  cols <- sapply(colnames(holdings), whichApply, colnames(randUnifMatrix))
  randUnifMatrix[,cols] <- holdings
  output <- replace(randUnifMatrix, randUnifMatrix < 1, 0)
  if(rlst == TRUE) {
    zeros <- matrix(0, 75, ncol(output))
    colnames(zeros) <- colnames(output)
    output <- rbind(zeros, output)
  }
  return(output)
}

#########################################################################################################

Random_SSIF_Port <- function(sellValue){
  
  cond <- SectorHoldings("S4COND", 5, sellValue, FALSE)
  cons <- SectorHoldings("S4CONS", 2, sellValue, FALSE)
  enrs <- SectorHoldings("S4ENRS", 3, sellValue, FALSE)
  finl <- SectorHoldings("S4FINL", 8, sellValue, FALSE)
  hlth <- SectorHoldings("S4HLTH", 4, sellValue, FALSE)
  indu <- SectorHoldings("S4INDU", 6, sellValue, FALSE)
  inft <- SectorHoldings("S4INFT", 7, sellValue, FALSE)
  matr <- SectorHoldings("S4MATR", 3, sellValue, FALSE)
  rlst <- SectorHoldings("S4RLST", 4, sellValue, TRUE)
  tels <- SectorHoldings("S4TELS", 1, sellValue, FALSE)
  util <- SectorHoldings("S4UTIL", 3, sellValue, FALSE)

  portfolio <- cbind(cond, cons, enrs, finl, hlth, indu, inft, matr, rlst, tels, util)
  
  return(portfolio)
}

#########################################################################################################


lapplyFunction <- function(values) {
  values <- values[values != 0]
  if(length(values) == 42) {
    values <- c(values, 0, 0, 0, 0)
  }
  return(values)
}

SSIF_Portfolio_Generator <- function(n,sellValue){
  foreach(i=1:n) %dopar% { 
    BinaryPortfolio <- Random_SSIF_Port(sellValue)
    intMatrix <- matrix(rep(1:709,each=108),nrow=108,ncol=709,byrow=FALSE)
    intPortfolio <- BinaryPortfolio * intMatrix
    PortfolioList <- apply(intPortfolio,1,unique)
    PortfolioList <- lapply(PortfolioList, lapplyFunction)
    PortfolioMatrix <- matrix(unlist(PortfolioList), 108, 46, byrow = TRUE)
    return(PortfolioMatrix)
  }
}

#########################################################################################################

Portfolios <- SSIF_Portfolio_Generator(Number_of_Rand_Portfolios,sellValue)

setwd("/Users/ari/Desktop/Projects/Monte-Carlo-Backtest/data")

save(Portfolios,file = "Portfolios.rds")

