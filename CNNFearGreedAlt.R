library(quantmod)
library(Quandl)

Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = "UTC")

#### Close data from TC2000 Convert TXT to XTS ####
CNNtxt <- read.table('CNN', header = FALSE, sep = ",")

CNNzoo <- read.zoo(CNNtxt, sep = ",", format = "%m/%d/%Y", split = 3)
CNNxts <- na.omit(as.xts(CNNzoo))

### Download additional Components to Create the CNN Fear Greed Index ###
getSymbols('BAMLH0A0HYM2', src  = 'FRED', return.xlass = 'xts', from = '1996-12-31', to = Sys.Date())
HYTSY <- na.omit(BAMLH0A0HYM2)

HYROC <- na.omit(ROC(HYTSY,1))
HY.Max <- max(HYROC)
HY.Min <- min(HYROC)

HYNorm <- ((HYROC-HY.Min)/(HY.Max-HY.Min))*-1

#### Higher Put/Call Ratio = Bullish for Markets 
PutCall <- Quandl("CBOE/EQUITY_PC", type = "xts", start_date = "2006-11-01")

PCRatio <- na.omit(EMA(PutCall[,4],5))
PC.Max <- max(PCRatio)
PC.Min <- min(PCRatio)

PCNorm <- (PCRatio-PC.Min)/(PC.Max-PC.Min)

#### Market Breadth ####
Breadth <- na.omit(ROC(CNNxts[,2],1))
BR.Max <- max(Breadth)
BR.Min <- min(Breadth)

BRNorm <- (Breadth-BR.Min)/(BR.Max-BR.Min)

### Safe Haven Demand ###
SP5.ret <- na.omit(SMA(dailyReturn(CNNxts[,1]),30))
TLT.ret <- na.omit(SMA(dailyReturn(CNNxts[,5]),30))

SHDemand <- SP5.ret - TLT.ret
SH.Max <- max(SHDemand)
SH.Min <- min(SHDemand)

SHNorm <- (SHDemand-SH.Min)/(SH.Max-SH.Min)

### Stock Price Momentum - SPX vs 125-day Mean ###
SP125Mu <- na.omit(CNNxts[,1] / runMean(CNNxts[,1], 125))
Mu.Max <- max(SP125Mu)
Mu.Min <- min(SP125Mu)

MuNorm <- (SP125Mu-Mu.Min)/(Mu.Max-Mu.Min)

### Stock Market Strenght - 52 Week High ###
WkNorm <- (CNNxts[,3]-min(CNNxts[,3]))/(max(CNNxts[,3]-min(CNNxts[,3])))

### Volatitlity - VIX ###
VIXNorm <- ((CNNxts[,6]-min(CNNxts[,6]))/(max(CNNxts[,6]-min(CNNxts[,6]))))*-1


CNN <- na.omit(cbind(BRNorm, WkNorm, HYNorm, VIXNorm, PCNorm, SHNorm, MuNorm))
names(CNN) <- c( "Breadth", "52WkHi", "HYTSY", "VIX", "Put.Call", "SafeHaven", "Momentum")


CNN.df <- as.data.frame(rowMeans(CNN))
CNNNorm <- (CNN.df-min(CNN.df))/(max(CNN.df)-min(CNN.df))
CNN.Index <- coredata(CNNNorm)
row.names(CNN.Index) <- index(CNN)

### Smooth Fear Greed Index ###
FearGreed <- as.xts(EMA(CNN.Index,4))
names(FearGreed) <- "FearGreed"

plot(FearGreed['2017:/'], main = "Fear Greed Alternative",
     sub = "Contrarian Indicator - > 50% = Greed = Bearish")

tail(FearGreed)
