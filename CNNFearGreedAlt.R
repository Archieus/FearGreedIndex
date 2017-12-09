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

HYROC <- ROC(HYTSY,1)

#### Higher Put/Call Ratio = Bullish for Markets 
PutCall <- Quandl("CBOE/EQUITY_PC", type = "xts", start_date = "2006-11-01")

PCRatio <- na.omit(EMA(PutCall[,4],5))

#### Market Breadth ####
Breadth <- na.omit(ROC(CNNxts[,2],1))

### Safe Haven Demand ###
SP5.ret <- na.omit(SMA(dailyReturn(CNNxts[,1]),30))
TLT.ret <- na.omit(SMA(dailyReturn(CNNxts[,5]),30))

### Stock Price Momentum - SPX vs 125-day Mean ###
SP125Mu <- na.omit(CNNxts[,1] / runMean(CNNxts[,1], 125))

SHDemand <- SP5.ret - TLT.ret

CNN <- na.omit(cbind(Breadth, CNNxts[,3], HYROC, (CNNxts[,6]*-1), PCRatio, SHDemand, SP125Mu))
names(CNN) <- c( "Breadth", "52WkHi", "HYTSY", "VIX", "Put.Call", "SafeHaven", "Momentum")

CNN.df <- as.data.frame(rowMeans(CNN))
CNN.Index <- coredata(CNN.df)
row.names(CNN.Index) <- index(CNN)
FearGreed <- as.xts(CNN.Index)



#### CREATE COMPONENTS OF THE ALT CNN FEAR GREED INDEX ####
