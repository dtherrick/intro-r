library(ggplot2)
library(GGally)

loansData <- read.csv('https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv')
head(loansData)
loansData$Interest.Rate.Dec <- as.numeric(sub('%', '', loansData$Interest.Rate))/100
loansData$FICO.Range.Min <- as.numeric(strtrim(loansData$FICO.Range, 3))
head(loansData)

str(loansData)
loansDataClean  <- na.omit(loansData)

hist(loansDataClean$FICO.Range.Min)
boxplot(loansDataClean$Amount.Funded.By.Investors)

numericColumns <- c("Amount.Requested", "Amount.Funded.By.Investors", "Monthly.Income", "Open.CREDIT.Lines",
                   "Revolving.CREDIT.Balance", "Inquiries.in.the.Last.6.Months", "Interest.Rate.Dec", 
                   "FICO.Range.Min")

loansNumeric <- loansDataClean[numericColumns]
ggpairs(loansNumeric)

fit <- lm(Interest.Rate.Dec ~ FICO.Range.Min + Amount.Requested, data=loansDataClean)
summary(fit)