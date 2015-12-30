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

library(broom)

tidy(fit)

loansDataClean$Estimate.10K <- tidy(fit)$estimate[1] + tidy(fit)$estimate[2] * loansDataClean$FICO.Range.Min + tidy(fit)$estimate[3] * 10000

loansDataClean$Estimate.30K <- tidy(fit)$estimate[1] + tidy(fit)$estimate[2] * loansDataClean$FICO.Range.Min + tidy(fit)$estimate[3] * 30000

ggplot(loansDataClean, aes(x=FICO.Range.Min, y=Interest.Rate.Dec)) + geom_point(shape=16, color='blue') + geom_line(y=Estimate.10K, color='red') + geom_line(y=Estimate.30K, color='green')

ggplot(loansDataClean, aes(sample=Amount.Requested))+stat_qq()

ggplot(loansDataClean, aes(sample=Amount.Funded.By.Investors))+stat_qq()

ggplot(loansDataClean, aes(Amount.Requested)) + geom_histogram(binwidth=5000)
ggplot(loansDataClean, aes(Amount.Funded.By.Investors)) + geom_histogram(binwidth=5000)

