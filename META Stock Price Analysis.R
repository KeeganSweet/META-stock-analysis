#Define pre & post period dates

#Pre-period: begin 2016
start="2016-01-01"

#'Treatment': CA whistleblower Guardian/NYT expose
treatment="2018-03-17"

#Post-period: 3mo after META congressional hearing
end="2018-07-17"

#Retrieve yfinance historical data
#Include various sectors to search for solid control group
#(Retail, entertainment, automotive, pharmaceutical, banking, manufacturing, food)

#install.packages("tseries")
#Time series for computational finance
#install.packages("zoo")
library(tseries)
library(zoo)


Facebook<-get.hist.quote(instrument="META",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

#compression means the increment of the data (w=week)

#Search for potential control variables

Disney<-get.hist.quote(instrument="DIS",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

BMW<-get.hist.quote(instrument="BMW.DE",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

Walmart<-get.hist.quote(instrument="WMT",
                       start=start,
                       end=end,
                       quote="Close",
                       compression="w")


Novartis<-get.hist.quote(instrument="NVS",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")


series<-cbind(Facebook, Disney, BMW, Walmart, Novartis)

#After running 'series,' we see the foreign stock, BMW.DE, is desynchronized with the others via the distribution of NA's. This is because the closing time zones are different. We can remedy this by using, "na.locf" ("last observation carried forward").

series<-na.locf(series)

#Plot stocks

#install.packages("ggplot2")
library(ggplot2)
autoplot(series, facet=NULL)+xlab("time")+ylab("Price Close")

#Correlation check
#Correlation essentially checks for similarity in line shape, and does not regard its overall position on the price axis. The stock's individual performance is being measured relative to its currency of origin, and these performances are being cross-referenced in a correlation matrix with the others, meaning different currencies are a non-issue for this test.


dataset_corr<-window(series, start=start, end=treatment)
dataset_corr<- as.data.frame(dataset_corr)
cor(dataset_corr)

#Adding more industries to select strong, correlated control group

GoldmanSachs<-get.hist.quote(instrument="GS",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

GeneralElectric<-get.hist.quote(instrument="GE",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

Heinz<-get.hist.quote(instrument="KHC",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

McDonalds<-get.hist.quote(instrument="MCD",
                         start=start,
                         end=end,
                         quote="Close",
                         compression="w")

Carlsberg<-get.hist.quote(instrument="CARL-B.CO",
                          start=start,
                          end=end,
                          quote="Close",
                          compression="w")

series2<-cbind(Facebook, Disney, BMW, Walmart, Novartis, GoldmanSachs, GeneralElectric, Heinz, McDonalds, Carlsberg)

series2<-na.locf(series2)

autoplot(series2, facet=NULL)+xlab("time")+ylab("Price Close")

dataset2_corr<-window(series2, start=start, end=treatment)
dataset2_corr<- as.data.frame(dataset2_corr)
cor(dataset2_corr)

#Selecting stocks most correlated to META for final dataset

final_series<-cbind(Facebook, Walmart, GoldmanSachs, McDonalds, Carlsberg)
final_series<-na.locf(final_series)
final_series<-na.omit(final_series)

#We use na.omit to delete Carlsberg's first row, as their close date is desynchronized with the start date.


#Create pre and post period objects
pre.period <- as.Date(c(start, treatment))
post.period <- as.Date(c(treatment, end))

#Running Causal Impact
#install.packages("CausalImpact")
library(CausalImpact)
impact <- CausalImpact(data = final_series,
                       pre.period = pre.period,
                       post.period = post.period,
                       model.args = list(niter = 2000,
                                         nseasons = 52))

#visualize results

plot(impact)
summary(impact)
summary(impact, "report")
