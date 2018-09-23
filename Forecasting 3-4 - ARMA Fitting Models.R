## ----setup, include=FALSE, message=FALSE---------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE)
library(huxtable)

## ----load_data, echo=FALSE, message=FALSE, warning=FALSE-----------------
load("landings.RData")
landings$log.metric.tons = log(landings$metric.tons)
landings = subset(landings, Year <= 1989)
anchovy = subset(landings, Species=="Anchovy")$log.metric.tons
sardine = subset(landings, Species=="Sardine")$log.metric.tons

library(ggplot2)
library(gridExtra)
library(reshape2)
library(tseries)
library(forecast)

## ----fitting.example.1---------------------------------------------------
set.seed(100)
a1 = arima.sim(n=100, model=list(ar=c(.8,.1)))
auto.arima(a1, seasonal=FALSE, max.d=0)

## ----fit.1000------------------------------------------------------------
save.fits = rep(NA,100)
for(i in 1:100){
  a1 = arima.sim(n=100, model=list(ar=c(.8,.1)))
  fit = auto.arima(a1, seasonal=FALSE, max.d=0, max.q=0)
  save.fits[i] = paste0(fit$arma[1], "-", fit$arma[2])
}
table(save.fits)

## ----fitting.example.2.data----------------------------------------------
set.seed(100)
a1 = arima.sim(n=100, model=list(ar=c(.8,.1)))
a1[sample(100,50)]=NA
plot(a1, type="l")
title("many missing values")

## ----fitting.example.2---------------------------------------------------
auto.arima(a1, seasonal=FALSE, max.d=0)

## ----fitting.example.3---------------------------------------------------
Arima(a1, order = c(2,0,0))

