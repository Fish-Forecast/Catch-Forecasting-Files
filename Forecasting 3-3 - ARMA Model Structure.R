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

## ----fig.acf.pacf, fig.height = 6, fig.width = 8, fig.align = "center", echo=FALSE----
par(mfrow=c(3,2))
#####
ys = arima.sim(n=1000, list(ar=.7))
acf(ys, main="")
title("AR-1 (p=1, q=0)")
pacf(ys, main="")

ys = arima.sim(n=1000, list(ma=.7))
acf(ys, main="")
title("MA-1 (p=0, q=1)")
pacf(ys, main="")

ys = arima.sim(n=1000, list(ar=.7, ma=.7))
acf(ys, main="")
title("ARMA (p=1, q=1)")
pacf(ys, main="")

## ----fig.acf, fig.height = 5, fig.width = 8, fig.align = "center", echo=FALSE----
par(mfrow=c(2,2))
#####
ys = arima.sim(n=1000, list(ar=.7))
acf(ys, main="")
title("AR-1 (p=1, q=0)")
pacf(ys, main="")

ys = arima.sim(n=1000, list(ar=c(.7,.2)))
acf(ys, main="")
title("AR-2 (p=2, q=0)")
pacf(ys, main="")

## ----fig.pacf, fig.height = 5, fig.width = 8, fig.align = "center", echo=FALSE----
par(mfrow=c(2,2))
#####
ys = arima.sim(n=1000, list(ma=.7))
acf(ys, main="")
title("MA-1 (p=0, q=1)")
pacf(ys, main="")

ys = arima.sim(n=1000, list(ma=c(.7,.2)))
acf(ys, main="")
title("MA-2 (p=0, q=2)")
pacf(ys, main="")

## ----fig.pacf.acf.model, fig.height = 5, fig.width = 8, fig.align = "center", echo=FALSE----
par(mfrow=c(2,2))
#####
ys = arima.sim(n=1000, list(ar=.7, ma=.7))
acf(ys, main="")
title("ARMA (p=1, q=1)")
pacf(ys, main="")

ys = arima.sim(n=1000, list(ar=c(.7,.2), ma=c(.7,.2)))
acf(ys, main="")
title("ARMA (p=2, q=2)")
pacf(ys, main="")

## ----auto.arima, eval=FALSE----------------------------------------------
## require(forecast)
## anchovy.diff1 = diff(anchovy)
## auto.arima(anchovy.diff1)

## ----auto.arima2---------------------------------------------------------
require(forecast)
anchovy.diff1 = diff(anchovy)
auto.arima(anchovy.diff1)

## ----auto.arima3---------------------------------------------------------
auto.arima(anchovy)

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

