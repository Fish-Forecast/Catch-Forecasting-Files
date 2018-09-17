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
library(urca)

## ----arima.sim-----------------------------------------------------------
dat = arima.sim(n=100, model=list(ar=c(.8,.1)))
plot(dat)

