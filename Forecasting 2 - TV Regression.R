## ----setup, include=FALSE, message=FALSE---------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE)
library(huxtable)

## ----load_data, echo=FALSE-----------------------------------------------
load("landings.RData")
landings$log.metric.tons = log(landings$metric.tons)
landings = subset(landings, Year <= 1989)

## ----poly.plot, echo=FALSE,fig.height=4,fig.width=8,fig.align="center"----
par(mfrow=c(1,4))
tt=seq(-5,5,.01)
plot(tt,type="l",ylab="",xlab="")
title("1st order")
plot(tt^2,type="l",ylab="",xlab="")
title("2nd order")
plot(tt^3-3*tt^2-tt+3,ylim=c(-100,50),type="l",ylab="",xlab="")
title("3rd order")
plot(tt^4+2*tt^3-12*tt^2-2*tt+6,ylim=c(-100,100),type="l",ylab="",xlab="")
title("4th order")

## ----tvreg.anchovy-------------------------------------------------------
landings$t = landings$Year-1963
model <- lm(log.metric.tons ~ poly(t,4), 
            data=landings, subset=Species=="Anchovy"&Year<=1987)

## ------------------------------------------------------------------------
summary(model)

## ----tvreg.anchovy2------------------------------------------------------
dat = subset(landings, Species=="Anchovy" & Year <= 1987)
model <- lm(log.metric.tons ~ t, data=dat)

## ------------------------------------------------------------------------
c(coef(model), summary(model)$adj.r.squared)

## ----example_LB_test-----------------------------------------------------
Box.test(rnorm(100), type="Ljung-Box")

## ------------------------------------------------------------------------
x <- resid(model)
Box.test(x, lag = 14, type = "Ljung-Box", fitdf=2)

## ----poly----------------------------------------------------------------
T1 = 1:24; T2=T1^2
c(mean(T1),mean(T2),cov(T1, T2))
T1 = poly(T1,2)[,1]; T2=poly(T1,2)[,2]
c(mean(T1),mean(T2),cov(T1, T2))

## ----tvreg.sardine-------------------------------------------------------
dat = subset(landings, Species=="Sardine" & Year <= 1987)
model <- lm(log.metric.tons ~ poly(t,4), data=dat)

## ----poly.summary--------------------------------------------------------
summary(model)

## ----tvreg.sardine2------------------------------------------------------
dat = subset(landings, Species=="Sardine" & Year <= 1987)
model <- lm(log.metric.tons ~ t + I(t^2) + I(t^3) + I(t^4), data=dat)

## ------------------------------------------------------------------------
c(coef(model), summary(model)$adj.r.squared)

## ------------------------------------------------------------------------
x <- resid(model)
Box.test(x, lag = 14, type = "Ljung-Box", fitdf=5)

