## ----setup, include=FALSE, message=FALSE---------------------------------
options(htmltools.dir.version = FALSE, servr.daemon = TRUE)
library(huxtable)

## ----load_data_TV_Regression_Forecasting, echo=FALSE---------------------
load("landings.RData")
landings$log.metric.tons = log(landings$metric.tons)
landings = subset(landings, Year <= 1989)
landings$t = landings$Year-landings$Year[1]
anchovy = subset(landings, Species=="Anchovy" & Year <= 1987)
sardine = subset(landings, Species=="Sardine" & Year <= 1987)

## ----tvreg.anchovy2------------------------------------------------------
model <- lm(log.metric.tons ~ t, data=anchovy)
coef(model)

## ----tvreg.forecast1-----------------------------------------------------
coef(model)[1]+coef(model)[2]*24

## ----TVregression.forecast2----------------------------------------------
library(forecast)
fr <- forecast(model, newdata = data.frame(t=24:28))
fr

## ----plot.TVreg.forecast-------------------------------------------------
plot(fr)

## ----tvreg.sardine2------------------------------------------------------
model <- lm(log.metric.tons ~ t + I(t^2) + I(t^3) + I(t^4), data=sardine)
fr <- forecast(model, newdata = data.frame(t=24:28))
fr

## ----plot.TVreg.forecast.bad---------------------------------------------
try(plot(fr))

## ----plot.TVreg.func, echo=FALSE-----------------------------------------
plotforecasttv <- function(object, h=10){
  dat <- object$model
  tlim <- (max(object$model$t)+1:h)
  pr95 <- predict(model, newdata = data.frame(t=(max(object$model$t)+1:h)), level=0.95, interval="prediction")
  pr80 <- predict(model, newdata = data.frame(t=(max(object$model$t)+1:h)), level=0.80, interval="prediction")
  pr95 <- as.data.frame(pr95); pr95$t <- tlim
  pr80 <- as.data.frame(pr80); pr80$t <- tlim
  ylims <- c(min(dat[,1],pr95$lwr, pr80$lwr), max(dat[,1],pr95$upr, pr80$upr))
  p1 <- ggplot(dat, aes_string(x = colnames(dat)[2], y = colnames(dat)[1])) +
  theme_bw() +
  geom_point(color = "blue") + xlim(0,max(tlim)) + ylim(ylims)
  p1 + 
    geom_ribbon(mapping=aes(x=t, ymin=lwr, ymax=upr), data=pr95, inherit.aes=FALSE, fill = "grey50") +
    geom_ribbon(mapping=aes(x=t, ymin=lwr, ymax=upr), data=pr80, inherit.aes=FALSE, fill = "grey75") +
    geom_line(aes(x=t, y=fit), pr95)
}

## ----plot.TVreg.forecast2------------------------------------------------
plotforecasttv(model)

