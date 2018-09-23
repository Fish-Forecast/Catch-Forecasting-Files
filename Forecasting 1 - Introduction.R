## ----load-packages, include=FALSE, message=FALSE-------------------------
require(ggplot2)

## ----load_data, fig.align = "center", fig.height = 4, fig.width = 8------
load("landings.RData")
landings$log.metric.tons = log(landings$metric.tons)
landings = subset(landings, Year <= 1989)
landings = subset(landings, Species %in% c("Anchovy","Sardine"))
ggplot(landings, aes(x=Year, y=log.metric.tons)) +
  geom_line() + facet_wrap(~Species)

