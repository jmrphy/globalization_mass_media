
#library(pwt)
#data("pwt7.1")
#pwt7.1$scode<-countrycode(pwt7.1$isocode, "iso3c", "cown")
#pwt7.1$pop.wpt<-pwt7.1$pop
#summary(merged2$year)
#pwt<-subset(pwt7.1, select=c("year", "country", "scode", "pop.wpt", "xrat", "tcgdp", "cgdp", "cg", "openc"))

#merged3<-merge(merged2, pwt, by=c("scode", "year"))

#merged3$gdp.wpt<-merged3$tcgdp
#merged3$gdpcap.wpt<-merged3$cgdp
#merged3$xrat.wpt<-merged3$xrat
#merged3$trade.wpt<-merged3$openc

#summary(merged3$trade)
#summary(merged3$trade.wb)
#summary(merged3$trade.wpt)
#plot(merged3$gdp, merged3$gdp.wb)
#plot(merged3$gdp.wpt, merged3$gdp.wb)
#plot(merged3$gdp, merged3$gdp.wpt)

#summary(merged3$trade)
#summary(merged3$trade.wb)
#summary(merged3$trade.wpt)
#plot(merged3$trade, merged3$trade.wb)
#sd(merged3$trade, na.rm=TRUE)
#sd(merged3$trade.wb, na.rm=TRUE)
#plot(merged3$trade.wpt, merged3$trade.wb)
#plot(merged3$trade, merged3$trade.wpt)

# Interpolate WB trade into PWT trade
#summary(merged3$trade.wpt) # missing=10,806
#merged3$trade.int<-merged3$trade.wpt
#myfun <- approxfun(merged3$trade.wb, merged3$trade.int)
#merged3$trade.int[is.na(merged3$trade.int)] <- myfun(merged3$trade.wb[is.na(merged3$trade.int)])
#summary(merged3$trade.int)  # +5000 observations gained
#boxplot(merged3$trade.int)  # no obvious outliers

# Interpolate Banks trade into PWT<-WB trade
#summary(merged3$trade.int) # missing=5,181
#boxplot(merged3$trade.int)
#merged3$trade.int2<-merged3$trade.int
#myfun <- approxfun(merged3$trade, merged3$trade.int2)
#merged3$trade.int2[is.na(merged3$trade.int2)] <- myfun(merged3$trade[is.na(merged3$trade.int2)])
#summary(merged3$trade.int2)  # 200 observations gained
#boxplot(merged3$trade.int2)  # no obvious outliers

#df<-merged3

#df$mdi<-rowSums(cbind(df$newspaperscap, df$tvscap, df$radioscap), na.rm = TRUE, dims = 1)
#df$mdi<-df$newspaperscap + df$tvscap + df$radioscap
#summary(df$newspaperscap)
#summary(df$tvscap)
#summary(df$radioscap)
#summary(df$mdi)
#plot(density(df$mdi))