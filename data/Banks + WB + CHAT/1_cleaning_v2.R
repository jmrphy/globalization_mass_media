setwd("~/Dropbox/Data General/Arthur Banks CNTS 2005")
options(scipen=999)
banks<-read.csv("banks_coded_cleaned.csv")

library(WDI)
wb <- WDI(country="all", indicator=c("SE.XPD.TOTL.GB.ZS", "GC.DOD.TOTL.GD.ZS", "BN.CAB.XOKA.GD.ZS", "UIS.XGOVEXP.FNCUR", "SP.POP.DPND", "AG.LND.TOTL.K2", "NY.GDP.MKTP.PP.CD", "NY.GDP.PCAP.CD", "SP.POP.TOTL", "IT.PRT.NEWS.P3", "IT.RAD.SETS", "IT.RAD.SETS.P3", "NE.IMP.GNFS.ZS", "NE.EXP.GNFS.ZS", "NE.TRD.GNFS.ZS", "BX.KLT.DINV.WD.GD.ZS", "BN.KLT.PRVT.GD.ZS", "NE.CON.GOVT.ZS", "NV.IND.MANF.ZS", "SP.URB.TOTL.IN.ZS", "BX.PEF.TOTL.CD.WD", "BM.KLT.DINV.GD.ZS ", "BX.KLT.DINV.WD.GD.ZS" ), start=1960, end=2012, extra=TRUE)

wb$urban<-wb$SP.URB.TOTL.IN.ZS   # percentage of population urban
wb$debt<-wb$GC.DOD.TOTL.GD.ZS      #central government debt as share of gdp
wb$curracct<-wb$BN.CAB.XOKA.GD.ZS   #current account balance as share of gdp
wb$edu<-wb$UIS.XGOVEXP.FNCUR   #public education spending as share of total govt spending
wb$dependency<-wb$SP.POP.DPND
wb$land<-wb$AG.LND.TOTL.K2
wb$pop<-wb$SP.POP.TOTL
wb$gdp<-wb$NY.GDP.MKTP.PP.CD
wb$gdpcap<-wb$NY.GDP.PCAP.CD
wb$imports<-wb$NE.IMP.GNFS.ZS
wb$exports<-wb$NE.EXP.GNFS.ZS
wb$trade<-wb$NE.TRD.GNFS.ZS
wb$fdi.out<-wb$BM.KLT.DINV.GD.ZS  # fdi outflows, as share of gdp
wb$fdi.in<-wb$BX.KLT.DINV.WD.GD.ZS  # fdi outflows, as share of gdp
wb$portfolio<-wb$BX.PEF.TOTL.CD.WD #portfolio equity, net inflows
wb$privatecapital<-wb$BN.KLT.PRVT.GD.ZS
wb$spending<-wb$NE.CON.GOVT.ZS
wb$industry<-wb$NV.IND.MANF.ZS

wb$newspaperscap.wb<-wb$IT.PRT.NEWS.P3/10  # convert to per 100 people
boxplot(wb$newspaperscap.wb)  # no obvious outliers

wb$radioscap.wb<-wb$IT.RAD.SETS.P3/10  # convert to per 100 people
boxplot(wb$radioscap.wb)   # obvious outliers, unclear if scaling error
wb$radioscap[wb$radioscap.wb>10000]<-NA   # remove to be safe
boxplot(wb$radioscap.wb)   # looks right


wb$countrylower<-tolower(wb$country)
banks$countrylower<-tolower(banks$country)

library(countrycode)
wb$scode<-countrycode(wb$iso3c, "iso3c", "cown")

# Merge Banks and WB
merged<-merge(wb,banks, by=c("scode", "year"), all.x=TRUE)

# interpolate WB radios per capita into Banks radios per capita
boxplot(merged$radioscap.wb)   # no obvious outliers, correct scale
boxplot(merged$radioscap.wb) # no obvious outliers, correct scale
summary(merged$radioscap.wb)   # missing=7,314
myfun <- approxfun(merged$radioscap.x, merged$radioscap.wb)
merged$radioscap.wb[is.na(merged$radioscap.wb)] <- myfun(merged$radioscap.x[is.na(merged$radioscap.wb)]) 
summary(merged$radioscap.wb) # ~200 gained
boxplot(merged$radioscap.wb)

# interpolate WB population into Banks population
summary(merged$pop)   # missing=432
boxplot(merged$pop)    # no obvious outliers, correct scale
summary(merged$pop)  # missing=8776
boxplot(merged$pop)  # no obvious outliers, correct scale
myfun <- approxfun(merged$pop.x, merged$pop.wb)
merged$pop.wb[is.na(merged$pop.wb)] <- myfun(merged$pop.x[is.na(merged$pop.wb)]) 
summary(merged$pop.wb)     #  +300 observations gained
boxplot(merged$pop.wb)    # no obvious outliers, correct scale


summary(merged$newspaperscap.wb)  # missing=12,626
summary(merged$newspaperscap.wb) # missing=22,698
myfun <- approxfun(merged$newspaperscap, merged$newspaperscap.wb)
merged$newspaperscap.wb[is.na(merged$newspaperscap.wb)] <- myfun(merged$newspaperscap[is.na(merged$newspaperscap.wb)])
summary(merged$newspaperscap.wb)  # ~1000 observations gained
boxplot(merged$newspaperscap.wb)  # no obvious outliers

##################################
### Add CHAT Data ################
##################################
setwd("~/Dropbox/Data General/Historical Technology Adoption")
library(foreign)
chat<-read.dta("CHAT.dta")
library(countrycode)
chat$scode<-countrycode(chat$wbcode, "wb", "cown")
chat$scode2<-countrycode(chat$country, "country.name", "cown")
summary(is.na(chat$scode))   # 3108 missing
summary(is.na(chat$scode2))   # 1036 missing
summary(is.na(chat$scode2)!=is.na(chat$scode))  # 2590 with different NA status
chat$scode<-ifelse(is.na(chat$scode), chat$scode2, chat$scode)  # insert scode2 into scode where missing
summary(is.na(chat$scode)) # +2000 observations gained
chat$scode2<-ifelse(is.na(chat$scode2), chat$scode, chat$scode2) #check vice-versa
summary(is.na(chat$scode2))   # no observations gained

chat<-subset(chat, select=c("tv", "radio", "newspaper", "scode", "year", "cabletv", "xlpopulation"))

merged2<-merge(merged, chat, by=c("scode", "year"), all.x=TRUE)

#merged2$tv<-(merged2$tv/merged2$pop)
merged2$xlpopulation<-merged2$xlpopulation*1000
merged2$tvscap.wb<-(merged2$tv/merged2$xlpopulation)  # looks better than with original pop
hist(merged2$tvscap.wb) # looks better
summary(merged2$tvscap.wb)  # missing=32,038
hist(merged2$tvscap.wb)
boxplot(merged2$tvscap.wb)  # no obvious outliers

# only for use with original population per capita calculation
#merged2$tv<-ifelse(merged2$tv>=500, merged2$tv/100, merged2$tv)  # distribution looks better than using NAs
#merged2$tv<-ifelse(merged2$tv>=500, NA, merged2$tv) 
#boxplot(merged2$tv) 
#summary(merged2$tv)  # missing=34,075

summary(merged2$tvscap)  # missing=23,863
myfun <- approxfun(merged2$tvscap.wb, merged2$tvscap)
merged2$tvscap.wb[is.na(merged2$tvscap.wb)] <- myfun(merged2$tvs[is.na(merged2$tvscap.wb)])
summary(merged2$tvscap.wb)  # +8000 observations gained
boxplot(merged2$tvscap.wb)  # no obvious outliers

df<-merged2
df$mdi<-rowSums(cbind(df$newspaperscap.wb, df$tvscap.wb, df$radioscap.wb), na.rm = FALSE, dims = 1)
df$mdi2<-rowMeans(cbind(df$newspaperscap, df$tvscap.wb, df$radioscap), na.rm = FALSE, dims = 1)

summary(df$mdi)
plot(density(df$mdi,  na.rm=TRUE))  #wrong
######### Erase Duplicates from Merges #########
df<-subset(df, !duplicated(subset(df,select=c(scode,year))))
df<-subset(df, scode!="NA")
df<-subset(df, year!="NA")
plot(density(df$mdi,  na.rm=TRUE))   #right

df$scode<-as.factor(df$scode)

df$mdi.log<-log(df$mdi + 1)
hist(df$mdi.log)

df$mdi.sq<-df$mdi*df$mdi
df$mdi.log.sq<-df$mdi.log*df$mdi.log

require(plm)

df2<-pdata.frame(df, index=c("scode", "year"))

df2$lmdi<-lag(df2$mdi,1)
df2$ltv<-lag(df2$tvscap,1)
df2$lnewspaper<-lag(df2$newspaperscap,1)
df2$lradio<-lag(df2$radioscap,1)

df2$ltrade<-lag(df2$trade,1)

df2$ldemos<-lag(df2$demos,1)
df2$lstrikes<-lag(df2$strikes,1)
df2$lassassins<-lag(df2$assassins,1)
df2$lriots<-lag(df2$riots,1)
df2$lrevs<-lag(df2$revs,1)
df2$lpurges<-lag(df2$purges,1)
df2$lgovtcrises<-lag(df2$govtcrises,1)
df2$lguerrillas<-lag(df2$guerrillas,1)
df2$lconflict<-lag(df2$conflict,1)

df2$mdi.d<-diff(lag(df2$mdi))
df2$tv.d<-diff(lag(df2$tvscap))
df2$newspaper.d<-diff(lag(df2$newspaperscap))
df2$radio.d<-diff(lag(df2$radioscap))

df2$trade.d<-diff(lag(df2$trade))

df2$demos.d<-diff(lag(df2$demos))
df2$strikes.d<-diff(lag(df2$strikes))
df2$assassins.d<-diff(lag(df2$assassins))
df2$riots.d<-diff(lag(df2$riots,1))
df2$revs.d<-diff(lag(df2$revs,1))
df2$purges.d<-diff(lag(df2$purges,1))
df2$govtcrises.d<-diff(lag(df2$govtcrises,1))
df2$guerrillas.d<-diff(lag(df2$guerrillas,1))
df2$conflict.d<-diff(lag(df2$conflict,1))

df<-as.data.frame(df2)
class(df)

setwd("~/Dropbox/Data General/Banks + WB + CHAT")
write.csv(df, "banks_wb_chat.csv")

rm(df2)
