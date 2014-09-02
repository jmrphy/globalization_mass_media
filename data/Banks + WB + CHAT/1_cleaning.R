setwd("~/Dropbox/Data General/Arthur Banks CNTS 2005")
options(scipen=999)
banks<-read.csv("banks_coded_cleaned.csv")

library(WDI)

#vars<-c("SE.XPD.TOTL.GB.ZS", "GC.DOD.TOTL.GD.ZS", "BN.CAB.XOKA.GD.ZS", "UIS.XGOVEXP.FNCUR", "SP.POP.DPND", "AG.LND.TOTL.K2", "NY.GDP.MKTP.PP.CD", "NY.GDP.PCAP.CD", "SP.POP.TOTL", "IT.PRT.NEWS.P3", "IT.RAD.SETS", "IT.RAD.SETS.P3", "NE.IMP.GNFS.ZS", "NE.EXP.GNFS.ZS", "NE.TRD.GNFS.ZS", "BX.KLT.DINV.WD.GD.ZS", "BN.KLT.PRVT.GD.ZS", "NE.CON.GOVT.ZS", "SL.IND.EMPL.ZS", "SP.URB.TOTL.IN.ZS")
            

WDIsearch("fiscal")
wb <- WDI(country="all", indicator=c("SE.XPD.TOTL.GB.ZS", "GC.DOD.TOTL.GD.ZS", "BN.CAB.XOKA.GD.ZS", "UIS.XGOVEXP.FNCUR", "SP.POP.DPND", "AG.LND.TOTL.K2", "NY.GDP.MKTP.PP.CD", "NY.GDP.PCAP.CD", "SP.POP.TOTL", "IT.PRT.NEWS.P3", "IT.RAD.SETS", "IT.RAD.SETS.P3", "NE.IMP.GNFS.ZS", "NE.EXP.GNFS.ZS", "NE.TRD.GNFS.ZS", "BX.KLT.DINV.WD.GD.ZS", "BN.KLT.PRVT.GD.ZS", "NE.CON.GOVT.ZS", "NV.IND.MANF.ZS", "SP.URB.TOTL.IN.ZS"), start=1960, end=2012, extra=TRUE)

wb$urban.wb<-wb$SP.URB.TOTL.IN.ZS   # percentage of population urban
wb$debt.wb<-wb$GC.DOD.TOTL.GD.ZS      #central government debt as share of gdp
wb$curracct.wb<-wb$BN.CAB.XOKA.GD.ZS   #current account balance as share of gdp
wb$edu.wb<-wb$UIS.XGOVEXP.FNCUR   #public education spending as share of total govt spending
wb$dependency.wb<-wb$SP.POP.DPND
wb$land.wb<-wb$AG.LND.TOTL.K2
wb$pop.wb<-wb$SP.POP.TOTL
wb$radios.wb<-wb$IT.RAD.SETS  

wb$newspaperscap.wb<-wb$IT.PRT.NEWS.P3/10  # convert to per 100 people
boxplot(wb$newspaperscap.wb)  # no obvious outliers

wb$radioscap.wb<-wb$IT.RAD.SETS.P3/10  # convert to per 100 people
boxplot(wb$radioscap.wb)   # obvious outliers, unclear if scaling error
wb$radioscap.wb[wb$radioscap.wb>10000]<-NA   # remove to be safe
boxplot(wb$radioscap.wb)   # looks right


wb$gdp.wb<-wb$NY.GDP.MKTP.PP.CD
wb$gdpcap.wb<-wb$NY.GDP.PCAP.CD
wb$imports.wb<-wb$NE.IMP.GNFS.ZS
wb$exports.wb<-wb$NE.EXP.GNFS.ZS
wb$trade.wb<-wb$NE.TRD.GNFS.ZS
wb$fdi.wb<-wb$BX.KLT.DINV.WD.GD.ZS
wb$privatecapital.wb<-wb$BN.KLT.PRVT.GD.ZS
wb$spending.wb<-wb$NE.CON.GOVT.ZS
wb$industry.wb<-wb$NV.IND.MANF.ZS

wb$countrylower<-tolower(wb$country)
banks$countrylower<-tolower(banks$country)

library(countrycode)
wb$scode<-countrycode(wb$iso3c, "iso3c", "cown")

# Merge Banks and WB
merged<-merge(banks, wb, by=c("scode", "year"), all.x=TRUE)

# interpolate WB radios per capita into Banks radios per capita
boxplot(merged$radioscap)   # no obvious outliers, correct scale
boxplot(merged$radioscap.wb) # no obvious outliers, correct scale
summary(merged$radioscap)   # missing=7,314
myfun <- approxfun(merged$radioscap.wb, merged$radioscap)
merged$radioscap[is.na(merged$radioscap)] <- myfun(merged$radioscap.wb[is.na(merged$radioscap)]) 
summary(merged$radioscap) # ~200 gained
boxplot(merged$radioscap)

# interpolate WB population into Banks population
summary(merged$pop)   # missing=432
boxplot(merged$pop)    # no obvious outliers, correct scale
summary(merged$pop.wb)  # missing=8776
boxplot(merged$pop.wb)  # no obvious outliers, correct scale
myfun <- approxfun(merged$pop.wb, merged$pop)
merged$pop[is.na(merged$pop)] <- myfun(merged$pop.wb[is.na(merged$pop)]) 
summary(merged$pop)     #  +300 observations gained
boxplot(merged$pop)    # no obvious outliers, correct scale

# interpolate WB radios total into Banks radios total
summary(merged$radios.wb)
boxplot(merged$radios.wb)    #no obvious outliers
summary(merged$radios)
boxplot(merged$radios)    #no obvious outliers
myfun <- approxfun(merged$radios.wb, merged$radios)
merged$radios[is.na(merged$radios)] <- myfun(merged$radios.wb[is.na(merged$radios)]) 
boxplot(merged$radios)

# divide interpolated radios by interpolated population
summary(merged$radioscap)   # missing=7176
boxplot(merged$radioscap)
merged$radioscap2<-(merged$radios/merged$pop)*100
merged$radioscap2<-ifelse(merged$radioscap2>=200, NA, merged$radioscap2)
summary(merged$radioscap2)   # missing=7335, first radioscap is better
boxplot(merged$radioscap2)

# interpolate interpolated radios/pop into interpolated radios per capita
myfun <- approxfun(merged$radioscap2, merged$radioscap)
merged$radioscap[is.na(merged$radioscap)] <- myfun(merged$radioscap2[is.na(merged$radioscap)])
summary(merged$radioscap)     # nothing gained
boxplot(merged$radioscap)     # no different

summary(merged$newspaperscap)  # missing=12,626
summary(merged$newspaperscap.wb) # missing=22,698
myfun <- approxfun(merged$newspaperscap.wb, merged$newspaperscap)
merged$newspaperscap[is.na(merged$newspaperscap)] <- myfun(merged$newspaperscap.wb[is.na(merged$newspaperscap)])
summary(merged$newspaperscap)  # ~1000 observations gained
boxplot(merged$newspaperscap)  # no obvious outliers

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
merged2$tv<-(merged2$tv/merged2$xlpopulation)  # looks better than with original pop
hist(merged2$tv) # looks better
summary(merged2$tv)  # missing=32,038
hist(merged2$tv)
boxplot(merged2$tv)  # no obvious outliers

# only for use with original population per capita calculation
#merged2$tv<-ifelse(merged2$tv>=500, merged2$tv/100, merged2$tv)  # distribution looks better than using NAs
#merged2$tv<-ifelse(merged2$tv>=500, NA, merged2$tv) 
#boxplot(merged2$tv) 
#summary(merged2$tv)  # missing=34,075

summary(merged2$tvscap)  # missing=23,863
myfun <- approxfun(merged2$tv, merged2$tvscap)
merged2$tvscap[is.na(merged2$tvscap)] <- myfun(merged2$tv[is.na(merged2$tvscap)])
summary(merged2$tvscap)  # +8000 observations gained
boxplot(merged2$tvscap)  # no obvious outliers

df<-merged2
df$mdi<-rowSums(cbind(df$newspaperscap, df$tvscap, df$radioscap), na.rm = FALSE, dims = 1)
df$mdi2<-rowMeans(cbind(df$newspaperscap, df$tvscap, df$radioscap), na.rm = FALSE, dims = 1)

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

df2$ltrade.wb<-lag(df2$trade.wb,1)
df2$trade.wb.d<-diff(lag(df2$trade.wb))

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

