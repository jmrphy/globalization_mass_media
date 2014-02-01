setwd("~/Dropbox/Data General/Article1")
source('1_load.R')
source('2_clean.R')
source('3_functions.R')


########################
### Survey Summary Stats
########################

library(reporttools)

tableContinuous(surveyvars[,sapply(surveyvars, is.numeric)], font.size=12)
tableNominal(surveyvars[,!sapply(surveyvars, is.numeric)], font.size=12)


#############################################
### Model 1.A: Media and Blame (direct) #####
#############################################
library(Zelig)
model1<-zelig(causes ~ Age + gender + urban + Interest + college + occ + leftparty + media + genprob2 + openprob2,
             model="logit",
             data=x,
             cite=FALSE)

############## Model 1 - Blame Coefficient Plot #################

coefplot(model1,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Logit Estimates", ylab="") +
         annotate("text", x=1.2, y=.85, label="N = 3,252", size=2.5) +
         ggtitle("Model 1: Blaming International Forces") +
         theme_bw() +
         theme(title=element_text(size=8)) + 
         theme(text=element_text(size=10))
ggsave(filename="blame_coefs.pdf", width=4, height=4)

############## Model 1 - Media-Blame Effect Plot #################

source('~/Dropbox/Data General/Article1/media_blame_effect.R')

##############################################
## Supporting Information for Model 1: Blame
##############################################

############## Model 1 - Media-Blame Effect Plot #################
library(apsrtable)
writeLines(apsrtable(model1, coef.rows=1), file("table1.tex"))

########## Model 1.B - Gov't Blame Model Results Table #################

model1.B<-zelig(cause.gov ~ Age + gender + urban + Interest + college + occ + leftparty + media + genprob2 + openprob2,
             model="logit",
             data=x,
             cite=FALSE)

############## Model 1.B - Gov't Blame Effect Plot #################

nobs(model1.B)
coefplot(model1.B,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Logit Estimates", ylab="") +
  annotate("text", x=.65, y=.7, label="N = 5,148", size=2.5) +
  ggtitle("Model 1.B: Blame Government") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="blame_coefs.1.B.pdf", width=4, height=4)

############## Model 1.B - Gov't Blame Model Results Table #################

writeLines(apsrtable(model1.B, coef.rows=1), file("table1.B.tex"))

############## Model 1.C - Int'l Blame Effect Plot #################

model1.C<-zelig(cause.intl ~ Age + gender + urban + Interest + college + occ + leftparty + media + genprob2 + openprob2,
             model="logit",
             data=x,
             cite=FALSE)

############## Model 1.C - Int't Blame Effect Plot #################

nobs(model1.C)
coefplot(model1.C,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Logit Estimates", ylab="") +
  annotate("text", x=.65, y=.7, label="N = 5,148", size=2.5) +
  ggtitle("Model 1.C: Blame International") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="blame_coefs.1.C.pdf", width=4, height=4)

############## Model 1.C - Int'l Blame Model Results Table #################

writeLines(apsrtable(model1.C, coef.rows=1), file("table1.C.tex"))

###########################################################
### Model 1.2: Media -> Openprob2 -> Blame (indirect) #####
###########################################################
model1.2<-zelig(openprob2 ~ foreignprob2 + econprob2 + Age + gender + urban + Interest + college + occ + mitterand + leftparty + media,
              model="logit",
              data=x,
              cite=FALSE)

############## Model 1.2 - Media -> Openprob2 #################
nobs(model1.2)
coefplot(model1.2,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Logit Estimates", ylab="") +
  annotate("text", x=2.5, y=.7, label="N = 4,770", size=2.5) +
  ggtitle("Model 1.2: Openness as a Problem") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="blame_coefs.1.2.pdf", width=4, height=4)

##############################################
## Supporting Information for Model 1.2: Media->Openprob2->Blame
##############################################

############## Model 1.2 - Media->Openprob2-> Blame Model Results Table #################

writeLines(apsrtable(model1.2, coef.rows=1), file("table1.2.tex"))

####################################
### Model 2: Government Handling ###
####################################

###################### Model 2 ####################################
library(Zelig)
options(scipen=999)
model<-glm(GovHandling ~ Age + gender + urban + Interest + college + occ + media + leftparty + mitterand + PresSatisfaction + genprob2 + openprob2 + causes, data=x, family=gaussian)
model2<-zelig(GovHandling ~ Age + gender + urban + Interest + college + occ + media + leftparty + mitterand + PresSatisfaction + genprob2 + openprob2 + causes,
              model="ls",
              data=x,
              cite=FALSE)

################# Model 2 - Coefficient Plot #####################
library(coefplot)
coefplot(model2,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Regression Coefficients", ylab="") +
  annotate("text", x=.9, y=2, label="N = 2,401", size=2.5) +
  ggtitle("Model 2: Government Handling") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="govhandling_coefs.pdf", width=4, height=4)

############# Model 2 - Simulation Plot #################################

source('~/Dropbox/Data General/Article1/blame_govhandle.R')

#########################################
## Supporting Information for Model 2 ###
#########################################

###################### Model 2 - Coefficient Table #####################
library(apsrtable)
writeLines(apsrtable(model2, coef.rows=1), file("table2.tex"))

################# Model 2 with alternative, full cause variable #########

model2.A<-zelig(GovHandling ~ Age + gender + urban + Interest + college + occ + media + leftparty + mitterand + PresSatisfaction + genprob2 + openprob2 + cause,
              summmodel="ls",
              data=x,
              cite=FALSE)

library(coefplot)
coefplot(model2.A,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Regression Coefficients", ylab="") +
  annotate("text", x=.9, y=2, label="N = 2,401", size=2.5) +
  ggtitle("Model 2.A: Government Handling") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="govhandling_altcause_coefs.pdf", width=4, height=4)


library(apsrtable)
writeLines(apsrtable(model2.A, coef.rows=1), file("table2.A.tex"))

################# Model 2.1 - PresSat Model Table #####################

model2.B<-zelig(PresSatisfaction ~ Age + gender + urban + Interest + college + occ + media + leftparty + mitterand + GovHandling + genprob1 + openprob2 + causes,
                model="ls",
                data=x,
                cite=FALSE)
writeLines(apsrtable(model2.B, coef.rows=1), file("table2.B.tex"))

############ Model 2.1 - PresSat Coefficient Plot ################

coefplot(model2.B,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Regression Coefficients", ylab="") +
  annotate("text", x=1.3, y=2, label="N = 2,401", size=2.5) +
  ggtitle("Model2.B: Presidential Satisfaction") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="pressat_coefs.pdf", width=4, height=4)

############## Model 2.B - PresSat Effect Plot #################

source('~/Dropbox/Data General/Article1/blame_pressat.R')

########################
### Model 3: Turnout
########################
model3<-zelig(turnoutint ~ Registered+Age+gender+Interest+college+leftcand+media+genprob1+OpennessXBlame+causes+openprob2,
             model="logit",
             data=x,
             cite=FALSE)
nobs(model3)
coefplot(model3,
         color="black",
         lwdInner=0,
         lwdOuter=.5,
         vertical=TRUE, xlab="Logit Estimates", ylab="", zeroLWD=.1) +
  annotate("text", x=3.2, y=.7, label="N = 2,406", size=2.5) +
  ggtitle("Model 3: Turnout") +
  theme_bw() +
  theme(title=element_text(size=8)) + 
  theme(text=element_text(size=10))
ggsave(filename="turnout_coefs.3.pdf", width=4, height=4)

source('~/Dropbox/Data General/Article1/blame_turnout.R')
ggsave(filename="turnout.pdf", width=5, height=4)


######################################################
### State-Level ANALYSES #############################
######################################################

### Variable Description Table and Summary Stats

setwd("~/Dropbox/Data General/Article1")

options(scipen=999)
require(arm)

mainvars<-subset(df, select=c("scode", "year", "spending.wb", "polity2", "trade.wb", "mdi", "gdpcap.wb", "dependency.wb", "land.wb"))

mainvars[,4:9]<-sapply(mainvars[,4:9], rescale)

dfextra<-subset(dfextra, select=c("scode", "year", "spending.wb", "lefts", "netden", "pr", "unitarism", "polity2", "trade.wb", "mdi", "gdpcap.wb", "dependency.wb", "land.wb", "industry.wb"))

dfextra[,4:14]<-sapply(dfextra[,4:14], rescale)

modelvars<-mainvars[complete.cases(mainvars),]

vartable<-rbind(
  rbind(WDIsearch(string="SP.POP.DPND", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="AG.LND.TOTL.K2", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="IT.PRT.NEWS.P3", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="IT.RAD.SETS.P3", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="NY.GDP.PCAP.CD", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="NE.CON.GOVT.ZS", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="IT.PRT.NEWS.P3", field="indicator", short=FALSE)),
  rbind(WDIsearch(string="NV.IND.MANF.ZS", field="indicator", short=FALSE))
)
require(xtable)

#sink("state_vars.tex")
#xtable(vartable[,2:3])
#sink()

#sink("state_vars_summary.tex")
#tableContinuous(allvars[,sapply(allvars, is.numeric)], font.size=12)
#sink()


require(plm)
require(lmtest)

## Simple Standard OLS

model<-plm(spending.wb ~
             lag(trade.wb, 1) +
             lag(polity2, 1) +
             lag(trade.wb, 1):lag(mdi, 1) +
             lag(trade.wb, 1):lag(polity2, 1) +
             lag(gdpcap.wb, 1) +
             lag(dependency.wb, 1) +
             lag(land.wb, 1) +
             lag(spending.wb, 1) +
             lag(spending.wb, 2),
           index = c("scode","year"),
           model="pooling",
           data=modelvars)
summary(model)
coeftest(model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))   #looks good!
fixef(model)
plmtest(model, type=c("bp"))   # Breusch-Pagan Multiplier Test; reject null of no significant differences between panels
pcdtest(model, test = c("lm"))


## Simple Standard OLS w/ Country Fixed Effects
model<-plm(spending.wb ~
             lag(trade.wb, 1) +
             lag(mdi, 1) +
             lag(polity2, 1) +
             lag(trade.wb, 1):lag(polity2, 1) +
             lag(gdpcap.wb, 1) +
             lag(dependency.wb, 1) +
             lag(land.wb, 1) +
             lag(spending.wb, 1),
           data=mainvars,
           index = c("scode","year"),
           model="within",
           effect="twoways")
summary(model)
coeftest(model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))   #looks good!
fixef(model)
plmtest(model, type=c("bp"))   # Breusch-Pagan Multiplier Test; reject null of no significant differences between panels

plmtest(model, c("time"), type=("bp")) # B-P L-M test for time effects; accept that we need time effects
pbgtest(model)                         #Breusch-Godfrey/Woolridge test for serial autocorrelation, failed

# Two-way Fixed Effects 
model.twoway<-plm(spending.wb ~
                    lag(trade.wb, 1) +
                    lag(mdi, 1) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(land.wb, 1) +
                    lag(spending.wb, 1) +
                    lag(spending.wb, 2) +
                    lag(trade.wb, 1):lag(mdi, 1),
                  index = c("scode","year"),
                  model="within",
                  effect="twoway",
                  data=mainvars)
summary(model.twoway)
coeftest(model.twoway, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))  #looks good!
fixef(model.twoway)
plmtest(model.twoway, type=c("bp"))   # Breusch-Pagan Multiplier Test; reject null of no significant differences between panels
plmtest(model.twoway, c("time"), type=("bp")) # B-P L-M test for time effects; accept that we need time effects
pbgtest(model.twoway)                         #Breusch-Godfrey/Woolridge test for serial autocorrelation, failed

data<-plm.data(modelvars, index = c("scode", "year"))
require(tseries)
adf.test(modelvars$spending.wb, k=2)                 #passes, p=0 means it is stationary

# Testing for homoskedasticity of errors
bptest(spending.wb ~
         lag(trade.wb, 1) +
         lag(mdi, 1) +
         lag(trade.wb, 1):lag(mdi, 1) +
         lag(polity2, 1) +
         lag(trade.wb, 1):lag(polity2, 1) +
         lag(gdpcap.wb, 1) +
         lag(dependency.wb, 1) +
         lag(spending.wb, 1) +
         lag(spending.wb, 2),
       data=df,
       studentize=F)                #BP test for homoscedasticity, p=0 means heteroskedastic


# Two-way Fixed Effects with Democracy Interaction
model.dem.twoway<-plm(spending.wb ~
                    lag(trade.wb, 1) +
                    lag(mdi, 1) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(land.wb, 1) +
                    lag(spending.wb, 1) +
                    lag(spending.wb, 2) +
                    lag(trade.wb, 1):lag(mdi, 1) +
                    lag(trade.wb, 1):lag(polity2, 1),
                  data=mainvars,
                  index = c("scode","year"),
                  model="within",
                  effect="twoway")
summary(model.dem.twoway)
coeftest(model.dem.twoway, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))  #looks good!


model.fd.twoway<-plm(diff(spending.wb) ~
                        lag(trade.wb) +
                        lag(diff(trade.wb), 1) +
                        lag(mdi, 1) +
                        lag(diff(mdi), 1) +
                        lag(diff(polity2), 1) +
                        lag(diff(gdpcap.wb), 1) +
                        lag(diff(dependency.wb), 1) +
                        lag(spending.wb, 1) +
                        lag(diff(spending.wb), 1) +
                        lag(trade.wb):lag(mdi) +
                        lag(diff(trade.wb)):lag(diff(mdi)) +
                        lag(trade.wb):lag(polity2) +
                        lag(diff(trade.wb)):lag(diff(polity2)),
                      data=mainvars,
                      index = c("scode","year"),
                      model="within",
                      effect="twoway")
summary(model.fd.twoway)
coeftest(model.fd.twoway, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))  #looks good!


require(estout)

estclear()
eststo(model.twoway)
eststo(model.dem.twoway)
eststo(model.fd.twoway)
esttab(filename="main_state_models_standardized")
estclear()



# plmtest(model.twoway, c("time"), type=("bp")) # # Won't compute bc time effects accounted for
pbgtest(model.twoway)                         #Breusch-Godfrey/Woolridge test for serial autocorrelation, passes with 2 LDVs

data<-plm.data(modelvars, index = c("scode", "year"))
require(tseries)
adf.test(modelvars$spending.wb, k=1)                 #passes, p=0 means it is stationary

# Testing for homoskedasticity of errors
bptest(spending.wb ~
         lag(trade.wb, 1) +
         lag(mdi, 1) +
         lag(trade.wb, 1):lag(mdi, 1) +
         lag(polity2, 1) +
         lag(trade.wb, 1):lag(polity2, 1) +
         lag(gdpcap.wb, 1) +
         lag(dependency.wb, 1) +
         lag(spending.wb, 1) +
         lag(spending.wb, 2),
       data=df,
       studentize=F)                #BP test for homoscedasticity, p=0 means heteroskedastic



coeftest(model.twoway, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))   #looks good!


pr.model<-plm(diff(spending.wb) ~
                lag(trade.wb, 1) +
                lag(diff(trade.wb),1) +
                lag(mdi, 1) +
                lag(diff(mdi),1) +
                lag(polity2, 1) +
                lag(gdpcap.wb, 1) +
                lag(dependency.wb, 1) +
                lag(land.wb, 1) +
                lag(trade.wb, 1):lag(mdi, 1) +
                lag(diff(trade.wb), 1):lag(diff(mdi),1) +
                lag(pr, 1) +
                lag(trade.wb, 1):lag(pr, 1) +
                lag(diff(trade.wb), 1):lag(diff(pr), 1) +
                lag(spending.wb, 1),
              index = c("scode","year"),
              model="within",
              effect="twoway",
              data=dfextra)
summary(pr.model)

coeftest(pr.model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))

unitarism.model<-plm(diff(spending.wb) ~
                       lag(trade.wb, 1) +
                       lag(diff(trade.wb),1) +
                       lag(mdi, 1) +
                       lag(diff(mdi),1) +
                       lag(polity2, 1) +
                       lag(gdpcap.wb, 1) +
                       lag(dependency.wb, 1) +
                       lag(land.wb, 1) +
                       lag(trade.wb, 1):lag(mdi, 1) +
                       lag(diff(trade.wb), 1):lag(diff(mdi),1) +
                       lag(unitarism, 1) +
                       lag(trade.wb, 1):lag(unitarism, 1) +
                       lag(diff(trade.wb), 1):lag(diff(unitarism), 1) +
                       lag(spending.wb, 1),
                     index = c("scode","year"),
                     model="within",
                     effect="twoway",
                     data=dfextra)
summary(unitarism.model)

coeftest(unitarism.model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))

netden.model<-plm(diff(spending.wb) ~
                    lag(trade.wb, 1) +
                    lag(diff(trade.wb),1) +
                    lag(mdi, 1) +
                    lag(diff(mdi),1) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(land.wb, 1) +
                    lag(trade.wb, 1):lag(mdi, 1) +
                    lag(diff(trade.wb), 1):lag(diff(mdi),1) +
                    lag(netden, 1) +
                    lag(trade.wb, 1):lag(netden, 1) +
                    lag(diff(trade.wb), 1):lag(diff(netden), 1) +
                    lag(spending.wb, 1),
                  index = c("scode","year"),
                  model="within",
                  effect="twoway",
                  data=dfextra)
summary(netden.model)

coeftest(netden.model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))

lefts.model<-plm(diff(spending.wb) ~
                    lag(trade.wb, 1) +
                    lag(diff(trade.wb),1) +
                    lag(mdi, 1) +
                    lag(diff(mdi),1) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(land.wb, 1) +
                    lag(trade.wb, 1):lag(mdi, 1) +
                    lag(diff(trade.wb), 1):lag(diff(mdi),1) +
                    lag(lefts, 1) +
                    lag(trade.wb, 1):lag(lefts, 1) +
                    lag(diff(trade.wb), 1):lag(diff(lefts), 1) +
                    lag(spending.wb, 1),
                  index = c("scode","year"),
                  model="within",
                  effect="twoway",
                  data=dfextra)
summary(lefts.model)

coeftest(lefts.model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))

industry.model<-plm(diff(spending.wb) ~
                      lag(trade.wb, 1) +
                      lag(diff(trade.wb),1) +
                      lag(mdi, 1) +
                      lag(diff(mdi),1) +
                      lag(trade.wb, 1):lag(mdi, 1) +
                      lag(diff(trade.wb), 1):lag(diff(mdi),1) +
                      lag(polity2, 1) +
                      lag(gdpcap.wb, 1) +
                      lag(dependency.wb, 1) +
                      lag(land.wb, 1) +
                      lag(diff(trade.wb), 1):lag(diff(mdi),1) +
                      lag(industry.wb, 1) +
                      lag(diff(industry.wb), 1) +
                      lag(industry.wb, 1):lag(mdi, 1) +
                      lag(diff(industry.wb), 1):lag(diff(mdi), 1) +
                      lag(spending.wb, 1),
                    index = c("scode","year"),
                    model="within",
                    effect="twoway",
                    data=dfextra)
summary(industry.model)

coeftest(industry.model, vcov=function(x) vcovBK(x, type="HC1", cluster="time"))

require(estout)
estclear()
eststo(pr.model)
eststo(unitarism.model)
eststo(netden.model)
eststo(lefts.model)
eststo(industry.model)
esttab(filename="rival_models_standard")
estclear()


#require(devtools)
#install_github("ZeligPanelmodels", 'napaxton')
require(Zelig)
require(ZeligPanelmodels)

z.out.fe <- zelig(spending.wb ~
                    lag(trade.wb, 1) +
                    lag(mdi, 1) +
                    lag(trade.wb, 1):lag(mdi, 1) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(spending.wb, 1) +
                    lag(spending.wb, 2),
                  model="pan.plm", 
                  pan.model="within",
                  pan.index=c("scode", "year"),
                  data=mainvars)

x.out<-setx(z.out.fe)
plot(x.out)

trade.r <- 6.32:412.20
mdi.r <- 0:313
x.low <- setx(z.out.fe, trade.wb = 6:32)
x.high <- setx(z.out.fe, trade.wb = 412.2)

s.out.fe <- sim(z.out.fe, x = x.low, x1 = x.high)


require(Amelia)

data.miss<-subset(df, select=c("scode", "year", "spending.wb", "polity2", "trade.wb", "mdi", "gdpcap.wb", "dependency.wb"))

data.miss$year<-as.numeric(data.miss$year)
data.miss$scode<-as.factor(data.miss$scode)
data.miss$gdpcap.wb<-log(data.miss$gdpcap.wb)

data.miss[,4:8]<-sapply(data.miss[,4:8], rescale)


imputed <- amelia(x = data.miss, m = 3, ts = "year", cs = "scode", intercs = TRUE)
plot(imputed)
summary(imputed)


z.out.fe <- zelig(spending.wb ~
                    lag(trade.wb, 1) +
                    lag(mdi, 1) +
                    lag(trade.wb, 1):lag(mdi, 1) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(spending.wb, 1) +
                    lag(spending.wb, 2),
                  model="pan.plm", 
                  pan.model="within",
                  pan.index=c("scode", "year"),
                  data=imputed$imputations)

summary(z.out.fe)

z.out.fe <- zelig(diff(spending.wb) ~
                    lag(trade.wb, 1) +
                    lag(diff(trade.wb), 1) +
                    lag(mdi, 1) +
                    lag(diff(mdi), 1) +
                    lag(trade.wb, 1):lag(mdi, 1) +
                    lag(diff(trade.wb)):lag(diff(mdi)) +
                    lag(polity2, 1) +
                    lag(gdpcap.wb, 1) +
                    lag(dependency.wb, 1) +
                    lag(land.wb, 1) +
                    lag(spending.wb, 1),
                  model="pan.plm", 
                  pan.model="within",
                  pan.index=c("scode", "year"),
                  data=imputed$imputations)

summary(z.out.fe)

x.out<-setx(z.out.fe)
plot(x.out)

trade.r <- 6.32:412.20
mdi.r <- 0:313
x.low <- setx(z.out.fe, trade.wb = 6:32)
x.high <- setx(z.out.fe, trade.wb = 412.2)

s.out.fe <- sim(z.out.fe, x = x.low, x1 = x.high)
























#modelvars<-modelvars[duplicated(modelvars$scode),]
#modelvars<-modelvars[duplicated(modelvars$year),]

#modelvars<-subset(modelvars, modelvars$scode!=366 & modelvars$scode!=620 & modelvars$scode!=781 & modelvars$scode!=365 & modelvars$scode!=694 & modelvars$scode!=317 & modelvars$scode!=372)

codes<-as.data.frame(table(modelvars$scode))
codes$scode<-codes$Var1
modelvars<-merge(modelvars, codes, by="scode")
modelvars<-subset(modelvars, Freq>5)

#library(plm)
pdf<-pdata.frame(modelvars, index=c("scode", "year"), row.names=TRUE) #convert dataframe "pdf" to panel dataframe "panel.data"

head(pdf)   # see first 6 rows

summary(pdf$spending.wb)  #percentage of total variation due to units (countries) and time
#more variation from countries than time

model.fe <- plm(spending.wb~
                  imports + exports + mdi + imports:mdi,
                data=pdf,
                model="within") # fixed-effects

#library(lmtest)
coeftest(model.fe, vcov=vcovBK)

pcse



model.lm<-lm(spending.wb~imports + exports + mdi + imports:mdi, data=modelvars)
library(pcse)
pcse(model.lm, groupN="scode", groupT="year")

ls(model.lm)
NROW(model.lm$residuals)
NROW(model.lm$effects)
NROW(model.lm$fitted.values)
modelvars$fitted.values
source("~/Dropbox/R Code/pcse2.R")
pcse2(model.lm, groupN="scode", groupT="year")

bk<-vcovBK(model.fe, cluster="time")
coeftest(model.fe, vcov=)


library(pcse)
pcse(model.lm, groupN="scode", groupT="year")


### Analysis of fixed effects
fixef(model.fe, type="dmean") #default type = level
summary(fixef(model.fe, type="dmean"))
fixef(model.fe, type="dmean", effect="time") # time fixed-effects rather than country
summary(fixef(model.fe, type="dmean", effect="time"))

df2<-df[complete.cases(df[,2]),]



library(pcse)

lm(diff(df$spending) ~ 
     
     
     source("~/Dropbox/R Code/pcse2.R")
   

   
   
   
   
   
   
   
