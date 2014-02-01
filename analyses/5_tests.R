source('~/Dropbox/Data General/Article1/3_functions.R')

require(effects)
require(car)
require(pcse)

###################################################
#  Survey Analyses ################################
###################################################

          ###################################################
          #  Media Effects on Top Prolem Salience ###########
          ###################################################
model<-glm(openprob1 ~ genprob2 + Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x, family=binomial())
summary(model)                           
plot(effect("media", model), ask=FALSE)

model<-glm(openprob1 ~ genprob2 + Interest + college + media + occ + leftparty + mitterand + Age + gender + urban + media:occ, data=x, family=binomial())
summary(model)                           
plot(effect("media:occ", model), ask=FALSE)  

model<-multinom(genprob1 ~ Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x)
summary(model)       
Anova(model)
plot(effect("media", model), ask=FALSE)

model<-multinom(genprob1 ~ Interest + college + media + occ + leftparty + mitterand + Age + gender + urban + media:occ, data=x)
summary(model)       
Anova(model)
plot(effect("media:occ", model), ask=FALSE)

model<-glm(foreignprob1 ~ Interest + college + media + occ + leftparty + Age + gender + urban, data=x, family=binomial())
summary(model)
model<-glm(econprob1 ~ Interest + college + media + occ + leftparty + Age + gender + urban, data=x, family=binomial())
summary(model)
model<-glm(polprob1 ~ Interest + college + media + occ + leftparty + Age + gender + urban, data=x, family=binomial())
summary(model)
model<-glm(socprob1 ~ Interest + college + media + occ + leftparty + Age + gender + urban, data=x, family=binomial())
summary(model)

          #################################################################
          #  Media Effects on 2nd Prolem Salience #####
          #################################################################
model<-glm(openprob2 ~ Interest + genprob2 + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x, family=binomial())
summary(model)                           
plot(effect("media", model), ask=FALSE)

model<-glm(openprob2 ~ Interest + genprob2 + cause + college + media + occ + leftparty + mitterand + Age + gender + urban + media:occ, data=x, family=binomial())
summary(model)                           
plot(effect("media:occ", model), ask=FALSE, main="Class, Media, and Openness as a Problem", xlab="Media", ylab="Pr(Openness chosen as 2nd Top Problem)")  

model<-multinom(genprob2 ~ Interest + college + media + occ + leftparty + mitterand + Age + gender + urban + media:occ, data=x)
summary(model)       
Anova(model)
plot(effect("media", model), ask=FALSE)

model<-multinom(genprob2 ~ Interest + college + media + occ + leftparty + Age + gender + urban + media:occ, data=x)
summary(model)       
Anova(model)
plot(effect("media:occ", model), ask=FALSE)

model<-glm(foreignprob2 ~ Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x, family=binomial())
summary(model)
model<-glm(econprob2 ~ Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x, family=binomial())
summary(model)
model<-glm(polprob2 ~ Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x, family=binomial())
summary(model)
model<-glm(socialprob2 ~ Interest + college + media + occ + leftparty + mitterand+ Age + gender + urban, data=x, family=binomial())
summary(model)

          ##############################################
          #  Media effects on blame  ###################
          ##############################################
### Effect of media in particular on blaming the government (vs. any non-government target)
options(scipen=999)
model<-zelig(cause.gov ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban,
      model="logit",
      data=x,
      cite=FALSE)
summary(model)

### Interaction of skills and media in particular more likely to blame the government
model<-glm(cause.gov ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban + media:occ, data=x, family=binomial())
summary(model)                           
plot(effect("occ:media", model), ask=FALSE)

#################### Interaction of media and political problem less likely to blame the government
model<-glm(cause.gov ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban + genprob1:media, data=x, family=binomial())
summary(model)                           
plot(effect("occ:media", model), ask=FALSE)

################### Interaction of media and political/economic problem more likely to blame international
model<-glm(cause.intl ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban + genprob1:media, data=x, family=binomial())
summary(model)                           
plot(effect("occ:media", model), ask=FALSE)

### Effect of media in particular on blaming international forces (vs. any other targets)
model<-glm(cause.intl ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban, data=x, family=binomial())
summary(model)                          
plot(effect("media", model), ask=FALSE)

### Interaction of skills and media in particular on blaming international forces (vs. any other) *no effect
model<-glm(cause.intl ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban + media:occ, data=x, family=binomial())
summary(model)                           
plot(effect("occ:media", model), ask=FALSE)

# Effect of media in particular on blaming government vs. international

model<-zelig(causes ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban,
             model="logit",
             data=x,
             cite=FALSE)
summary(model)

model<-glm(causes ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban, data=x, family=binomial())
summary(model)
plot(Effect("media", model), ask=FALSE)

# Effect of media in particular on blaming government vs. international
model<-glm(causes ~ openprob2 + genprob2 + media + Interest + college + occ + leftparty + Age + gender + urban, data=x, family=binomial)
summary(model)
plot(effect("media", model, given.values=(c(genderFemale=1, collegeUniversity=0, genprob2Economic=0, genprob2Foreign=0, genprob2Political=0, Age=3, leftpartyLeft=1, "occWhite collar"=0, urbanurban=0, openprob21=0))), main="Effect of Media on Blame", xlab="Media", ylab="Pr(Blaming international forces rather than government)")
plot(effect("genprob2", model), ask=FALSE, xlab="Media")

# Effect of problem and media type on full range of causes
model<-multinom(allcauses ~ openprob2 + genprob2 + Interest + college + media + occ + leftparty + Age + gender + urban + media:occ, data = x)
Anova(model)
summary(model)
plot(effect("media", model), ask=FALSE)
plot(effect("media:occ", model), ask=FALSE)

# Effect of problem and infosource on range of causes
model<-multinom(cause ~ openprob2 + genprob2 + Interest + college + occ + infosource + leftparty + Age + gender + urban, data = x)
summary(model)
Anova(model)                           
plot(effect("infosource", model), ask=FALSE)

# Effect of problem and media in particular on range of causes
model<-multinom(cause ~ openprob2 + genprob2 + Interest + college + occ + media + leftparty + Age + gender + urban, data = x)
summary(model)
Anova(model)                           
plot(effect("media", model), ask=FALSE)

# Interaction of media and occupation on full range of causes
model<-multinom(cause ~ openprob2 + genprob2 + Interest + college + media + occ + leftparty + Age + gender + urban + media:occ, data = x)
summary(model)
Anova(model)                 #media neutralizes unskilled blame of government relative to skilled  
plot(effect("media:occ", model), ask=FALSE)

          #################################################################
          #  Media Effects on Turnout ####################################
          #################################################################

model<-glm(turnoutint ~ registered+causes+genprob1+openprob2+Interest+media+leftcand+Age+gender+college+causes:openprob2, data=x, family=binomial())
summary(model)                           
plot(effect("causes:openprob2", model), ask=FALSE, ylab="Pr(Turnout Intention)", xlab="Openness as a Problem", main="Effect of Blame And Openness Together on Turnout Intention")

model<-glm(turnoutint ~ turnout88+openprob2+causes+Interest+media+leftparty+occ+urban+openprob2:occ, data=x, family=binomial())
summary(model)                           
plot(effect("openprob2:occ", model), ask=FALSE)

model<-glm(turnoutint ~ turnout88+openprob2+causes+PresSatisfaction+Interest+college+occ+Age+gender+urban, data=x, family=binomial())
summary(model)                           
plot(effect("openprob2:causes", model), ask=FALSE)

model<-glm(turnoutint ~ turnout88 + genprob1 + genprob2 + allcauses + Interest + college + media + occ + leftparty + Age + gender + urban + genprob1:media + genprob2:media, data=x, family=binomial())
summary(model)                           
plot(effect("media:occ", model), ask=FALSE)

model<-glm(turnoutint ~ Registered + genprob1 + genprob2 + cause + Interest + college + media + occ + leftparty + Age + gender + urban + cause:occ, data=x, family=binomial())
summary(model)                           
plot(effect("media:occ", model), ask=FALSE)


          #################################################################
          #  Effects on Evaluation of Government #########################
          #################################################################

model<-lm(PresSatisfaction ~ openprob1 + genprob1 + causes + Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x)
summary(model) 
plot(effect("causes", model), ask=FALSE, main="Effect of Blame on Presidential Satisfaction", xlab="Blame", ylab="Satisfaction with the President (1-4)")
plot(effect("openprob2", model), ask=FALSE, main="Effect of Openness as a Problem on Presidential Satisfaction", xlab="Openness as 2nd Top Problem", ylab="Satisfaction with the President (1-4)")

model<-lm(PresSatisfaction ~ openprob1 + openprob2 + causes + Interest + college + media + occ + leftparty + Age + gender + urban + openprob2:media, data=x)
summary(model)
plot(effect("openprob1", model), ask=FALSE, main="Effect of Blame on Presidential Satisfaction", xlab="Openness as Top Problem", ylab="Satisfaction with the President (1-4)")
plot(effect("openprob2:media", model), ask=FALSE)

model<-lm(PresSatisfaction ~ foreignprob1 + causes + Interest + college + media + occ + leftparty + Age + gender + urban + foreignprob1:media, data=x)
summary(model)
plot(effect("openprob1", model), ask=FALSE, main="Effect of Blame on Presidential Satisfaction", xlab="Openness as Top Problem", ylab="Satisfaction with the President (1-4)")
plot(effect("openprob2:media", model), ask=FALSE)

model<-lm(PresSatisfaction ~ openprob1 + openprob2 + causes + Interest + college + media + occ + leftparty + mitterand + Age + gender + urban + openprob2:causes, data=x)
summary(model)
plot(effect("openprob2:media", model), ask=FALSE)

model<-lm(GovHandling ~ openprob1 + openprob2 + causes + Interest + college + media + occ + leftparty + mitterand + Age + gender + urban, data=x)
summary(model) 
plot(effect("causes", model), ask=FALSE, main="Effect of Blame on Evaluation of Gov't Handling", xlab="Blame", ylab="Evaluation of Gov't Handling (1-4)")
plot(effect("openprob2", model), ask=FALSE, main="Effect of Openness as a Problem on Gov't Handling", xlab="Openness as 2nd Top Problem", ylab="Evaluation of Gov't Handling (1-4)")

model<-glm(votesocialist ~ mitterand + openprob2 + causes + Interest + college + media + occ + Age + gender + urban, data=x, family=binomial())
summary(model)

###################################################
#  State Analyses ################################
###################################################

###########################
##### Main ################
###########################

xtpcse D.Spending D.L.LogTrade D.L.ict D.L.TradeXict D.L.Democracy D.L.LogGDPcap D.LogLand D.L.LogUrban D.L.LogDependency EastAsiaPacific EuropeCentralAsia LatinAmerCarib MidEastNorthAfrica NorthAmerica SouthAsia SSAfrica, correlation(ar1) hetonly pairwise
xtreg D.Spending D.L.LogTrade D.L.ict D.L.TradeXict D.L.Democracy D.L.LogGDPcap D.LogLand D.L.LogUrban D.L.LogDependency OECD, fe

###########################
### Robustness checks  ####
###########################

xtpcse D.Spending D.L.LogTrade D.L.ict D.L.TradeXict D.L.LogGDPcap D.LogLand D.L.LogUrban D.L.LogDependency Geering Fiscal D.L.netden D.L.lefts PRelec checks D.L.manufacturing OECD, correlation(ar1) hetonly pairwise
xtreg D.Spending D.L.LogTrade D.L.ict D.L.TradeXict D.L.LogGDPcap D.LogLand D.L.LogUrban D.L.LogDependency D.L.netden D.L.lefts checks, fe
xtreg D.Spending D.L.LogTrade D.L.ict D.L.TradeXict D.L.LogGDPcap D.LogLand D.L.LogUrban D.L.LogDependency Geering Fiscal elecpr checks



options(scipen=999)
options(digits=4)

simple<-dynformula(Spending ~ LogTrade + ict + LogTrade:ict + Democracy + LogUrban + LogDependency + LogGDPcap + LogLand + region +year, log=list(ict=TRUE, FALSE), lag=list(TRUE, Spending=FALSE, LogLand=TRUE, region=FALSE, year=FALSE))

fd<-dynformula(Spending ~ LogTrade + ict + LogTrade:ict + Democracy + LogUrban + LogDependency + LogGDPcap + region + lag(Spending), log=list(ict=TRUE, FALSE), lag=list(TRUE, Spending=FALSE, region=FALSE), diff=list(region=FALSE, TRUE))

model<-plm(diff(Spending) ~ lag(diff(LogTrade)) + lag(diff(log(ict))) + lag(diff(LogTrade)):lag(diff(log(ict))) + lag(diff(Democracy)) + lag(diff(LogUrban)) + lag(diff(LogDependency)) + lag(diff(LogGDPcap)) + region + as.numeric(year), data=xtdf, model="pooling")

summary(model)

model<-plm(simple, data=xtdf, model="pooling", )
summary(model)
model<-plm(fd, data=xtdf, model="pooling", effect="time")
summary(model)

plot(model$residuals, model$model$Spending)
(model$model$Spending)


state.comp <- y[complete.cases(y[statevars1]),]
state.comp[state.comp[state.comp!=statevars1]]
state.comp$Region<-as.factor(state.comp$year)
model<-lm(Revenue ~ LogTrade + DemAverAge + TradeXDem + LogUrban + LogDependency + LogGDPcap + LogLand + Region,
           data=state.comp,
          na.action="na.exclude"
          )

modelpcse<-pcse(model, groupN=state.comp$countrycode, groupT=state.comp$year, pairwise=TRUE)

fe.mod <- plm(y~x, data=state,index=c("state","time"), model="within")

model<-plm(dynformula(Spending ~ LogTrade + ict + LogTrade:ict + Democracy + LogUrban + LogDependency + LogGDPcap + LogLand + region, diff=list(TRUE, region=FALSE), lag=list(TRUE, Spending=FALSE)), data=xtdf, model="pooling")
summary(model)

model<-plm(dynform, data=xtdf, model="pooling")
summary(model)

model<-plm(diff(Spending) ~ lag(diff(LogTrade))+lag(diff(ict))+lag(diff(LogTrade:ict))+lag(diff(Democracy))+lag(diff(LogGDPcap))+diff(LogLand)+lag(diff(LogUrban))+lag(diff(LogDependency)), data=xtdf, effect="individual")
summary(model)

dynformula(emp ~ wage + capital, log = list(capital = FALSE, TRUE),
+ lag = list(emp = 2, c(2, 3)), diff = list(FALSE, capital = TRUE))
hist(y$ict)

model<-lm(dynform, data=x)
summary(model)

dynform<-dynformula(Spending ~ LogTrade + ict + LogTrade:ict + Democracy + LogUrban + LogDependency + LogGDPcap + LogLand + region, log=list(TRUE), lag=list(TRUE, Spending=FALSE))
dynform

z$MediaDummy<-ifelse(z$LogMediaScale<=1.89, "Media Low", "Media High")
z$MediaDummy<-factor(z$MediaDummy, levels=c("Media Low", "Media High"))

model<-lm(Spending~LogTrade+MediaDummy+LogTrade:MediaDummy+DemAverAge+LogGDPcap+LogLand+LogUrban+LogDependency+EastAsiaPacific+EuropeCentralAsia+LatinAmerCarib+MidEastNorthAfrica+NorthAmerica+SouthAsia+SSAfrica+lag(Spending), data=z)
summary(model)
plot(effect("LogTrade:MediaDummy", model), ask=FALSE)

summary(model)
plot(effect("LogTrade:LogMediaScale", model), ask=FALSE)

model<-plm(diff(Spending) ~ diff(Trade)+diff(Cellphone)+diff(Trade):diff(Cellphone)+diff(DemAverAge)+diff(LogGDPcap)+diff(LogLand)+diff(LogUrban)+diff(LogDependency)+diff(lag(Spending)), data=xtdf, effect="individual")
summary(model)
model<-plm(diff(Spending) ~ diff(Trade)+diff(Banksradios)+diff(Trade):diff(Banksradios)+diff(DemAverAge)+diff(LogGDPcap)+diff(LogLand)+diff(LogUrban)+diff(LogDependency)+diff(lag(Spending)), data=xtdf, effect="individual")
summary(model)
model<-plm(diff(Spending) ~ diff(Trade)+diff(Newspaper)+diff(Trade):diff(Newspaper)+diff(DemAverAge)+diff(LogGDPcap)+diff(LogLand)+diff(LogUrban)+diff(LogDependency)+diff(lag(Spending)), data=xtdf, effect="individual")
summary(model)

model <- plm(Spending ~ LogGDPcap + LogTrade + MediaEnvirons, data=xtdf)
summary(model)

