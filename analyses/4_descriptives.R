library(nnet)
library(ggplot2)
library(apsrtable)
library(reporttools)
library(pastecs)
# writeLines(apsrtable(model1, model2), file("models.tex"))

#################################################################
#  Survey Descriptives ##########################################
#################################################################

causeplot <- 
  ggplot(x, aes(allcauses)) +
  geom_bar() +
  ggtitle("Blame for 2nd Top Problem")
causeplot

infoplot <- 
  ggplot(x, aes(infosource)) +
  geom_bar() +
  ggtitle("Main Source of Information")

ageplot <- 
  ggplot(x, aes(age)) +
  geom_bar() +
  ggtitle("Age Group")

regionplot <- 
  ggplot(x, aes(region)) +
  geom_bar() +
  ggtitle("Region") +
  theme(axis.text.x=element_text(angle = 90))

cityplot <- 
  ggplot(x, aes(city)) +
  geom_bar() +
  ggtitle("City")

collegeplot <- 
  ggplot(x, aes(college)) +
  geom_bar() +
  ggtitle("College")

turnoutplot <- 
  ggplot(x, aes(turnoutint)) +
  geom_bar() +
  ggtitle("Turnout Intention")
turnoutplot

turnout88plot <- 
  ggplot(x, aes(turnout88)) +
  geom_bar() +
  ggtitle("Turnout in 1988")
turnout88plot

leftcandplot <- 
  ggplot(x, aes(leftcand)) +
  geom_bar() +
  ggtitle("Left Candidate")

leftpartyplot <- 
  ggplot(x, aes(leftparty)) +
  geom_bar() +
  ggtitle("Left Party")

pressatplot <- 
  ggplot(x, aes(pressat)) +
  geom_bar() +
  ggtitle("Presidential Satisfaction")

govhandleplot <- 
  ggplot(x, aes(govhandle)) +
  geom_bar() +
  ggtitle("Government Handling")

occplot <- 
  ggplot(x, aes(occ)) +
  geom_bar() +
  ggtitle("White Collar")

prob1plot <- 
  ggplot(x, aes(genprob1)) +
  geom_bar() +
  ggtitle("genprob1")

open1plot <- 
  ggplot(x, aes(openprob1)) +
  geom_bar() +
  ggtitle("openprob1")

prob2plot <- 
  ggplot(x, aes(genprob2)) +
  geom_bar() +
  ggtitle("genprob2")

open2plot <- 
  ggplot(x, aes(openprob2)) +
  geom_bar() +
  ggtitle("openprob2")


#################################################################
#  State Descriptives ###########################################
#################################################################

df$Periods<-ifelse(df$year>=1985, "1985-2010", "1960-1984")
ggplot(df, aes(y=spending.wb, x=trade.wb, color=Periods)) +    
  geom_smooth(method=lm)