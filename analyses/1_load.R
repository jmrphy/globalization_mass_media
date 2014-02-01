setwd("~/Dropbox/Data General/Article1")
library(foreign)
x<-read.dta("France_Survey.dta")

setwd("~/Dropbox/Data General/Banks + WB + CHAT")
df<-read.csv("banks_wb_chat.csv")
