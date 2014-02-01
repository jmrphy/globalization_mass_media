library(memisc)
library(plm)
#####################################
#  Survey Recodings   ###############
#####################################
x$registered<-x$q2
x$registered<-factor(x$registered, levels=c("no", "yes"))
x$Registered<-factor(x$registered, labels=c("Unregistered", "Registered"))

x$gender<-x$q3
x$gender<-factor(x$gender, labels=c("Male", "Female"))

x$Age<-as.numeric(x$q5)

x$region<-x$q44

x$urban<-x$q45
x$urban<-recode(x$urban, c("100,000 +", "paris agglomeration") -> "Urban", otherwise="Not Urban")
x$urban<-factor(x$urban, levels=c("Not Urban", "Urban"))

x$q39<-factor(x$q39)
x$college<-recode(x$q39, "No university"<-c("none", "primary", "middle school", "technical", "secondary-bac", "secondary-tech"),
                           "University"<-c("some university", "university plus"))
x$college<-factor(x$college, levels=c("No university", "University"))

x$turnoutint<-ifelse(x$q11=="yes", "Yes", "No")
x$turnoutint<-as.factor(x$turnoutint)

x$turnout88<-recode(x$q37, "did not vote"->"no", "too young to vote"->NA, "no response"->NA, NA->NA, otherwise="yes")
x$turnout88<-factor(x$turnout88)

x$leftcand<-recode(x$q13, "Left"<-c("extreme left", "communist party", "socialist party", "ecologist"), "Not left"<-c("udf", "rpr", "front national"), otherwise=NA)
x$leftcand<-factor(x$leftcand, levels=c("Not left", "Left"))

x$leftparty<-recode(x$q36, "Left"<-c("extreme left", "communists", "socialists", "generation ecologie", "greens"), "Not left"<-c("udf", "rpr", "front national"), otherwise=NA)
x$leftparty<-factor(x$leftparty, levels=c("Not left", "Left"))
x$leftparty<-factor(x$leftparty, labels=c("Not Left Party", "Left Party"))

x$votesocialist<-recode(x$q13, "Socialist"<-"socialist party", "Not Socialist"<-c("extreme left", "communist party", "ecologist", "udf", "rpr", "front national", "other", otherwise=NA))
x$votesocialist<-factor(x$votesocialist, levels=c("Not Socialist", "Socialist"))

x$mitterand<-ifelse(x$q37=="mitterand", "Mitterand", "Other")
x$mitterand<-as.factor(x$mitterand)
x$mitterand<-factor(x$mitterand, levels=c("Other", "Mitterand"))

x$q19[x$q19=="no response"]<-NA
x$interestcat<-factor(x$q19)
x$interestcat<-factor(x$interestcat, levels=c("not at all", "only a little", "somewhat interested", "very interested"))
x$interestcat<-ordered(x$interestcat)
x$Interest<-as.numeric(x$interestcat)

x$pressatcat<-factor(x$q20, levels=c("completely dissatisfied", "somewhat dissatisfied", "somewhat satisfied", "completely satisfied"))
x$PresSatisfaction<-as.numeric(x$pressatcat)

x$govhandle<-factor(x$q33, levels=c("completely dissatisfied", "somewhat dissatisfied", "somewhat satisfied", "completely satisfied"))
x$GovHandling<-as.numeric(x$govhandle)

# Occupation #####################
# Summary: Occupation refers to the respondent, except when the respondent gives no response,
# in which case the head of household occupation is used.

x$occc<-ifelse(x$q6toq7=="farmer" & x$q8=="yes", "Not white collar", "Missing")
x$occc<-ifelse(x$q6toq7=="blue collar worker" & x$q8=="yes", "Not white collar", x$occc)
x$occc<-ifelse(x$q6toq7=="retired" & x$q8=="yes", "Not white collar", x$occc)
x$occc<-ifelse(x$q6toq7=="white collar" & x$q8=="yes", "White collar", x$occc)
x$occc<-ifelse(x$q6toq7=="professional" & x$q8=="yes", "White collar", x$occc)
x$occc<-ifelse(x$q6toq7=="shopkeeper" & x$q8=="yes", "Not white collar", x$occc)
x$occc<-ifelse(x$q6toq7=="clerical" & x$q8=="yes", "Not white collar", x$occc)
x$occc<-ifelse(x$q6toq7=="no response" & x$q8=="yes", "Missing", x$occc)

x$occc<-ifelse(x$q9toq10=="farmer" & x$q8=="no", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="blue collar worker" & x$q8=="no", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="retired" & x$q8=="no", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="white collar" & x$q8=="no", "White collar", x$occc)
x$occc<-ifelse(x$q9toq10=="professional" & x$q8=="no", "White collar", x$occc)
x$occc<-ifelse(x$q9toq10=="shopkeeper" & x$q8=="no", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="clerical" & x$q8=="no", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="no response" & x$q8=="no", "Missing", x$occc)

x$occc<-ifelse(x$q9toq10=="farmer" & x$occc=="Missing", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="blue collar worker" & x$occc=="Missing", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="retired" & x$occc=="Missing", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="white collar" & x$occc=="Missing", "White collar", x$occc)
x$occc<-ifelse(x$q9toq10=="professional" & x$occc=="Missing", "White collar", x$occc)
x$occc<-ifelse(x$q9toq10=="shopkeeper" & x$occc=="Missing", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="clerical" & x$occc=="Missing", "Not white collar", x$occc)
x$occc<-ifelse(x$q9toq10=="no response" & x$occc=="Missing", "Missing", x$occc)
x$occ<-factor(x$occc, levels=c("Not white collar", "White collar"))
x$occ<-factor(x$occ, labels=c("Not White Collar", "White Collar"))
#  Media  ###########################

x$infosource<-recode(x$q15, "family"->"family", "friends"->"friends", "opinion leaders"->"opinion leaders", "the media"->"the media",
                             otherwise=NA)
x$media<-ifelse(x$infosource=="the media", "Media", "Other")
x$media<-factor(x$media, levels=c("Other", "Media"))
x$tv<-ifelse(x$q15=="the media" & x$q16=="tv", "TV", "Other")
x$tv<-factor(x$tv)
x$radio<-ifelse(x$q15=="the media" & x$q16=="radio", "Radio", "Other")
x$radio<-factor(x$radio)
x$newspapers<-ifelse(x$q15=="the media" & x$q16=="newspapers", "Newspapers", "Other")
x$newspapers<-factor(x$newspapers, levels=c("Other", "Newspapers"))
x$magazines<-ifelse(x$q15=="the media" & x$q16=="magazines", "Magazines", "Other")
x$magazines<-factor(x$magazines, levels=c("Other", "Magazines"))

#  Perceived Problems  ##############

x$openprob1<-ifelse(x$q24=="intl econ compttn" | x$q24=="ec-92" | x$q24=="foreign trade" |
  x$q25=="maastricht referendum" | x$q26=="maastricht treaty", 1, 0)
x$openprob1<-factor(x$openprob1)
x$openprob2<-ifelse(x$q29=="intl econ compttn" | x$q29=="ec-92" | x$q29=="foreign trade" |
  x$q30=="maastricht referendum" | x$q31=="maastricht treaty", 1, 0)
x$openprob2<-factor(x$openprob2)
x$openprob2<-factor(x$openprob2, labels=c("Problem:Not Openness", "Problem:Openness"))

x$genprob1<-x$q22
x$genprob1[x$q22=="other" | x$q22=="none" | x$q22=="no response"]<-NA
x$genprob1<-factor(x$genprob1, levels=c("economic issues", "social issues", "political issues", "foreign affairs"))
x$genprob1<-factor(x$genprob1, labels=c("Economic", "Social", "Political", "Foreign"))
x$genprob1<-factor(x$genprob1, levels=c("Economic", "Social", "Political", "Foreign"))
x$genprob1<-factor(x$genprob1, labels=c("Problem:Economic", "Problem:Social", "Problem:Political", "Problem:Foreign"))


x$genprob2<-x$q27
x$genprob2[x$q27=="other" | x$q27=="none" | x$q27=="no response"]<-NA
x$genprob2<-factor(x$genprob2)
x$genprob2<-factor(x$genprob2, levels=c("economic issues", "social issues", "political issues", "foreign affairs"))
x$genprob2<-factor(x$genprob2, labels=c("Problem:Economic", "Problem:Social", "Problem:Political", "Problem:Foreign"))

x$econprob1<-ifelse(x$genprob1=="Problem:Economic", "Problem:Economic", "Other")
x$econprob1<-factor(x$econprob1, levels=c("Other", "Problem:Economic"))
x$socialprob1<-ifelse(x$genprob1=="Problem:Social", "Problem:Social", "Other")
x$socialprob1<-factor(x$socialprob1, levels=c("Other", "Problem:Social"))
x$foreignprob1<-ifelse(x$genprob1=="Problem:Foreign", "Problem:Foreign", "Other")
x$foreignprob1<-factor(x$foreignprob1)
x$polprob1<-ifelse(x$genprob1=="Problem:Political", "Problem:Political", "Other")
x$polprob1<-factor(x$polprob1)

x$econprob2<-ifelse(x$genprob2=="Problem:Economic", "Problem:Economic", "Other")
x$socialprob2<-ifelse(x$genprob2=="Problem:Social", "Problem:Social", "Other")
x$socialprob2<-as.factor(x$socialprob2)
x$foreignprob2<-ifelse(x$genprob2=="Problem:Foreign", "Problem:Foreign", "Other")
x$foreignprob2<-factor(x$foreignprob2)
x$polprob2<-ifelse(x$genprob2=="Problem:Political", "Problem:Political", "Other")
x$polprob2<-factor(x$polprob2)


#  Causes  ##########################

x$q32[x$q32=="no response"]<-NA
x$allcauses<-factor(x$q32)

x$cause<-factor(x$q32)
x$cause<-recode(x$cause, c("society", "other", "personal choices")->"other", otherwise="copy")

x$cause.gov<-ifelse(x$q32=="government", "government", "not government")
x$cause.gov<-factor(x$cause.gov, levels=c("not government", "government"))
x$cause.intl<-ifelse(x$q32=="outside forces", "international", "not international")
x$cause.intl<-factor(x$cause.intl, levels=c("not international", "international"))

x$causes<-factor(x$q32, levels=c("government", "outside forces"))
x$causes<-factor(x$causes, labels=c("Blame Government", "Blame International"))

x$blame<-as.numeric(x$causes) 
x$open<-as.numeric(x$openprob2)
x$OpennessXBlame<-x$open * x$blame


#####################################
#  State Data Cleaning   ###########
#####################################

#########################
### Merge Polity ########
#########################
setwd("~/Dropbox/Data General/Polity IV")
polity<-read.csv("polityiv.csv") # read in the Polity IV dataset
polity$scode<-as.factor(polity$ccode)   
dem<-subset(polity, select=c("scode", "year", "democ", "autoc", "polity2")) # make subset of key variables
merged3<-merge(df, dem, by=c("scode", "year"))  

df<-subset(merged3, year>=1960 & year<=2010)

############################################
### Gerring and Thacker Unitarism and PR ###
############################################
setwd("~/Dropbox/Data General/Gerring & Thacker Centripetalism")
gt<-read.csv("Gerring_Thacker.csv")
names(gt)
#gt$scode<-countrycode(gt$Country, "country.name", "cown")
#gt$scode<-as.factor(gt$scode)
gt$year<-gt$Year
gt$code<-as.factor(gt$Banks)

#merged4<-merge(merged3, gt, by=c("scode", "year"))
merged4<-merge(merged3, gt, by=c("code", "year"))
merged4$pr<-merged4$PR
merged4$unitarism<-merged4$Unit2


############################################
### Swank Comparative Parties ##############
############################################
require(countrycode)
setwd("~/Dropbox/Data General/Swank Comparative Parties")
swank<-read.csv("Swank_Comparative_Parties.csv")
swank$scode<-countrycode(swank$countrycode, "iso3c", "cown")
swank$scode<-as.factor(swank$scode)
swank$year<-swank$YEAR
swank$lefts<-swank$LEFTS
swank$lefts[swank$lefts==-999]<-NA
swank<-subset(swank, select=c("scode", "year", "lefts"))

dfex<-merge(merged4, swank, by=c("scode", "year"), all.x=TRUE)


############################################
### Golden et al. Union Data  ##############
############################################

setwd("~/Dropbox/Data General/Golden Union Data")
union<-read.csv("Golden_Lange_Wallerstein_Union_Data_OECD_1950_2000.csv")
names(union)
union$country<-ifelse(union$country=="ARL", "AUS", paste(union$country))
union$country<-ifelse(union$country=="GER", "DEU", paste(union$country))
union$country<-as.factor(union$country)
union$scode<-countrycode(union$country, "iso3c", "cown")
union$scode<-as.factor(union$scode)


union<-subset(union, select=c("scode", "year", "netden"))
dfextra<-merge(dfex, union, by=c("scode", "year"), all.x=TRUE)

rm(merged3, merged4, union, swank, gt, polity, dem, dfex)



