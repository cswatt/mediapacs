FEC <- read.csv(file="C:/Users/Ownr/Downloads/FEC_data.csv",header=TRUE,stringsAsFactors=FALSE)

aggdata <- with(FEC,aggregate(TRANSACTION_AMT,
                by=list(EMPLOYER=FEC$EMPLOYER), 
				FUN = function(x) {c(SUM=sum(x),COUNT=length(x))}))
# Total donation amount by employer

write.table(aggdata,"Total.csv",sep=",")				

install.packages("devtools")
install.packages("Rcpp")
install.packages(c("RJSONIO", "knitr", "shiny", "httpuv"))
library(devtools)
library(Rcpp)
install_github('ramnathv/rCharts')
require(rCharts)
devtools::install_github("mages/googleVis")
require(googleVis)
SectorToParty <- read.csv(file="SECTOR_VS_PTY.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
plot(
  gvisSankey(SectorToParty, from="SECTOR", 
             to="CMTE_PTY_AFFILIATION", weight="TRANSACTION_AMT",
             options=list(
               height=250,
               sankey="{link:{color:{fill:'lightblue'}}}"
               ))
)

#SECTOR by POLITICAL PARTY Sankey visualization



IndepenExpen <- read.csv(file="independent_expenditure.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
PurposeTable <- read.csv(file="Purpose.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
MergedSet <- merge(IndepenExpen, PurposeTable, by = "CustomerId",by.x="pur",by.y="purpose")
# Joins the data frames
SampleExp <- MergedSet[,"exp_amo"]
SampleExp <- as.numeric(sub('\\$','',sub(',','',sub(',','',SampleExp))))
# Gets rid of dollar sign & commas in "exp_amo" field
MergedSet[,"exp_amo"] <- SampleExp


SocialMedia <- MergedSet[which(MergedSet$platform=="digital"),]
AmountBySocialMedia <- aggregate(SocialMedia[,"exp_amo"], 
                                 by=list(EMAIL=SocialMedia$email,
								         FACEBOOK=SocialMedia$facebook,
										 GOOGLE=SocialMedia$google,
										 TWITTER=SocialMedia$twitter,
										 SNAPCHAT=SocialMedia$snapchat),FUN=sum)
# Minimum for each social media platform: a single value from this query
AmountBySocialMedia_Email <- aggregate(SocialMedia[,"exp_amo"], 
                                 by=list(EMAIL=SocialMedia$email),FUN=sum)
AmountBySocialMedia_Facebook <- aggregate(SocialMedia[,"exp_amo"], 
                                 by=list(FACEBOOK=SocialMedia$facebook),FUN=sum)
AmountBySocialMedia_Google <- aggregate(SocialMedia[,"exp_amo"], 
                                 by=list(GOOGLE=SocialMedia$google),FUN=sum)
AmountBySocialMedia_Twitter <- aggregate(SocialMedia[,"exp_amo"], 
                                 by=list(TWITTER=SocialMedia$twitter),FUN=sum)
AmountBySocialMedia_Snapchat <- aggregate(SocialMedia[,"exp_amo"], 
                                 by=list(SNAPCHAT=SocialMedia$snapchat),FUN=sum)
# Maximum for each social media platform: sum up all values
sum(MergedSet[which(MergedSet$pay=="Facebook" | MergedSet$pay=="FACEBOOK" | MergedSet$pay=="Facebook - [Memo Item]"),"exp_amo"])
sum(MergedSet[which(MergedSet$pay=="GOOGLE INC"),"exp_amo"])
MergedSet[which(MergedSet$pay=="Facebook" | MergedSet$pay=="FACEBOOK" | MergedSet$pay=="Facebook - [Memo Item]"),"platform"] <- rep("digital",16)
MergedSet[which(MergedSet$pay=="GOOGLE INC"),"platform"] <- rep("digital",5)
TransactionsCountByPlatform <- aggregate(MergedSet[,"exp_amo"], by=list(PLATFORM=MergedSet$platform),FUN=length)
AmountByPlatform <- aggregate(MergedSet[,"exp_amo"], by=list(PLATFORM=MergedSet$platform),FUN=median)

library(ggplot2)
ggplot(TransactionsCountByPlatform,
       aes(x=factor(TransactionsCountByPlatform$PLATFORM),y=TransactionsCountByPlatform$x)) +
	   geom_bar(stat = "identity")+labs(x="Platform",y="Count of Transactions",title="Number of Transactions Per Platform")
ggplot(AmountByPlatform,
       aes(x=factor(AmountByPlatform$PLATFORM),y=AmountByPlatform$x)) + 
	   geom_bar(stat = "identity")+labs(x="Platform",y="Median Amount Spent",title="Median Amount Spent Per Platform")