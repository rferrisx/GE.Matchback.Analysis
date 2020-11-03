# R 4.02
#install.packages("x") below if needed

library(data.table)
library(lubridate)
library(lattice)
fsum <- function(x) {sum(x,na.rm=TRUE)}

# apply and receive current VRDB from https://www.sos.wa.gov/elections/vrdb/default.aspx
# setwd("D:/Politics/General.2020/")
# OctVDB <- fread("202010_VRDB_Extract.txt",fill=TRUE)
# setkey(OctVDB,StateVoterID)
# OctVDB <- fread("202010_VRDB_Extract.txt")
# # source("D:/Politics/General.2020/BallotReturnStats/BallotReturnAnalysis.r",print.eval=TRUE)

# unzip all the files you want row bound .... to this directory
setwd("D:/Politics/General.2020/BallotReturnStats/11.02.2020")

l1 <- as.data.table({})
for(i in dir()) {l1 <- rbind(l1,fread(i),fill=TRUE)}
colnames(l1) <- l1[,gsub(" ","",names(.SD))]
print("All Ballots:")
l1[,.N]
print("All Status Returns by County:")
l1[,.N,.(County)]
print("By Return Status:")
l1[,.N,.(BallotStatus)][order(-N)]
print("By Challenge Reason:")
l1[,.N,.(ChallengeReason)][order(-N)]

# Ballot Status
print("By Ballot Status by County with Rejected PCT Rejected/(Received + Accepted) * 100")
#l1[,.(County),by="BallotStatus"]
lbs <- l1[,.(County),by="BallotStatus"][
,.N,.(BallotStatus,County)][
,dcast(.SD,County ~ BallotStatus,value.var="N",fun.aggregate=fsum)][County != "WA",][
,.SD[,.(RejectPCT=round(Rejected/(Accepted+Received),4) * 100)],
.(County,Accepted,Received,Rejected)]
lbs[]
lbs[order(-Accepted)]

#Challenge Return
print("Some Top Challenge Returns by County:")
lcr <- l1[,.(County),by="ChallengeReason"][,.N,.(ChallengeReason,County)][
,dcast(.SD,County ~ ChallengeReason,value.var="N",fun.aggregate=fsum)][,
c("County","V1","Signature Does Not Match","Review","Unsigned","HOLD","ID Required","Other than Voter","Hold")][County != "WA",]
colnames(lcr) <- lcr[,gsub(" ","",names(.SD))]
setnames(lcr,"V1","Accepted.Returned")
lcr[]

# Top Counties for Turnout Percent 
# needs OctVDB
print("Merges October 7th votedb with current ballot status list. Thus later registrations are not part of the calculus here.")	
print("Please see https://www.sos.wa.gov/elections/research/ballot-return-statistics.aspx for up to date percentages and turnout.")
OctVDB$StateVoterID <- as.integer(OctVDB$StateVoterID)
l1$VoterID <- as.integer(l1$VoterID)
OctVDB$StateVoterID <- as.integer(OctVDB$StateVoterID)
t1 <- merge(l1[,.(VoterID,BallotStatus)],OctVDB[,.(StateVoterID,CountyCode,PrecinctCode)],all.x=TRUE,by.x="VoterID",by.y="StateVoterID")[BallotStatus !="",][,.N,keyby=.(CountyCode,PrecinctCode)]
t2 <- OctVDB[,.N,.(CountyCode,PrecinctCode)]
t3 <- merge(t1,t2,all=TRUE,by=c("CountyCode","PrecinctCode"))
t4 <- t3[,.(sumBallotsRet=fsum(N.x),sumOctoberActive=fsum(N.y)),keyby="CountyCode"][,.SD[,.(TO.pct=round(sumBallotsRet/sumOctoberActive,3) * 100)],.(CountyCode,sumBallotsRet,sumOctoberActive)][order(-TO.pct)]
print("Top 5 Counties by Turnout Pct")
t4[order(-sumOctoberActive)][1:5][order(-TO.pct)]
print("Top 10 Counties by Turnout Pct")
t4[order(-sumOctoberActive)][1:10][order(-TO.pct)]

# Blue vs Red County Totals
BlueCountiesGE2018 <- c("Thurston","Clark","Clallam","Pierce","Kitsap","King","Snohomish","Island","Skagit","Whatcom","San Juan","Whitman","Jefferson","Pacific")
BlueCountyCodeGE2018 <- c("TH","CR","CM","PI","KP","KI","SN","IS","SK","WM","SJ","WT","JE","PA")
print("Blue vs. Red County Totals. Use 2018 Senate Race as county (blue/red) marker")
print("See https://komonews.com/news/local/washington-state-election-results-interactive-map for D & R WA county divisions")
setnames(
as.data.table(
cbind(Categories=c("2020GE.BlueCountyBallots","2020GE.NotBlueCountyBallots","OctVDB.Blue.Active","OctVDB.NotBlue.Active"),
rbind(l1[County %in% BlueCountiesGE2018,.N],
l1[!County %in% BlueCountiesGE2018,.N],
OctVDB[StatusCode == "Active" & CountyCode %in% BlueCountyCodeGE2018,.N],
OctVDB[StatusCode == "Active" & !CountyCode %in% BlueCountyCodeGE2018,.N]))),c("Categories","Count"))[]

# Historical "LastVoted" profile
print("Last Voted Profile of Ballots to inference low propensity voter participation:")
OctVDB$StateVoterID <- as.numeric(OctVDB$StateVoterID)
l1$VoterID <- as.numeric(l1$VoterID)
LastVoted <- merge(l1[,.(VoterID,BallotStatus)],
OctVDB[,.(StateVoterID,LastVoted)],
all.x=TRUE,by.x="VoterID",by.y="StateVoterID")[
BallotStatus !="",][,.N,.(LastVoted)][order(-N)]

tsum <- LastVoted[,sum(N)]
LastVoted[1:20][order(-LastVoted)]

ssum <- 
cbind(Category=c("Total.Ballots","PCT.Total.Ballots"),
rbind(
cbind(
LastVoted[year(ymd(LastVoted)) >= 2019,.(LastVoted2019orLater=sum(N))],
LastVoted[year(ymd(LastVoted)) <= 2018,.(LastVoted2018orEarlier=sum(N))],
LastVoted[year(ymd(LastVoted)) <= 2016,.(LastVoted2016orEarlier=sum(N))],
LastVoted[year(ymd(LastVoted)) <= 2012,.(LastVoted2012orEarlier=sum(N))]),

cbind(
LastVoted[year(ymd(LastVoted)) >= 2019,.(LastVoted2019orLater=round(sum(N)/tsum,3) * 100)],
LastVoted[year(ymd(LastVoted)) <= 2018,.(LastVoted2018orEarlier=round(sum(N)/tsum,3) * 100)],
LastVoted[year(ymd(LastVoted)) <= 2016,.(LastVoted2016orEarlier=round(sum(N)/tsum,3) * 100)],
LastVoted[year(ymd(LastVoted)) <= 2012,.(LastVoted2012orEarlier=round(sum(N)/tsum,3)* 100)])))

t(ssum)

# Location Top 20s
print("Top 20 aggregate counts for County,City,Zip,c(City,Zip),c(County,Zip).")
cbind(
l1[,.N,.(County)][order(-N)][1:20],
l1[,.N,.(City)][order(-N)][1:20],
l1[,.N,.(Zip)][order(-N)][1:20],
l1[,.N,.(City,Zip)][order(-N)][1:20],
l1[,.N,.(County,Zip)][order(-N)][1:20])

