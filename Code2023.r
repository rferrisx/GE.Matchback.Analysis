# 2023 Gen Ballot matchbacks
library(data.table)
library(lattice)
setwd("F:/Elections")
# Data from https://www.sos.wa.gov/elections/data-research/2023-general-election
# Column names don't work without adjustment for fread()
col_names <- c("BallotID","VoterID","County","FirstName","LastName","Gender","Election","BallotStatus","ChallengeReason","SentDate","ReceivedDate","Address","City","State","Zip","Country","Split","Precinct","ReturnMethod","ReturnLocation","Party","V1")
# Binds all counties
All <- rbind(
fread("F:/Elections/Ballot Status Report 2023-11-15 All Other Counties.csv",col.names=col_names),
fread("Ballot Status Report 2023-11-15 PI SN SP TH.csv",col.names=col_names),
fread("Ballot Status Report 2023-11-15 KI.csv",col.names=col_names))

# Summary information for All Counties
All[,.N,.(duplicated(BallotID))]
All[,.N,.(duplicated(VoterID))]

All[,.N,.(ReturnMethod)][order(-N)]
All[,.N,.(BallotStatus)] [order(-N)]
All[BallotStatus == "Rejected",.N,.(ChallengeReason)] [order(-N)]
All[BallotStatus == "Rejected",.N,.(City,ChallengeReason)][order(-N)][1:10]
All[BallotStatus == "Rejected",.N,.(County,ChallengeReason)][order(-N)][1:10]

All[BallotStatus == "Rejected",.N,.(City)][order(-N)][1:10]
All[BallotStatus == "Rejected",.N,.(County)][order(-N)][1:10]
All[BallotStatus == "Rejected" & ChallengeReason == "Signature Does Not Match",.N,.(City,ChallengeReason)][order(-N)][1:5]
All[BallotStatus == "Rejected" & ChallengeReason == "Signature Does Not Match",.N,.(County,ChallengeReason)][order(-N)][1:5]
All[BallotStatus == "Rejected" & ChallengeReason == "Unsigned",.N,.(City,ChallengeReason)][order(-N)][1:5]
All[BallotStatus == "Rejected" & ChallengeReason == "Unsigned",.N,.(County,ChallengeReason)][order(-N)][1:10]

# Percentage of Challenged Ballots for all Counties and Select Cities (GTR 40K Ballots) and Counties (GTR 80K Ballots)
merge(All[BallotStatus == "Rejected",.N,.(County)][,.(Rejected=N,County)],All[,.(TotalBallots=length(BallotStatus)),.(County)],by="County")[,.SD[,.(RejectPCT=round(Rejected/TotalBallots * 100,2))],.(County,Rejected,TotalBallots)]
RejectedDensity <- All[,.(Rejected=sum(BallotStatus == "Rejected"),Voters=length(VoterID)),.(City)];RejectedDensity[,RejectedPCT:=(Rejected/Voters) * 100];RejectedDensity[Voters > 40000,][order(-RejectedPCT)] 
RejectedDensity <- All[,.(Rejected=sum(BallotStatus == "Rejected"),Voters=length(VoterID)),.(County)];RejectedDensity[,RejectedPCT:=(Rejected/Voters) * 100];RejectedDensity[Voters > 80000,][order(-RejectedPCT)]


# Whatcom County only summary information  
Small.Whatcom <- fread("F:/Elections/Ballot Status Report 2023-11-15 All Other Counties.csv",col.names=col_names)[County == "Whatcom",]
Small.Whatcom[,.N,.(ReturnMethod)][order(-N)]
Small.Whatcom[,.N,.(BallotStatus)] [order(-N)]
Small.Whatcom[BallotStatus == "Rejected",.N,.(ChallengeReason)] [order(-N)]
Small.Whatcom[,.N,.(City)][order(-N)][1:10]
Small.Whatcom[,.N,.(City)][order(-N)][1:10]
Small.Whatcom[,.N,.(City)][order(-N)][1:20]
Small.Whatcom[,.N,.(Zip)][order(-N)][1:20]
Small.Whatcom[,.N,.(Precinct)][order(-N)][1:20]
Small.Whatcom[,.N,.(Precinct)][order(-N)][1:40]
Small.Whatcom[,.N,.(PrecinctDecade=as.integer(Precinct)%/%10)][order(-N)][1:22]

 All[,.N]
 All[BallotStatus == "Rejected",.N]
 Small.Whatcom[,.N]
 Small.Whatcom[BallotStatus == "Rejected",.N]

 # Top 10 Challenge Reasons Statewide 
 All[BallotStatus == "Rejected",.N,.(ChallengeReason)] [order(-N)][1:10]

 # Top Challenge Reasons for Whatcom
 Small.Whatcom[BallotStatus == "Rejected",.N,.(ChallengeReason)] [order(-N)]
 
 # Top 10 Challenge Reasons Statewide 
 All[BallotStatus == "Rejected",.N,.(ChallengeReason)] [order(-N)][1:10]

 # Top Challenge Reasons for Whatcom
 Small.Whatcom[BallotStatus == "Rejected",.N,.(ChallengeReason)] [order(-N)]
 
 # Rejected Ballots by Percentage For Cities with Voters GTR 40000
 RejectedDensity <- All[,.(Rejected=sum(BallotStatus == "Rejected"),Voters=length(VoterID)),.(City)]
 RejectedDensity[,RejectedPCT:=(Rejected/Voters) * 100]
 RejectedDensity[Voters > 40000,][order(-RejectedPCT)] 

 # Rejected Ballots by Percentage For Counties with Voters GTR 80000
 RejectedDensity <- All[,.(Rejected=sum(BallotStatus == "Rejected"),Voters=length(VoterID)),.(County)]
 RejectedDensity[,RejectedPCT:=(Rejected/Voters) * 100]
 RejectedDensity[Voters > 80000,][order(-RejectedPCT)]
 
 
  merge(All[ChallengeReason !="",.N,.(County,ChallengeReason)][,
	dcast(.SD,County ~ ChallengeReason ,value.var="N",fun.aggregate=sum)][,c(1,20)],
	All[,.N,.(County)],by="County")[,setnames(.SD, c("County","Unsigned","Ballots"))][,
	.SD[,.(pctUnsigned=round((Unsigned/Ballots) * 100,3))],.(County,Unsigned,Ballots)][order(-Ballots)]
  
 merge(All[ChallengeReason !="",.N,.(County,ChallengeReason)][,
	dcast(.SD,County ~ ChallengeReason ,value.var="N",fun.aggregate=sum)][,c(1,19)],
	All[,.N,.(County)],by="County")[,setnames(.SD, c("County","TooLate","Ballots"))][,
	.SD[,.(pctTooLate=round((TooLate/Ballots) * 100,3))],.(County,TooLate,Ballots)][order(-Ballots)]
	
 merge(All[ChallengeReason !="",.N,.(County,ChallengeReason)][,
	dcast(.SD,County ~ ChallengeReason ,value.var="N",fun.aggregate=sum)][,c(1,18)],
	All[,.N,.(County)],by="County")[,setnames(.SD, c("County","SignatureNotMatch","Ballots"))][,
	.SD[,.(pctSigNotMatch=round((SignatureNotMatch/Ballots) * 100,3))],.(County,SignatureNotMatch,Ballots)][order(-Ballots)]
	
m1 <-	
 merge(
  merge(All[ChallengeReason !="",.N,.(County,ChallengeReason)][,
	dcast(.SD,County ~ ChallengeReason ,value.var="N",fun.aggregate=sum)][,c(1,20)],
	All[,.N,.(County)],by="County")[,setnames(.SD, c("County","Unsigned","Ballots"))][,
	.SD[,.(pctUnsigned=round((Unsigned/Ballots) * 100,3))],.(County,Unsigned,Ballots)][order(-Ballots)],
  
 merge(All[ChallengeReason !="",.N,.(County,ChallengeReason)][,
	dcast(.SD,County ~ ChallengeReason ,value.var="N",fun.aggregate=sum)][,c(1,19)],
	All[,.N,.(County)],by="County")[,setnames(.SD, c("County","TooLate","Ballots"))][,
	.SD[,.(pctTooLate=round((TooLate/Ballots) * 100,3))],.(County,TooLate,Ballots)][order(-Ballots)], by=c("County","Ballots"))
	
 m2 <- merge(m1,
 merge(All[ChallengeReason !="",.N,.(County,ChallengeReason)][,
	dcast(.SD,County ~ ChallengeReason ,value.var="N",fun.aggregate=sum)][,c(1,18)],
	All[,.N,.(County)],by="County")[,setnames(.SD, c("County","SignatureNotMatch","Ballots"))][,
	.SD[,.(pctSigNotMatch=round((SignatureNotMatch/Ballots) * 100,3))],.(County,SignatureNotMatch,Ballots)][order(-Ballots)], by=c("County","Ballots"))

m3 <- m2[,.(County,Ballots,Unsigned,TooLate,SignatureNotMatch,pctUnsigned,pctTooLate,pctSigNotMatch)][order(-Ballots)]
m3

dev.new();plot.new()
m3[County == "King",barchart(~ TooLate+Unsigned+SignatureNotMatch | County,main="King County",origin=0,stack=TRUE,lwd=3,auto.key=TRUE)]

dev.new();plot.new()
m3[Ballots > 50000 & County != "King",barchart(~ TooLate+Unsigned+SignatureNotMatch | County,main="Total Ballots GTR 50,000 but not 'King County'",origin=0,stack=TRUE,lwd=3,auto.key=TRUE)]

dev.new();plot.new()
m3[between(Ballots,25000,50000),barchart(~ TooLate+Unsigned+SignatureNotMatch | County,main="Total Ballots BTW 25K - 50K",origin=0,stack=TRUE,lwd=3,auto.key=TRUE)]

dev.new();plot.new()
m3[Ballots < 25000,barchart(~ TooLate+Unsigned+SignatureNotMatch | County,main="Total Ballots LT 25,000",origin=0,stack=TRUE,lwd=3,auto.key=TRUE)]
	
 

setwd("F:/Elections")
# Code to pair Precinct Returns (from Whatcom County) with Matchback Totals
fread("20221108_whatcomprecincts.csv")[,setnames(.SD,c("Race","Status","Precinct1","Precinct2","Votes"))][,unique(Race)]
fread("20221108_whatcomprecincts.csv")[,setnames(.SD,c("Race","Candidate","Precinct1","Precinct2","Votes"))][
	Race == "LEGISLATIVE DISTRICT 40 - State Representative Pos. 1" | Race == "LEGISLATIVE DISTRICT 42 - State Representative Pos. 1",][
	!grepl("Total",Precinct1) & Candidate != "WRITE-IN",dcast(.SD,Precinct1~Candidate,value.var="Votes",fun.aggregate=sum)]

merge(
fread("20221108_whatcomprecincts.csv")[,setnames(.SD,c("Race","Candidate","Precinct","Precinct2","Votes"))][
	Race == "LEGISLATIVE DISTRICT 40 - State Representative Pos. 1" | Race == "LEGISLATIVE DISTRICT 42 - State Representative Pos. 1",][
	!grepl("Total",Precinct) & Candidate != "WRITE-IN",dcast(.SD,Precinct~Candidate,value.var="Votes",fun.aggregate=sum)],
	
fread("F:/Elections/Ballot Status Report 2023-11-15 All Other Counties.csv",col.names=col_names)[County == "Whatcom",][,.N,.(Precinct)], by="Precinct")[,
setnames(.SD,c("Precinct","Alicia.Rule","Debra.Lekanoff","Shannon.Perkes","Tawsha.Dykstra.Thompson","x2023BallotsToDate"))]
# [order(-x2023BallotsToDate)][1:40]

m2022wmb2023 <- merge(
fread("20221108_whatcomprecincts.csv")[,setnames(.SD,c("Race","Candidate","Precinct","Precinct2","Votes"))][
	Race == "LEGISLATIVE DISTRICT 40 - State Representative Pos. 1" | Race == "LEGISLATIVE DISTRICT 42 - State Representative Pos. 1",][
	!grepl("Total",Precinct) & Candidate != "WRITE-IN",dcast(.SD,Precinct~Candidate,value.var="Votes",fun.aggregate=sum)],
	
fread("F:/Elections/Ballot Status Report 2023-11-15 All Other Counties.csv",col.names=col_names)[County == "Whatcom",][,.N,.(Precinct)], by="Precinct")[,
setnames(.SD,c("Precinct","Alicia.Rule","Debra.Lekanoff","Shannon.Perkes","Tawsha.Dykstra.Thompson","x2023BallotsToDate"))]

m2022wmb2023[Alicia.Rule > 1 | Tawsha.Dykstra.Thompson > 1,
	Dpct:=Alicia.Rule/(Alicia.Rule + Tawsha.Dykstra.Thompson)];
m2022wmb2023[Debra.Lekanoff > 1 | Shannon.Perkes > 1,
	Dpct:=Debra.Lekanoff/(Debra.Lekanoff + Shannon.Perkes)]
m2022wmb2023[,DVotes:= Dpct * x2023BallotsToDate]
m2022wmb2023[,RVotes:= (1 - Dpct) * x2023BallotsToDate]
m2022wmb2023[,.(TotalDVotes=sum(DVotes),TotalRVotes=sum(RVotes))]
m2022wmb2023

# Speculative
dev.new();plot.new()
All[,.N,.(VoterID1E3=as.numeric(VoterID)%/%1000)][order(-N)][N > 100,][order(VoterID1E3)][,xyplot(N ~ VoterID1E3)]
dev.new();plot.new()
All[,.N,.(BallotID1E4=as.numeric(VoterID)%/%10000)][order(-N)][N > 1000,][order(BallotID1E4)][,xyplot(N ~ BallotID1E4)]








