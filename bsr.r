library(data.table)
library(lattice)
library(grid)
library(gridExtra)

# Code for 2024 GE Matchbacks and Voter lists
# Ballot Results (Matchbacks):
# https://www.sos.wa.gov/elections/data-research/election-data-and-maps/ballot-status-reports
# VRDB voter database:
# https://www.sos.wa.gov/washington-voter-registration-database-extract
# Important: Read all the RCWs and disclaimer on the use of Voting Data.

setwd("F:\\Politics")
# Names <- fread("Whatcom2024.11.06.csv")[,gsub(" ","",names(.SD)))]
# Note Well: I manually edit the fields (Notepad++) to remove spaces and a comma at the end the column names row.
a1 <- fread("Whatcom.2024.11.06.csv")
# a1 <- fread("Whatcom2024-11.06.csv")

a1[,.N,.(Precinct)][order(-N)][1:20]
a1[,.N,.(Precinct)][order(-N)][,barchart(as.factor(Precinct%/%100)~N)]
a1[,.N,.(Precinct)][order(-N)][,barchart(as.factor(Precinct%/%10)~N)]

par(mfrow=c(1,1))

grob1 <- a1[,.N,.(PrecinctGroup=Precinct%/%10)][order(-N)][,
	barchart(as.factor(PrecinctGroup)~N,origin=0,cex=1,main=paste0("Precinct Group '(Decade)' for all Whatcom Ballot Status groups as of 11.06.2024 where N=",nrow(a1)))]
grob2 <- a1[BallotStatus == "Accepted",.N,.(PrecinctGroup=Precinct%/%10)][order(-N)][,
	barchart(as.factor(PrecinctGroup)~N,origin=0,col="navy",cex=1,main=paste0("Precinct Group '(Decade)' for all Whatcom Ballot Status = 'Accepted' on 11.06.2024 where N=",a1[BallotStatus == "Accepted",.N]))]
grob3 <- a1[BallotStatus == "Accepted" | BallotStatus == "Received",.N,.(PrecinctGroup=Precinct%/%10)][order(-N)][,
	barchart(as.factor(PrecinctGroup)~N,origin=0,col="black",cex=1,main=paste0("Precinct Group '(Decade)' for all Whatcom Ballot Status = 'Accepted' or 'Received' on 11.06.2024 where N=",a1[BallotStatus == "Accepted" | BallotStatus == "Received",.N]))]
grob4 <- a1[BallotStatus == "Rejected",.N,.(PrecinctGroup=Precinct%/%10)][order(-N)][,
	barchart(as.factor(PrecinctGroup)~N,origin=0,col="red",cex=1,main=paste0("Precinct Group '(Decade)' for all Whatcom Ballot Status = 'Rejected' on 11.06.2024 where N=",a1[BallotStatus == "Rejected",.N]))]

dev.new();plot.new()
grid.arrange(grob1,grob2,grob3,grob4)
		
a1[,.N,.(BallotStatus)][order(-N)]
a1[,.N,.(Precinct%/%100)][order(-N)]
a1[,.N,.(Gender)][order(-N)]
a1[,.N,.(City)][order(-N)][1:20]
# print("Sketchy metric of unclear value")
a1[,.N,.(VoterID%/%10)][order(-N)][1:20]

a1[BallotStatus == "Accepted",.N,.(Precinct)][order(-N)][1:20]
a1[BallotStatus != "Accepted",.N,.(Precinct)][order(-N)][1:20]
a1[BallotStatus == "Rejected",.N,.(Precinct)][order(-N)][1:20]
a1[BallotStatus == "Rejected",.N,.(Precinct,ChallengeReason)][,dcast(.SD,Precinct ~ ChallengeReason,value.var="N",fun.aggregate=sum)]
a1[BallotStatus=="Accepted",.N,.(Precinct,Gender)][,dcast(.SD,Precinct ~ Gender,value.var="N",fun.aggregate=sum)][order(-(F+M))][,.(Precinct,F,M,Total=(F+M))][order(-Total)][1:30]

# merge recent WM VRDB with most recent Ballot Status Report (matchbacks)
# merging these two datasets is a bit dicey for a number of reasons especially during Election Week with same day registration
# and StatusCode changes (Active and Inactive) that don't keep tracking of re-activated voters now casting ballots.

# Note Well: I manually edit the fields (Notepad++) to remove spaces and a comma at the end the column names row.
m1 <- merge(fread("F:\\Politics\\7892872772wm\\WM.txt"),fread("F:\\Politics\\Whatcom.2024.11.06.csv"),by.x="StateVoterID",by.y="VoterID",all=TRUE)

m1[!is.na(BallotStatus),.N]
m1[BallotStatus != "",.N]
m1[,.N,.(ReturnMethod,BallotStatus)]
m1[is.na(ReturnMethod),.N,.(StatusCode)]
m1[StatusCode == "Inactive",.N]

m1[,.N,.(PrecinctCode%/%100)][order(PrecinctCode)][1:8]
m1[StatusCode == "Active",.N,.(PrecinctCode%/%100)][order(PrecinctCode)]


# Some Charts and Tables
dev.new();plot.new()
m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][,
barchart(N ~ as.factor(Age2024),origin=0,horizontal=FALSE,cex.main=3,main="2024.11.06 Whatcom Accepted ballot matchbacks by Age")]
grid(8,8)


m2 <- merge(m1[BallotStatus != "",.N,.(PrecinctDecade=PrecinctCode%/%10)],
	m1[is.na(BallotStatus),.N,.(PrecinctDecade=PrecinctCode%/%10)],by="PrecinctDecade",suffixes = c(".voted", ".not_voted"))
m2[,pctVoted:=(N.voted/(N.voted+N.not_voted)) * 100]
m2
	
m3 <- merge(m1[BallotStatus != "",.N,.(Age2024= 2024 - Birthyear)],
	m1[is.na(BallotStatus) & StatusCode != "Inactive",.N,.(Age2024= 2024 - Birthyear)],by="Age2024",suffixes = c(".voted", ".not_voted"))
m3[,pctVoted:=(N.voted/(N.voted+N.not_voted)) * 100]

dev.new();plot.new()
m3[,barchart(pctVoted ~ as.factor(Age2024),origin=0,horizontal=FALSE,cex.main=3,main="2024.11.06 Whatcom pct Voted by Age of 2024 GE Active Voters")]
grid(8,8)

# 2024 - 50
m1[BallotStatus == "Accepted" & Birthyear <= 1974,.N]
m1[BallotStatus == "Accepted" & Birthyear >= 1975,.N]

# Age differences:
# 11/06/2024
m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][between(Age2024,18,30),sum(N)]
m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][between(Age2024,78,90),sum(N)]
m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][between(Age2024,18,50),sum(N)]
m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][between(Age2024,51,105),sum(N)]

# Age Groups 18:30 vs. 78:90  Accepted Ballots
BallotComparison <-cbind(m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][between(Age2024,18,30),][order(Age2024)],
	m1[BallotStatus == "Accepted" ,.N,.(Age2024= 2024 - Birthyear)][between(Age2024,78,90)][order(-Age2024)])[
	,setnames(.SD,c("Youth","YouthBallots","Elders","ElderBallots")),]
BallotComparison[,diff.Elder.minus.Youth:=(ElderBallots - YouthBallots)]
BallotComparison
