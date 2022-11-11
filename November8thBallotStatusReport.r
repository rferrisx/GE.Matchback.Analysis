
# For November 10th 2022 Ballot Status Report Data from State:
# https://www.sos.wa.gov/elections/research/2022-general-election.aspx
# initial data.table code
options(prompt = " ") 

library(data.table)
library(lubridate)
# set appropriate paths:
setwd("F:/Politics/ballot status report 2022-11-10 all other counties a-k")
BSR_ak <- fread("Ballot Status Report 2022-11-10 all other counties A-K.csv")
setwd("F:/Politics/ballot status report 2022-11-10 all other counties l-y")
BSR_ly <- fread("Ballot Status Report 2022-11-10 all other counties L-Y.csv")
# rbind small counties
BSR <- rbind(BSR_ak,BSR_ly)
colnames(BSR) <- gsub(" ","",names(BSR))

# Whatcom Total Ballots to date
BSR[County == "Whatcom",.N]

# Whatcom County Ballots by Ballot Status:
BSR[County == "Whatcom",.N,.(BallotStatus)][order(-N)]

# Whatcom County Ballots by ReturnMethod:
BSR[County == "Whatcom",.N,.(ReturnMethod)][order(-N)]

# Whatcom County Ballots by ReturnDate:
BSR[County == "Whatcom",.N,.(Received=as.Date(mdy_hm(ReceivedDate)))][order(-Received)]


# Rejected Ballots

# Rejected Ballots Count
BSR[County == "Whatcom" & BallotStatus == "Rejected",.N]

# Top 20 Precincts with Rejected Ballots by Precinct
BSR[County == "Whatcom" & BallotStatus == "Rejected",
.N,.(Precinct)][order(-N)][1:20]

# Precincts with Rejected Ballots by Challenge Reason
BSR[County == "Whatcom" & BallotStatus == "Rejected",
.N,.(ChallengeReason)][order(-N)]

# Top 20 Precincts with Rejected Ballots by Precinct & Challenge Reason
BSR[County == "Whatcom" & BallotStatus == "Rejected",
.N,.(Precinct,ChallengeReason)][order(-N)][1:20]


# Return Locations

# Top Return Locations Accepted and Received. Blank is most probably mail-in
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.N,.(ReturnLocation)][order(-N)]

# Top Return Locations Accepted only. Blank is most probably mail-in
BSR[County == "Whatcom" & (BallotStatus == "Accepted" ),
.N,.(ReturnLocation)][order(-N)]

# Top Return Locations Received only. Blank is most probably mail-in
BSR[County == "Whatcom" & (BallotStatus == "Received"),
.N,.(ReturnLocation)][order(-N)]

# Precincts

# Top 40 Precincts Accepted or Received 
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.N,.(Precinct)][order(-N)][1:40]

# Top 40 Precincts Accepted only.
BSR[County == "Whatcom" & (BallotStatus == "Accepted" ),
.N,.(Precinct)][order(-N)][1:40]

# Top Precincts Received only.
BSR[County == "Whatcom" & (BallotStatus == "Received"),
.N,.(Precinct)][order(-N)]


# By Precincts and Return Location

# Top 40 aggregated Received and Accepted by Precinct *and* Return Locations.
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.(Precinct,ReturnLocation)][,.N,.(Precinct,ReturnLocation)][order(-N)][1:40]

# By Precinct Decade

# Top aggregated Accepted and Received by Precinct Decade.
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.N,.(PrecinctDecade=as.integer(Precinct)%/%10)][order(-N)]

# Top 40 aggregated Accepted and Received by Precinct Decade *and* Return Locations.
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.(PrecinctDecade=as.integer(Precinct)%/%10,ReturnLocation)][,
.N,.(PrecinctDecade,ReturnLocation)][order(-N)][1:40]

library(lattice)
# lattice charts
dev.new()
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.N,.(PrecinctDecade=as.integer(Precinct)%/%10)][order(PrecinctDecade)][,
barchart(N ~ as.factor(PrecinctDecade),data=.SD,
main="Whatcom County Ballots by Precinct Decade",
xlab="Precinct 'Decades' e.g '10' = summed Precincts numbered 100 : 109",
ylab="Accepted and Received Ballots",origin=0,col=rainbow(nrow(.SD)))]

# Charts

# lattice charts
dev.new()
BSR[County == "Whatcom" & (BallotStatus == "Accepted" | BallotStatus == "Received"),
.N,.(ReturnLocation)][,
barchart(N ~ as.factor(substr(ReturnLocation,1,13)),data=.SD,
main="Whatcom County Ballots by Return Location (Drop Box). Unlabeled first column is probably mail-in ballots.",
xlab="Return Location",
ylab="Accepted and Received Ballots",origin=0,col=rainbow(nrow(.SD)))]

# lattice charts
dev.new()
BSR[(BallotStatus == "Accepted" | BallotStatus == "Received"),
.N,.(County)][order(N)][,
barchart(N ~ as.factor(substr(County,1,13)),data=.SD,
main="WA Small Counties: Accepted and Received Ballots",
xlab="County",
ylab="Accepted and Received Ballots",origin=0,col=rainbow(nrow(.SD)))]

# lattice charts
dev.new()
BSR[(BallotStatus == "Rejected"),
.N,.(County)][order(N)][,
barchart(N ~ as.factor(substr(County,1,13)),data=.SD,
main="WA Small Counties Rejected Ballots",
xlab="County",
ylab="Challenged Ballots",origin=0,col=rainbow(nrow(.SD)))]


# Miscellaneous

BSR[County == "Whatcom",.N,.(ReturnMethod)][order(-N)]
BSR[County == "Whatcom",.N,.(City)][order(-N)][1:40]
BSR[County == "Whatcom",.N,.(Zip)][order(-N)][1:40]

library(lubridate)

# Return Date
BSR[County == "Whatcom",.N,.(Received=as.Date(mdy_hm(ReceivedDate)))][order(-N)]
BSR[County == "Whatcom",.N,.(ReturnMethod,Received=as.Date(mdy_hm(ReceivedDate)))][,
 dcast(.SD,Received ~ ReturnMethod,value.var="N",fun.aggregate=sum)][order(-Received)]

# mean (Sent - Received)
BSR[County == "Whatcom" & ReturnMethod == "Mail",.(Sent=as.Date(mdy_hm(SentDate)), Received=as.Date(mdy_hm(ReceivedDate)))][,.(mean(Received - Sent))]
BSR[County == "Whatcom" & ReturnMethod != "Mail",.(Sent=as.Date(mdy_hm(SentDate)), Received=as.Date(mdy_hm(ReceivedDate)))][,.(mean(Received - Sent))]
BSR[County == "Whatcom",.(Sent=as.Date(mdy_hm(SentDate)), Received=as.Date(mdy_hm(ReceivedDate)))][,.(mean(Received - Sent))]
BSR[County == "Whatcom",.(BallotStatus,Sent=as.Date(mdy_hm(SentDate)), Received=as.Date(mdy_hm(ReceivedDate)))][,.(mean(Received - Sent))]
BSR[County == "Whatcom",.(BallotStatus,Sent=as.Date(mdy_hm(SentDate)), Received=as.Date(mdy_hm(ReceivedDate)))][,.(mean(Received - Sent)),.(BallotStatus)]











