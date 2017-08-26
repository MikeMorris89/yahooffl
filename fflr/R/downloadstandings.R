#' main
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' #' downloadstandings()
#' Team Name ID Streak Type Streak NA   NA   NA NA
#' A-mahr'i Kilpatrick  5           7      6  0 .538 loss  1
#' Leach Daddy  8           9      4  0 .692  win  1
#' zza  2           9      4  0 .692  win  1
#' PAT'S PUNKS  7           7      6  0 .538 loss  1
#' FloJohnsons 10           7      6  0 .538  win  1
#' WDGS  9           7      6  0 .538  win  1
#' Dr Brian  3           5      8  0 .385 loss  1
#' Set RGFree 11           5      8  0 .385  win  1
#' Catfish Jenner  1           7      6  0 .538  win  2
#' Dez-d and Confused 12           6      7  0 .462 loss  2
#' TANK  6           4      9  0 .308 loss  1
#' ManPearBig  4           5      8  0 .385 loss  1

downloadstandings<-function(){
  require(httr)
  require(XML)
  require(httpuv)
# Setup the app
cKey     <- readLines("personal/keys/yahoo_R2.txt", warn=FALSE)[1]
cSecret  <- readLines("personal/keys/yahoo_R2.txt", warn=FALSE)[2]

yahoo    <-oauth_endpoints("yahoo")

creds <- read.table("personal/keys/yahoo_R2.txt", stringsAsFactors=F)
consumer.key <- creds[1,1]
consumer.secret <- creds[2,1]
oauth_endpoints("yahoo")
myapp <- oauth_app("yahoo", key = consumer.key, secret = consumer.secret)
token <- oauth1.0_token(oauth_endpoints("yahoo"), myapp)

myapp <- oauth_app("yahoo", key=cKey, secret=cSecret)
yahoo_token<- oauth2.0_token(yahoo, myapp, cache=T, use_oob = T)
sig <- sign_oauth1.0(myapp, yahoo_token$oauth_token, yahoo_token$oauth_token_secret)
unlink("Fantasy.Rdata")
save(sig,file="Fantasy.Rdata")


baseURL     <- "http://fantasysports.yahooapis.com/fantasy/v2/league/"
# 2016
leagueID    <- "359.l.57539"
# 2015
leagueID    <- "348.l.531904"
standingsURL<-paste(baseURL, leagueID, "/standings", sep="")

page <-GET(standingsURL, config(token = token))
XMLstandings<- content(page, as="text", encoding="utf-8")

doc<-xmlTreeParse(XMLstandings, useInternal=TRUE)
myList<- xmlToList(xmlRoot(doc))


FromMyList<- function(x){
  A<-myList$league$standings$teams[[x]]$name
  B<-myList$league$standings$teams[[x]]$team_id
  C<-unlist(myList$league$standings$teams[[x]]$team_standings$outcome_totals)
  D<-unlist(myList$league$standings$teams[[x]]$team_standings$streak)
  names(A)<-names(B)<-names(C)<-names(D)<-NULL
  c(A,B,C,D)
}

FromMyList

# Combine the data into a dataframe
Standings<- as.data.frame(matrix(unlist(lapply(1:12, function(x) FromMyList(x))),
                                 byrow=T,
                                 ncol=8),
                          row.names = NULL)

# Apply formatting
names(Standings)<- c("Team Name", "ID",  "Streak Type", "Streak")

print(Standings, row.names = FALSE)
}


