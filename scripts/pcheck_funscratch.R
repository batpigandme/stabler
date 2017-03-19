## player check function scratch
player_check <- function(df, pid, pname){
  mutate(df, pname = if_else(grepl(pid, players), 1, 0))
}
df = "home_v_min"

pid = "203521"

pname = "MatthewDellavedova"

pname <- "MatthewDellavedova"

pasted <- paste(df,", ",pname," = if_else(grepl(",pid,", players), 1, 0)", sep = "")

as.name(pasted)

playfun <- function(df, pname, pid){
paste(df,", ",pname," = if_else(grepl(",pid,", players), 1, 0)", sep = "")
}

f2 <- function(df, pname, pid){
  step1 <- paste(df,", ",pname," = if_else(grepl(",pid,", players), 1, 0)", sep = "")
  eval(step1)
}

f2("home_v_min", "MatthewDellavedova", "203521")


thing <- playfun("home_v_min", "MatthewDellavedova", "203521")

thing <- paste(pname," = if_else(grepl(",pid,", players), 1, 0)", sep = "")


test <- home_v_min

headers <- unlist(pname_vec)

test[,pname_vec] <- NA

named_pids[['JabariParker']]

paste(colnames(test[50:67]))

df %>%
  thing <- paste(df,", ",pname," = if_else(grepl(",pid,", players), 1, 0)", sep = "")
  mutate() %>%

test <- player_check(home_v_min, "203953", "JabariParker")



##### define function to get boxscoretraditionalv2#######
get_boxtrad <- function(gameid, startrange, endrange){
  URL1 <-paste("http://stats.nba.com/stats/boxscoretraditionalv2EndPeriod=1&EndRange=",endrange,"&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=",startrange,"", sep = "")
  df<-fromJSON(URL1)
  test <- unlist(df$resultSets$rowSet[[1]])
  test1 <- as.data.frame(test)
  test1[, c(10,11,13,14,16,17,19:28)] <- sapply(test1[, c(10,11,13,14,16,17,19:28)], as.integer)
  test1[, c(12,15,18)] <- sapply(test1[, c(12,15,18)], as.numeric)
  test2 <- tbl_df(test1)
  headers <- unlist(unlist(df$resultSets$headers[[1]]))
  names(test2)[1:28] = c(headers)
  return(test2)
}



## CRUMPLEDJUMPER STUFF ##

#####Find players who started each quarter#####
## set arguments for quarter #1
startrange <- "0"
endrange <- pbp_subs$range_clock1[match("1",pbp_subs$PERIOD)]
boxscore_start_Q1<-get_boxtrad(gameid, startrange, endrange)
starters_Q1<-cbind(boxscore_start_Q1$PLAYER_ID, boxscore_start_Q1$TEAM_ABBREVIATION, boxscore_start_Q1$MIN)
starters_Q1<-subset(starters_Q1,starters_Q1[,2]=="GSW")
starters_Q1<-subset(starters_Q1,starters_Q1[,3]!="0:00")
starters_Q1<-starters_Q1[,1]


## The issue that I had was that, occasionally, this process would produce too many starters, i.e., N>5 for a single team.  For some reason, some of the "starters" of a quarter played a "0:00" stint.  That's why I added the following line:

starters_Q1<-subset(starters_Q1,starters_Q1[,3]!="0:00")



## WTF with paste and parse
playfun <- function(df, pname, pid){
  step1 <- paste("mutate(",df,", ",pname," = if_else(grepl(",pid,", players), 1, 0))", sep = "")
  ## no parse will try to read in file
  parse(step1)}

thing <- playfun("home_v_min", "MatthewDellavedova", "203521")

## messing around with lazyeval and dots
## NSE w dplyr
## source: http://rmhogervorst.nl/cleancode/blog/2016/06/13/NSE_standard_evaluation_dplyr.html
f <- function(col1, col2, new_col_name) {
  mutate_call <- lazyeval::interp(~ a + b, a = as.name(col1), b = as.name(col2))
  mtcars %>% mutate_(.dots = setNames(list(mutate_call), new_col_name))
}

pfun <- function(df, pid, pname) {
  pasted <- paste(df,", ",pname," = if_else(grepl(pid, players), 1, 0)", sep = "")
  call <- as.name(pasted)
  mutate_call <- lazyeval::interp(call)
  mutate_(.dots = setNames(list(mutate_call), pid))
}

pfun(df = df, pid = pid, pname = pname)

## ******* WORKING ******** ##
pfunner <- function(df, pid, pname) {
  mutate_call <- lazyeval::interp(~ if_else(grepl(a, players), 1, 0), a = deparse(as.name(pid)))
  df %>% mutate_(.dots = setNames(list(mutate_call), pname))
}

pfunner(df = df, pid = pid, pname = pname)

test2 <- pfunner(home_v_min, "203521", "MatthewDellavedova")

pid <- "203503"

pname <- "TonySnell"

test3 <- pfunner(home_v_min, pid = pid, pname = pname)
