# On/Off Testing
Mara Averick  
`r Sys.Date()`  




```r
library(readr)
library(tidyverse)
library(stringr)
```

## Read the data in


```r
## read in NBA person IDs
nba_person_ids <- read_csv("~/nba_stats_docs/other/data/nba_personIds.csv")
```


```r
## read in pbp
pbp <- read_csv("data/pbp.csv",
                col_types = cols(HOME_PLAYER1 = col_character(),
                                 HOME_PLAYER2 = col_character(),
                                 HOME_PLAYER3 = col_character(),
                                 HOME_PLAYER4 = col_character(),
                                 HOME_PLAYER5 = col_character(),
                                 VISITOR_PLAYER1 = col_character(),
                                 VISITOR_PLAYER2 = col_character(),
                                 VISITOR_PLAYER3 = col_character(),
                                 VISITOR_PLAYER4 = col_character(),
                                 VISITOR_PLAYER5 = col_character()))
```


```r
## read in games
games <-  read_csv("data/games.csv")

## read in boxscores
advanced_boxscores <- read_csv("data/advanced_boxscores.csv", col_types = cols(MIN = col_character()))
```

## Isolate 2016-2017 season


```r
## filter games to this season
library(tidyverse)
pbp_1617 <- pbp %>%
  mutate(GAME_NUMERIC = as.numeric(GAME_ID)) %>%
  filter(GAME_NUMERIC >= 21600008) %>%
  filter(GAME_NUMERIC < 41400121)

games_1617 <- games %>%
  mutate(GAME_NUMERIC = as.numeric(GAME_ID)) %>%
  filter(GAME_NUMERIC >= 21600008) %>%
  filter(GAME_NUMERIC < 41400121)

box_1617 <- advanced_boxscores %>%
  mutate(GAME_NUMERIC = as.numeric(GAME_ID)) %>%
  filter(GAME_NUMERIC >= 21600008) %>%
  filter(GAME_NUMERIC < 41400121)
```

## Home vs Away
Because the columns containing the relevant data depend on whether the game was played at home or away, we'll separate those out in order to create a list of team players on the floor.

```r
## home vs away games
home_1617 <- games_1617 %>%
  filter(AT_HOME == 1)

away_1617 <- games_1617 %>%
  filter(AT_HOME == 0)

## home game id and away game id vectors
home_gid <- home_1617$GAME_ID
away_gid <- away_1617$GAME_ID
```

Use the game ids for the home and away games to separate play-by-play data for each, and convert seconds remaining in each quarter to running elapsed-time variables.

We'll also add a `players` column containing the five players for the desierd team on the court for any given observation/event number.


```r
## home play by play
home_pbp <- pbp_1617 %>%
  filter(GAME_ID %in% home_gid) %>%
  filter(PERIOD <= 4) %>%
  mutate(qsecs_elapsed = 720 - SECONDS_REMAINING) %>%
  mutate(tsecs_elapsed = ((PERIOD -1) * 720) + qsecs_elapsed) %>%
  mutate(players = paste(HOME_PLAYER1, HOME_PLAYER2, HOME_PLAYER3, HOME_PLAYER4, HOME_PLAYER5, sep = ", "))

## away play by play
away_pbp <- pbp_1617 %>%
  filter(GAME_ID %in% away_gid) %>%
  filter(PERIOD <= 4) %>%
  mutate(qsecs_elapsed = 720 - SECONDS_REMAINING) %>%
  mutate(tsecs_elapsed = ((PERIOD -1) * 720) + qsecs_elapsed) %>%
  mutate(players = paste(VISITOR_PLAYER1, VISITOR_PLAYER2, VISITOR_PLAYER3, VISITOR_PLAYER4, VISITOR_PLAYER5, sep = ", "))
```

Now we have a data frame with a few more variables.

```r
glimpse(home_pbp)
```

```
## Observations: 7,300
## Variables: 49
## $ GAME_ID                   <chr> "0021600978", "0021600978", "0021600...
## $ EVENTNUM                  <int> 506, 504, 502, 499, 497, 491, 489, 4...
## $ CORRECTED_EVENTNUM        <int> 427, 426, 424, 422, 421, 416, 414, 4...
## $ EVENTMSGTYPE              <int> 3, 3, 4, 3, 3, 3, 3, 4, 2, 4, 2, 5, ...
## $ EVENTMSGACTIONTYPE        <int> 12, 11, 0, 12, 11, 12, 11, 0, 5, 0, ...
## $ PERIOD                    <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
## $ WCTIMESTRING              <time> 22:23:00, 22:23:00, 22:22:00, 22:22...
## $ PCTIMESTRING              <time> 00:10:00, 00:10:00, 00:11:00, 00:18...
## $ SECONDS_REMAINING         <int> 10, 10, 11, 18, 18, 22, 22, 23, 32, ...
## $ HOMEDESCRIPTION           <chr> "Snell Free Throw 2 of 2 (19 PTS)", ...
## $ NEUTRALDESCRIPTION        <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ VISITORDESCRIPTION        <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ SCORE                     <chr> "93 - 102", "93 - 101", "93 - 100", ...
## $ SCOREMARGIN               <int> 9, 8, 7, 7, 6, 5, 4, 4, 4, 4, 4, 4, ...
## $ PERSON1TYPE               <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
## $ PLAYER1_ID                <int> 203503, 203503, 203503, 203114, 2031...
## $ PLAYER1_NAME              <chr> "Tony Snell", "Tony Snell", "Tony Sn...
## $ PLAYER1_TEAM_ID           <int> 1610612749, 1610612749, 1610612749, ...
## $ PLAYER1_TEAM_CITY         <chr> "Milwaukee", "Milwaukee", "Milwaukee...
## $ PLAYER1_TEAM_NICKNAME     <chr> "Bucks", "Bucks", "Bucks", "Bucks", ...
## $ PLAYER1_TEAM_ABBREVIATION <chr> "MIL", "MIL", "MIL", "MIL", "MIL", "...
## $ PERSON2TYPE               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ PLAYER2_ID                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ PLAYER2_NAME              <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_ID           <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_CITY         <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_NICKNAME     <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_ABBREVIATION <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PERSON3TYPE               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ PLAYER3_ID                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ PLAYER3_NAME              <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_ID           <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_CITY         <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_NICKNAME     <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_ABBREVIATION <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ HOME_PLAYER1              <chr> "203114", "203114", "203114", "20311...
## $ HOME_PLAYER2              <chr> "203503", "203503", "203503", "20350...
## $ HOME_PLAYER3              <chr> "202328", "202328", "202328", "20232...
## $ HOME_PLAYER4              <chr> "203507", "203507", "203507", "20350...
## $ HOME_PLAYER5              <chr> "203521", "203521", "203521", "20352...
## $ VISITOR_PLAYER1           <chr> "1626157", "1626157", "1626157", "16...
## $ VISITOR_PLAYER2           <chr> "201937", "201937", "201937", "20193...
## $ VISITOR_PLAYER3           <chr> "202357", "202357", "202357", "20235...
## $ VISITOR_PLAYER4           <chr> "201575", "201575", "201575", "20157...
## $ VISITOR_PLAYER5           <chr> "203952", "203952", "203952", "20395...
## $ GAME_NUMERIC              <dbl> 21600978, 21600978, 21600978, 216009...
## $ qsecs_elapsed             <dbl> 710, 710, 709, 702, 702, 698, 698, 6...
## $ tsecs_elapsed             <dbl> 2870, 2870, 2869, 2862, 2862, 2858, ...
## $ players                   <chr> "203114, 203503, 202328, 203507, 203...
```


## Gathering Observations

One way to approach this, would be to gather the data such that you have an observation for each of the players on the court for a given event.


```r
home_players_pbp <- home_pbp %>%
  gather("player_num", "pid_on", 36:40)

home_players_pbp$player_on <- nba_person_ids[match(home_players_pbp$pid_on, nba_person_ids$personId),]$playerName

away_players_pbp <- away_pbp %>%
  gather("player_num", "pid_on", 41:45)

away_players_pbp$player_on <- nba_person_ids[match(away_players_pbp$pid_on, nba_person_ids$personId),]$playerName
```

Another way to look at this would be as a sort of matrix where there is a logical variable for each player for each observation. To do so, let's take a closer look at player ids.

## Player names and IDs

Isolate list of relevant player/person IDs for this season.


```r
## get 1617 season pids
pids_1617 <- pbp_1617$PLAYER1_ID

pids_1617 <- data_frame(pids_1617)

pids_1617 <- unique(pids_1617)

pids_1617 <- pids_1617 %>%
  rename(pid = pids_1617)

## get player name
pids_1617$player_name <- nba_person_ids[match(pids_1617$pid, nba_person_ids$personId),]$playerName

## create player name column without spaces
pids_1617 <- pids_1617 %>%
  mutate(pname = str_replace_all(string = pids_1617$player_name, pattern = " ", replacement = ""))
```

Build vectors of player IDs and spaceless player names.


```r
pid_vec <- pids_1617$pid

pname_vec <- pids_1617$pname
```

## Test Game

For starters, let's just take a look at a single game, which we can do by filtering on `GAME_ID`. Remember, it's important to note whether the game was home, or away, as that dictates the relevant columns containing player id values.


```r
## home game vs MIN
home_v_min <- home_pbp %>%
  filter(GAME_ID == "0021600978")
```

## Adding logical player columns

Now we want to determine whether or not a player was on the court for each of the observations. For example, we might create a variable for Giannis Antetokounmpo that looks and sees if his corresponding player ID number is in the `players` column we created earlier (which, in case you forgot, you can see five rows of, below).


```r
head(home_v_min$players, n = 5)
```

```
## [1] "203114, 203503, 202328, 203507, 203521"
## [2] "203114, 203503, 202328, 203507, 203521"
## [3] "203114, 203503, 202328, 203507, 203521"
## [4] "203114, 203503, 202328, 203507, 203521"
## [5] "203114, 203503, 202328, 203507, 203521"
```



```r
## testing ways to build on/off matrix
home_v_min$GiannisAntetokounmpo <- ifelse(grepl("203507", home_v_min$players), 1, 0)

home_v_min2 <- home_v_min

home_v_min2$MatthewDellavedova <- if_else(grepl("203521", home_v_min2$players), 1, 0)

home_v_min2 <- home_v_min %>%
  mutate(MatthewDellavedova = if_else(grepl("203521", players), 1, 0))
```


```r
## nope uses "pname" as actual colname
player_on_off <- function(df, pid, pname){
  pof <- ifelse(grepl(pid, df$players), 1, 0)
  pof_df <- data_frame(pof)
  new_df <- bind_cols(df, pof_df)
  new_df <- rename(new_df, pname = pof)
}

## this pne actually works
pfunner <- function(df, pid, pname) {
  mutate_call <- lazyeval::interp(~ if_else(grepl(a, players), 1, 0), a = deparse(as.name(pid)))
  df %>% mutate_(.dots = setNames(list(mutate_call), pname))
}
```

However, we'll need an equivalent variable for all of the players.


```r
## pid list
pid_list <- as.list(pids_1617$pid)

pname_list <- as.list(pids_1617$pname)

## name pid_vec
named_pids <- pid_vec

names(named_pids) <- pname_vec

names(named_pids[1])
```

```
## [1] "GiannisAntetokounmpo"
```

```r
named_pids
```

```
## GiannisAntetokounmpo       MalcolmBrogdon   MatthewDellavedova 
##               203507              1627763               203521 
##            TonySnell           GregMonroe           JohnHenson 
##               203503               202328               203089 
##       MirzaTeletovic           JasonTerry       KhrisMiddleton 
##               203141                 1891               203114 
##            ThonMaker         RashadVaughn         SpencerHawes 
##              1627748              1626173               201150 
##        TerrenceJones          AxelToupane       MichaelBeasley 
##               203093              1626253               201563 
##         JabariParker         MilesPlumlee           SteveNovak 
##               203953               203101               200779
```




```r
names(named_pids)
```

```
##  [1] "GiannisAntetokounmpo" "MalcolmBrogdon"       "MatthewDellavedova"  
##  [4] "TonySnell"            "GregMonroe"           "JohnHenson"          
##  [7] "MirzaTeletovic"       "JasonTerry"           "KhrisMiddleton"      
## [10] "ThonMaker"            "RashadVaughn"         "SpencerHawes"        
## [13] "TerrenceJones"        "AxelToupane"          "MichaelBeasley"      
## [16] "JabariParker"         "MilesPlumlee"         "SteveNovak"
```

```r
name_tibble <- enframe(named_pids, name = "pname", value = "pid")
```


```r
## df_list <- as.list(rep(home_v_min), each=18)

## args2 <- list(pid = pid_list, pname = pname_list)

## works, using named list of player ids 
## manually passing the name of the game data frame
output <- for (i in 1:length(named_pids)) {
  test4 <- pfunner(home_v_min, pname = names(named_pids[i]), pid = named_pids[[i]])
  home_v_min <- test4
}

test3 <- pfunner(home_v_min, pname = names(named_pids[2]), pid = named_pids[[2]])
```


```r
home_test <- home_pbp
for (i in 1:length(named_pids)) {
  test4 <- pfunner(home_test, pname = names(named_pids[i]), pid = named_pids[[i]])
  home_test <- test4
}
```

----

```r
sessionInfo()
```

```
## R version 3.3.3 (2017-03-06)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: macOS Sierra 10.12.3
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] stringr_1.2.0   dplyr_0.5.0     purrr_0.2.2     tidyr_0.6.1    
## [5] tibble_1.2      ggplot2_2.2.1   tidyverse_1.1.1 readr_1.0.0    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.9        plyr_1.8.4         forcats_0.2.0     
##  [4] tools_3.3.3        digest_0.6.12      packrat_0.4.8-1   
##  [7] lubridate_1.6.0    jsonlite_1.3       evaluate_0.10     
## [10] nlme_3.1-131       gtable_0.2.0       lattice_0.20-34   
## [13] psych_1.6.12       DBI_0.6            yaml_2.1.14       
## [16] parallel_3.3.3     haven_1.0.0        xml2_1.1.1        
## [19] httr_1.2.1         knitr_1.15.1       hms_0.3           
## [22] rprojroot_1.2      grid_3.3.3         R6_2.2.0          
## [25] readxl_0.1.1       foreign_0.8-67     rmarkdown_1.3.9004
## [28] modelr_0.1.0       reshape2_1.4.2     magrittr_1.5      
## [31] backports_1.0.5    scales_0.4.1       htmltools_0.3.5   
## [34] rvest_0.3.2        assertthat_0.1     mnormt_1.5-5      
## [37] colorspace_1.3-2   stringi_1.1.2      lazyeval_0.2.0    
## [40] munsell_0.4.3      broom_0.4.2
```

