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

Now we want to determine whether or not a player was on the court for each of the observations. For example, we might create a variable for Giannis Antetokounmpo that looks and sees if his corresponding player ID number is in the `players` column we created earlier.


```r
## testing ways to build on/off matrix
home_v_min$GiannisAntetokounmpo <- ifelse(grepl("203507", home_v_min$players), 1, 0)
```

However, we'll need an equivalent variable for all of the players.

```r
## name pid_vec
named_pids <- pid_vec

names(named_pids) <- pname_vec

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

