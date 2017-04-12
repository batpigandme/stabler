# OKC On/Off Testing
Mara Averick  
`r Sys.Date()`  



## Getting the data


```sql
SELECT * FROM `pbp` WHERE `PLAYER1_TEAM_ID` = 1610612760 ORDER BY `GAME_ID` DESC
```


```sql
SELECT * FROM `games` WHERE `TEAM_ID` = 1610612760 AND `SEASON` = '2016-17' ORDER BY `DATE` ASC 
```


```sql
SELECT * FROM `advanced_boxscores` WHERE `TEAM_ID` = 1610612760 ORDER BY `GAME_ID` DESC 
```

## Read the data in


```r
library(readr)
library(tidyverse)
library(stringr)
```


```r
## read in NBA person IDs
nba_person_ids <- read_csv("~/nba_stats_docs/other/data/nba_personIds.csv")
```


```r
## read in pbp
pbp <- read_csv("data/okc/pbp.csv",
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
games <-  read_csv("data/okc/games.csv")

## read in boxscores
advanced_boxscores <- read_csv("data/okc/advanced_boxscores.csv", col_types = cols(MIN = col_character()))
```

## Isolate 2016-2017 season


```r
## filter games to this season
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
## combo gid vector
combo_gid <- c(away_gid, home_gid)
```

Use the game ids for the home and away games to separate play-by-play data for each, and convert seconds remaining in each quarter to running elapsed-time variables.

We'll also add a `players` column containing the five players for the desired team on the court for any given observation/event number. Note that we are excluding overtime by filtering on `PERIOD <= 4`. 


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
## Observations: 8,811
## Variables: 49
## $ GAME_ID                   <chr> "0021601159", "0021601159", "0021601...
## $ EVENTNUM                  <int> 546, 543, 542, 538, 539, 537, 532, 5...
## $ CORRECTED_EVENTNUM        <int> 452, 449, 448, 445, 444, 443, 439, 4...
## $ EVENTMSGTYPE              <int> 4, 2, 4, 2, 4, 2, 6, 5, 1, 4, 2, 2, ...
## $ EVENTMSGACTIONTYPE        <int> 0, 1, 0, 97, 0, 78, 2, 1, 108, 0, 1,...
## $ PERIOD                    <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
## $ WCTIMESTRING              <time> 22:27:00, 22:27:00, 22:27:00, 22:26...
## $ PCTIMESTRING              <time> 00:27:00, 00:43:00, 00:57:00, 01:09...
## $ SECONDS_REMAINING         <int> 27, 43, 57, 69, 69, 70, 85, 89, 121,...
## $ HOMEDESCRIPTION           <chr> "Collison REBOUND (Off:1 Def:3)", "M...
## $ NEUTRALDESCRIPTION        <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ VISITORDESCRIPTION        <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ SCORE                     <chr> "79 - 110", "79 - 110", "79 - 110", ...
## $ SCOREMARGIN               <int> 31, 31, 31, 31, 31, 31, 32, 32, 34, ...
## $ PERSON1TYPE               <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
## $ PLAYER1_ID                <int> 2555, 202708, 203902, 2555, 2555, 20...
## $ PLAYER1_NAME              <chr> "Nick Collison", "Norris Cole", "Sem...
## $ PLAYER1_TEAM_ID           <int> 1610612760, 1610612760, 1610612760, ...
## $ PLAYER1_TEAM_CITY         <chr> "Oklahoma City", "Oklahoma City", "O...
## $ PLAYER1_TEAM_NICKNAME     <chr> "Thunder", "Thunder", "Thunder", "Th...
## $ PLAYER1_TEAM_ABBREVIATION <chr> "OKC", "OKC", "OKC", "OKC", "OKC", "...
## $ PERSON2TYPE               <int> 0, 0, 0, 0, 0, 0, 5, 5, 4, 0, 0, 0, ...
## $ PLAYER2_ID                <int> 0, 0, 0, 0, 0, 0, 1627748, 1627780, ...
## $ PLAYER2_NAME              <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_ID           <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_CITY         <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_NICKNAME     <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER2_TEAM_ABBREVIATION <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PERSON3TYPE               <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, ...
## $ PLAYER3_ID                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ PLAYER3_NAME              <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_ID           <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_CITY         <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_NICKNAME     <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ PLAYER3_TEAM_ABBREVIATION <chr> "NULL", "NULL", "NULL", "NULL", "NUL...
## $ HOME_PLAYER1              <chr> "202708", "202708", "202708", "20270...
## $ HOME_PLAYER2              <chr> "2555", "2555", "2555", "2555", "255...
## $ HOME_PLAYER3              <chr> "203902", "203902", "203902", "20390...
## $ HOME_PLAYER4              <chr> "203924", "203924", "203924", "20392...
## $ HOME_PLAYER5              <chr> "203926", "203926", "203926", "20392...
## $ VISITOR_PLAYER1           <chr> "1627748", "1627748", "1627748", "16...
## $ VISITOR_PLAYER2           <chr> "1627780", "1627780", "1627780", "16...
## $ VISITOR_PLAYER3           <chr> "201150", "201150", "201150", "20115...
## $ VISITOR_PLAYER4           <chr> "203141", "203141", "203141", "20314...
## $ VISITOR_PLAYER5           <chr> "1626173", "1626173", "1626173", "16...
## $ GAME_NUMERIC              <dbl> 21601159, 21601159, 21601159, 216011...
## $ qsecs_elapsed             <dbl> 693, 677, 663, 651, 651, 650, 635, 6...
## $ tsecs_elapsed             <dbl> 2853, 2837, 2823, 2811, 2811, 2810, ...
## $ players                   <chr> "202708, 2555, 203902, 203924, 20392...
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
## all players from pbp and then reduce to unique
## get 1617 season pids
pids_1617 <- pbp_1617$PLAYER1_ID

pids_1617 <- data_frame(pids_1617)

pids_1617 <- unique(pids_1617)

pids_1617 <- pids_1617 %>%
  rename(pid = pids_1617)

## get player name
pids_1617$player_name <- nba_person_ids[match(pids_1617$pid, nba_person_ids$personId),]$playerName

## create player name column without spaces to use as colname
pids_1617 <- pids_1617 %>%
  mutate(pname = str_replace_all(string = pids_1617$player_name, pattern = " ", replacement = ""))

## remove cases where there is no name
pids_1617 <- filter(pids_1617, !is.na(pname))
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
  filter(GAME_ID == "0021600459")
```

### Adding logical player columns

Now we want to determine whether or not a player was on the court for each of the observations. For example, we might create a variable for Giannis Antetokounmpo that looks and sees if his corresponding player ID number is in the `players` column we created earlier (which, in case you forgot, you can see five rows of, below).


```r
head(home_v_min$players, n = 5)
```

```
## [1] "203924, 203530, 1627734, 203902, 203518"
## [2] "203924, 203530, 1627734, 203902, 203518"
## [3] "203924, 203530, 1627734, 203902, 203518"
## [4] "203924, 203530, 1627734, 203902, 203518"
## [5] "203924, 203530, 1627734, 203902, 203518"
```

We could manually check for each player's `pid` and add variables with their respective (spaceless) names.


```r
## testing ways to build on/off matrix
home_v_min$RussellWestbrook <- ifelse(grepl("201566", home_v_min$players), 1, 0)

home_v_min2 <- home_v_min

home_v_min2$StevenAdams <- if_else(grepl("203500", home_v_min2$players), 1, 0)

home_v_min2 <- home_v_min %>%
  mutate(StevenAdams = if_else(grepl("203500", players), 1, 0))
```

However, that would be repetitive, so we'll make a function to do that for us. 


```r
## this pne actually works
pfunner <- function(df, pid, pname) {
  mutate_call <- lazyeval::interp(~ if_else(grepl(a, players), 1, 0), a = deparse(as.name(pid)))
  df %>% mutate_(.dots = setNames(list(mutate_call), pname))
}
```

A named list will come in handy, so let's build that quickly as well.


```r
## pid list
pid_list <- as.list(pids_1617$pid)

pname_list <- as.list(pids_1617$pname)

## name pid_vec
named_pids <- pid_vec

names(named_pids) <- pname_vec

named_pids
```

```
##       JeremyLamb     NickCollison       SergeIbaka RussellWestbrook 
##           203087             2555           201586           201566 
##      KevinDurant    ReggieJackson      StevenAdams   ThaboSefolosha 
##           201142           202704           203500           200757 
##    AndreRoberson        TajGibson    DougMcDermott    VictorOladipo 
##           203460           201959           203926           203506 
##       EnesKanter    SemajChriston  DomantasSabonis      JeramiGrant 
##           202683           203902          1627734           203924 
##      AlexAbrines      KyleSingler     CameronPayne JoffreyLauvergne 
##           203518           202713          1626166           203530 
##    AnthonyMorrow      JoshHuestis    ErsanIlyasova 
##           201627           203962           101141
```


_Though we don't need to do so for this exercise,_ we _can_ turn `named_pids` into a `tibble` using `enframe()`. 


```r
name_tibble <- enframe(named_pids, name = "pname", value = "pid")
```

Now we can use a for loop to create a column for each player saying whether he was on the court at a given second in a given game. 


```r
## works, using named list of player ids 
## manually passing the name of the game data frame
home_v_min_players <- for (i in 1:length(named_pids)) {
  test4 <- pfunner(home_v_min, pname = names(named_pids[i]), pid = named_pids[[i]])
  home_v_min <- test4
}
```

## All the Games Untidied

Ok, now let's try that for all of the home games, and then for all of the away games.


```r
home_test <- home_pbp
for (i in 1:length(named_pids)) {
  test4 <- pfunner(home_test, pname = names(named_pids[i]), pid = named_pids[[i]])
  home_test <- test4
}

away_test <- away_pbp
for (i in 1:length(named_pids)) {
  test4 <- pfunner(away_test, pname = names(named_pids[i]), pid = named_pids[[i]])
  away_test <- test4
}
```

Because we now have our players identified as variables, we can now bind the away game and home game observations into a single data frame.


```r
combo_pbp <- bind_rows(home_test, away_test)
```

Now we'll make some time variables that will make it easier to fill in the blanks later on.


```r
## get elapsed time for event number
combo_elapse <- combo_pbp %>%
  mutate(duration = tsecs_elapsed - lead(tsecs_elapsed))

## get only the non-negative values
pos_duration <- combo_elapse %>%
  filter(duration > -1)

## within game rank
within_rank <- pos_duration %>%
  group_by(GAME_ID) %>%
  mutate(within_rank = order(GAME_ID, desc(EVENTNUM), decreasing=TRUE))

start_var <- within_rank %>%
  mutate(start_sec = ifelse(within_rank == 1, 0, (lead(tsecs_elapsed) + 1)))

## end var
rev_within_rank <- start_var %>%
  group_by(GAME_ID) %>%
  mutate(rev_win = order(GAME_ID, EVENTNUM, decreasing=TRUE))

end_var <- rev_within_rank %>%
  mutate(end_sec = ifelse(rev_win == 1, 2880, tsecs_elapsed))

## get rid of starts before ends
pos_time <- end_var %>%
  filter(end_sec >= start_sec)

combo_time <- pos_time
for (i in 1:length(named_pids)) {
  test4 <- pfunner(combo_time, pname = names(named_pids[i]), pid = named_pids[[i]])
  combo_time <- test4
}
```

We'll now group the records by `GAME_ID` and arrange them in order by `start_sec`.


```r
combo_time <- combo_time %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, start_sec)
```

We'll also create a data frame containing an observation for each second in a four-quarter NBA game (2,880 seconds, which have be a length of 2,881).


```r
seconds <- c(0:2880)
df <- data.frame(seconds)
```

We'll be working with each second of each game, so we'll create a frame with `x` number of each second, one per game. Because names cannot start with a number, we'll create a new variable that prefaces the game id value with the letter g.


```r
## create variable to be row name (can't start w/ number)
g_combo <- c(paste("g",combo_gid, sep = ""))
named_combo <- combo_gid
## use row-name variable to name combo gid vector
names(named_combo) <- g_combo
game_time <- enframe(named_combo, name = "g_combo", value = "combo_gid")
```

Since we want a record for each second of each game, we'll need to repeat the vector of seconds by the number of games played. In order to keep the game ids in a specified order, we'll create a list.


```r
## repeat seconds variable per number of games for new df
repeated_secs <- rep(c(0:2880), each=length(combo_gid))
repeated_sec2 <- data.frame(repeated_secs)
combo_list <- as.list(combo_gid)
## create game id per seconds (2881, or length of seconds vector)
## note: repeating list retains order
combo_gids <- rep(c(combo_list), times=length(seconds))
combo_gids <- data_frame(combo_gids)
## bind
gid_secs <- bind_cols(repeated_sec2, combo_gids)
```

Now we'll prepare a our data frame so that we can join it with our records from the play-by-play data. Then we can join our play-by-play observations to the matching game id and starting second.


```r
## create df with key cols for joining
gid_secs2 <- gid_secs %>%
  rename(start_sec = repeated_secs) %>%
  rename(GAME_ID = combo_gids) %>%
  mutate(GAME_ID = as.character(GAME_ID))
## left join (keep all for full length) by key cols
gid_secs_pbp <- left_join(gid_secs2, combo_time, by = c("GAME_ID", "start_sec"))
```

Because we don't have an actual record for every single second of each game, the new data frame will have a lot of empty cells/`NA`s. To fill in those observations, we'll use the preceding given value. In order to do so accurately, we'll need to arrange the data frame by game and seconds.


```r
## important to sort by GAME_ID **
gid_sorted_pbp <- gid_secs_pbp %>%
  arrange(GAME_ID, start_sec) %>%
  rename(second = start_sec)
```

Then we can fill in the player values for each second using the `fill()` function from `tidyr`.


```r
## filling previous column value
## note: done by position, so double-check for different setup
gid_sorted_pbp <- fill(gid_sorted_pbp, players:ErsanIlyasova)

write.csv(gid_sorted_pbp, file = "data/okc/combo_gid_sorted_pbp.csv", row.names = FALSE)
```

## Creating a summary frame

Because we want to know the frequency that each player was on the court for a given second, we can create a smaller data frame with just the seconds and spaceless player name (`pname`) variables.


```r
## create vector of desired columns
select_cols <- c("second", pname_vec)
## select those columns for new data frame
player_seconds_df <- select(gid_sorted_pbp, one_of(c(select_cols)))
```

Now we'll summarize our data frame to get the total times that each player was on the court for each second.


```r
player_secs_sum <- player_seconds_df %>%
  group_by(second) %>%
  summarise_all(funs(sum))

write.csv(player_secs_sum, file="data/okc/combo_player_secs_sum.csv", row.names = FALSE)
```

Now we have a data frame with the information we want, but it is not tidy. So, we'll use `gather()` to clean things up.


```r
## note: change depending on number of players
gathered_player_secs <- gather(player_secs_sum, "player", "freq", 2:length(select_cols))
```

We'll also create a summary table with the total number of seconds played by each player, for the purposes of ordering the player variable later on, and add the calculated values to our existing data frames.

```r
## note: change depending on number of players
total_player_secs <- player_secs_sum %>%
  summarise_all(funs(sum)) %>%
  rename(total = second) %>%
  gather("player", "tot_secs", 1:length(select_cols)) %>%
  filter(player != "total") %>%
  arrange(desc(tot_secs))

total_player_secs <- total_player_secs %>%
  mutate(tot_rank = dense_rank(tot_secs))

## add total player secs to gathered
gathered_player_secs$player_tot_secs <- total_player_secs[match(gathered_player_secs$player, total_player_secs$player),]$tot_secs
pids_1617$player_tot_secs <- total_player_secs[match(pids_1617$pname, total_player_secs$player),]$tot_secs
## and rank secs
gathered_player_secs$player_tot_rank <- total_player_secs[match(gathered_player_secs$player, total_player_secs$player),]$tot_rank
pids_1617$player_tot_rank <- total_player_secs[match(pids_1617$pname, total_player_secs$player),]$tot_rank
```

To get the players in their desired order for plotting, we'll create a new, _ordered_ vector of players, which we'll use as our `factor` levels.

```r
ordered_pname <- pids_1617 %>%
  arrange(player_tot_secs)

ordered_pnames <- ordered_pname$pname
```

Now we'll convert the `pname` variable into a factor, using the order built, above.


```r
library(forcats)
gathered_player_secs$player <- factor(gathered_player_secs$player, levels = ordered_pnames)
## check if factor
class(gathered_player_secs$player)
```

```
## [1] "factor"
```



## Plotting the Data


```r
ggplot(gathered_player_secs,aes(x=second,y=player,fill=freq)) +
  geom_tile(alpha = 0.9) +
  scale_fill_gradient(low = "grey90", high = "gold") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  geom_vline(xintercept = 720, color = "white") +
  geom_vline(xintercept = 1440, color = "white") +
  geom_vline(xintercept = 2160, color = "white") +
  ggtitle("Home and Away Games") +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_blank())
```

<img src="okc_regseason_files/figure-html/plot no facets-1.png" width="1000" />

If we wanted to build the plot using facets, instead of players along the y-axis, we'll want to reverse the order of the `player` factor levels.


```r
rev_ordered_pname <- pids_1617 %>%
  arrange(desc(player_tot_secs))

rev_ordered_pnames <- rev_ordered_pname$pname

gathered_player_secs$player <- factor(gathered_player_secs$player, levels = rev_ordered_pnames)
```


```r
## faceted option
ggplot(gathered_player_secs,aes(x=second,y=1,fill=freq)) +
  facet_grid(player~., switch = "y") +
  geom_tile(alpha = 0.9) +
  scale_fill_gradient(low = "grey90", high = "deeppink") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  geom_vline(xintercept = 720, color = "white", size = 0.2) +
  geom_vline(xintercept = 1440, color = "white", size = 0.2) +
  geom_vline(xintercept = 2160, color = "white", size = 0.2) +
  ggtitle("Home and Away Games") +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(strip.text.y = element_text(angle = 180)) +
  theme(strip.background = element_blank()) +
  theme(panel.spacing.y =unit(.02, "lines"))
```

<img src="okc_regseason_files/figure-html/faceted plot-1.png" width="1000" />

We could also restrict the players included in our plot by, say, total seconds played. For example, let's include only players whose total seconds exceed 6,000 (100 minutes).


```r
filtered_psecs <- gathered_player_secs %>%
  filter(player_tot_secs > 6000)

ggplot(filtered_psecs,aes(x=second,y=1,fill=freq)) +
  facet_grid(player~., switch = "y") +
  geom_tile(alpha = 0.9) +
  scale_fill_gradient(low = "grey90", high = "turquoise") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  geom_vline(xintercept = 720, color = "white", size = 0.2) +
  geom_vline(xintercept = 1440, color = "white", size = 0.2) +
  geom_vline(xintercept = 2160, color = "white", size = 0.2) +
  ggtitle("Home and Away Games") +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(strip.text.y = element_text(angle = 180)) +
  theme(strip.background = element_blank()) +
  theme(panel.spacing.y =unit(.02, "lines"))
```

<img src="okc_regseason_files/figure-html/filtered plot-1.png" width="1000" />

If we wanted to focus our visualization on starters vs. non-starters, we could re-sort our player factor by the number of times a player was on at the start of the game (i.e. when zero seconds had elapsed).



We'll filter for players with over 100 minutes, as we did in the previous chart.


```r
ggplot(filtered_psecs,aes(x=second,y=1,fill=freq)) +
  facet_grid(player~., switch = "y") +
  geom_tile(alpha = 0.9) +
  scale_fill_gradient(low = "grey90", high = "mediumblue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  geom_vline(xintercept = 720, color = "white", size = 0.2) +
  geom_vline(xintercept = 1440, color = "white", size = 0.2) +
  geom_vline(xintercept = 2160, color = "white", size = 0.2) +
  ggtitle("Home and Away Games") +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(strip.text.y = element_text(angle = 180)) +
  theme(strip.background = element_blank()) +
  theme(panel.spacing.y =unit(.02, "lines"))
```

<img src="okc_regseason_files/figure-html/fs ordered-1.png" width="1000" />

----

```r
sessionInfo()
```

```
## R version 3.3.3 (2017-03-06)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: macOS Sierra 10.12.4
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] forcats_0.2.0   stringr_1.2.0   dplyr_0.5.0     purrr_0.2.2    
## [5] tidyr_0.6.1     tibble_1.3.0    ggplot2_2.2.1   tidyverse_1.1.1
## [9] readr_1.1.0    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.10     plyr_1.8.4       tools_3.3.3      digest_0.6.12   
##  [5] packrat_0.4.8-1  lubridate_1.6.0  jsonlite_1.4     evaluate_0.10   
##  [9] nlme_3.1-131     gtable_0.2.0     lattice_0.20-35  psych_1.7.3.21  
## [13] DBI_0.6-1        yaml_2.1.14      parallel_3.3.3   haven_1.0.0     
## [17] xml2_1.1.1       httr_1.2.1       knitr_1.15.1     hms_0.3         
## [21] rprojroot_1.2    grid_3.3.3       R6_2.2.0         readxl_0.1.1    
## [25] foreign_0.8-67   rmarkdown_1.4    modelr_0.1.0     reshape2_1.4.2  
## [29] magrittr_1.5     backports_1.0.5  scales_0.4.1     htmltools_0.3.5 
## [33] rvest_0.3.2      assertthat_0.1   mnormt_1.5-5     colorspace_1.3-2
## [37] labeling_0.3     stringi_1.1.5    lazyeval_0.2.0   munsell_0.4.3   
## [41] broom_0.4.2
```

