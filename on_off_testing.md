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

We could manually check for each player's `pid` and add variables with their respective (spaceless) names.


```r
## testing ways to build on/off matrix
home_v_min$GiannisAntetokounmpo <- ifelse(grepl("203507", home_v_min$players), 1, 0)

home_v_min2 <- home_v_min

home_v_min2$MatthewDellavedova <- if_else(grepl("203521", home_v_min2$players), 1, 0)

home_v_min2 <- home_v_min %>%
  mutate(MatthewDellavedova = if_else(grepl("203521", players), 1, 0))
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
##           JohnHenson       KhrisMiddleton            TonySnell 
##               203089               203114               203503 
##       MalcolmBrogdon GiannisAntetokounmpo           JasonTerry 
##              1627763               203507                 1891 
##   MatthewDellavedova           GregMonroe       MirzaTeletovic 
##               203521               202328               203141 
##            ThonMaker         RashadVaughn         SpencerHawes 
##              1627748              1626173               201150 
##        TerrenceJones          AxelToupane       MichaelBeasley 
##               203093              1626253               201563 
##         JabariParker         MilesPlumlee           SteveNovak 
##               203953               203101               200779
```


We can turn `named_pids` into a `tibble` using `enframe()`. 


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
combo_pbp <- bind_rows(away_test, home_test)
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

We'll also create a data frame containing an observation for each second in a four-quarter NBA game (2,880 seconds).

```r
seconds <- c(0:2880)
df <- data.frame(seconds)
```

Because we'll be working with each second of each game, we'll combine the two. Because names cannot start with a number, we'll create a new variable that prefaces the game id value with the letter g.


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

Because we don't have an actual record for every single second of each game, the new data frame will have a lot of empty cells. To fill in those observations, we'll use the preceding given value. In order to do so accurately, we'll need to arrange the data frame by game and seconds.

```r
## important to sort by GAME_ID **
gid_sorted_pbp <- gid_secs_pbp %>%
  arrange(GAME_ID, start_sec) %>%
  rename(second = start_sec)
```

Then we can fill in the player values for each second.
## Note to self: turn into loop ##

```r
## filling previous column value
gid_sorted_pbp <- fill(gid_sorted_pbp, GiannisAntetokounmpo)
gid_sorted_pbp <- fill(gid_sorted_pbp, MalcolmBrogdon)
gid_sorted_pbp <- fill(gid_sorted_pbp, MatthewDellavedova)
gid_sorted_pbp <- fill(gid_sorted_pbp, TonySnell)
gid_sorted_pbp <- fill(gid_sorted_pbp, GregMonroe)
gid_sorted_pbp <- fill(gid_sorted_pbp, JohnHenson)
gid_sorted_pbp <- fill(gid_sorted_pbp, MirzaTeletovic)
gid_sorted_pbp <- fill(gid_sorted_pbp, JasonTerry)
gid_sorted_pbp <- fill(gid_sorted_pbp, KhrisMiddleton)
gid_sorted_pbp <- fill(gid_sorted_pbp, ThonMaker)
gid_sorted_pbp <- fill(gid_sorted_pbp, RashadVaughn)
gid_sorted_pbp <- fill(gid_sorted_pbp, SpencerHawes)
gid_sorted_pbp <- fill(gid_sorted_pbp, TerrenceJones)
gid_sorted_pbp <- fill(gid_sorted_pbp, AxelToupane)
gid_sorted_pbp <- fill(gid_sorted_pbp, MichaelBeasley)
gid_sorted_pbp <- fill(gid_sorted_pbp, JabariParker)
gid_sorted_pbp <- fill(gid_sorted_pbp, MilesPlumlee)
gid_sorted_pbp <- fill(gid_sorted_pbp, SteveNovak)

write.csv(gid_sorted_pbp, file = "data/combo_gid_sorted_pbp.csv", row.names = FALSE)
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

write.csv(player_secs_sum, file="data/combo_player_secs_sum.csv", row.names = FALSE)
```

Now we have a data frame with the information we want, but it is not tidy. So, we'll use `gather()` to clean things up.


```r
gathered_player_secs <- gather(player_secs_sum, "player", "freq", 2:19)
```

We'll also create a summary table with the total number of seconds played by each player, for the purposes of ordering the player variable later on, and add the calculated values to our existing data frames.

```r
total_player_secs <- player_secs_sum %>%
  summarise_all(funs(sum)) %>%
  rename(total = second) %>%
  gather("player", "tot_secs", 1:19) %>%
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

<img src="on_off_testing_files/figure-html/plot no facets-1.png" width="1000" />

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

<img src="on_off_testing_files/figure-html/faceted plot-1.png" width="1000" />

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
## [1] forcats_0.2.0   stringr_1.2.0   dplyr_0.5.0     purrr_0.2.2    
## [5] tidyr_0.6.1     tibble_1.2      ggplot2_2.2.1   tidyverse_1.1.1
## [9] readr_1.0.0    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.9        plyr_1.8.4         tools_3.3.3       
##  [4] digest_0.6.12      packrat_0.4.8-1    lubridate_1.6.0   
##  [7] jsonlite_1.3       evaluate_0.10      nlme_3.1-131      
## [10] gtable_0.2.0       lattice_0.20-34    psych_1.6.12      
## [13] DBI_0.6            yaml_2.1.14        parallel_3.3.3    
## [16] haven_1.0.0        xml2_1.1.1         httr_1.2.1        
## [19] knitr_1.15.1       hms_0.3            rprojroot_1.2     
## [22] grid_3.3.3         R6_2.2.0           readxl_0.1.1      
## [25] foreign_0.8-67     rmarkdown_1.3.9004 modelr_0.1.0      
## [28] reshape2_1.4.2     magrittr_1.5       backports_1.0.5   
## [31] scales_0.4.1       htmltools_0.3.5    rvest_0.3.2       
## [34] assertthat_0.1     mnormt_1.5-5       colorspace_1.3-2  
## [37] labeling_0.3       stringi_1.1.2      lazyeval_0.2.0    
## [40] munsell_0.4.3      broom_0.4.2
```

