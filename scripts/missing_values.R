## get elapsed time for event number
away_elpase <- away_pbp %>%
  mutate(duration = tsecs_elapsed - lead(tsecs_elapsed))

## get only the non-negative values
pos_duration <- away_elpase %>%
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

away_time <- pos_time
  for (i in 1:length(named_pids)) {
  test4 <- pfunner(away_time, pname = names(named_pids[i]), pid = named_pids[[i]])
  away_time <- test4
}

away_time <- away_time %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, start_sec)

seconds <- c(1:2880)
df <- data.frame(seconds)

test5 <- data.frame(seconds, x = "x")

thing2 <- c(paste("g",away_gid, sep = ""))
library(tidyverse)
named_away <- away_gid
names(named_away) <- thing2
game_time <- enframe(named_away, name = "thing2", value = "away_gid")

repeated_secs <- rep(c(0:2880), each=31)
repeated_sec2 <- data.frame(repeated_secs)
agid_list <- as.list(away_gid)
away_gids <- rep(c(agid_list), times=2881)
away_gids <- data_frame(away_gids)
gid_secs <- bind_cols(repeated_sec2, away_gids)

## now do mutating join (maybe merge?) with away_test
## then fill in values

gid_secs2 <- gid_secs %>%
  rename(start_sec = repeated_secs) %>%
  rename(GAME_ID = away_gids) %>%
  mutate(GAME_ID = as.character(GAME_ID))

gid_secs_pbp <- left_join(gid_secs2, away_time, by = c("GAME_ID", "start_sec"))

gid_sorted_pbp <- arrange(gid_secs_pbp, GAME_ID, start_sec)

gid_sorted_pbp[[55]]

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

write.csv(gid_sorted_pbp, file = "data/gid_sorted_pbp.csv", row.names = FALSE)

colnames(gid_sorted_pbp[55:72])

select_cols <- c("start_sec", pname_vec)
player_seconds_df <- select(gid_sorted_pbp, one_of(c(select_cols)))

## get sum of players 1, 0 for each second
player_secs_sum <- player_seconds_df %>%
  group_by(start_sec) %>%
  summarise_all(funs(sum))

write.csv(player_secs_sum, file="data/player_secs_sum.csv", row.names = FALSE)
