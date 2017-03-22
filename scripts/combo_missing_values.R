## combine home and away dfs
## * IMPORTANT you've done `players` column before combining **
combo_pbp <- bind_rows(away_test, home_test)

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

combo_time <- combo_time %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, start_sec)

seconds <- c(0:2880)
df <- data.frame(seconds)


library(tidyverse)
## join away and home gid vectors
combo_gid <- c(away_gid, home_gid)
## create variable to be row name (can't start w/ number)
g_combo <- c(paste("g",combo_gid, sep = ""))
named_combo <- combo_gid
## use row-name variable to name combo gid vector
names(named_combo) <- g_combo
game_time <- enframe(named_combo, name = "g_combo", value = "combo_gid")

## repeat seconds variable per number of games for new df
repeated_secs <- rep(c(0:2880), each=length(combo_gid))
repeated_sec2 <- data.frame(repeated_secs)
combo_list <- as.list(combo_gid)
## create game id per seconds (2881)
combo_gids <- rep(c(combo_list), times=2881)
combo_gids <- data_frame(combo_gids)
## bind cols
gid_secs <- bind_cols(repeated_sec2, combo_gids)

## now do mutating join (maybe merge?) with away_test
## then fill in values
## create df with key cols for joining
gid_secs2 <- gid_secs %>%
  rename(start_sec = repeated_secs) %>%
  rename(GAME_ID = combo_gids) %>%
  mutate(GAME_ID = as.character(GAME_ID))
## left join (keep all for full length) by key cols
gid_secs_pbp <- left_join(gid_secs2, combo_time, by = c("GAME_ID", "start_sec"))

## important to sort by GAME_ID **
gid_sorted_pbp <- gid_secs_pbp %>%
  arrange(GAME_ID, start_sec) %>%
  rename(second = start_sec)

## get this into a loop
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

# colnames(gid_sorted_pbp[55:72])

select_cols <- c("second", pname_vec)
player_seconds_df <- select(gid_sorted_pbp, one_of(c(select_cols)))

## get sum of players 1, 0 for each second
player_secs_sum <- player_seconds_df %>%
  group_by(second) %>%
  summarise_all(funs(sum))

write.csv(player_secs_sum, file="data/combo_player_secs_sum.csv", row.names = FALSE)

gathered_player_secs <- gather(player_secs_sum, "player", "freq", 2:19)

total_player_secs <- player_secs_sum %>%
  summarise_all(funs(sum)) %>%
  rename(total = second) %>%
  gather("player", "tot_secs", 1:19) %>%
  filter(player != "total") %>%
  arrange(desc(tot_secs))

total_player_secs <- total_player_secs %>%
  mutate(tot_rank = dense_rank(tot_secs))

library(forcats)
## create a version using gather
## add total player secs to gathered
gathered_player_secs$player_tot_secs <- total_player_secs[match(gathered_player_secs$player, total_player_secs$player),]$tot_secs
pids_1617$player_tot_secs <- total_player_secs[match(pids_1617$pname, total_player_secs$player),]$tot_secs
## and rank secs
gathered_player_secs$player_tot_rank <- total_player_secs[match(gathered_player_secs$player, total_player_secs$player),]$tot_rank
pids_1617$player_tot_rank <- total_player_secs[match(pids_1617$pname, total_player_secs$player),]$tot_rank

ordered_pname <- pids_1617 %>%
  arrange(player_tot_secs)

ordered_pnames <- ordered_pname$pname

gathered_player_secs$player <- factor(gathered_player_secs$player, levels = ordered_pnames)
## check if factor
class(gathered_player_secs$player)

## faceted option
ggplot(gathered_player_secs,aes(x=second,y=1,fill=freq)) +
  facet_grid(player~.) +
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
  theme(axis.ticks = element_blank())

## non-faceted
## faceted option
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
  theme(axis.ticks = element_blank())

## unique list of players
players <- as.list(gathered_player_secs$player)
players <- unique(players)
