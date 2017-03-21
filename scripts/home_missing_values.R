## get elapsed time for event number
home_elapse <- home_pbp %>%
  mutate(duration = tsecs_elapsed - lead(tsecs_elapsed))

## get only the non-negative values
pos_duration <- home_elapse %>%
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

home_time <- pos_time
for (i in 1:length(named_pids)) {
  test4 <- pfunner(home_time, pname = names(named_pids[i]), pid = named_pids[[i]])
  home_time <- test4
}

home_time <- home_time %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, start_sec)

seconds <- c(0:2880)
df <- data.frame(seconds)

thing2 <- c(paste("g",home_gid, sep = ""))
library(tidyverse)
named_home <- home_gid
names(named_home) <- thing2
game_time <- enframe(named_home, name = "thing2", value = "home_gid")

repeated_secs <- rep(c(0:2880), each=length(home_gid))
repeated_sec2 <- data.frame(repeated_secs)
hgid_list <- as.list(home_gid)
home_gids <- rep(c(hgid_list), times=2881)
home_gids <- data_frame(home_gids)
gid_secs <- bind_cols(repeated_sec2, home_gids)

## now do mutating join (maybe merge?) with home_test
## then fill in values

gid_secs2 <- gid_secs %>%
  rename(start_sec = repeated_secs) %>%
  rename(GAME_ID = home_gids) %>%
  mutate(GAME_ID = as.character(GAME_ID))

gid_secs_pbp <- left_join(gid_secs2, home_time, by = c("GAME_ID", "start_sec"))

gid_sorted_pbp <- gid_secs_pbp %>%
  arrange(GAME_ID, start_sec) %>%
  rename(second = start_sec)

## get this into a loop
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

write.csv(gid_sorted_pbp, file = "data/home_gid_sorted_pbp.csv", row.names = FALSE)

# colnames(gid_sorted_pbp[55:72])

select_cols <- c("second", pname_vec)
player_seconds_df <- select(gid_sorted_pbp, one_of(c(select_cols)))

## get sum of players 1, 0 for each second
player_secs_sum <- player_seconds_df %>%
  group_by(second) %>%
  summarise_all(funs(sum))

write.csv(player_secs_sum, file="data/home_player_secs_sum.csv", row.names = FALSE)

gathered_player_secs <- gather(player_secs_sum, "player", "freq", 2:19)

total_player_secs <- player_secs_sum %>%
  summarise_all(funs(sum)) %>%
  rename(total = second) %>%
  gather("player", "tot_secs", 1:19) %>%
  filter(player != "total") %>%
  arrange(desc(tot_secs))

library(forcats)

## add total player secs to gathered
gathered_player_secs$player_tot_secs <- total_player_secs[match(gathered_player_secs$player, total_player_secs$player),]$tot_secs
pids_1617$player_tot_secs <- total_player_secs[match(pids_1617$pname, total_player_secs$player),]$tot_secs

ordered_pname <- pids_1617 %>%
  arrange(player_tot_secs)

ordered_pnames <- ordered_pname$pname

gathered_player_secs$player <- factor(gathered_player_secs$player, levels = ordered_pnames)
## check if factor
class(gathered_player_secs$player)


## plotting frequency by second by player
ggplot(gathered_player_secs,aes(x=second,y=player,fill=freq)) +
  geom_tile() +
  scale_fill_gradient(low = "grey90", high = "turquoise") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  geom_vline(xintercept = 720, color = "white") +
  geom_vline(xintercept = 1440, color = "white") +
  geom_vline(xintercept = 2160, color = "white") +
  ggtitle("Home Games") +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank())

