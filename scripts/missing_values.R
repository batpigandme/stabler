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
