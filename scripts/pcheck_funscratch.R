## player check function scratch
player_check <- function(df, pid, pname){
  mutate(df, pname = if_else(grepl(pid, players), 1, 0))
}

test <- player_check(home_v_min, "203953", "JabariParker")
