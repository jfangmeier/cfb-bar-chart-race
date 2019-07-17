library(tidyverse)
library(zoo)
library(RcppRoll)
library(gganimate)
library(shadowtext)


#Import and prep data
team_record_raw <-
  readRDS(here::here("data", "team_record_raw.rds"))

team_record_prep <- team_record_raw %>%
  mutate_at(vars(year, w, l, t, pct, srs, sos, ap_pre, ap_high, ap_post),
            list( ~ as.numeric(.))) %>%
  mutate(adjustment = ifelse(str_detect(notes, "^(record adjusted to )"), str_squish((
    substr(notes, 20, 25)
  )), NA)) %>%
  separate(
    col = adjustment,
    sep = "\\-",
    into = c("w_adj", "l_adj", "t_adj")
  ) %>%
  #record if adjusted by NCAA
  mutate_at(vars(w_adj, l_adj, t_adj), list( ~ as.numeric(.))) %>%
  mutate(
    w = ifelse(is.na(w_adj), w, w_adj),
    l = ifelse(is.na(l_adj), l, l_adj),
    t = ifelse(is.na(t_adj), t, t_adj)
  ) %>%
  filter(year <= 2018) %>%
  complete(year, school) %>% #provide all school/year combinations to account for years that a school takes off
  group_by(school) %>%
  arrange(desc(from)) %>%
  mutate_at(vars(to, from), list( ~ na.locf(.))) %>%
  ungroup() %>%
  filter(year >= from) %>% #remove school/year combinations before the school began playing
  mutate(played_season = ifelse(is.na(w), 0, 1)) %>%
  mutate_at(vars(w, l, t), list( ~ ifelse(is.na(.), 0, .))) %>% #assign 0-0-0 for seasons not played
  arrange(school, year) %>%
  group_by(school) %>%
  mutate(
    cumulative_wins = cumsum(w),
    cumulative_losses = cumsum(l),
    rolling_wins_10yr =  roll_sumr(w, n = 10),
    rolling_losses_10yr = roll_sumr(l, n = 10),
    conf = na.locf(conf)
  ) %>%
  ungroup() %>%
  mutate_at(vars(rolling_wins_10yr, rolling_losses_10yr), list( ~ ifelse(is.na(.), 0, .)))

team_record_final <- team_record_prep %>%
  group_by(year) %>%
  mutate(
    rank_cm_wins = rank(desc(cumulative_wins), ties.method = "first"),
    rank_cm_losses = rank(desc(cumulative_losses), ties.method = "first"),
    rank_r10_wins = rank(desc(rolling_wins_10yr), ties.method = "first"),
    rank_r10_losses = rank(desc(rolling_losses_10yr), ties.method = "first"),
    school = ifelse(school == "Washington & Jefferson", "Wash & Jefferson", school)
  ) %>%
  ungroup() %>%
  filter_at(
    vars(rank_cm_wins, rank_cm_losses, rank_r10_wins, rank_r10_losses),
    any_vars(. %in% 1:10)
  ) %>%
  select(
    year,
    school,
    conf,
    cumulative_wins,
    cumulative_losses,
    rolling_wins_10yr,
    rolling_losses_10yr,
    rank_cm_wins,
    rank_cm_losses,
    rank_r10_wins,
    rank_r10_losses
  )


#Setup theme
my_background <- '#196F0C' #Astroturf Green
my_theme <- theme(
  rect = element_rect(fill = my_background),
  plot.background = element_rect(fill = my_background, color = NA),
  panel.background = element_rect(fill = my_background, color = NA),
  panel.border = element_blank(),
  plot.title = element_text(face = 'bold', size = 20, color = 'white'),
  plot.subtitle = element_text(size = 14, color = 'white'),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_line(color = 'white'),
  panel.grid.minor.x = element_line(color = 'white'),
  legend.position = 'none',
  plot.caption = element_text(size = 10, color = 'white'),
  axis.ticks = element_blank(),
  axis.text.y =  element_blank(),
  axis.text = element_text(color = 'white')
)

theme_set(theme_light() + my_theme)


#Rolling wins chart
rolling_wins_chart <- team_record_final %>%
  filter(rank_r10_wins %in% 1:10 & rolling_wins_10yr > 0) %>%
  ggplot(aes(rank_r10_wins * -1, group = school)) +
  geom_tile(aes(
    y = rolling_wins_10yr / 2,
    height = rolling_wins_10yr,
    width = 0.9,
    fill = conf
  ),
  alpha = 0.9) +
  geom_text(
    aes(y = rolling_wins_10yr, label = school),
    nudge_y = -20,
    nudge_x = .2,
    size = 4
  ) +
  geom_text(
    aes(y = rolling_wins_10yr, label = conf),
    nudge_y = -20,
    nudge_x = -.2,
    size = 2.5
  ) +
  geom_text(aes(y = rolling_wins_10yr, label = as.character(rolling_wins_10yr)), nudge_y = 5) +
  geom_shadowtext(aes(
    x = -10,
    y = 118,
    label = paste0(year)
  ),
  size = 8,
  color = 'white') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(
    title = 'Most College Football Wins',
    subtitle = 'Ten Year Rolling Total of Major Program Games',
    caption = 'bar colors represent conferences\ndata source: Sports Reference | graphic by @joshfangmeier',
    x = '',
    y = ''
  ) +
  transition_states(year,
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

rolling_wins_animation <-
  animate(
    rolling_wins_chart,
    duration = 70,
    fps = 30,
    end_pause = 30,
    rewind = FALSE,
    width = 600,
    height = 400,
    res = 80,
    detail = 3
  )

anim_save("cfb_rolling_wins.gif",
          animation = rolling_wins_animation,
          path = here::here("images"))


#Rolling losses chart
rolling_losses_chart <- team_record_final %>%
  filter(rank_r10_losses %in% 1:10 & rolling_losses_10yr > 0) %>%
  ggplot(aes(rank_r10_losses * -1, group = school)) +
  geom_tile(
    aes(
      y = rolling_losses_10yr / 2,
      height = rolling_losses_10yr,
      width = 0.9,
      fill = conf
    ),
    alpha = 0.9
  ) +
  geom_text(
    aes(y = rolling_losses_10yr, label = school),
    nudge_y = -15,
    nudge_x = .2,
    size = 4
  ) +
  geom_text(
    aes(y = rolling_losses_10yr, label = conf),
    nudge_y = -15,
    nudge_x = -.2,
    size = 2.5
  ) +
  geom_text(aes(y = rolling_losses_10yr, label = as.character(rolling_losses_10yr)), nudge_y = 5) +
  geom_shadowtext(aes(
    x = -10,
    y = 105,
    label = paste0(year)
  ),
  size = 8,
  color = 'white') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(
    title = 'Most College Football Losses',
    subtitle = 'Ten Year Rolling Total of Major Program Games',
    caption = 'bar colors represent conferences\ndata source: Sports Reference | graphic by @joshfangmeier',
    x = '',
    y = ''
  ) +
  transition_states(year,
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

rolling_losses_animation <-
  animate(
    rolling_losses_chart,
    duration = 70,
    fps = 30,
    end_pause = 30,
    rewind = FALSE,
    width = 600,
    height = 400,
    res = 80,
    detail = 3
  )

anim_save("cfb_rolling_losses.gif",
          animation = rolling_losses_animation,
          path = here::here("images"))


#All time wins chart
alltime_wins_chart <- team_record_final %>%
  filter(rank_cm_wins %in% 1:10 & cumulative_wins > 0) %>%
  ggplot(aes(rank_cm_wins * -1, group = school)) +
  geom_tile(aes(
    y = cumulative_wins / 2,
    height = cumulative_wins,
    width = 0.9,
    fill = conf
  ),
  alpha = 0.9) +
  geom_text(
    aes(y = cumulative_wins, label = school),
    nudge_y = -90,
    nudge_x = .2,
    size = 4
  ) +
  geom_text(
    aes(y = cumulative_wins, label = conf),
    nudge_y = -90,
    nudge_x = -.2,
    size = 2.5
  ) +
  geom_text(aes(y = cumulative_wins, label = as.character(cumulative_wins)), nudge_y = 25) +
  geom_shadowtext(aes(
    x = -10,
    y = 925,
    label = paste0(year)
  ),
  size = 8,
  color = 'white') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(
    title = 'Most College Football Wins',
    subtitle = 'Cumulative Total of Major Program Games',
    caption = 'bar colors represent conferences\ndata source: Sports Reference | graphic by @joshfangmeier',
    x = '',
    y = ''
  ) +
  transition_states(year,
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

alltime_wins_animation <-
  animate(
    alltime_wins_chart,
    duration = 70,
    fps = 30,
    end_pause = 30,
    rewind = FALSE,
    width = 600,
    height = 400,
    res = 80,
    detail = 3
  )

anim_save("cfb_alltime_wins.gif",
          animation = alltime_wins_animation,
          path = here::here("images"))


#All time losses chart
alltime_losses_chart <- team_record_final %>%
  filter(rank_cm_losses %in% 1:10 & cumulative_losses > 0) %>%
  ggplot(aes(rank_cm_losses * -1, group = school)) +
  geom_tile(aes(
    y = cumulative_losses / 2,
    height = cumulative_losses,
    width = 0.9,
    fill = conf
  ),
  alpha = 0.9) +
  geom_text(
    aes(y = cumulative_losses, label = school),
    nudge_y = -80,
    nudge_x = .2,
    size = 4
  ) +
  geom_text(
    aes(y = cumulative_losses, label = conf),
    nudge_y = -80,
    nudge_x = -.2,
    size = 2.5
  ) +
  geom_text(aes(y = cumulative_losses, label = as.character(cumulative_losses)), nudge_y = 25) +
  geom_shadowtext(aes(
    x = -10,
    y = 675,
    label = paste0(year)
  ),
  size = 8,
  color = 'white') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(
    title = 'Most College Football Losses',
    subtitle = 'Cumulative Total of Major Program Games',
    caption = 'bar colors represent conferences\ndata source: Sports Reference | graphic by @joshfangmeier',
    x = '',
    y = ''
  ) +
  transition_states(year,
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

alltime_losses_animation <-
  animate(
    alltime_losses_chart,
    duration = 70,
    fps = 30,
    end_pause = 30,
    rewind = FALSE,
    width = 600,
    height = 400,
    res = 80,
    detail = 3
  )

anim_save("cfb_alltime_losses.gif",
          animation = alltime_losses_animation,
          path = here::here("images"))