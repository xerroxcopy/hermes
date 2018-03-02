library(tidyverse)
library(rstan)
library(plyr)  # for ddply, https://stackoverflow.com/questions/18379933/plotting-cumulative-counts-in-ggplot2
library(ggbeeswarm)
library(poweRlaw)
library(scales) # date_format
library(viridis)
library(ggridges)
library(extrafont)
library(lubridate)
library(franc) # for detecting language
my_name = "Minoru Matsui"
start_day <- "2014-07-01"
# func --------------------------------------------------------------------
daybreak <- function(x){
  # this func is copy-pasted for ticklabels for scale_colour_things(breaks = daybreak)
  # with this, you get colour legends with proper formatting
  breaks <- c(min(x),median(x),max(x))
  attr(breaks,"labels") <- as.POSIXct(breaks, origin="1970-01-01")
  names(breaks) <- attr(breaks,"labels")
  return(breaks)
}
# data --------------------------------------------------------------------
# use fbchat-archive-parser to get tidy data
# https://github.com/ownaginatious/fbchat-archive-parser
# fbcap ./messages.htm -f csv | tee messages.csv
data <- read_csv("input/20180221_messages.csv") %>%
  arrange(desc(date))
save(data, file = paste0("data/", Sys.Date(), "messeenger_data.Rdata"))

load("data/2018-02-21messenger_data.Rdata") # the file name depends on the date you saved the file


# plot --------------------------------------------------------------------

# for reordering, see https://qiita.com/kazutan/items/7840f743d642122d1219
# for date_time, see https://qiita.com/gingi99/items/4d3b8793f9918d8d20ad

# cumulative messages------------------------------------------------
mess_threshold <- 200 # omit senders who sent messages less than this amount

data %>%
  filter(sender != my_name) %>% 
  filter(sender == thread) %>%
  ddply(.(sender), transform, total_count = length(date)) %>% 
  filter(total_count > mess_threshold) %>%
  arrange(desc(total_count)) %>%
  mutate(sender = as.factor(sender)) %>%
  as.tibble() %>%
  ggplot(aes(x = date, color = sender)) + 
  geom_step(aes(total_count = total_count, y = ..y.. * total_count),
            stat = "ecdf") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y-%m",
                   limits = as.POSIXct(c(start_day, NA)),
                   expand = c(0,0)
  ) +
  scale_y_continuous(limits = c(1, NA), "total messages received") +
  theme_linedraw() +
  scale_colour_viridis(option = "D",
                       discrete = TRUE) +
  theme(text = element_text(family ="HGMaruGothicMPRO"),
        # legend.position = "none",
        NULL
  ) +
  ggsave(paste0("output/messenger_", Sys.Date(), "_cumulative_messages.pdf"), width = 200, height = 200, unit = "mm")

# to me / by me -----------------------------------------------------------
data %>%
  mutate(is.me = sender == my_name) %>% 
  group_by(thread) %>% 
  dplyr::summarise(count = n(), Minoru = sum(is.me), sender = n() - sum(is.me)) %>% 
  mutate(thread = as.factor(thread)) %>%
  filter(!grepl(",", thread)) %>% 
  as.tibble() %>%
  ggplot(aes(x = Minoru, y = sender)) + 
  geom_smooth(method = glm,
              # se = FALSE,
              fill = "grey90",
              colour = "black",
              size = 0.3
              # method.args = list(family = "poisson")
  ) +
  geom_point(colour = "black", size = 0.3) +
  geom_text(hjust = 0, nudge_x = 0.03, aes(label = thread, colour = thread), 
            family = "Gill Sans MT", size = 3, alpha = 0.8) +
  geom_abline(slope = 1, size = 0.1, linetype = "longdash") +
  scale_colour_viridis(discrete = TRUE) +
  scale_x_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000, 10000)) +
  coord_cartesian(xlim = c(5, 20000), ylim = c(5, 20000)) +
  ggtheme() +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) 
# save plot
ggsave(paste0("output/messenger_", Sys.Date(), "_frombyMe.pdf"), width = 200, height = 200, unit = "mm")

# dens-t, beeswarm --------------------------------------------------------
data %>%
  filter(sender != my_name) %>% 
  filter(sender == thread) %>%
  # filter(date > Sys.Date() - 365) %>%
  ddply(.(sender), transform, total_count = length(date)) %>% 
  filter(total_count > mess_threshold) %>%
  arrange(desc(total_count)) %>%
  mutate(sender = as.factor(sender)) %>%
  as.tibble() %>% 
  ggplot(aes(date, 
             y = reorder(x = sender, 
                         X = date,
                         FUN = min), 
             colour = date)) + # X = total_count
  geom_quasirandom(groupOnX = FALSE, 
                   size = 0.1, 
                   width = 0.95, # どれだけほかの人にかぶりかけるかdefault0.4
                   varwidth = TRUE, # 全体の大きさに応じてかわる
                   bandwidth = 0.2, # smoothness
                   # alpha = 0.03
  ) +
  geom_vline(xintercept = as.POSIXct(Sys.Date()), linetype = "longdash", size = 0.1) +
  scale_colour_viridis() + # discrete = TRUE
  scale_x_datetime(
    date_breaks = "1 year",
    labels = date_format("%Y-%m", tz = "Asia/Tokyo")
  ) + # "2014-07-01"
  theme_light(12) +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) 
# save plot
ggsave(paste0("output/messenger_", Sys.Date(), "_beeswarm.pdf"), width = 250, height = 200, unit = "mm")

# dens-t, density --------------------------------------------------------------

df <- 
  data %>%
  filter(sender != my_name) %>% 
  filter(sender == thread) %>%
  filter(date > start_day) %>%
  ddply(.(sender), transform, total_count = length(date)) %>%
  arrange(desc(total_count))

# create dataframe of senders sorted by total messages sent by them
# https://stackoverflow.com/questions/35113873/combine-result-from-top-n-with-an-other-category-in-dplyr
ranking <- 
  df %>% 
  group_by(sender) %>%
  tally(length(sender), sort = TRUE) 

# plot
# to simplify, other than top n messengers are aggregated as NA
top_n_messengers <- 10

df %>% 
  mutate(
    sender = ifelse(
      sender %in% ranking$sender[1:top_n_messengers],
      sender, NA # aggregate misc senders into nameless, one, "others" category
    )
  ) %>% 
  mutate(sender = as.factor(sender)) %>%
  as.tibble() %>%
  ggplot(aes(date, 
             y = ..count.., # to avoid accute ping
             colour = reorder(x = sender, 
                              X = date,
                              FUN = min)
  )) + 
  stat_density(
    geom = "line",
    size = 1,
    position = "identity", 
    # alpha = 0.5,
    bw = 5 * 10 ^ 5 # change this value to get detailed<->smoothed plot
    ) +
  geom_vline(xintercept = as.POSIXct(Sys.Date()), linetype = "longdash", size = 0.1) +
  # scale_fill_viridis(discrete = TRUE, option = "D") + # discrete = TRUE
  scale_colour_brewer("sender",
                      palette = "Set3") +
  # facet_grid(sender ~ .) +
  scale_x_datetime(
    date_breaks = "1 year",
    labels = date_format("%Y-%m", tz = "Asia/Tokyo"),
    expand = c(0, 0)
  ) + # "2014-07-01"
  theme_light(12) +
  theme(
    legend.justification = c(0,1),
    legend.position = c(0.01,0.99)
    # aspect.ratio = 1
  ) 
# save plot
ggsave(paste0("output/messenger_", Sys.Date(), "_beeswarm.pdf"), width = 250, height = 200, unit = "mm")

# joyplot ----------------------------------------------------------------
top_n_messengers2 <- 20
data %>%
  filter(sender != my_name) %>% 
  filter(sender == thread) %>%
  mutate(
    sender = ifelse(
      sender %in% ranking$sender[1:top_n_messengers2],
      sender, NA
    )
  ) %>% 
  mutate(sender = as.factor(sender)) %>%
  as.tibble() %>%
  ggplot(aes(date, 
             y = reorder(x = sender, 
                         X = date,
                         FUN = min),
             fill = NA,
             height = ..count..
  )) + # X = total_count
  geom_density_ridges(
    bw = 0.005,
    # bins = 1000,
    colour = "white", 
    size = 0.2,
    scale = 2,
    # alpha = 0.7,
    rel_min_height = 0.0000000001,
    stat = "density"  # "binline"
  ) +
  geom_vline(xintercept = as.POSIXct(Sys.Date()), linetype = "longdash", size = 0.1) +
  # scale_fill_viridis(discrete = TRUE, option = "D") + # discrete = TRUE
  scale_fill_brewer("sender",
                    palette = "Set3") +
  scale_y_discrete("sender") +
  scale_x_datetime(
    date_breaks = "1 year",
    labels = date_format("%Y-%m", tz = "Asia/Tokyo")
  ) + # "2014-07-01"
  theme_linedraw() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black"),
    # aspect.ratio = 
  ) 
# save plot
ggsave(paste0("output/messenger_", Sys.Date(), "_transitionridges.pdf"), width = 200, height = 150, unit = "mm")
# hour --------------------------------------------------------------------
hourlyplot <- 
  data %>%
  filter(sender != my_name) %>% 
  filter(sender == thread) %>%
  # filter(date > start_day) %>%
  ddply(.(sender), transform, total_count = length(date)) %>% 
  filter(total_count > 100) %>%
  # group_by(sender) %>% 
  # top_n(25, total_count) %>%
  # dplyr::summarise(count = n())
  # ungroup() 
  arrange(date) %>%
  mutate(sender = as.factor(sender)) %>%
  mutate(hour = format(date, "%H:%M:%S") %>% 
           parse_date_time("HMS")) %>%
  as.tibble() %>%
  ggplot(
    aes(
      y = 0, 
      x = hour,
      # y = reorder(x = sender, 
      #             X = total_count),
      colour = date,
      alpha = date
    )
  ) + # X = total_count
  # geom_density() +
  geom_quasirandom(groupOnX = FALSE,
                   size = 0.0001, 
                   width = 10, # どれだけほかの人にかぶりかけるかdefault0.4
                   varwidth = TRUE, # 全体の大きさに応じてかわる
                   # alpha = 0.1,
                   bandwidth = 0.05 # smoothness
  ) +
  scale_x_datetime("", breaks = date_breaks("1 hour"),
                   labels = date_format("%k", tz = "Asia/Tokyo")  # tz must be in here
  ) +
  coord_polar(theta = "x", 
              start = 2 * pi / 24 * 9 # time-zone offset of 9 hours
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_alpha_continuous(breaks = daybreak, range = c(0.001, 0.6)) +
  # scale_colour_viridis(direction = -1,
  #   breaks = daybreak) +
  scale_colour_gradientn(colours = spectral.3(10), breaks = daybreak) +
  ggtheme2(10, base_family = "Centaur MT") +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.box = "horizontal",
    panel.border = element_rect(fill = NA, colour = NA),
    panel.grid.major.x = element_line(colour = "#ffffbbbb", size = 0.25), 
    axis.text.x=element_text(size = 8),
    axis.ticks =element_blank(),
    axis.text.y =element_blank(),axis.title=element_blank(),
    panel.grid.minor = element_line(colour = NA), # white? grey?
  ) # https://stackoverflow.com/questions/7830022/rotate-x-axis-text-in-ggplot2-when-using-coord-polar
ggsave(hourlyplot, file = paste0("output/messenger_", Sys.Date(), "_hourlydata.pdf"), width = 200, height = 150, unit = "mm")
hourlyplot +   
  facet_wrap(~reorder(x = sender, 
                      X = date, 
                      FUN = min), ncol = 7) +
  ggsave(paste0("output/messenger_", Sys.Date(), "_hourlydata_individual.pdf"), width = 400, height = 300, unit = "mm")

# hour plot, year transition ----------------------------------------------
df <-
  data %>%
  mutate(is.me = sender == my_name) %>%  # for shape
  ddply(.(sender), transform, total_count = length(date)) %>% 
  filter(total_count > 500) %>%
  filter(!grepl(",", thread)) %>%  # delete group conv
  arrange(date) %>%
  mutate(sender = as.factor(sender)) %>%
  mutate(hour = format(date, "%H:%M:%S") %>% 
           parse_date_time("HMS")) %>%
  as.tibble()

# facet by senders, omit messages sent by myself
df %>%
  filter(!is.me) %>%
  ggplot(
    aes(
      y = hour, 
      x = date,
      # y = reorder(x = sender, 
      #             X = total_count),
      colour = hour
    )
  ) + 
  # stat_density_2d(geom = "raster", aes(fill = ..density.., alpha = ..density..),
  #                 contour = FALSE) +
  geom_count(aes(alpha = ..n..)) +
  scale_size(range = c(0, 1)) +
  scale_y_datetime(labels = date_format("%H", tz = "Asia/Tokyo"),  # tz must be in here
                   breaks = date_breaks(width = "6 hours"), # なぜか動かないことがあった
                   expand = c(0,0)
  ) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
                   limits = as.POSIXct(c("2013-01-01", NA)),
                   NULL
  ) +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_colour_viridis(direction = -1) +
  facet_grid(sender ~ .) +
  theme_linedraw() +
  theme(
    legend.position = "none",
    # legend.justification = c(1, 0),
    # legend.box = "horizontal",
    panel.border = element_rect(fill = NA, colour = NA),
    # panel.grid.major.y = element_line(colour = NA, size = 0.25),
    # axis.text.x=element_text(size = 8),
    # axis.ticks =element_blank(),
    # axis.text.y =element_blank(),axis.title=element_blank(),
    panel.grid.minor = element_line(colour = NA), # white? grey?
    NULL) # https://stackoverflow.com/questions/7830022/rotate-x-axis-text-in-ggplot2-when-using-coord-polar

# save plot
ggsave(hourlyplot, file = paste0("output/messenger_", Sys.Date(), "_hour_year.pdf"), width = 200, height = 150, unit = "mm")

# by myself

# facet by senders, omit messages sent by myself
df %>%
  filter(is.me) %>%
  ggplot(
    aes(
      y = hour, 
      x = date,
      # y = reorder(x = sender, 
      #             X = total_count),
      colour = hour
    )
  ) + 
  stat_density_2d(geom = "raster", aes(fill = ..density.., alpha = ..density..),
                  n = 500, # default 100, pixels on each direction
                  h = 10 ^ -10,
                  contour = FALSE) +
  # geom_count(aes(alpha = ..n..)) +
  scale_size(range = c(0, 1)) +
  scale_y_datetime(labels = date_format("%H", tz = "Asia/Tokyo"),  # tz must be in here
                   # breaks = date_breaks(width = "6 hours"), # なぜかdensity_2dだと動かない
                   expand = c(0,0)
  ) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
                   limits = as.POSIXct(c("2013-01-01", NA)),
                   NULL
  ) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  scale_fill_viridis(direction = -1) +
  # facet_grid(sender ~ .) +
  theme_linedraw() +
  theme(
    # legend.position = "none",
    # legend.justification = c(1, 0),
    # legend.box = "horizontal",
    panel.border = element_rect(fill = NA, colour = NA),
    # panel.grid.major.y = element_line(colour = NA, size = 0.25),
    # axis.text.x=element_text(size = 8),
    # axis.ticks =element_blank(),
    # axis.text.y =element_blank(),axis.title=element_blank(),
    panel.grid.minor = element_line(colour = NA), # white? grey?
    NULL) # https://stackoverflow.com/questions/7830022/rotate-x-axis-text-in-ggplot2-when-using-coord-polar
ggsave(hourlyplot, file = paste0("output/messenger_", Sys.Date(), "_hour_year.pdf"), width = 200, height = 150, unit = "mm")
# message length -----------------------------------------------------------
df_ml <- 
  data %>%
  filter(sender != my_name) %>%
  filter(sender == thread) %>%
  ddply(.(sender), transform, total_count = length(date)) %>% 
  filter(total_count > 100) %>%
  arrange(date) %>%
  mutate(sender = as.factor(sender)) %>%
  mutate(hour = format(date, "%H:%M:%S") %>% 
           parse_date_time("HMS")) %>%
  filter(!grepl("image reference", message)) %>%  # filter out images
  # mutate(lang = ifelse(grepl("![\x01-\x7E]", message), "Japanese", "Others")) %>% umakuikan
  # mutate(lang = franc(message)) %>% 
  mutate(message_length = nchar(message)) %>% 
  as.tibble()

# grepl("![\x01-\x7E]", df_ml$message)
# df_ml$lang

df_ml %>% 
  ggplot(
    aes(
      x = message_length,
      y = reorder(x = sender,
                  X = total_count),
      # colour = lang
    )
  ) + # X = total_count
  # geom_density() +
  scale_x_continuous(trans = "log10", breaks=  c(10 ^ (1:10))) +
  coord_cartesian(xlim = c(1, 1000)) +
  geom_density_ridges(bandwidth = 0.05,
                      fill = NA, 
                      colour = "white",
                      size = 0.2,
                      scale = 2,
                      # alpha = 0.7,
                      rel_min_height = 0.0000000001,
                      # stat = "density"  # "binline"
  ) +
  theme_linedraw() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black"),
    # aspect.ratio = 
  ) +
  ggsave(paste0("output/messenger_", Sys.Date(), "_MessageLength.pdf"), width = 200, height = 150, unit = "mm")


# for science: power-law -------------------------------------------------

aggr_data <- data %>%
  filter(sender == thread) %>% # avoid group conv
  group_by(sender) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  filter(sender != my_name)

power_law <- displ$new(aggr_data$count)
log_normal <- dislnorm$new(aggr_data$count)
# power_law$setXmin(estimate_xmin(power_law))
power_law$setXmin(1)
log_normal$setXmin(1)
power_law$setPars(estimate_pars(power_law))
log_normal$setPars(estimate_pars(log_normal))
plot(power_law)
lines(power_law)
lines(log_normal)
df_pl <- plot(power_law)
df_pl2 <- left_join(aggr_data, df_pl, by = c("count" = "x"))
# latest message
last_date <- data %>% 
  group_by(sender) %>%
  filter(date == max(date)) %>% 
  distinct(sender, .keep_all = TRUE) # http://a-habakiri.hateblo.jp/entry/2016/11/29/215013 .を忘れず
# merge them
df_pl3 <- left_join(df_pl2, last_date, by = "sender")

ggplot(df_pl3, aes(count, y, colour = date)) +
  geom_point(size = 2) +
  # geom_text(check_overlap = FALSE, aes(label = sender), size = 1, 
  #           # position = position_jitter(width = 0.4, height = 0), 
  #           alpha = 0.8, 
  #           hjust = 0, nudge_x = 0.05
  #           ) +
  scale_x_continuous(trans = "log10", breaks = 10^(0:10))+
  scale_y_continuous(trans = "log10") +
  scale_colour_viridis(breaks = daybreak) + # discrete = TRUE
  theme_light() +
  theme(aspect.ratio = 1,
        # legend.position = "none"
  ) 
ggsave(paste0("output/messenger_", Sys.Date(), "_powerlaw.pdf"), width = 200, height = 150, unit = "mm")

# power-law on message length ---------------------------------------------
power_law <- displ$new(na.omit(df_ml$message_length))
power_law$setXmin(estimate_xmin(power_law))
power_law$setPars(estimate_pars(power_law))
plot(power_law)
lines(power_law)
lines(log_normal)
df_pl <- plot(power_law)

ggplot(df_pl, aes(x, y)) +
  geom_point(size = 2) +
  # geom_text(check_overlap = FALSE, aes(label = sender), size = 1, 
  #           # position = position_jitter(width = 0.4, height = 0), 
  #           alpha = 0.8, 
  #           hjust = 0, nudge_x = 0.05
  #           ) +
  # scale_x_continuous("length of message (characters)", trans = "log10", breaks = 10^(0:10)) +
  # scale_y_continuous("CCDF", trans = "log10") +
  ggtheme(14) +
  theme(aspect.ratio = 1,
        # legend.position = "none"
  ) +
  ggsave(paste0("output/messenger_", Sys.Date(), "_MessageLength_powerlaw.pdf"), width = 200, height = 150, unit = "mm")


