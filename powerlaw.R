library(poweRlaw)
library(tidyverse)



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
