#!/usr/bin/env Rscript --vanilla
# build video steam train plot from gapminder and berkeley earth data
# james goldie ('rensa'), august 2017

library(tidyverse)
library(magrittr)
library(gganimate)
# library(tweenr)
library(plotly)
library(emojifont)
# library(viridis)
filter = dplyr::filter # this is going to kill me one day
source('util.r')

# create data directory if it's missing
if(!dir.exists('data') | !file.exists('data/gapminder-berkeley-tidy.csv'))
{
  stop(paste('Error: can\'t find the tidied data! If it\'s been deleted from',
    'the repo, you can run tidy.r to recalculate it (or just clone the repo',
    'again)'))
}

# constants
# on_url = 'https://raw.githubusercontent.com/open-numbers/'
# berk_url = 'http://berkeleyearth.lbl.gov/'
year_start = 1900
year_end = 2012
frames_per_year = 30
plot_font_1 = 'Helvetica Neue Light'
plot_font_2 = 'Helvetica Neue'

all_data =
  read_csv('data/gapminder-berkeley-tidy.csv') %>%
  replace_na(list(flag_emoji = emoji('x')))

message(run.time(), ' trimming excess data for bubble geom')
# bubble_data = all_data %>%
#   select(co2, pop_poorer_fraction, pop_fraction, year, emission_id) %>%
#   mutate(emission_year = year - year_start)
# bar_data = all_data %>%
#   select(name, temp, pop_poorer_fraction, pop_fraction, year) %>%
#   mutate(
#     emission_year = year - year_start,
#     ease = 'exponential-in-out')

# message(run.time(), ' interpolating emissions and temp data for animation')
# bubble_tw = tween_appear(bubble_data, time = 'emission_year',
#   timerange = c(year_start, year_end),
#   nframes = frames_per_year * (year_end - year_start))
# bar_tw = tween_elements(bar_data, time = 'emission_year',
#   group = 'name', ease = 'ease',
#   nframes = frames_per_year * (year_end - year_start))

message(run.time(), ' building emissions plot')
steamtrain = ggplot(all_data) +
  geom_pointrange(
    aes(
      x = gdppc,
      ymin = temp_min,
      y = temp,
      ymax = temp_max,
      frame = year)) +
  geom_text(
    aes(
      x = gdppc,
      y = max(temp_max) + 0.25,
      label = flag_emoji,
      size = co2_cum,
      frame = year),
    family = 'EmojiOne', position = position_jitter(width = 0)) +
  scale_x_log10(name = 'GDP (2010 US Dollars)') +
  scale_y_continuous(name = 'Temperature anomaly (°C)', limits = c(-2.5, 3.5)) +
  scale_size(name = 'Cumulative CO2 emissions (Mt)', range = c(0, 20))
  
message(run.time(), ' rendering emissions plot')
animation::ani.options(interval = 0.5 / frames_per_year)
gganimate(stplot2, 'steamtrain.mp4',
  ani.width = 1920, ani.height = 1080, title_frame = FALSE)

steamtrain_plotly = steamtrain %>%


message(run.time(), ' done! Render in browser with ggplotly(steamtrain_plotly)')

# stplot = ggplot() +
#   # temperature columns
#   geom_col(data = bar_tw,
#     aes(
#       x = pop_poorer_fraction + pop_fraction / 2,
#       fill = pop_poorer_fraction + pop_fraction / 2,
#       y = temp,
#       width = pop_fraction,
#       frame = .frame),
#     position = 'identity') +
#   # annual emission bubbles
#   geom_jitter(data = bubble_tw,
#     aes(
#       x = pop_poorer_fraction + pop_fraction / 2,
#       y = 1.75 * atan(2 * .age) + 0.3 * sin(2 * .age),
#       size = co2,
#       frame = .frame),
#     alpha = 0.1, width = 0, height = 0.1, colour = '#333333') +
#   geom_text(data = bar_tw,
#     aes(label = as.integer(.frame / frames_per_year + year_start),
#     frame = .frame, x = 0.5, y = 0.125),
#     size = 14, family = plot_font_2, fontface = 'bold') +
#   annotate('text', x = 0.0375, y = 0.125, size = 14, label = 'Poorest',
#     family = plot_font_1) +
#   annotate('text', x = 0.945, y = 0.125, size = 14, label = 'Wealthiest',
#     family = plot_font_1) +
#   scale_x_continuous(labels = scales::percent,
#     name = 'Percentile of GDP per capita (2010 $US)',
#       breaks = seq(0, 1, 0.25), limits = c(0, 1), expand = c(0, 0.02)) +
#   scale_y_continuous(breaks = 1:3, limits = c(0, 3.5),
#     name = 'Temperature anomaly (°C)', expand = c(0, 0)) +
#   scale_size_area(name = 'Annual CO2 emissions (excluding land use)',
#     max_size = 80, guide = FALSE) +
#   scale_fill_viridis(begin = 0.75, end = 0.875, limits = c(0, 1),
#     option = 'plasma', direction = -1, guide = FALSE) +
#   ggtitle('Historical CO2 emissions, GDP per capita and temperature rise',
#     subtitle = paste('Who contributed to the greenhouse effect,',
#       'and who suffers for it?')) +
#   theme_classic(base_size = 32, base_family = plot_font_1) +
#   theme(
#     plot.title = element_text(family = plot_font_2, face = 'bold'),
#     plot.margin = margin(rep(20, 4), 'px'),
#     axis.ticks.x = element_blank(),
#     axis.line.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.background = element_rect(fill = '#f8f9f9'),
#     plot.background = element_rect(fill = '#f8f9f9'))



# ggplotly(stplot2)

message(run.time(), ' done!')
