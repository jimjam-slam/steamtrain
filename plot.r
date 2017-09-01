#!/usr/bin/env Rscript --vanilla
# build video steam train plot from gapminder and berkeley earth data
# james goldie ('rensa'), august 2017

library(tidyverse)
library(magrittr)
library(stringr)
library(plotly)
library(ggflags)
library(gganimate)
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
  replace_na(list(flag_emoji = '274e'))

message(run.time(), ' trimming excess data for bubble geom')

message(run.time(), ' building emissions plot')
steamtrain = ggplot(all_data) +
  geom_pointrange(
    aes(
      x = gdppc,
      ymin = temp_min,
      y = temp,
      ymax = temp_max,
      colour = world_6region,
      frame = year)) +
  geom_flag(
    aes(
      x = gdppc,
      y = 3,
      country = str_to_lower(iso3166_1_alpha2),
      group = world_6region,
      size = co2_cum / 1000,
      frame = year),
    colour = 'black', stroke = 2,
    position = position_jitter(width = 0, height = 0.25)) +
  scale_x_log10(name = 'GDP (2010 US Dollars)') +
  scale_y_continuous(name = 'Temperature anomaly (°C)', limits = c(-1.5, 3.5)) +
  scale_size(name = 'Cumulative CO2 (Gt)', range = c(0, 25)) +
  scale_colour_discrete(name = 'Region') +
  ggtitle('Cumulative CO2 emissions, GDP per capita and temperature anomaly',
    subtitle = paste('Who contributed to the greenhouse effect,',
      'and who suffers for it?')) +
  theme_classic(base_size = 10, base_family = plot_font_1) +
  theme(plot.title = element_text(family = plot_font_2, face = 'bold'))
gganimate(steamtrain, 'steamtrain.mp4', title_frame = TRUE, interval = 0.5)

# static version (just 2012)
all_data_2012 = all_data %>% filter(year == 2012)
st2012 = ggplot(all_data_2012) +
  geom_pointrange(
    aes(
      x = gdppc,
      ymin = temp_min,
      y = temp,
      ymax = temp_max,
      colour = world_6region)) +
  geom_flag(
    aes(
      x = gdppc,
      y = 3,
      country = str_to_lower(iso3166_1_alpha2),
      group = world_6region,
      size = co2_cum / 1000),
    colour = 'black', stroke = 2,
    position = position_jitter(width = 0, height = 0.25)) +
    # position = position_jitter(width = 0, height = 0.25)) +
  scale_x_log10(name = 'GDP (2010 US Dollars)') +
  scale_y_continuous(name = 'Temperature anomaly (°C)', limits = c(-0.5, 3.5)) +
  scale_size(name = 'Cumulative CO2 (Gt as of 2012)', range = c(0, 25)) +
  scale_colour_discrete(name = 'Region') +
  labs(
    title = paste(
      'Cumulative CO2 emissions (1900–2012), GDP per capita and',
      'temperature anomaly'),
    subtitle = paste('Who contributed to the greenhouse effect,',
      'and who suffers for it?',
    caption = paste('Data from Open Numbers and Berkeley Earth; Flags from EmojiOne'))) +
  theme_classic(base_size = 10, base_family = plot_font_1) +
  theme(plot.title = element_text(family = plot_font_2, face = 'bold'))
ggsave('st2012.png', st2012,
  width = 192, height = 108, units = 'mm', scale = 1.5)

message(run.time(), ' done! Static (2012) frame saved as st2012.png; ',
  'animated version saved as steamtrain.mp4')
