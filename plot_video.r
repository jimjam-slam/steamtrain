#!/usr/bin/env Rscript --vanilla
# build video steam train plot from gapminder and berkeley earth data
# james goldie ('rensa'), august 2017

library(tidyverse)
library(magrittr)
library(gganimate)
library(tweenr)
filter = dplyr::filter # this is going to kill me one day
source('util.r')

# create data directory if it's missing
if(!dir.exists('data') |
  !file.exists('data/gapminder-tidy.csv') |
  !file.exists('data/gistemp-tidy.csv'))
{
  stop(paste('Error: can\'t find the tidied data! If it\'s been deleted from',
    'the repo, you can run tidy.r to recalculate it (or just clone the repo',
    'again)'))
}

# constants
on_url = 'github.com/open-numbers'
edgar_url = 'edgar.jrc.ec.europa.eu'
giss_url = 'data.giss.nasa.gov'
year_start = 1900
year_end = 2015
frames_per_year = 2
plot_font_1 = 'Helvetica Neue Light'
plot_font_2 = 'Helvetica Neue'
bg_colours = c('#4575b4', '#91bfdb', '#e0f3f8', '#ffffff', '#fee090', '#fc8d59',
  '#d73027')

gapminder = read_csv('data/gapminder-tidy.csv')
gistemp = read_csv('data/gistemp-tidy.csv')

message(run.time(), ' trimming and tweening data for plotting')
bubble_data = gapminder %>%
  select(co2, pop_poorer_fraction, pop_fraction, year, emission_id) %>%
  mutate(emission_year = year - year_start) %>%
  tween_appear(time = 'emission_year',
    timerange = c(year_start, year_end + 2),
    nframes = frames_per_year * (year_end - year_start + 2))
bar_data = gapminder %>%
  select(name, co2, pop_poorer_fraction, pop_fraction, population, year) %>%
  mutate(
    emission_year = year - year_start,
    ease = 'exponential-in-out') %>%
  tween_elements(time = 'emission_year', group = 'name', ease = 'ease',
    nframes = frames_per_year * (year_end - year_start + 2))
bg_data = gistemp %>%
  mutate(
    emission_year = year - year_start,
    group = 'background',
    ease = 'linear') %>%
  tween_elements(time = 'emission_year',
    group = 'group', ease = 'ease',
    nframes = frames_per_year * (year_end - year_start + 2))
  
message(run.time(), ' interpolating emissions and temp data for animation')

message(run.time(), ' building emissions plot and background')
stplot_bg = ggplot(data = bg_data, aes(frame = .frame)) +
  geom_rect(data = bg_data, aes(fill = temp),
    xmin = 0, xmax = 1, ymin = 0, ymax = 1, size  = 0) +
  geom_text(data = bg_data, aes(label = year %>% as.integer),
    x = 0.25, y = 0.25, size = 64, fontface = 'bold') +
  geom_text(data = bg_data, aes(label = .frame),
    x = 0.25, y = 0.5, size = 64, fontface = 'bold') +
  geom_text(data = bg_data, aes(label = temp),
    x = 0.25, y = 0.75, size = 64, fontface = 'bold') +
  scale_fill_gradientn(colours = bg_colours,
    na.value = 'grey50', guide = FALSE) +
  theme_void(base_size = 32, base_family = plot_font_2) +
  theme(plot.margin = unit(rep(-40, 4), 'mm'))

stplot = ggplot() +
  # emission columns
  geom_col(data = bar_data,
    aes(
      x = pop_poorer_fraction + pop_fraction / 2,
      y = co2 / population * 1000000,
      width = pop_fraction,
      frame = .frame),
    position = 'identity', fill = '#000000') +
  # emission bubbles
  geom_jitter(data = bubble_data,
    aes(
      x = pop_poorer_fraction + pop_fraction / 2,
      y = 25 * atan(2 * .age) + 2.5 * sin(2 * .age),
      size = co2,
      frame = .frame),
    alpha = 0.1, width = 0, height = 0.1, colour = '#000000') +
  geom_text(data = bar_data,
    aes(label = as.integer(.frame / frames_per_year + year_start),
      frame = .frame, x = 0.5, y = 15),
    size = 14, family = plot_font_2, fontface = 'bold') +
  annotate('text', x = 0.0375, y = 15, size = 14, label = 'Poorest',
    family = plot_font_1) +
  annotate('text', x = 0.945, y = 15, size = 14, label = 'Wealthiest',
    family = plot_font_1) +
  scale_x_continuous(labels = scales::percent,
    name = 'Percentile of per capita GDP (2010 USD)',
    breaks = seq(0, 1, 0.25), limits = c(0, 1), expand = c(0, 0.02)) +
  scale_y_continuous(
    name = 'Annual per capita CO2 emissions (tonnes)',
    limits = c(0, 50), breaks = c(10, 20, 30, 40), expand = c(0, 0)) +
  scale_size_area(max_size = 80, guide = FALSE) +
  labs(
    title = 'Historical per capita CO2 emissions, GDP per capita and temperature rise',
    subtitle = paste('Who contributed to the greenhouse effect,',
      'and who suffers for it?'),
    caption = paste0(
      'Data from Open Numbers (', on_url, '), EDGAR (', edgar_url, ') and GISTEMP (', giss_url, ')')) +
  theme_classic(base_size = 32, base_family = plot_font_1) +
  theme(
    plot.title = element_text(family = plot_font_2, face = 'bold'),
    plot.margin = margin(rep(20, 4), 'px'),
    axis.ticks.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank())

message(run.time(), ' rendering emissions plot and background')
animation::ani.options(interval = 0.5 / frames_per_year)
gganimate(stplot_bg, 'steamtrain_bg.mp4',
  ani.width = 1920, ani.height = 1080, title_frame = FALSE)
animation::ani.options(interval = 0.5 / frames_per_year)
gganimate(stplot, 'steamtrain_content.mp4',
  ani.width = 1920, ani.height = 1080, title_frame = TRUE)

message(run.time(), ' done! Output video is steamtrain.mp4.')
