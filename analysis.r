# build fancy plot from data

library(tidyverse)
library(readxl)
library(gganimate)

src.life = download.file()

# download and extract source data from github.com/open-numbers
source.url = 'https://raw.githubusercontent.com/open-numbers/'
source.sets = data_frame(
  repo = c(
    'ddf--gapminder--life_expectancy',
    'ddf--gapminder--population',
    'ddf--cait--historical_emissions'),
  file = c(
    'ddf--datapoints--life_expectancy--by--geo--time.csv',
    'ddf--datapoints--population--by--country--year.csv',
    paste0(
      'ddf--datapoints--total_co2_emissions_excluding_land_use_change_and_',
      'forestry_mtco2--by--country--year.csv')))
src.life =
  read_csv(
    paste0(source.url, source.sets$repo[1], '/master/', source.sets$file[1])) %>%
  dplyr::filter(time >= 1850) %>%
  rename(country = geo, year = time)
src.pop =
  read_csv(
    paste0(source.url, source.sets$repo[2], '/master/', source.sets$file[2])) %>%
  dplyr::filter(time >= 1850)
src.co2 =
  read_csv(
    paste0(source.url, source.sets$repo[3], '/master/', source.sets$file[3]))

data =
  inner_join(src.life, src.pop)
  