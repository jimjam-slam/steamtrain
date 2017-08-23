# build fancy steam train plot from downloaded gapminder and berkley earth data
# james goldie ('rensa'), august 2017

library(RCurl)
library(rvest)
library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)
library(fuzzyjoin)
library(gganimate)
library(viridis)
filter = dplyr::filter # this is going to kill me one day

source('util.r')
if(!dir.exists('data'))
{
  dir.create('data')
}

# constants
on_url = 'https://raw.githubusercontent.com/open-numbers/'
giss_url = 'https://data.giss.nasa.gov/tmp/modelE/ltmap/'
berk_url = 'http://berkeleyearth.lbl.gov/auto/Regional/TAVG/Text/'
year_start = 1900
year_end = 2012

# download gapminder files if they haven't been downloaded before
message(run.time(), ' downloading any missing gapminder data')
gap_files = data_frame(
  repo = c(
    'ddf--gapminder--population',
    'ddf--cait--historical_emissions',
    'ddf--gapminder--gdp_per_capita_cppp',
    'ddf--gapminder--population',
    'ddf--cait--historical_emissions'),
  file = c(
    'ddf--entities--geo--country.csv',
    'ddf--entities--country.csv',
    'ddf--datapoints--gdp_per_capita_cppp--by--geo--time.csv',
    'ddf--datapoints--population--by--country--year.csv',
    paste0(
      'ddf--datapoints--total_co2_emissions_excluding_land_use_change_and_',
      'forestry_mtco2--by--country--year.csv')),
  url = paste0(on_url, repo, '/master/', file))
mapply(download.file,
  gap_files$url[which(!file.exists(paste0('data/', gap_files$file)))],
  destfile =
    paste0('data/', gap_files$file[which(!file.exists(gap_files$file))]))

# load and tidy gapminder country equivalences
message(run.time(), ' loading and tidying any missing gapminder data')
geo_gap =
  read_csv('data/ddf--entities--geo--country.csv') %>%
  select(country, name)
geo_co2 =
  read_csv('data/ddf--entities--country.csv')

# download and tidy gapminder data sets
gdp_percap = 
  read_csv('data/ddf--datapoints--gdp_per_capita_cppp--by--geo--time.csv') %>%
  rename(country = geo, year = time, gdppc = gdp_per_capita_cppp) %>%
  filter(year >= year_start & year <= year_end) %>%
  inner_join(geo_gap) %>%
  select(name, year, gdppc)
pop =
  read_csv('data/ddf--datapoints--population--by--country--year.csv') %>%
  filter(year >= year_start & year <= year_end) %>%
  inner_join(geo_gap) %>%
  select(name, year, population)
co2 =
  read_csv(paste0(
    'data/ddf--datapoints--total_co2_emissions_excluding_land_use_change_','and_forestry_mtco2--by--country--year.csv')) %>%
  rename(
    co2 = total_co2_emissions_excluding_land_use_change_and_forestry_mtco2) %>%
  filter(year >= year_start & year <= year_end) %>%
  inner_join(geo_co2) %>%
  select(name, year, co2)

# combine gapminder datasets
message(run.time(), ' combining gapminder data')
gapdata = co2 %>%
  inner_join(gdp_percap, by = c('name', 'year')) %>%
  inner_join(pop, by = c('name', 'year')) %>%
  mutate(name_lowercase = str_to_lower(name))

# this is the gistemp version... it's not available by country
# temp =
#   read_csv(
#     paste0(giss_url, 'tmp.4_obsLOTI_E5_12_1880_2017_1951_1980-0/global.csv'),
#     skip = 1, col_types = cols(.default = col_double())) %>%
#   rename(year = Year, monthly_temp = `Global Mean`) %>%
#   filter(year >= year_start, year <= year_end) %>%
#   mutate(year = floor(year)) %>%
#   group_by(year) %>%
#   summarise(mean_temp = mean(monthly_temp))

# scrape a list of available berkley earth country temp data files and
# match them fuzzily against gapdata countries
message(run.time(), ' scraping list of available berkley temperature data')
berk_files =
  data_frame(
    filename = berk_url %>% read_html %>% html_nodes('a') %>% html_text) %>%
  slice(-(1:6)) %>%
  mutate(
    name_lowercase = str_replace(filename, '-TAVG-Trend.txt', ''),
    name = str_to_title(name_lowercase)) %>%
  # problematic rows
  filter(!name %in% c('Pará', 'Côte-D\'ivoire')) %>%
  stringdist_semi_join(gapdata, by = 'name_lowercase',
    distance_col = 'gapmatch')

# TODO - got about 180 countries between all datasets at this point
# might need to fiddle with the strings to get the edge cases...

# download them all, tag each one with the country and bind them together
temp =
  lapply(
    berk_files$name_lowercase, function(x)
    {
      if (!file.exists(paste0('data/', x, '-TAVG-Trend.txt')))
      {
        message(run.time(), ' downloading berkley temperature data for ', x)
        download.file(paste0(berk_url, x, '-TAVG-Trend.txt'),
          destfile = paste0('data/', x, '-TAVG-Trend.txt'))
      } else
      {
        message(run.time(), ' found berkley temperature data for ', x)
      }
      read_table2(
        paste0('data/', x, '-TAVG-Trend.txt'),
        comment = '%', skip = 1, col_types = 'ii--dd------',
        col_names = FALSE) %>%
        mutate(name_lowercase = x)
    }) %>%
  bind_rows %>%
  rename(year = X1, month = X2, temp = X5, temp_unc = X6) %>%
  filter(year >= year_start & year <= year_end & month == 6) %>%
  select(-month) %>%
  mutate(
    name = str_to_title(name_lowercase),
    temp_min = temp - temp_unc,
    temp_max = temp + temp_unc)

# WARNING - this looks like it takes a looooooong time. maybe don't.
# all_data = temp %>%
#   stringdist_inner_join(gapdata, by = c('name', 'year'))

# start plotting! 
