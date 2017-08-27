#!/usr/bin/env Rscript --vanilla
# download and combine gapminder and berkeley earth data
# james goldie ('rensa'), august 2017

library(tidyverse)
library(magrittr)
library(readxl)
filter = dplyr::filter # this is going to kill me one day
source('util.r')

# constants
on_url = 'https://raw.githubusercontent.com/open-numbers/'
edgar_url = 'http://edgar.jrc.ec.europa.eu/news_docs/'
giss_url = paste0(
  'https://data.giss.nasa.gov/tmp/modelE/ltmap/',
  'tmp.4_obsLOTI_E5_12_1880_2017_1951_1980-0/')
year_start = 1900
year_end = 2012

# create data directory if it's missing
if(!dir.exists('data'))
{
  dir.create('data')
}

# download gapminder files if they're missing
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
gap_files_to_download = which(!file.exists(paste0('data/', gap_files$file)))
if (length(gap_files_to_download) > 0)
{
  message(run.time(), ' downloading missing gapminder data')
  mapply(download.file,
    gap_files$url[which(!file.exists(paste0('data/', gap_files$file)))],
    destfile =
      paste0('data/', gap_files$file[which(!file.exists(gap_files$file))])) 
} else
{
  message(run.time(), ' found all gapminder data')
}

# load and tidy gapminder country code maps
message(run.time(), ' loading and tidying any missing gapminder data')
geo_gap =
  read_csv('data/ddf--entities--geo--country.csv') %>%
  select(country, name)
geo_co2 =
  read_csv('data/ddf--entities--country.csv')

# load and tidy gapminder data sets
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

# now the edgar data (to extend our series an extra two years to 2015)
# if (!file.exists('data/edgar-co2-raw.xls'))
# {
#   message(run.time(), ' downloading edgar data')
#   download.file(
#     paste0(edgar_url, 'CO2_1970-2015_dataset_of_CO2_report_2016.xls'),
#     destfile = 'data/edgar-co2-raw.xls')
# } else
# {
#   message(run.time(), ' found edgar data')
# }
# 
# load and tidy the edgar co2 estimates
# co2_edgar =
#   read_excel('data/edgar-co2-raw.xls', skip = 11) %>%
#   gather(key = year, value = co2, `1970`:`2015`) %>%
#   rename(name = Country) %>%
#   filter(!is.na(name)) %>%
#   mutate(
#     year = year %>% as.integer,
#     co2 = co2 / 1000)

# combine gapminder datasets,
# rank countries each year by their gdp per capita
# claculation global pop, pop fraction and number of poorer people each year
message(run.time(), ' combining gapminder data')
gapdata = co2 %>%
  inner_join(gdp_percap, by = c('name', 'year')) %>%
  inner_join(pop, by = c('name', 'year')) %>%
  mutate(annual_devrank = ave(gdppc, year, FUN = rank)) %>%
  group_by(year) %>%
  arrange(annual_devrank) %>%
  mutate(
    pop_global = sum(population),
    pop_fraction = population / pop_global,
    pop_poorer = cumsum(population) - population,
    pop_poorer_fraction = pop_poorer / pop_global) %>%
  ungroup() %>%
  mutate(., emission_id = group_indices(., name, year)) %>%
  write_csv('data/gapminder-tidy.csv')

# now the gistemp data
if (!file.exists('data/gistemp-raw.csv'))
{
  message(run.time(), ' downloading gistemp data')
  download.file(paste0(giss_url, 'global.csv'),
    destfile = 'data/gistemp-raw.csv')
} else
{
  message(run.time(), ' found gistemp data')
}

gistemp =
  read_csv(
    'data/gistemp-raw.csv', skip = 1, col_names = c('year', 'temp'),
    col_types = cols(.default = col_double())) %>%
  filter(year %% 1 > 0.45 & year %% 1 < 0.55) %>%
  mutate(year = year %>% floor %>% as.integer) %>%
  filter(year >= year_start & year <= year_end) %>%
  write_csv('data/gistemp-tidy.csv')

message(run.time(), ' done!')
