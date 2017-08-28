# helper functions for analysis.r

# run.time: returns the time elapsed since this script was
# sourced in sensible units as a string ('**h**m**s')
script_start_time = Sys.time()
run.time = function()
{   
  now = Sys.time()
  return(paste0(
    as.integer(difftime(now, script_start_time, units = 'hours')), 'h',
    as.integer(difftime(now, script_start_time, units = 'mins')) %% 60, 'm',
    as.integer(difftime(now, script_start_time, units = 'secs')) %% 60, 's'))
}

# jitter_emissions: produce a uniformly-distributed random number that is
# constant for a given seed (in this case, a unique id tied to each
# emission bubble). designed to break up the emission bubbles, since jitter
# isn't an aesthetic in ggplot2.
jitter_emissions = function(seed, min_jitter = 1, max_jitter = 5)
{
  set.seed(seed)
  return(runif(1, min = min_jitter, max = max_jitter))
}

# iso_to_emoji: for a vector of two-letter iso codes, returns a corresponding
# vector of unicode control sequences.
# great for use with countrycode and emojifont!
iso_to_emoji = function(iso_codes)
{
  if (!any(
    nchar(iso_codes) == 2 |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be two (2) letters long.')
  }
  if (!any(
    str_detect(iso_codes, pattern = '[a-z][a-z]') |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be letters only.')
  }
  
  return(str_replace_all(str_to_lower(iso_codes), c('a' = '🇦', 'b' = '🇧', 'c' = '🇨',
    'd' = '🇩', 'e' = '🇪', 'f' = '🇫', 'g' = '🇬', 'h' = '🇭', 'i' = '🇮',
    'j' = '🇯', 'k' = '🇰', 'l' = '🇱', 'm' = '🇲', 'n' = '🇳', 'o' = '🇴',
    'p' = '🇵', 'q' = '🇶', 'r' = '🇷', 's' = '🇸', 't' = '🇹', 'u' = '🇺',
    'v' = '🇻', 'w' = '🇼', 'x' = '🇽', 'y' = '🇾', 'z' = '🇿')))
}