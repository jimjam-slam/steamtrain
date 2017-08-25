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