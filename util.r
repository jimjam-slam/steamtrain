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