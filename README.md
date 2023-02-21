# steamtrain

> **Note:** this project has been offline for a while and is now archived and read-only.

This is a plot of countries' CO2 emissions and temperature change over the last century (1900–2012), arranged by GDP per capita (poor countries on the left; wealthy countries on the right).

### Background

This plot brings together several sets of annual data for about 160 of the world's countries: from Gapminder, [population](https://github.com/open-numbers/ddf--gapminder--population/) and [GDP per capita](https://github.com/open-numbers/ddf--gapminder--population/); from CAIT, [CO2 emissions (excluding land use)](https://github.com/open-numbers/ddf--cait--historical_emissions) and from Berkeley Earth, [annual temperature anomalies](berkeleyearth.lbl.gov).

There're a few caveats to the data and the visualisation:

- The temperature estimates come with substantial uncertainties that are not shown on this plot (though I do import the data, so you can inspect it here and modify the plot to show them if you like). They're not substantial enough to cast the global trend into doubt.
- I chose GDP per capita partly because I think wealth is an interesting angle, btu partly also because there're estimates available for it far earlier than most other indicators of development.

This plot uses (and, in the case of the plotting packages, abuses) a bunch of great R packages: the [`tidyverse`](https://tidyverse.org), of course, but also [`rvest`](https://cran.r-project.org/web/packages/rvest/) and [`RCurl`](https://cran.r-project.org/web/packages/RCurl/) (to scrape the available temperature data from BE), [`fuzzyjoin`](https://cran.r-project.org/web/packages/fuzzyjoin/) (to join the countries up between Berkeley and Gapminder's slightly different names) and [`gganimate`](https://github.com/dgrtwo/gganimate) and [`tweenr`](https://cran.r-project.org/web/packages/tweenr/) (to animate the plot across and between years).
