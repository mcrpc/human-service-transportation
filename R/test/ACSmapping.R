library(tidyverse)
library(tidycensus)
library(tmap)
options(tigris_use_cache = TRUE)

#get tract geometry to test mapping methods in R
vars = c(total_pop = "B01003_001",
          median_income = "B19013_001",
          gini = "B19083_001")

countyLyr <- get_acs(
  geography = "county",
  variables = "B01001_001",
  state = c("17", "18"),
  geometry = TRUE,
  output = "wide"
)

tractLyr <- get_acs(
  geography = "tract",
  variables = vars,
  state = "17",
  county = counties,
  geometry = TRUE,
  output = "wide"
)

# interactive map ---------------------------------------------------------


# the following code generates a neat interactive map using the tmap library
# https://mattherman.info/blog/tidycensus-mult/
# define a little helper function to format dollars for map
make_dollar <- function(x, digits = 0) {
  paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
}

tmap_mode("plot")
mapEcon <- tm_shape(tractSF) +
    tm_fill(
      col = "giniE",
      n = 7,
      palette = "-RdYlGn",
      style = "jenks",
      contrast = c(0.3, 1),
      title = "GINI Index",
      textNA = "Not Available",
      id = "NAME",
      popup.vars = c(
        "GINI Index" = "giniE",
        "Median HH Income" = "median_incomeE",
        "Total Population" = "total_popE"
      ),
      popup.format = list(
        giniE = list(format = "f", digits = 2),
        median_incomeE = list(fun = make_dollar),
        total_popE = list(format = "f", digits = 0)
      ),
      legend.format = list(format = "f", digits = 2),
      legend.reverse = TRUE
    ) +
    tm_borders(col = "darkgray") +
    tm_view(
      alpha = 0.85,
      basemaps = "Stamen.TonerLite",
      view.legend.position = c("right", "bottom")
    )

mapEcon

# HSTP map function -------------------------------------------------------

hstpmap <- function(sf, var, n = 7, palette = "Greens") {
  require(tidycensus, tidyverse, tmap)
  make_dollar <- function(x, digits = 0) {
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
  } #helper function to format dollar amounts
  tmap_mode("plot")
  map <- tm_shape(countyLyr, bbox = sf) +
    tm_fill(col = "lightgray") +
    tm_borders(col = "white") +
    tm_shape(sf) +
    tm_fill(
      col = var,
      n = n,
      palette = palette,
      style = "jenks",
      legend.format = list(format = "f", digits = 2)
    ) +
    tm_borders(col = "darkgray") +
    tmap_style("col_blind")
  map
}

hstpmap(tractLyr, var = "giniE")
