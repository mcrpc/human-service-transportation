# getMaps.R
#
# written to generate maps for 2019-2020 HSTP update
#
# by Tim Riley, 10/30/2019
#
#
#

# set up layers -----------------------------------------------------------
countyLyr <- get_acs(  # this will only be used as a background to the region 6 data
  geography = "county",
  variables = "B01001_001",  # we won't use these estimates. There doesn't seem to be a way to pull only geometry via tidycensus, and I don't want to load another library or dataset just for the background
  state = c("17", "18"),  # include Indiana because it borders Kankakee on the east and white space is bad
  geometry = TRUE,
  output = "wide"
)

r6countyLyr <- get_acs(  # this will be used to highlight region 6 county boundaries in the foreground and for labeling said counties
  geography = "county",
  variables = "B01001_001",  # we won't use these estimates. There doesn't seem to be a way to pull only geometry via tidycensus, and I don't want to load another library or dataset just for the background
  state = c("17"),
  county = counties,
  geometry = TRUE,
  output = "wide"
) %>%
  mutate(NAME = word(NAME, 1)) # replaces the entries in the NAME column with only the first word before a space, for labeling purposes

trGeo <- get_acs(
  geography = "tract",
  variables = c(popTot = "B01001_001",
                hhTot = "B11001_001"),
  state = "17",
  county = counties,
  year = year,
  survey = survey,
  geometry = TRUE,
  output = "wide"  # wide format is better for mapping
) %>%
  getarea() %>%
  rename(popTot = popTotE,
         hhTot = hhTotE) %>%
  mutate(popDen = popTot / area_sq_mi)

bgGeo <- get_acs(
  geography = "block group",
  variables = c(popTot = "B01001_001",
                hhTot = "B11001_001"),
  state = "17",
  county = counties,
  year = year,
  survey = survey,
  geometry = TRUE,
  output = "wide"
) %>%
  getarea() %>%
  rename(popTot = popTotE,
         hhTot = hhTotE) %>%
  mutate(popDen = popTot / area_sq_mi)

trUR <- NULL

for (c in counties) {
  temp <- getCensus(name = "dec/sf1",
                    vintage = 2010,
                    vars = c("P002001", "P002003"),
                    region = "tract:*",
                    regionin = paste("state:17+county:", c))
  trUR <- rbind(trUR, temp)
}

urbanTR <- transmute(trUR,
                     GEOID = paste(state, county, tract, sep = ""),
                     popUrban = P002003 / P002001)

bgUR <- NULL

for (c in counties) {
  for (t in subset(trUR, county == c)$tract) {
    temp <- getCensus(name = "dec/sf1",
                      vintage = 2010,
                      vars = c("P002001", "P002003"),
                      region = "block group:*",
                      regionin = paste("state:17+county:", c, "+tract:", t))
    bgUR <- rbind(bgUR, temp)
  }
}

urbanBG <- transmute(bgUR,
                     GEOID = paste(state, county, tract, block_group, sep = ""),
                     popUrban = P002003 / P002001) %>%
  distinct()    # may not always be necessary, not sure why there are so many duplicates


#below doesn't work currently
# bgGeo_d10 <- get_decennial(geography = "block group",
#                            variables = c(popTot = "P002001"),
#                            state = "17",
#                            year = 2010,
#                            survey = "sf1",
#                            geometry = TRUE,
#                            output = "wide") %>%
#   right_join(map_dfr(counties,
#                        ~ get_decennial(geography = "block group",
#                              variables = c(popUrban = "P002003"),
#                              state = "17",
#                              year = 2010,
#                              survey = "sf1",
#                              county = .,
#                              geometry = FALSE,
#                              output = "wide")))

# set up tables, get and summarize estimates ---------------------------
get_acsTbl <- function(arg1, arg2, arg3 = counties, arg4 = year, arg5 = survey) {
  tbl <- get_acs(
    geography = arg1,
    variables = arg2,
    state = "17",
    county = arg3,
    year = arg4,
    survey = arg5,
    geometry = FALSE,
    output = "tidy"
  )
  tbl
} # helper function to make the following code less of a nightmare to read

if(!is.null(st_read(paste(datadir,"/trLyr.shp", sep="")))) {
  trLyr <- st_read(paste(datadir,"/trLyr.shp", sep=""))
} else {
  
  trTbl <- left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              get_acsTbl("tract", popSub18vars) %>%
                group_by(GEOID) %>%
                summarize(popSub18 = sum(estimate),
                          popSub18M = moe_sum(moe, estimate)),
              get_acsTbl("tract", pop18_65vars) %>%
                group_by(GEOID) %>%
                summarize(pop18_65 = sum(estimate),
                          pop18_65M = moe_sum(moe, estimate))
            ),
            get_acsTbl("tract", popOvr65vars) %>%
              group_by(GEOID) %>%
              summarize(popOvr65 = sum(estimate),
                        popOvr65M = moe_sum(moe, estimate))
          ),
          get_acsTbl("tract", popVetsvars) %>%
            group_by(GEOID) %>%
            summarize(popVets = sum(estimate),
                      popVetsM = moe_sum(moe, estimate))
        ),
        get_acsTbl("tract", popDisvars) %>%
          group_by(GEOID) %>%
          summarize(popDis = sum(estimate),
                    popDisM = moe_sum(moe, estimate))
        # get_acsTbl("tract", popVetsPovvars) %>%
        #   group_by(GEOID) %>%
        #   summarize(popVetsPov = sum(estimate),
        #             popVetsPovM = moe_sum(moe, estimate))
      ),
      get_acsTbl("tract", popPovvars) %>%
        group_by(GEOID) %>%
        summarize(popPov = sum(estimate),
                  popPovM = moe_sum(moe, estimate))
      # get_acsTbl("tract", popVetsDisvars) %>%
      #   group_by(GEOID) %>%
      #   summarize(popVetsDis = sum(estimate),
      #             popVetsDisM = moe_sum(moe, estimate))
    ),
    get_acsTbl("tract", hhNoCarvars) %>%
      group_by(GEOID) %>%
      summarize(hhNoCar = sum(estimate),
                hhNoCarM = moe_sum(moe, estimate))
  )
  
  trLyr <- left_join(trGeo, trTbl) %>%
    mutate(popVets = popVets / popTot,
           popVetsM = moe_ratio(popVets, popTot, popVetsM, popTotM),
           popSub18 = popSub18 / popTot,
           popSub18M = moe_ratio(popSub18, popTot, popSub18M, popTotM),
           pop18_65 = pop18_65 / popTot,
           pop18_65M = moe_ratio(pop18_65, popTot, pop18_65M, popTotM),
           popOvr65 = popOvr65 / popTot,
           popOvr65M = moe_ratio(popOvr65, popTot, popOvr65M, popTotM),
           popDis = popDis / popTot,
           popDisM = moe_ratio(popDis, popTot, popDisM, popTotM),
           popPov = popPov / popTot,
           popPovM = moe_ratio(popPov, popTot, popPovM, popTotM),
           hhNoCar = hhNoCar / hhTot,
           hhNoCarM = moe_ratio(hhNoCar, hhTot, hhNoCarM, hhTotM)) %>%
    left_join(urbanTR)
  
  # popVetsDisPov = (popVetsDis + popVetsPov) / popTot,
  # popVetsDisPovM = moe_ratio(sum(popVetsDis, popVetsPov), popTot, moe_sum(popVetsDisM, popVetsPovM), popTotM)
  
  if(sum(is.null(trLyr)) == 0) {
    print("trLyr is not missing data")
  } else {
    print("Data are missing from trLyr")
  }  # quick check for integrity
}

if(!is.null(st_read(paste(datadir,"/bgLyr.shp", sep="")))) {
  bgLyr <- st_read(paste(datadir,"/bgLyr.shp", sep=""))
} else {
  
  bgTbl <- left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              get_acsTbl("block group", popSub18vars) %>%
                group_by(GEOID) %>%
                summarize(popSub18 = sum(estimate),
                          popSub18M = moe_sum(moe, estimate)),
              get_acsTbl("block group", pop18_65vars) %>%
                group_by(GEOID) %>%
                summarize(pop18_65 = sum(estimate),
                          pop18_65M = moe_sum(moe, estimate))
            ),
            get_acsTbl("block group", popOvr65vars) %>%
              group_by(GEOID) %>%
              summarize(popOvr65 = sum(estimate),
                        popOvr65M = moe_sum(moe, estimate))
          ),
          get_acsTbl("block group", popVetsvars) %>%
            group_by(GEOID) %>%
            summarize(popVets = sum(estimate),
                      popVetsM = moe_sum(moe, estimate))
        ),
        get_acsTbl("block group", popDisvars) %>%
          group_by(GEOID) %>%
          summarize(popDis = sum(estimate),
                    popDisM = moe_sum(moe, estimate))
        # get_acsTbl("block group", popVetsPovvars) %>%
        #   group_by(GEOID) %>%
        #   summarize(popVetsPov = sum(estimate),
        #             popVetsPovM = moe_sum(moe, estimate))
      ),
      get_acsTbl("block group", popPovvars) %>%
        group_by(GEOID) %>%
        summarize(popPov = sum(estimate),
                  popPovM = moe_sum(moe, estimate))
      # get_acsTbl("block group", popVetsDisvars) %>%
      #   group_by(GEOID) %>%
      #   summarize(popVetsDis = sum(estimate),
      #             popVetsDisM = moe_sum(moe, estimate))
    ),
    get_acsTbl("block group", hhNoCarvars) %>%
      group_by(GEOID) %>%
      summarize(hhNoCar = sum(estimate),
                hhNoCarM = moe_sum(moe, estimate))
  )
  
  bgLyr <- left_join(bgGeo, bgTbl) %>%
    mutate(popVets = popVets / popTot,
           popVetsM = moe_ratio(popVets, popTot, popVetsM, popTotM),
           popSub18 = popSub18 / popTot,
           popSub18M = moe_ratio(popSub18, popTot, popSub18M, popTotM),
           pop18_65 = pop18_65 / popTot,
           pop18_65M = moe_ratio(pop18_65, popTot, pop18_65M, popTotM),
           popOvr65 = popOvr65 / popTot,
           popOvr65M = moe_ratio(popOvr65, popTot, popOvr65M, popTotM),
           popDis = popDis / popTot,
           popDisM = moe_ratio(popDis, popTot, popDisM, popTotM),
           popPov = popPov / popTot,
           popPovM = moe_ratio(popPov, popTot, popPovM, popTotM),
           hhNoCar = hhNoCar / hhTot,
           hhNoCarM = moe_ratio(hhNoCar, hhTot, hhNoCarM, hhTotM)) %>%
    left_join(urbanBG)
  
  if(sum(is.null(bgLyr)) == 0) {
    print("bgLyr is not missing data")
  } else {
    print("Data are missing from bgLyr")
  }  # quick check for integrity
}

# HSTP map function -------------------------------------------------------

hstpmap <- function(sf,
                    backLyr = countyLyr,   # "background layer"
                    frontLyr = r6countyLyr, # "foreground layer"
                    var,
                    title,
                    proj = 3443,
                    n = 7,
                    vals = "whole",
                    style = "jenks",
                    palette = "seq"
) {
  require(tidycensus, tidyverse, tmap)
  make_dollar <- function(x, digits = 0) {
    paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
  } # helper function to format dollar amounts
  tmap_mode("plot")
  if (str_length(sf$GEOID[1]) == 11) {
    geoName = "Rural Census Tracts"
  } else if (str_length(sf$GEOID[1]) == 12) {
    geoName = "Rural Census Block Groups"
  } else {
    geoName = "ACS Estimates"
  }
  if (vals == "dollars") {
    lformat = list(fun = make_dollar)
    ltitle = paste("US Dollars,", year)
  } else if (vals == "percent") {
    lformat = list(fun = percent, format = "f", suffix = "%", digits = 2)
    ltitle = paste(geoName, year, sep = ", ")
  } else if (vals == "decimal") {
    lformat = list(format = "f", digits = 2)
    ltitle = paste(geoName, year, sep = ", ")
  } else {
    lformat = list(format = "f", digits = 0)
    ltitle = paste(geoName, year, sep = ", ")
  }
  map <- tm_shape(backLyr,
                  bbox = sf,
                  projection = proj,
                  unit = "mi") +
    tm_fill(col = "grey85") +
    tm_borders(col = "white", lwd = 2) +
    tm_shape(sf) +
    tm_fill(
      col = var,
      n = n,
      style = style,
      title = ltitle,
      palette = palette
    ) +
    tm_borders(col = "grey50", lwd = .5) +
    tm_shape(frontLyr) +
    tm_borders(col = "black", lwd = 2) +
    tm_text(
      text = "NAME",
      size = 1,
      shadow = T
    ) +
    tm_layout(
      legend.position = c("left", "top"),
      legend.title.size = 1.1,
      legend.title.fontface = "bold",
      title = title,
      title.size = 1.2,
      title.fontface = "bold",
      fontfamily = "sans",
      legend.format = lformat,
      frame.lwd = 2,
      outer.bg.color = "#00000000",
    ) +
    tm_scale_bar(
      width = 0.25,
      text.size = .5
    ) +
    tm_credits(
      text = paste("Data Source: Census ACS 5-year estimates,", year-5, "-", year),
      size = .5,
      bg.color = "white",
      bg.alpha = .5
    )
  map
}

map_popVets <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "popVets", title = "Veterans as Percent of Total Population", vals = "percent", palette = "Reds") %T>%
  tmap_save(filename = paste(mapdir, "/popVets_", year, ".pdf", sep = ""))
map_popSub18 <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "popSub18", title = "Percent of Population Under 18", vals = "percent", palette = "Greens") %T>%
  tmap_save(filename = paste(mapdir, "/popSub18_", year, ".pdf", sep = ""))
map_pop18_65 <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "pop18_65", title = "Percent of Population Ages 18-65", vals = "percent", palette = "Blues") %T>%
  tmap_save(filename = paste(mapdir, "/pop18_65_", year, ".pdf", sep = ""))
map_popOvr65 <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "popOvr65", title = "Percent of Population Over 65", vals = "percent", palette = "Oranges") %T>%
  tmap_save(filename = paste(mapdir, "/popOvr65_", year, ".pdf", sep = ""))
map_popDis <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "popDis", title = "Percent of Population with Disabilities", vals = "percent", palette = "Purples") %T>%
  tmap_save(filename = paste(mapdir, "/popDis_", year, ".pdf", sep = ""))
map_popPov <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "popPov", title = "Percent of Population under Poverty Level", vals = "percent", palette = "Reds") %T>%
  tmap_save(filename = paste(mapdir, "/popPov_", year, ".pdf", sep = ""))
map_hhNoCar <- hstpmap(sf = subset(bgLyr, popUrban < ruralThreshold), var = "hhNoCar", title = "Percent of Households with No Vehicle", vals = "percent", palette = "Greens") %T>%
  tmap_save(filename = paste(mapdir, "/hhNoCar_", year, ".pdf", sep = ""))


# save data for quick retrieval -------------------------------------------
st_write(bgLyr,paste(datadir,"/bgLyr.shp", sep=""))
write_csv(bgLyr,paste(datadir,"/blockGroups.csv", sep=""))
st_write(trLyr,paste(datadir,"/trLyr.shp", sep=""))
write_csv(trLyr,paste(datadir,"/tracts.csv", sep=""))
st_write(countyLyr,paste(datadir,"/countyLyr.shp", sep=""))
st_write(r6countyLyr,paste(datadir,"/r6countyLyr.shp", sep=""))
st_write(hwyLyr,paste(datadir,"/hwyLyr.shp", sep=""))



