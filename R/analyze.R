tbl <- read_csv("G:/_Projects/HSTP/data_out/TrTbl_T.csv", col_types = cols(GEOID = col_character()))


# Region 6 Overall --------------------------------------------------------

# Region 6 -- Rural Tracts Only -------------------------------------------
# total population estimate
estPopR <- filter(tbl, variable == "B01001_001" & urban == FALSE) %>%
  summarize("Estimated Population" = sum(estimate))

# estimate percent of adults who are veterans
vars <- c("B21001_001", "B21001_002")
denom <- filter(tbl, variable == vars[1] & urban == FALSE) %>%
  summarize(sum(estimate))
num <- filter(tbl, variable == vars[2] & urban == FALSE) %>%
  summarize("Estimated Percent Veterans" = sum(estimate))
estVetPerR <- as_tibble(num / denom)

# estimate percent of population with disabilities
vars <- c("C18130_003", "C18130_010", "C18130_017")
denom <- filter(tbl, variable == "C18130_001" & urban == FALSE) %>%
  summarize(sum(estimate))
num <- filter(tbl, variable %in% vars & urban == FALSE) %>%
  summarize("Estimated Percent with Disability" = sum(estimate))
estDisPerR <- as_tibble(num / denom)

# estimate percent of population in poverty
denom <- filter(tbl, variable == "B17020_001" & urban == FALSE) %>%
  summarize(sum(estimate))
num <- filter(tbl, variable == "B17020_002" & urban == FALSE) %>%
  summarize("Estimated Percent in Poverty" = sum(estimate))

estPovPerR <- as_tibble(num / denom)

# estimate median income
medIncR <- filter(tbl, variable == "B22008_001" & urban == FALSE) %>%
  summarize(median(estimate))

# estimate

# Region 6 -- Urban Tracts Only -------------------------------------------


# begin LODES data analysis -----------------------------------------------
#
#
#

# Summarize LODES data per region 6 county
sumWCty <- function(ODCtytbl, ctyFIPS, yr = "2017") {
  ODCtytbl %>%
    subset(year == yr & h_county == ctyFIPS) %>%
    group_by(w_county) %>%
    summarize("jobs" = sum(S000)) %>%
    arrange(desc(jobs)) %>%
    left_join(ctyNames, by = c("w_county" = "GEOID")) %>%
    mutate(percent = jobs/sum(jobs), w_state = str_trunc(w_county, 2, "right", "")) %>%
    print()
}

# iroquois county - 17075 -------------------------------------------------
ic <- "17075"
icOD <- subset(ODCountyTbl, h_county == ic | w_county == ic)
icODWCty <- sumWCty(icOD, ic)
# count folks who commute out of state
group_by(icODWCty, w_state) %>%
  summarize(sum(jobs))
# ford county - 17053 -----------------------------------------------------
fc <- "17053"
fcOD <- subset(ODCountyTbl, h_county == fc)
fcODWCty <- sumWCty(fcOD, fc)
# mclean county - 17113 ---------------------------------------------------
mc <- "17113"
mcOD <- subset(ODCountyTbl, h_county == mc)
mcODWCty <- sumWCty(mcOD, mc)
# count folks who commute out of state
group_by(mcODWCty, w_state) %>%
  summarize(sum(jobs))
# kankakee county - 17091 -------------------------------------------------
kc <- "17091"
kcOD <- subset(ODCountyTbl, h_county == kc)
kcODWCty <- sumWCty(kcOD, kc)
# livingston county - 17105 -----------------------------------------------
lc <- "17105"
lcOD <- subset(ODCountyTbl, h_county == lc)
lcODWCty <- sumWCty(lcOD, lc)
# map to show flow of commuters between counties --------------------------
ODCountyList <- unique(append(with(subset(ODCountyTbl, S000 > 100 & year == 2017), w_county), with(subset(ODCountyTbl, S000 > 100 & year == 2017), h_county)))
ODStateList <- unique(str_trunc(ODCountyList, 2, "right", ""))
ODCountyLyr <- counties(state = ODStateList, cb = TRUE, resolution = "500k", class = "sf") %>%
  semi_join(enframe(ODCountyList, value = "GEOID"))
USStateLyr <- states(cb = TRUE, resolution = "500k", class = "sf")
ODsumJobsTbl <- summarize(group_by(subset(ODCountyTbl, year == 2017), w_county, h_county), jobs = sum(S000))

r6fillcols <- tibble(NAME = r6ctyLyr$NAME, fcolor = RColorBrewer::brewer.pal(5, "Set2"))
r6linecols <- tibble(NAME = r6ctyLyr$NAME, lcolor = RColorBrewer::brewer.pal(5, "Dark2"))
# r6ctyLyr %<>% left_join(r6fillcols) %>% left_join(r6linecols)
ctyPts <- ctyLyr %>%
  st_transform(3443) %>%
  st_centroid() %>%
  select(c(GEOID, NAME))
ODlines <- subset(ODCountyTbl, S000 > 100 & year == 2017) %>%
  select(h_county, w_county, "jobs" = S000) %>%
  od2line(ctyPts, zone_code = "GEOID") %>%
  right_join(select(as_tibble(r6ctyLyr), c(GEOID, NAME)), by = c("h_county" = "GEOID")) %>%
  left_join(r6linecols) %>%
  st_transform(102008) %>%
  subset(st_is_valid(geometry) == TRUE)

# initially had set up to plot data using library(tmap) but was not able to figure out how to vary color based on one attribute
# and line thicknesses based on another

# Helpful Functions -------------------------------------------------------
#save table
write_csv(tbl, "G:/_Projects/HSTP/data_out/TrTbl_T.csv")
