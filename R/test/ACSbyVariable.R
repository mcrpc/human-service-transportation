library(tidyverse)
library(tidycensus)


setwd("G:/_Projects/HSTP/ACS")
api_key <- "2f44a09c684e6b031d3e76f3655c169c167aeba8"


# retrieve ACS data via API -----------------------------------------------
#census_api_key(api_key)
# run these lines to get a searchable list of variables
v17 <- load_variables(2017, "acs5", cache = TRUE)

View(v17)

# change these objects to modify the geography, area, year, and scope of the estimates retrieved
geography <- "block group"
counties <- c("McLean",
              "Ford",
              "Livingston",
              "Iroquois",
              "Kankakee")
year <- 2017
survey <- "acs5"

# add new population variables to retreive here
# https://stackoverflow.com/questions/5812493/how-to-add-leading-zeros
popBelow18vars <- c(paste0("B01001_00", c(3:6)), paste0("B01001_0", c(27:30)))
pop18_65vars <- c(paste0("B01001_00", c(7:9)), paste0("B01001_0", c(10:19, 31:43)))
pop65Plusvars <- paste0("B01001_0", c(20:25, 44:49))
popVetsvars <- "B21001_002"
popVetsPovvars <- c("C21007_004", "C21007_019")
popVetsDisvars <- c("C21007_005", "C21007_008", "C21007_020", "C21007_023")

# retrieve population variables from ACS API
popBelow18 <- get_acs(geography = geography,
                      variables = popBelow18vars,
                      state = "IL",
                      county = counties,
                      year = year,
                      survey = survey)

popBelow18 <- popBelow18 %>%
  group_by(GEOID) %>%
  summarize(popBelow18 = sum(estimate),
            popBelow18_moe = moe_sum(moe, estimate))

pop18_65 <- get_acs(geography = geography,
                      variables = pop18_65vars,
                      state = "IL",
                      county = counties,
                      year = year,
                      survey = survey)

pop18_65 <- pop18_65 %>%
  group_by(GEOID) %>%
  summarize(pop18_65 = sum(estimate),
            pop18_65_moe = moe_sum(moe, estimate))

pop65Plus <- get_acs(geography = geography,
                      variables = pop65Plusvars,
                      state = "IL",
                      county = counties,
                      year = year,
                      survey = survey)

pop65Plus <- pop65Plus %>%
  group_by(GEOID) %>%
  summarize(pop65Plus = sum(estimate),
            pop65Plus_moe = moe_sum(moe, estimate))

popVets <- get_acs(geography = geography,
                   variables = popVetsvars,
                   state = "IL",
                   county = counties,
                   year = year,
                   survey = survey) %>%
  select(-c(NAME,variable))
colnames(popVets) <- c("GEOID", "popVets", "popVets_moe")

popVetsPov <- get_acs(geography = geography,
                      variables = popVetsPovvars,
                      state = "IL",
                      county = counties,
                      year = year,
                      survey = survey)

popVetsPov <- popVetsPov %>%
  group_by(GEOID) %>%
  summarize(popVetsPov = sum(estimate),
            popVetsPov_moe = moe_sum(moe, estimate))

popVetsDis <- get_acs(geography = geography,
                      variables = popVetsDisvars,
                      state = "IL",
                      county = counties,
                      year = year,
                      survey = survey)

popVetsDis <- popVetsDis %>%
  group_by(GEOID) %>%
  summarize(popVetsDis = sum(estimate),
            popVetsDis_moe = moe_sum(moe, estimate))

# insert any additional population variables here
#
#

#get total population estimates
popTotal <- get_acs(geography = geography,
                    variables = "B01001_001",
                    state = "IL",
                    county = counties,
                    year = year,
                    survey = survey) %>%
  select(-c(NAME,variable))
colnames(popTotal) <- c("GEOID", "popTotal", "popTotal_moe")

# combine total and variables into one table
popTableBG <- left_join(popTotal, popBelow18) %>%
  left_join(pop18_65) %>%
  left_join(pop65Plus) %>%
  left_join(popVets) %>%
  left_join(popVetsPov) %>%
  left_join(popVetsDis)

# get tract-level population variables (some are unreliable or unavailable at block group level)
popPovvars <- "B14006_002"
popPovColvars <- c("B14006_010", "B14006_009") #this estimate may be used to "filter out" college students from the poverty estimates
popTotalTRvars <- "B14006_001"

popTotalTR <- get_acs(geography = "tract",
                  variables = popTotalTRvars,
                  state = "IL",
                  county = counties,
                  year = year,
                  survey = survey) %>%
  select(-c(NAME,variable))
colnames(popTotalTR) <- c("GEOID", "popTotal", "popTotal_moe")

popPov <- get_acs(geography = "tract",
                  variables = popPovvars,
                  state = "IL",
                  county = counties,
                  year = year,
                  survey = survey) %>%
  select(-c(NAME,variable))
colnames(popPov) <- c("GEOID", "popPov", "popPov_moe")

popPovCol <- get_acs(geography = "tract",
                         variables = popPovColvars,
                         state = "IL",
                         county = counties,
                         year = year,
                         survey = survey)

popPovCol <- popPovCol %>%
  group_by(GEOID) %>%
  summarize(popPovCol = sum(estimate),
            popPovCol_moe = moe_sum(moe, estimate))

popTableTR <- left_join(popTotalTR, popPov) %>%
  left_join(popPovCol) %>%
  group_by(GEOID) %>%
  summarize(popTotal = popTotal,
            popTotal_moe = popTotal_moe,
            popPovNC = (popPov - popPovCol),
            popPovNC_moe = moe_sum(c(popPov_moe, popPovCol_moe), c(popPov, popPovCol)), #moe_sum should still work even though we subtracted popPovCol from popPov
            popPovNCPer = (popPovNC / popTotal),
            popPovNCPer_moe = moe_prop(popPovNC, popTotal, popPovNC_moe, popTotal_moe))

# add new household variables to retrieve here
hhTotalvars <- c("B08201_001")
hhNoCarvars <- c("B08201_002")

# retrieve household variables from ACS API
# household variables only available at tract level
hhTotal <- get_acs(geography = "tract",
                   variables = hhTotalvars,
                   state = "IL",
                   county = counties,
                   year = year,
                   survey = survey) %>%
  select(-c(NAME,variable))
colnames(hhTotal) <- c("GEOID", "hhTotal", "hhTotal_moe")

hhNoCar <- get_acs(geography = "tract",
                   variables = hhNoCarvars,
                   state = "IL",
                   county = counties,
                   year = year,
                   survey = survey) %>%
  select(-c(NAME,variable))
colnames(hhNoCar) <- c("GEOID", "hhNoCar", "hhNoCar_moe")


# combine total and variables into one table
hhTableTR <- left_join(hhTotal, hhNoCar)

# write tables to csv
write_csv(popTableBG, "r6BGpopTable.csv")
write_csv(popTableTR, "r6TRpopTable.csv")
write_csv(hhTableTR, "r6TRhhTable.csv")