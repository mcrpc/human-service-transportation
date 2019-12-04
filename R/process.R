
# initialize --------------------------------------------------------------
region6CountyList <- c("113", "053", "105", "075", "091")
acsYear <- 2017                                              # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!
acsSurvey <- "acs5"
columnTypeList <- readr::cols(GEOID = col_character())

# retrieve data -----------------------------------------------------------
# create lists of variables to be retrieved
acsTableInventory <- readr::read_csv("inventory.csv")

# retrieve illinois county data from csv
illinoisCountyData2017File <- paste(
  inputDataDirectory,
  "/illinoisCountyData2017.csv",
  sep = ""
)
illinoisCountyData2017 <- readr::read_csv(illinoisCountyData2017File, col_types = columnTypeList)

# retrieve illinois county data from api if csv does not exist
if (!exists("illinoisCountyData2017")) {
  censusTable2010 <- censusapi::getCensus(
    name = "dec/sf1",
    vintage = 2010,
    vars = c("P002001", "P002003"),
    region = "county:*",
    regionin = "state:17"
  ) %>%
    dplyr::mutate(
      GEOID = paste0(state, county, sep = ""),
      population2010 = P002001,
      percentUrban2010 = P002003/P002001
    ) %>%
    dplyr::select(-c(state, county, P002001, P002003)) %>%
    tibble::as_tibble()
  
  acsTable2017 <- purrr::map_dfr(
    dplyr::filter(acsTableInventory, geo_level != "block")[[1]], 
    ~ tidycensus::get_acs(
      geography = "county",
      table = .,
      state = "17",
      year = acsYear,
      survey = acsSurvey,
      output = "wide"
    )
  ) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize_all(coalesceByColumn)
  
  illinoisCountyData2017 <- dplyr::left_join(censusTable2010, acsTable2017)
  readr::write_csv(illinoisCountyData2017, illinoisCountyData2017File)
}


# retrieve region 6 tract data from csv
region6TractData2017File <- paste(
  inputDataDirectory,
  "/region6TractData2017.csv",
  sep = ""
)
region6TractData2017 <- readr::read_csv(region6TractData2017File, col_types = columnTypeList)

# retrieve illinois tract data from api if csv does not exist
if (!exists("region6TractData2017")) {
  censusTable2010 <- censusapi::getCensus(
    name = "dec/sf1",
    vintage = 2010,
    vars = c("P002001", "P002003"),
    region = "tract:*",
    regionin = "state:17"
  ) %>%
    dplyr::mutate(
      GEOID = paste0(state, county, tract, sep = ""),
      population2010 = P002001,
      percentUrban2010 = P002003/P002001
    ) %>%
    dplyr::select(-c(state, county, P002001, P002003)) %>%
    tibble::as_tibble()
  
  acsTable2017 <- purrr::map_dfr(
    dplyr::filter(acsTableInventory, geo_level != "block")[[1]], 
    ~ tidycensus::get_acs(
      geography = "tract",
      table = .,
      state = "17",
      county = region6CountyList,
      year = acsYear,
      survey = acsSurvey,
      output = "wide"
    )
  ) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize_all(coalesceByColumn)
  
  region6TractData2017 <- dplyr::right_join(censusTable2010, acsTable2017)
  readr::write_csv(region6TractData2017, region6TractData2017File)
}


# retrieve region 6 block group data from csv
region6BlockGroupData2017File <- paste(
  inputDataDirectory, 
  "/region6BlockGroupData2017.csv", 
  sep = ""
)
region6BlockGroupData2017 <- read_csv(region6BlockGroupData2017File, col_types = columnTypeList)

# retrieve region 6 block group data from api if csv does not exist
if (!exists("region6BlockGroupData2017")) {
  censusTable2010 <- censusapi::getCensus(
    name = "dec/sf1",
    vintage = 2010,
    vars = c("P002001", "P002003"),
    region = "block group:*",
    regionin = "state:17+county:113+tract:005700"
  ) %>%
    dplyr::mutate(
      GEOID = paste0(state, county, tract, sep = ""),
      population2010 = P002001,
      percentUrban2010 = P002003/P002001
    ) %>%
    dplyr::select(-c(state, county, P002001, P002003)) %>%
    tibble::as_tibble()
  
  acsTable2017 <- purrr::map_dfr(
    dplyr::filter(acsTableInventory, geo_level == "block group")[[1]], 
    ~ tidycensus::get_acs(
      geography = "block group",
      table = .,
      state = "17",
      county = region6CountyList,
      year = acsYear,
      survey = acsSurvey,
      output = "wide"
    )
  ) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize_all(coalesceByColumn)
  
  region6BlockGroupData2017 <- dplyr::right_join(censusTable2010, acsTable2017)
  readr::write_csv(region6BlockGroupData2017, region6BlockGroupData2017File)
}


# start making sense of all this data -------------------------------------
acsVariableList <- c(
  estimatedTotalVariable <- "B01001_001E",
  estimatedUnder18Variable <- paste0(
    "B01001_0",
    str_pad(c(3:6,27:30), 2, "left", pad = "0"),
    "E"
  ),
  estimatedOver18Under65Variable <- paste0(
    "B01001_0", 
    str_pad(c(7:9,10:19,31:43), 2, "left", pad = "0"), 
    "E"
  ),
  estimatedOver65Variable <- paste0("B01001_0", c(20:25, 44:49), "E"),
  estimatedVeteransVariable <- "B21001_002E",
  #popVetsPovvar <- c("C21007_004E", "C21007_019E"),
  #popVetsDisvar <- c("C21007_005E", "C21007_008E", "C21007_020E", "C21007_023E"),
  estimatedWithDisabilityVariable <- subset(
    acsVariableTable2017,
    str_detect(name, "B18101_") & str_detect(label, "With a disability")
  ) %>%
    .$name %>%
    paste(.,"E",sep=""),
  estimatedIncomeBelowPovertyVariable <- "B17020_002E",
  estimatedNoCarsAvailableVariable <- "B08141_002E",
  estimatedGroupQuartersVariable <- "B26001_001E",
  giniCoefficientVariable <- "B19083_001E",
  incomePerCapitaVariable <- "B19301_001E",
  estimatedNoDiplomaVariable <- paste0(
    "B15003_0",
    str_pad(c(2:16), 2, "left", pad = "0"),
    "E"
  ),
  hhSnpOvr60var <- "B22001_003E",
  medHHIncvar <- "B22008_001E"
)

acsVariableLabelList <- c(
  "GEOID" = "GEOID",
  "NAME" = "Geography Name",
  "popTot" = "Total Population",
  "popSub18" = "Population under 18 years old",
  "pop18_65" = "Population between 18 and 65 years old",
  "popOvr65" = "Population over 65 years old",
  "popVets" = "",
  "popDis" = "",
  "popPov" = "",
  "popNoCar" = "",
  "gini" = "",
  "perCapInc" = "",
  "popNoDip" = "",
  "hhSnpOver60" = "Households with Persons Over 60 receiving SNAP",
  "medHHInc" = "Median Household Income"
)

r6ctyTbl <- select(r6ctyTbl_raw, c(GEOID, NAME, one_of(acsVariableList))) %>%
  mutate(
    popTot = rowSums(.[names(.) %in% estimatedTotalVariable]),
    popSub18 = rowSums(.[names(.) %in% estimatedUnder18Variable]),
    pop18_65 = rowSums(.[names(.) %in% estimatedOver18Under65Variable]),
    popOvr65 = rowSums(.[names(.) %in% estimatedOver65Variable]),
    popVets = rowSums(.[names(.) %in% estimatedVeteransVariable]),
    popDis = rowSums(.[names(.) %in% estimatedWithDisabilityVariable]),
    popPov = rowSums(.[names(.) %in% estimatedIncomeBelowPovertyVariable]),
    popNoCar = rowSums(.[names(.) %in% estimatedNoCarsAvailableVariable]),
    popGQ = rowSums(.[names(.) %in% estimatedGroupQuartersVariable]),
    gini = rowSums(.[names(.) %in% giniCoefficientVariable]),
    perCapInc = rowSums(.[names(.) %in% incomePerCapitaVariable]),
    popNoDip = rowSums(.[names(.) %in% estimatedNoDiplomaVariable]),
    hhSnpOvr60 = rowSums(.[names(.) %in% hhSnpOvr60var]),
    medHHInc = rowSums(.[names(.) %in% medHHIncvar])
  ) %>%
  select(-matches("^(B|C)"))

r6trTbl <- select(r6trTbl_raw, c(GEOID, NAME, one_of(acsVariableList))) %>%
  mutate(
    popTot = rowSums(.[names(.) %in% estimatedTotalVariable]),
    popSub18 = rowSums(.[names(.) %in% estimatedUnder18Variable]),
    pop18_65 = rowSums(.[names(.) %in% estimatedOver18Under65Variable]),
    popOvr65 = rowSums(.[names(.) %in% estimatedOver65Variable]),
    popVets = rowSums(.[names(.) %in% estimatedVeteransVariable]),
    popDis = rowSums(.[names(.) %in% estimatedWithDisabilityVariable]),
    popPov = rowSums(.[names(.) %in% estimatedIncomeBelowPovertyVariable]),
    popNoCar = rowSums(.[names(.) %in% estimatedNoCarsAvailableVariable]),
    popGQ = rowSums(.[names(.) %in% estimatedGroupQuartersVariable]),
    gini = rowSums(.[names(.) %in% giniCoefficientVariable]),
    perCapInc = rowSums(.[names(.) %in% incomePerCapitaVariable]),
    popNoDip = rowSums(.[names(.) %in% estimatedNoDiplomaVariable]),
    hhSnpOvr60 = rowSums(.[names(.) %in% hhSnpOvr60var]),
    medHHInc = rowSums(.[names(.) %in% medHHIncvar])
  ) %>%
  dplyr::select(-matches("^(B|C)"))


# get data for Tableau ----------------------------------------------------
CtyTbl_T <- purrr::map_dfr(
  dplyr::filter(acsTableInventory, geo_level != "block")[[1]], 
  ~ tidycensus::get_acs(
    geography = "county",
    table = .,
    state = "17",
    county = region6CountyList,
    year = acsYear,
    survey = acsSurvey,
    output = "tidy"
  )
) %>%
  dplyr::left_join(acsVariableTable2017, by = c("variable" = "name")) %T>%
  readr::write_csv(paste(chartDataDirectory, "data", "CtyTbl_T.csv", sep = "/"))

TrTbl_T <- purrr::map_dfr(
  dplyr::filter(acsTableInventory, geo_level != "block")[[1]], 
  ~ tidycensus::get_acs(
    geography = "tract",
    table = .,
    state = "17",
    county = region6CountyList,
    year = acsYear,
    survey = acsSurvey,
    output = "tidy"
  )
) %>%
  dplyr::left_join(acsVariableTable2017, by = c("variable" = "name")) %>%
  dplyr::semi_join(ruralTractTbl, by = "GEOID") %T>%
  readr::write_csv(paste(chartDataDirectory, "RrlTrTbl_T.csv", sep = "/"))

BgTbl_T <- tidycensus::get_acs(geography = "block group",
                               table = "B01001",
                               state = "17",
                               county = region6CountyList,
                               year = acsYear,
                               survey = acsSurvey,
                               output = "wide")


# LEHD data ---------------------------------------------------------------
setwd("G:/_Projects/HSTP/data_in")
# ctyOD <- grab_lodes(state = "il", year = 2017:2010, lodes_type = "od", job_type = "JT01", segment = "S000", state_part = "main", agg_geo = "county"))
# ctyOD <- subset(ctyOD, str_trunc(w_county, 3, "left", ellipsis = "") %in% region6CountyList | str_trunc(h_county, 3, "left", ellipsis = "") %in% region6CountyList, select = 1:5)
# readr::write_csv(ctyOD, paste(outputDataDirectory, "ctyOD.csv", sep = "/"))
# ctyODaux <- grab_lodes(state = c("il", "in", "ia", "mn", "wi", "mo", "mi"), year = 2017:2007, lodes_type = "od", job_type = "JT01", segment = "S000", state_part = "aux", agg_geo = "county") %>%
#   subset(w_county %in% paste("17", region6CountyList, sep = "") | h_county %in% paste("17", region6CountyList, sep = ""), select = 1:5) %T>%
#   readr::write_csv(paste(outputDataDirectory, "ctyODaux.csv", sep = "/"))

ODCountyTbl <- read_csv(paste(outputDataDirectory, "ctyOD.csv", sep = "/"), col_types = "cccci") %>%
  bind_rows(read_csv(paste(outputDataDirectory, "ctyODaux.csv", sep = "/"), col_types = "cccci"))
ctyNames <- read_csv("ctyNames.csv")





region6CountyString <- paste(region6CountyList, collapse = ",")
region6TractString <- paste(unique(subset(region6TractData2017, str_trunc(GEOID, 5, "right", "")==), collapse = ",")

censusapi::getCensus(name = "dec/sf1",
          vintage = 2010,
          vars = c("P002001", "P002003"),
          region = "block group:*",
          regionin = paste("state:17+county:", region6CountyString, sep = ""))
