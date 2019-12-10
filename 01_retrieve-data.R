
# initialize local variables ----------------------------------------------
region6CountyList <- c("113", "053", "105", "075", "091")
acsYear <- 2017                                              # !!CHANGE THIS WHEN UPDATED DATA IS RELEASED!!
acsSurvey <- "acs5"
censusYear <- 2010
columnTypeList <- readr::cols(GEOID = col_character())

# load data from network drive --------------------------------------------
# illinois county data
illinoisCountyDataFile <- paste(
  outputDataDirectory,
  "illinois-counties_ACS-2013-2017.csv",
  sep = "/"
)
illinoisCountyData <- tryCatch(
  {
    readr::read_csv(
      illinoisCountyDataFile,
      col_types = columnTypeList
    )
  },
  error = function(err) {
    censusTable <- censusapi::getCensus(
      name = "dec/sf1",
      vintage = censusYear,
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
    
    acsTable <- purrr::map_dfr(
      dplyr::filter(censusDataInventory, geo_level != "block")[[1]], 
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
    
    illinoisCountyData <- dplyr::left_join(censusTable, acsTable)
    readr::write_csv(illinoisCountyData, illinoisCountyDataFile, sep = "/")
  }
)
  
# region 6 tract data
illinoisTractDataFile <- paste(
  outputDataDirectory,
  "illinois-tracts_ACS-2013-2017.csv",
  sep = "/"
)
illinoisTractData <- tryCatch(
  {
    readr::read_csv(
      illinoisTractDataFile,
      col_types = columnTypeList
    )
  },
  error = function(err) {
    censusTable <- censusapi::getCensus(
      name = "dec/sf1",
      vintage = censusYear,
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
    
    acsTable <- purrr::map_dfr(
      dplyr::filter(censusDataInventory, geo_level != "block")[[1]], 
      ~ tidycensus::get_acs(
        geography = "tract",
        table = .,
        state = "17",
        #county = region6CountyList,
        year = acsYear,
        survey = acsSurvey,
        output = "wide"
      )
    ) %>%
      tidyr::gather(variable, value, -GEOID, na.rm = TRUE) %>%
      dplyr::group_by(GEOID, variable) %>%
      dplyr::distinct(value) %>%
      tidyr::spread(variable, value)
    
    illinoisTractData <- dplyr::right_join(censusTable, acsTable)
    readr::write_csv(illinoisTractData, illinoisTractDataFile, sep = "/")
  }
)

# region 6 block group data
region6BlockGroupDataFile <- paste(
  outputDataDirectory,
  "region-6-block-groups_ACS-2013-2017.csv",
  sep = "/"
)
region6BlockGroupData <- tryCatch(
  {
    readr::read_csv(
      region6BlockGroupDataFile,
      col_types = columnTypeList
    )
  },
  error = function(err) {
    # 2019-12-06 - TRRILEY:
    # census API does not support requests for decennial census data for block
    # groups across different tracts and counties, so P002001 and P002003 cannot
    # be retrieved for region 6 block groups and urban population percentage
    # cannot be determined at this time
    
    acsTable <- purrr::map_dfr(
      dplyr::filter(censusDataInventory, geo_level == "block group")[[1]], 
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
    
    region6BlockGroupData <- acsTable
    readr::write_csv(region6BlockGroupData, region6BlockGroupDataFile)
  }
)

# region 6 block group data
if (!exists("region6BlockGroupData")) {
  # 2019-12-06 - TRRILEY:
  # census API does not support requests for decennial census data for block
  # groups across different tracts and counties, so P002001 and P002003 cannot
  # be retrieved for region 6 block groups and urban population percentage
  # cannot be determined at this time

  
  acsTable <- purrr::map_dfr(
    dplyr::filter(censusDataInventory, geo_level == "block group")[[1]], 
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
  
  region6BlockGroupData <- acsTable
  readr::write_csv(
    region6BlockGroupData,
    here::here("data", "raw", region6BlockGroupDataFile)
  )
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
    acs2017VariableTable,
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
  householdOnSNAPFamilyMemberOver60Variable <- "B22001_003E",
  medianHouseholdIncomeVariable <- "B22008_001E"
)

# 2019-12-06 TRRILEY: 
# turn the following list into a data frame with 3 columns:
# 1. variable name, 2. description, 3. shapefile-compatible field name
acsVariableLabelList <- c(
  "GEOID" = "GEOID",
  "NAME" = "Geography Name",
  "estimatedTotal" = "Total Population",
  "estimatedUnder18" = "Population under 18 years old",
  "estimatedOver18Under65" = "Population between 18 and 65 years old",
  "estimatedOver65" = "Population over 65 years old",
  "estimatedVeterans" = "Population with Veteran Status",
  "estimatedWithDisability" = "",
  "estimatedIncomeBelowPoverty" = "",
  "estimatedNoCarsAvailable" = "",
  "giniCoefficient" = "",
  "incomePerCapita" = "",
  "estimatedNoDiploma" = "",
  "householdOnSNAPFamilyMemberOver60" = "Households receiving SNAP with Persons Over 60",
  "medianHouseholdIncome" = "Median Household Income"
)

illinoisCountyTable <- select(
  illinoisCountyData,
  c(GEOID, NAME, one_of(acsVariableList))
) %>%
  mutate(
    estimatedTotal = rowSums(.[names(.) %in% estimatedTotalVariable]),
    estimatedUnder18 = rowSums(.[names(.) %in% estimatedUnder18Variable]),
    estimatedOver18Under65 = rowSums(.[names(.) %in% estimatedOver18Under65Variable]),
    estimatedOver65 = rowSums(.[names(.) %in% estimatedOver65Variable]),
    estimatedVeterans = rowSums(.[names(.) %in% estimatedVeteransVariable]),
    estimatedWithDisability = rowSums(.[names(.) %in% estimatedWithDisabilityVariable]),
    estimatedIncomeBelowPoverty = rowSums(.[names(.) %in% estimatedIncomeBelowPovertyVariable]),
    estimatedNoCarsAvailable = rowSums(.[names(.) %in% estimatedNoCarsAvailableVariable]),
    estimatedGroupQuarters = rowSums(.[names(.) %in% estimatedGroupQuartersVariable]),
    giniCoefficient = rowSums(.[names(.) %in% giniCoefficientVariable]),
    incomePerCapita = rowSums(.[names(.) %in% incomePerCapitaVariable]),
    estimatedNoDiploma = rowSums(.[names(.) %in% estimatedNoDiplomaVariable]),
    householdOnSNAPFamilyMemberOver60 = rowSums(.[names(.) %in% householdOnSNAPFamilyMemberOver60Variable]),
    medianHouseholdIncome = rowSums(.[names(.) %in% medianHouseholdIncomeVariable])
  ) %>%
  select(-matches("^(B|C)"))

region6TractTable <- select(
  region6TractData,
  c(GEOID, NAME, one_of(acsVariableList))
) %>%
  mutate(
    estimatedTotal = rowSums(.[names(.) %in% estimatedTotalVariable]),
    estimatedUnder18 = rowSums(.[names(.) %in% estimatedUnder18Variable]),
    estimatedOver18Under65 = rowSums(.[names(.) %in% estimatedOver18Under65Variable]),
    estimatedOver65 = rowSums(.[names(.) %in% estimatedOver65Variable]),
    estimatedVeterans = rowSums(.[names(.) %in% estimatedVeteransVariable]),
    estimatedWithDisability = rowSums(.[names(.) %in% estimatedWithDisabilityVariable]),
    estimatedIncomeBelowPoverty = rowSums(.[names(.) %in% estimatedIncomeBelowPovertyVariable]),
    estimatedNoCarsAvailable = rowSums(.[names(.) %in% estimatedNoCarsAvailableVariable]),
    estimatedGroupQuarters = rowSums(.[names(.) %in% estimatedGroupQuartersVariable]),
    giniCoefficient = rowSums(.[names(.) %in% giniCoefficientVariable]),
    incomePerCapita = rowSums(.[names(.) %in% incomePerCapitaVariable]),
    estimatedNoDiploma = rowSums(.[names(.) %in% estimatedNoDiplomaVariable]),
    householdOnSNAPFamilyMemberOver60 = rowSums(.[names(.) %in% householdOnSNAPFamilyMemberOver60variable]),
    medianHouseholdIncome = rowSums(.[names(.) %in% medianHouseholdIncomevar])
  ) %>%
  dplyr::select(-matches("^(B|C)"))


# get data for Tableau ----------------------------------------------------
CtyTbl_T <- purrr::map_dfr(
  dplyr::filter(censusDataInventory, geo_level != "block")[[1]], 
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
  dplyr::left_join(acs2017VariableTable, by = c("variable" = "name")) %T>%
  readr::write_csv(paste(chartDataDirectory, "data", "CtyTbl_T.csv", sep = "/"))

TrTbl_T <- purrr::map_dfr(
  dplyr::filter(censusDataInventory, geo_level != "block")[[1]], 
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
  dplyr::left_join(acs2017VariableTable, by = c("variable" = "name")) %>%
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
# ctyOD <- grab_lodes(state = "il", year = 2017:2010, lodes_type = "od", job_type = "JT01", segment = "S000", state_part = "main", agg_geo = "county"))
# ctyOD <- subset(ctyOD, str_trunc(w_county, 3, "left", ellipsis = "") %in% region6CountyList | str_trunc(h_county, 3, "left", ellipsis = "") %in% region6CountyList, select = 1:5)
# readr::write_csv(ctyOD, paste(outputDataDirectory, "ctyOD.csv", sep = "/"))
# ctyODaux <- grab_lodes(state = c("il", "in", "ia", "mn", "wi", "mo", "mi"), year = 2017:2007, lodes_type = "od", job_type = "JT01", segment = "S000", state_part = "aux", agg_geo = "county") %>%
#   subset(w_county %in% paste("17", region6CountyList, sep = "") | h_county %in% paste("17", region6CountyList, sep = ""), select = 1:5) %T>%
#   readr::write_csv(paste(outputDataDirectory, "ctyODaux.csv", sep = "/"))

ODCountyTbl <- read_csv(paste(outputDataDirectory, "ctyOD.csv", sep = "/"), col_types = "cccci") %>%
  bind_rows(read_csv(paste(outputDataDirectory, "ctyODaux.csv", sep = "/"), col_types = "cccci"))
ctyNames <- read_csv("ctyNames.csv")
