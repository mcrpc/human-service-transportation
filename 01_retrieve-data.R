# initialize local variables ----------------------------------------------
acsSurvey <- "acs5"
censusYear <- 2010
columnTypeList <- readr::cols(GEOID = col_character())


# load data ---------------------------------------------------------------
# for Illinois counties, Illinois tracts, and HSTP region 6 block groups
#   (pulling data for all Illinois block groups resulted in obscenely long 
#     processing times)
# 
# for each dataset, first try to load from saved .csv file,
# if that fails, get data from census API using censusapi and tidycensus
# packages, combining into one table using dplyr

# illinois county data
illinoisCountyDataFileName <- addACSYearsToFilename(
  "illinois-counties.csv",
  acsYear
)

illinoisCountyDataFile <- paste(
  outputDataDirectory,
  illinoisCountyDataFileName,
  sep = "/"
)

illinoisCountyRawData <- tryCatch(
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
    
    illinoisCountyRawData <- dplyr::left_join(censusTable, acsTable)
    readr::write_csv(illinoisCountyRawData, illinoisCountyDataFile)
  }
)

# illinois county previous year data
previousYear <- acsYear - 1

illinoisCountyPreviousYearDataFileName <- addACSYearsToFilename(
  "illinois-counties.csv",
  previousYear
)

previousYearVariableList <- unlist(
  acsVariableTibble$acs_variables_2017
) %>%
  stringr::str_trunc(10, side = "right", ellipsis = "") %>%
  append(
    unlist(acsVariableTibble$denominator) %>%
      stringr::str_trunc(10, side = "right", ellipsis = "")
  )


illinoisCountyPreviousYearDataFile <- paste(
  outputDataDirectory,
  illinoisCountyPreviousYearDataFileName,
  sep = "/"
)

illinoisCountyPreviousYearRawData <- tryCatch(
  {
    readr::read_csv(
      illinoisCountyPreviousYearDataFile,
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
    
    illinoisCountyPreviousYearRawData <- tidycensus::get_acs(
      geography = "county",
      variable = previousYearVariableList,
      state = "17",
      year = previousYear,
      survey = acsSurvey,
      output = "wide"
    ) %>%
      dplyr::left_join(censusTable) %T>%
      readr::write_csv(illinoisCountyPreviousYearDataFile)
  }
)
  
# illinois tract data
illinoisTractDataFileName <- addACSYearsToFilename(
  "illinois-tracts.csv",
  acsYear
)

illinoisTractDataFile <- paste(
  outputDataDirectory,
  illinoisTractDataFileName,
  sep = "/"
)

illinoisTractRawData <- tryCatch(
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
    
    illinoisTractRawData <- dplyr::right_join(censusTable, acsTable)
    readr::write_csv(illinoisTractRawData, illinoisTractDataFile)
  }
)

# illinois tract previous year data
illinoisTractPreviousYearDataFileName <- addACSYearsToFilename(
  "illinois-tracts.csv",
  previousYear
)

illinoisTractPreviousYearDataFile <- paste(
  outputDataDirectory,
  illinoisTractPreviousYearDataFileName,
  sep = "/"
)

illinoisTractPreviousYearRawData <- tryCatch(
  {
    readr::read_csv(
      illinoisTractPreviousYearDataFile,
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
    
    illinoisTractPreviousYearRawData <- tidycensus::get_acs(
      geography = "tract",
      variable = previousYearVariableList,
      state = "17",
      year = previousYear,
      survey = acsSurvey,
      output = "wide"
    ) %>%
      dplyr::right_join(censusTable, .) %T>%
      readr::write_csv(illinoisTractPreviousYearDataFile)
  }
)

# region 6 block group data
region6BlockGroupDataFileName <- addACSYearsToFilename(
  "region-6-block-groups.csv",
  acsYear
)

region6BlockGroupDataFile <- paste(
  outputDataDirectory,
  region6BlockGroupDataFileName,
  sep = "/"
)

region6BlockGroupRawData <- tryCatch(
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
    # the script hangs on the above line if we don't limit ourselves to just
    # region 6 block groups--probably an issue with coalesceByColumn function
    
    region6BlockGroupRawData <- acsTable
    readr::write_csv(region6BlockGroupRawData, region6BlockGroupDataFile)
  }
)

# LEHD data ---------------------------------------------------------------
# horribly outdated, all of this needs to be reworked - TRRILEY, 12/12/19
# ctyOD <- grab_lodes(state = "il", year = 2017:2010, lodes_type = "od", job_type = "JT01", segment = "S000", state_part = "main", agg_geo = "county"))
# ctyOD <- subset(ctyOD, str_trunc(w_county, 3, "left", ellipsis = "") %in% region6CountyList | str_trunc(h_county, 3, "left", ellipsis = "") %in% region6CountyList, select = 1:5)
# readr::write_csv(ctyOD, paste(outputDataDirectory, "ctyOD.csv", sep = "/"))
# ctyODaux <- grab_lodes(state = c("il", "in", "ia", "mn", "wi", "mo", "mi"), year = 2017:2007, lodes_type = "od", job_type = "JT01", segment = "S000", state_part = "aux", agg_geo = "county") %>%
#   subset(w_county %in% paste("17", region6CountyList, sep = "") | h_county %in% paste("17", region6CountyList, sep = ""), select = 1:5) %T>%
#   readr::write_csv(paste(outputDataDirectory, "ctyODaux.csv", sep = "/"))

# ODCountyTbl <- read_csv(paste(outputDataDirectory, "ctyOD.csv", sep = "/"), col_types = "cccci") %>%
#   bind_rows(read_csv(paste(outputDataDirectory, "ctyODaux.csv", sep = "/"), col_types = "cccci"))
# ctyNames <- read_csv("ctyNames.csv")
