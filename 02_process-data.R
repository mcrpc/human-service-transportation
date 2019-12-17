acsVariableTibble <- tibble::tribble(
  ~short_name, ~long_name, ~acs_variables_2017,
  "est_pop", "Population Estimate", c("B01001_001E"),
  "est_blw18", "Population Under 18 Estimate", paste0("B01001_0", stringr::str_pad(c(3:6,27:30), 2, "left", pad = "0"), "E"),
  "est_18to65", "Population Between 18 and 65 Estimate", paste0("B01001_0", str_pad(c(7:9,10:19,31:43), 2, "left", pad = "0"), "E"),
  "est_ovr65", NULL, NULL,
  "est_vet", NULL, NULL,
  "est_dsblty", NULL, NULL,
  "est_blwpov", NULL, NULL,
  "est_nocars", NULL, NULL,
  "gini", NULL, NULL,
  "est_nodipl", NULL, NULL,
  "inc_percap", NULL, NULL,
  "inc_medhh", NULL, NULL,
  "est_hhSo65", NULL, NULL,
)

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
