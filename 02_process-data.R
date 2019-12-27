# create tibble to store relationship between desired variable's name,
# its description, and the ACS variables from which it is derived
acsVariableTibble <- tibble::tribble(
  ~short_name, ~long_name, ~acs_variables_2017, ~denominator,
  "est_pop", "Total Population", est_pop <- c("B01001_001E"), NULL,
  "est_blw18", "Population Under 18", est_blw18 <- paste0("B01001_0", stringr::str_pad(c(3:6,27:30), 2, "left", pad = "0"), "E"), dnm_blw18 <- "B01001_001E",
  "est_18to65", "Population Between 18 and 65", est_18to65 <- paste0("B01001_0", stringr::str_pad(c(7:9,10:19,31:43), 2, "left", pad = "0"), "E"), dnm_18to65 <- "B01001_001E",
  "est_ovr65", "Population Over 65", est_ovr65 <- paste0("B01001_0", c(20:25, 44:49), "E"), dnm_ovr65 <- "B01001_001E",
  "est_vet", "Population of Veterans", est_vet <- "B21001_002E", dnm_vet <- "B21001_001E",
  "est_dsblty", "Population with a Disability", est_dsblty <- subset(acs2017VariableTable, stringr::str_detect(name, "B18101_") & stringr::str_detect(label, "With a disability")) %>% .$name %>% paste(.,"E",sep=""), dnm_dsblty <- "B18101_001E",
  "est_blwpov", "Population with household income below poverty", est_blwpov <- "B17020_002E", dnm_blwpov <- "B17020_001E",
  "est_nocars", "Population with no vehicles available", est_nocars <- "B08141_002E", dnm_nocars <- "B08141_001E",
  "gini", "GINI Coefficient", gini <- "B19083_001E", NULL,
  "est_nodipl", "Population without High School Diploma", est_nodipl <- paste0("B15003_0", stringr::str_pad(c(2:16), 2, "left", pad = "0"), "E"), dnm_nodipl <- "B15003_001E",
  "inc_percap", "Per Capita Income", inc_percap <- "B19301_001E", NULL,
  "inc_medhh", "Median Household Income", inc_medhh <- "B22008_001E", NULL,
  "est_hhSo60", "Population in households receiving SNAP with at least one person over 60", est_hhSo60 <- "B22001_003E", dnm_hhSo60 <- "B22001_001E"
)

# create function to print warnings when a tibble has already been transmuted
warningMessage <- function(object) { 
  paste(
    "Error returned while transmuting",
    substitute(object),
    "\nThis is most likely because it was already transmuted"
  )
}

# the tryCatch statement allows us to avoid problems when the tibble has
# already been transmuted, but will mask when other errors happen
tryCatch(
  {
    illinoisTractData <- illinoisTractData %>%
      dplyr::transmute(
        GEOID = GEOID,
        NAME = NAME,
        per_urban = percentUrban2010,
        est_pop = rowSums(.[names(.) %in% est_pop]),
        est_blw18 = rowSums(.[names(.) %in% est_blw18]),
        est_18to65 = rowSums(.[names(.) %in% est_18to65]),
        est_ovr65 = rowSums(.[names(.) %in% est_ovr65]),
        est_vet = rowSums(.[names(.) %in% est_vet]),
        est_dsblty = rowSums(.[names(.) %in% est_dsblty]),
        est_blwpov = rowSums(.[names(.) %in% est_blwpov]),
        est_nocars = rowSums(.[names(.) %in% est_nocars]),
        gini = rowSums(.[names(.) %in% gini]),
        est_nodipl = rowSums(.[names(.) %in% est_nodipl]),
        inc_percap = rowSums(.[names(.) %in% inc_percap]),
        inc_medhh = rowSums(.[names(.) %in% inc_medhh]),
        est_hhSo60 = rowSums(.[names(.) %in% est_hhSo60]),
        dnm_blw18 = rowSums(.[names(.) %in% dnm_blw18]),
        dnm_18to65 = rowSums(.[names(.) %in% dnm_18to65]),
        dnm_ovr65 = rowSums(.[names(.) %in% dnm_ovr65]),
        dnm_vet = rowSums(.[names(.) %in% dnm_vet]),
        dnm_dsblty = rowSums(.[names(.) %in% dnm_dsblty]),
        dnm_blwpov = rowSums(.[names(.) %in% dnm_blwpov]),
        dnm_nocars = rowSums(.[names(.) %in% dnm_nocars]),
        dnm_nodipl = rowSums(.[names(.) %in% dnm_nodipl]),
        dnm_hhSo60 = rowSums(.[names(.) %in% dnm_hhSo60]),
        per_blw18 = est_blw18 / dnm_blw18,
        per_18to65 = est_18to65 / dnm_18to65,
        per_ovr65 = est_ovr65 / dnm_ovr65,
        per_vet = est_vet / dnm_vet,
        per_dsblty = est_dsblty / dnm_dsblty,
        per_blwpov = est_blwpov / dnm_blwpov,
        per_nocars = est_nocars / dnm_nocars,
        per_nodipl = est_nodipl / dnm_nodipl,
        per_hhSo60 = est_hhSo60 / dnm_hhSo60
      )
  },
  error = function(err) {
    warning(warningMessage(illinoisTractData), call. = FALSE)
  }
)

tryCatch(
  {
    illinoisCountyData <- illinoisCountyData %>%
      dplyr::transmute(
        GEOID = GEOID,
        countyFIPS = stringr::str_trunc(GEOID, 3, "left", ""),
        NAME = NAME,
        per_urban = percentUrban2010,
        est_pop = rowSums(.[names(.) %in% est_pop]),
        est_blw18 = rowSums(.[names(.) %in% est_blw18]),
        est_18to65 = rowSums(.[names(.) %in% est_18to65]),
        est_ovr65 = rowSums(.[names(.) %in% est_ovr65]),
        est_vet = rowSums(.[names(.) %in% est_vet]),
        est_dsblty = rowSums(.[names(.) %in% est_dsblty]),
        est_blwpov = rowSums(.[names(.) %in% est_blwpov]),
        est_nocars = rowSums(.[names(.) %in% est_nocars]),
        gini = rowSums(.[names(.) %in% gini]),
        est_nodipl = rowSums(.[names(.) %in% est_nodipl]),
        inc_percap = rowSums(.[names(.) %in% inc_percap]),
        inc_medhh = rowSums(.[names(.) %in% inc_medhh]),
        est_hhSo60 = rowSums(.[names(.) %in% est_hhSo60]),
        dnm_blw18 = rowSums(.[names(.) %in% dnm_blw18]),
        dnm_18to65 = rowSums(.[names(.) %in% dnm_18to65]),
        dnm_ovr65 = rowSums(.[names(.) %in% dnm_ovr65]),
        dnm_vet = rowSums(.[names(.) %in% dnm_vet]),
        dnm_dsblty = rowSums(.[names(.) %in% dnm_dsblty]),
        dnm_blwpov = rowSums(.[names(.) %in% dnm_blwpov]),
        dnm_nocars = rowSums(.[names(.) %in% dnm_nocars]),
        dnm_nodipl = rowSums(.[names(.) %in% dnm_nodipl]),
        dnm_hhSo60 = rowSums(.[names(.) %in% dnm_hhSo60]),
        per_blw18 = est_blw18 / dnm_blw18,
        per_18to65 = est_18to65 / dnm_18to65,
        per_ovr65 = est_ovr65 / dnm_ovr65,
        per_vet = est_vet / dnm_vet,
        per_dsblty = est_dsblty / dnm_dsblty,
        per_blwpov = est_blwpov / dnm_blwpov,
        per_nocars = est_nocars / dnm_nocars,
        per_nodipl = est_nodipl / dnm_nodipl,
        per_hhSo60 = est_hhSo60 / dnm_hhSo60
      )
  },
  error = function(err) {
    warning(warningMessage(illinoisCountyData), call. = FALSE)
  }
)

tryCatch(
  {
    region6BlockGroupData <- region6BlockGroupData %>%
      dplyr::transmute(
        GEOID = GEOID,
        NAME = NAME,
        # per_urban = percentUrban2010,
        est_pop = rowSums(.[names(.) %in% est_pop]),
        est_blw18 = rowSums(.[names(.) %in% est_blw18]),
        est_18to65 = rowSums(.[names(.) %in% est_18to65]),
        est_ovr65 = rowSums(.[names(.) %in% est_ovr65]),
        est_vet = rowSums(.[names(.) %in% est_vet]),
        # est_dsblty = rowSums(.[names(.) %in% est_dsblty]),
        # est_blwpov = rowSums(.[names(.) %in% est_blwpov]),
        # est_nocars = rowSums(.[names(.) %in% est_nocars]),
        # gini = rowSums(.[names(.) %in% gini]),
        # est_nodipl = rowSums(.[names(.) %in% est_nodipl]),
        inc_percap = rowSums(.[names(.) %in% inc_percap]),
        # inc_medhh = rowSums(.[names(.) %in% inc_medhh]),
        # est_hhSo60 = rowSums(.[names(.) %in% est_hhSo60]),
        dnm_blw18 = rowSums(.[names(.) %in% dnm_blw18]),
        dnm_18to65 = rowSums(.[names(.) %in% dnm_18to65]),
        dnm_ovr65 = rowSums(.[names(.) %in% dnm_ovr65]),
        dnm_vet = rowSums(.[names(.) %in% dnm_vet]),
        # dnm_dsblty = rowSums(.[names(.) %in% dnm_dsblty]),
        # dnm_blwpov = rowSums(.[names(.) %in% dnm_blwpov]),
        # dnm_nocars = rowSums(.[names(.) %in% dnm_nocars]),
        # dnm_nodipl = rowSums(.[names(.) %in% dnm_nodipl]),
        # dnm_hhSo60 = rowSums(.[names(.) %in% dnm_hhSo60]),
        per_blw18 = est_blw18 / dnm_blw18,
        per_18to65 = est_18to65 / dnm_18to65,
        per_ovr65 = est_ovr65 / dnm_ovr65,
        per_vet = est_vet / dnm_vet,
        # per_dsblty = est_dsblty / dnm_dsblty,
        # per_blwpov = est_blwpov / dnm_blwpov,
        # per_nocars = est_nocars / dnm_nocars,
        # per_nodipl = est_nodipl / dnm_nodipl,
        # per_hhSo60 = est_hhSo60 / dnm_hhSo60
      )
  },
  error = function(err) {
    warning(warningMessage(region6BlockGroupData), call. = FALSE)
  }
)
