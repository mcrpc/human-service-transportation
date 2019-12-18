acsVariableTibble <- tibble::tribble(
  ~short_name, ~long_name, ~acs_variables_2017,
  "est_pop", "Population Estimate", est_pop <- c("B01001_001E"),
  "est_blw18", "Population Under 18 Estimate", est_blw18 <- paste0("B01001_0", stringr::str_pad(c(3:6,27:30), 2, "left", pad = "0"), "E"),
  "est_18to65", "Population Between 18 and 65 Estimate", est_18to65 <- paste0("B01001_0", stringr::str_pad(c(7:9,10:19,31:43), 2, "left", pad = "0"), "E"),
  "est_ovr65", "est_ovr65", est_ovr65 <- paste0("B01001_0", c(20:25, 44:49), "E"),
  "est_vet", "est_vet", est_vet <- "B21001_002E",
  "est_dsblty", "est_dsblty", est_dsblty <- subset(acs2017VariableTable, stringr::str_detect(name, "B18101_") & stringr::str_detect(label, "With a disability")) %>% .$name %>% paste(.,"E",sep=""),
  "est_blwpov", "est_blwpov", est_blwpov <- "B17020_002E",
  "est_nocars", "est_nocars", est_nocars <- "B08141_002E",
  "gini", "GINI Coefficient", gini <- "B19083_001E",
  "est_nodipl", "est_nodipl", est_nodipl <- paste0("B15003_0", stringr::str_pad(c(2:16), 2, "left", pad = "0"), "E"),
  "inc_percap", "inc_percap", inc_percap <- "B19301_001E",
  "inc_medhh", "inc_medhh", inc_medhh <- "B22008_001E",
  "est_hhSo65", "est_hhSo65", est_hhSo65 <- "B22001_003E"
)

# create function to print warnings when 
warningMessage <- function(object) { 
  paste(
    "Error returned while transmuting",
    substitute(object),
    "\nThis is most likely because it was already transmuted"
  )
}

tryCatch(
  {
    illinoisTractData <- illinoisTractData %>%
      transmute(
        GEOID = GEOID,
        NAME = NAME,
        pct_urban = percentUrban2010,
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
        est_hhSo65 = rowSums(.[names(.) %in% est_hhSo65])
      )
  },
  error = function(err) {
    warning(warningMessage(illinoisTractData), call. = FALSE)
  }
)
