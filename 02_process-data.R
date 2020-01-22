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
transmuteDataTables <- function(dataTibble) {
  dataTibble <- dataTibble %>%
    dplyr::transmute(
      GEOID = GEOID,
      NAME = NAME,
      per_urban = percentUrban2010,
      est_pop = rowSums(.[names(.) %in% est_pop]),
      est_blw18 = rowSums(.[names(.) %in% est_blw18]),
      est_18to65 = rowSums(.[names(.) %in% est_18to65]),
      est_ovr65 = rowSums(.[names(.) %in% est_ovr65]),
      est_vet = rowSums(.[names(.) %in% est_vet]),
      est_veto55 = rowSums(.[names(.) %in% est_veto55]),
      est_dsblty = rowSums(.[names(.) %in% est_dsblty]),
      est_blwpov = rowSums(.[names(.) %in% est_blwpov]),
      est_nocars = rowSums(.[names(.) %in% est_nocars]),
      gini = rowSums(.[names(.) %in% gini]),
      est_nodipl = rowSums(.[names(.) %in% est_nodipl]),
      inc_percap = rowSums(.[names(.) %in% inc_percap]),
      inc_medhh = rowSums(.[names(.) %in% inc_medhh]),
      est_hhSo60 = rowSums(.[names(.) %in% est_hhSo60]),
      est_alttrn = rowSums(.[names(.) %in% est_alttrn]),
      dnm_blw18 = rowSums(.[names(.) %in% dnm_blw18]),
      dnm_18to65 = rowSums(.[names(.) %in% dnm_18to65]),
      dnm_ovr65 = rowSums(.[names(.) %in% dnm_ovr65]),
      dnm_vet = rowSums(.[names(.) %in% dnm_vet]),
      dnm_veto55 = rowSums(.[names(.) %in% dnm_veto55]),
      dnm_dsblty = rowSums(.[names(.) %in% dnm_dsblty]),
      dnm_blwpov = rowSums(.[names(.) %in% dnm_blwpov]),
      dnm_nocars = rowSums(.[names(.) %in% dnm_nocars]),
      dnm_nodipl = rowSums(.[names(.) %in% dnm_nodipl]),
      dnm_hhSo60 = rowSums(.[names(.) %in% dnm_hhSo60]),
      dnm_alttrn = rowSums(.[names(.) %in% dnm_alttrn]),
      per_blw18 = est_blw18 / dnm_blw18,
      per_18to65 = est_18to65 / dnm_18to65,
      per_ovr65 = est_ovr65 / dnm_ovr65,
      per_vet = est_vet / dnm_vet,
      per_veto55 = est_veto55 / dnm_veto55,
      per_dsblty = est_dsblty / dnm_dsblty,
      per_blwpov = est_blwpov / dnm_blwpov,
      per_nocars = est_nocars / dnm_nocars,
      per_nodipl = est_nodipl / dnm_nodipl,
      per_hhSo60 = est_hhSo60 / dnm_hhSo60,
      per_alttrn = est_alttrn / dnm_alttrn
    )
}

# adds rows with desired summary stats
getSummaryStatistic <- function(df, stat) {
  dplyr::bind_rows(
    df,
    dplyr::bind_cols(
      GEOID = "17", NAME = deparse(substitute(stat)),
      select(df, -c(GEOID, NAME)) %>%
        summarize_all(stat)
    )
  )
}

illinoisCountyData <- illinoisCountyRawData %>%
  transmuteDataTables()

illinoisCountyPreviousYearData <- illinoisCountyPreviousYearRawData %>%
  transmuteDataTables()

illinoisTractData <- illinoisTractRawData %>%
  transmuteDataTables()

illinoisTractPreviousYearData <- illinoisTractPreviousYearRawData %>%
  transmuteDataTables()

region6BlockGroupData <- region6BlockGroupRawData %>%
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
  ) %>%
  getSummaryStatistic(mean) %>%
  getSummaryStatistic(median) %>%
  getSummaryStatistic(sd) %>%
  getSummaryStatistic(min) %>%
  getSummaryStatistic(max)

# consider removing these percent change data frames in favor of the time series graphs
illinoisCountyPercentChangeData <- illinoisCountyData %>%
  dplyr::left_join(
    illinoisCountyPreviousYearData,
    by = c("GEOID", "NAME", "per_urban")
  ) %>%
  dplyr::transmute(
    GEOID = GEOID,
    NAME = NAME,
    per_urban = per_urban,
    growth_pop = ((est_pop.x - est_pop.y) / est_pop.y),
    dif_blw18 = per_blw18.x - per_blw18.y,
    dif_18to65 = per_18to65.x - per_18to65.y,
    dif_ovr65 = per_ovr65.x - per_ovr65.y,
    dif_vet = per_vet.x - per_vet.y,
    dif_dsblty = per_dsblty.x - per_dsblty.y,
    dif_blwpov = per_blwpov.x - per_blwpov.y,
    dif_nocars = per_nocars.x - per_nocars.y,
    dif_gini = gini.x - gini.y,
    dif_nodipl = per_nodipl.x - per_nodipl.y,
    dif_percap = inc_percap.x - inc_percap.y,
    dif_medhh = inc_medhh.x - inc_medhh.y,
    dif_hhSo60 = per_hhSo60.x - per_hhSo60.y
  ) %>%
  getSummaryStatistic(mean) %>%
  getSummaryStatistic(median) %>%
  getSummaryStatistic(sd) %>%
  getSummaryStatistic(min) %>%
  getSummaryStatistic(max)

illinoisTractPercentChangeData <- illinoisTractData %>%
  dplyr::left_join(
    illinoisTractPreviousYearData,
    by = c("GEOID", "per_urban")
  ) %>%
  dplyr::transmute(
    GEOID = GEOID,
    NAME = NAME.x,
    per_urban = per_urban,
    growth_pop = ((est_pop.x - est_pop.y) / est_pop.y),
    dif_blw18 = per_blw18.x - per_blw18.y,
    dif_18to65 = per_18to65.x - per_18to65.y,
    dif_ovr65 = per_ovr65.x - per_ovr65.y,
    dif_vet = per_vet.x - per_vet.y,
    dif_dsblty = per_dsblty.x - per_dsblty.y,
    dif_blwpov = per_blwpov.x - per_blwpov.y,
    dif_nocars = per_nocars.x - per_nocars.y,
    dif_gini = gini.x - gini.y,
    dif_nodipl = per_nodipl.x - per_nodipl.y,
    dif_percap = inc_percap.x - inc_percap.y,
    dif_medhh = inc_medhh.x - inc_medhh.y,
    dif_hhSo60 = per_hhSo60.x - per_hhSo60.y
  )
