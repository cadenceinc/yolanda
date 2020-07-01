
storms <-
  dplyr::storms %>%
  dplyr::mutate("period_ending" = as.Date(paste(
    .$year,
    .$month,
    .$day,
    sep = "-"))
    ) %>%
  dplyr::select(-c("year", "month", "day"))
