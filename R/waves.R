wave_table <- function(config) {
  tibble::tibble(
    wave = names(config$waves),
    start = as.Date(vapply(
      config$waves,
      function(definition) as.character(definition$start),
      character(1)
    )),
    end_exclusive = as.Date(vapply(
      config$waves,
      function(definition) as.character(definition$end_exclusive),
      character(1)
    ))
  )
}

assign_wave <- function(date, config) {
  dates <- as.Date(date)
  definitions <- wave_table(config)
  assigned <- rep(NA_character_, length(dates))

  for (index in seq_len(nrow(definitions))) {
    in_wave <- dates >= definitions$start[[index]] &
      dates < definitions$end_exclusive[[index]]
    assigned[in_wave] <- definitions$wave[[index]]
  }

  assigned
}
