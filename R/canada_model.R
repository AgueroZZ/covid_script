canada_age_labels <- function() {
  c(
    "Age at time of death, 0 to 44 years" = "0-44",
    "Age at time of death, 45 to 64 years" = "45-64",
    "Age at time of death, 65 to 84 years" = "65-84",
    "Age at time of death, 85 years and over" = "85+"
  )
}

canada_province_labels <- function() {
  c(
    "Newfoundland and Labrador, place of occurrence" = "NL",
    "Prince Edward Island, place of occurrence" = "PE",
    "Nova Scotia, place of occurrence" = "NS",
    "New Brunswick, place of occurrence" = "NB",
    "Quebec, place of occurrence" = "QC",
    "Ontario, place of occurrence" = "ON",
    "Manitoba, place of occurrence" = "MB",
    "Saskatchewan, place of occurrence" = "SK",
    "Alberta, place of occurrence" = "AB",
    "British Columbia, place of occurrence" = "BC",
    "Yukon, place of occurrence" = "YT",
    "Northwest Territories, place of occurrence" = "NT",
    "Nunavut, place of occurrence" = "NU"
  )
}

read_canada_model_input <- function(path, stratified_by_sex = TRUE) {
  raw <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = TRUE,
    fileEncoding = "UTF-8-BOM"
  )
  required <- c(
    "REF_DATE", "GEO", "Age.at.time.of.death", "Sex",
    "Characteristics", "VALUE"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop("Statistics Canada input is missing columns: ",
         paste(missing, collapse = ", "), ".")
  }

  selected_sexes <- if (isTRUE(stratified_by_sex)) {
    c("Females", "Males")
  } else {
    "Both sexes"
  }
  raw <- raw[
    raw$GEO != "Canada, place of occurrence" &
      raw$Age.at.time.of.death %in% names(canada_age_labels()) &
      raw$Sex %in% selected_sexes &
      raw$Characteristics == "Number of deaths",
    ,
    drop = FALSE
  ]

  province <- unname(canada_province_labels()[raw$GEO])
  age_group <- unname(canada_age_labels()[raw$Age.at.time.of.death])
  if (any(is.na(province)) || any(is.na(age_group))) {
    stop("Statistics Canada input contains an unmapped geography or age group.")
  }

  output <- data.frame(
    date = as.Date(raw$REF_DATE),
    province = province,
    age = age_group,
    sex = if (isTRUE(stratified_by_sex)) raw$Sex else "total",
    death = suppressWarnings(as.numeric(raw$VALUE)),
    analysis_path = if (isTRUE(stratified_by_sex)) {
      "sex_stratified"
    } else {
      "non_sex_stratified"
    },
    stringsAsFactors = FALSE
  )
  output <- output[!is.na(output$date) & !is.na(output$death), , drop = FALSE]
  output$year <- as.integer(format(output$date, "%Y"))
  output[order(
    output$analysis_path, output$province, output$age, output$sex, output$date
  ), , drop = FALSE]
}

canada_model_manifest <- function(data, base_seed = 20260831L) {
  groups <- split(
    data,
    interaction(
      data$analysis_path,
      data$province,
      data$age,
      data$sex,
      drop = TRUE,
      lex.order = TRUE
    )
  )
  rows <- lapply(groups, function(group) {
    if (max(group$date) <= as.Date("2021-12-31")) return(NULL)
    model_id <- paste(
      "canada",
      group$analysis_path[[1]],
      group$province[[1]],
      group$age[[1]],
      group$sex[[1]],
      sep = "__"
    )
    model_id <- gsub("[^A-Za-z0-9_]+", "_", model_id)
    span_years <- as.numeric(difftime(max(group$date), min(group$date), units = "days")) / 365.25
    data.frame(
      model_id = model_id,
      analysis_path = group$analysis_path[[1]],
      province = group$province[[1]],
      age_group = group$age[[1]],
      sex = group$sex[[1]],
      first_date = min(group$date),
      final_date = max(group$date),
      observations = nrow(group),
      training_observations = sum(group$date < as.Date("2020-01-01")),
      k_iwp = if (span_years >= 10) 100L else 50L,
      k_sgp = if (span_years >= 10) 40L else 20L,
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  rownames(manifest) <- NULL
  manifest <- manifest[order(manifest$model_id), , drop = FALSE]
  manifest$seed <- as.integer(base_seed + seq_len(nrow(manifest)))
  manifest
}

canada_model_data <- function(data, manifest_row) {
  selected <- data[
    data$analysis_path == manifest_row$analysis_path &
      data$province == manifest_row$province &
      data$age == manifest_row$age_group &
      data$sex == manifest_row$sex,
    ,
    drop = FALSE
  ]
  selected[order(selected$date), , drop = FALSE]
}
