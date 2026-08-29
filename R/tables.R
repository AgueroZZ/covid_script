format_table_pscore <- function(male, female, digits = 3L) {
  paste0(
    "M: ",
    formatC(male, digits = digits, format = "f"),
    "; F: ",
    formatC(female, digits = digits, format = "f")
  )
}
prepare_table_01_region <- function(
  wave_summary,
  vaccination,
  region,
  analysis_path,
  age_group
) {
  reporting_required_columns(
    wave_summary,
    c("analysis_path", "geography", "age_group", "sex", "wave", "p_median", "status"),
    paste(region, "wave summary")
  )
  selected <- wave_summary[
    wave_summary$analysis_path == analysis_path &
      wave_summary$age_group == age_group &
      wave_summary$sex %in% c("female", "male") &
      wave_summary$status == "success",
  ]
  selected <- join_reporting_vaccination(selected, vaccination)
  selected <- selected[selected$vaccination_group %in% c("high", "low"), ]
  reporting_require_unique(
    selected,
    c("geography", "sex", "wave"),
    paste(region, "Table 1 rows")
  )

  wide_sex <- tidyr::pivot_wider(
    selected[, c("geography", "wave", "sex", "p_median")],
    names_from = "sex",
    values_from = "p_median"
  )
  reporting_required_columns(wide_sex, c("female", "male"), paste(region, "sex rows"))
  wide_sex$value <- format_table_pscore(wide_sex$male, wide_sex$female)
  wide_wave <- tidyr::pivot_wider(
    wide_sex[, c("geography", "wave", "value")],
    names_from = "wave",
    values_from = "value"
  )
  membership <- vaccination[
    vaccination$vaccination_group %in% c("high", "low"),
    c("geography", "people_vaccinated_per_hundred", "vaccination_group")
  ]
  output <- dplyr::left_join(wide_wave, membership, by = "geography")
  output$region_set <- region
  output$estimand_age_group <- age_group
  output
}

build_table_01 <- function(
  europe_wave_summary,
  us_wave_summary,
  europe_vaccination,
  us_vaccination
) {
  europe <- prepare_table_01_region(
    europe_wave_summary,
    europe_vaccination,
    region = "Europe",
    analysis_path = "europe_sex",
    age_group = "60-79"
  )
  us <- prepare_table_01_region(
    us_wave_summary,
    us_vaccination,
    region = "United States",
    analysis_path = "us_sex",
    age_group = "65-84"
  )
  waves <- c("initial", "alpha", "delta", "omicron")
  output <- dplyr::bind_rows(europe, us)
  reporting_required_columns(output, waves, "Table 1 output")
  output <- output[, c(
    "region_set",
    "geography",
    "people_vaccinated_per_hundred",
    "vaccination_group",
    "estimand_age_group",
    waves
  )]
  output[order(output$region_set, -output$people_vaccinated_per_hundred), ]
}

write_table_01 <- function(data, csv_path, html_path) {
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(html_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, csv_path)

  escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  headers <- paste0("<th>", escape_html(names(data)), "</th>", collapse = "")
  rows <- apply(data, 1, function(row) {
    paste0(
      "<tr>",
      paste0("<td>", escape_html(as.character(row)), "</td>", collapse = ""),
      "</tr>"
    )
  })
  html <- c(
    "<!doctype html>",
    "<html><head><meta charset=\"utf-8\"><title>Table 1</title>",
    "<style>body{font-family:Arial,sans-serif;margin:24px}table{border-collapse:collapse;font-size:12px}th,td{padding:5px 8px;border-bottom:1px solid #ddd;text-align:left}th{border-top:2px solid #222;border-bottom:2px solid #222}.group{border-top:1px solid #777}</style>",
    "</head><body>",
    "<h1>Table 1. P-scores by wave for selected regions</h1>",
    "<p>Values are posterior medians, shown as male and female P-scores. Regions are ordered by decreasing vaccination coverage.</p>",
    paste0("<table><thead><tr>", headers, "</tr></thead><tbody>"),
    rows,
    "</tbody></table></body></html>"
  )
  writeLines(html, html_path, useBytes = TRUE)
  c(csv_path, html_path)
}
