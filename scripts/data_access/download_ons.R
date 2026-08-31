#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "ons")
output_dir <- prepare_download_directory(arguments$output_dir)

base <- "https://www.ons.gov.uk/file?uri="
downloads <- data.frame(
  url = paste0(
    base,
    c(
      "/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/14173dailydeathsoccurrencesenglandandwales1981and2020/dailydeaths19812020adhocfinal.xlsx",
      "/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2021/publishedweek522021.xlsx",
      "/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2022/publicationfileweek522022.xlsx",
      "/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2023/previous/v37/publicationfileweek352023.xlsx"
    )
  ),
  filename = c(
    "daily_deaths_occurrences_1981_2020.xlsx",
    "weekly_deaths_2021_week52.xlsx",
    "weekly_deaths_2022_week52.xlsx",
    "weekly_deaths_2023_week35.xlsx"
  ),
  stringsAsFactors = FALSE
)

destinations <- file.path(output_dir, downloads$filename)
for (i in seq_len(nrow(downloads))) {
  download_provider_file(downloads$url[[i]], destinations[[i]])
}
write_download_record(
  output_dir,
  "Office for National Statistics",
  downloads$url,
  destinations
)
message("Downloaded four ONS source workbooks to ", output_dir, ".")
