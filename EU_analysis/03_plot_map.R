.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)
require(mapmisc)
source(file = "script/function.R")

# theFile= tempfile(fileext='.zip')
# download.file(
#   'https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries_lakes.zip',
#   theFile)
# allFiles= unzip(theFile)
# world = vect(grep("shp$", allFiles, value=TRUE))
world = vect("ne_10m_admin_0_countries_lakes.shp")
world = project(world, crs('EPSG:3035'))
world <- world[!(is.na(world$ISO_A2_EH) | world$ISO_A2_EH == "-99"),]

library(dplyr)
library(mapmisc)
library(terra)  # Assuming you're using terra for your 'world' object

plot_age_group_map <- function(result_age, age_group, sex_group) {
  result_age <- result_age %>% filter(age == age_group, sex == sex_group)
  colnames(result_age)[8] <- "ISO_A2_EH"
  result_age$ISO_A2_EH <- ifelse(result_age$ISO_A2_EH == "EL", "GR", result_age$ISO_A2_EH)
  result_age$ISO_A2_EH <- ifelse(result_age$ISO_A2_EH == "UK", "GB", result_age$ISO_A2_EH)
  colnames(result_age)[1] <- "waves"

  for (wave in c("initial", "alpha", "delta", "omicron")) {
    world_df <- as.data.frame(world)
    result_age_year <- result_age %>% filter(waves == wave)
    result_age_year <- left_join(world_df, result_age_year, by = "ISO_A2_EH")
    world$p_med_new <- result_age_year$p_med

    custom_palette <- RColorBrewer::brewer.pal(8, 'RdYlGn')
    custom_palette <- rev(custom_palette)
    custom_breaks <- c(-Inf, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 1)
    custom_labels <- as.character(c(0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 1) * 100)

    iceland = world[grep("Iceland", world$NAME), ]
    iceland2 = project(iceland, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=2710000 +ellps=GRS80 +units=m +no_defs")
    world2 = world[grep("Iceland", world$NAME, invert=TRUE), ]
    crs(iceland2) = crs(world2)
    # world3 = vect(c(world2, iceland2))
    world4 = crop(world, ext(2000000, 7500000, 1000000, 5300000))


    theCol = mapmisc::colourScale(world4$p_med_new, breaks=custom_breaks,
                                  style='fixed', col = custom_palette)
    theCol$plot[is.na(theCol$plot)] = "#FFFFFF"

    theExt = terra::ext(2700000, 5686000, 1530000, 4660000)
    theDiff = diff(as.vector(theExt))
    theRatio = theDiff[3]/theDiff[1]

    pdf(file = paste0("./maps/EU_map_", age_group, "_",  sex_group, "_", wave, ".pdf"), width = 10, height = 10*theRatio)

    mapmisc::map.new(#world[grep("Spain|Italy|Ireland|United Kingdom|Estonia|Turkey",
                    #            world$NAME),],
                    theExt,
                      bg='lightblue')
    maltaCyprusIceland = vect(cbind(c(4910146, 5182984, 3071476, 5700000), c(1524662, 1534767, 4596126, 2200000)),
                              atts = data.frame(NAME = c("Malta", "Cyprus", "Iceland", "Armenia"),
                                                shortName = c("MT", "CY", "IS", "AM")))


    plot(world4, add=TRUE, col=theCol$plot)
    plot(maltaCyprusIceland, add=TRUE, cex=3, pch=16,
         col = theCol$plot[match(maltaCyprusIceland$shortName, world4$ISO_A2_EH)])
    text(maltaCyprusIceland, maltaCyprusIceland$shortName)

    mapmisc::legendBreaks("left", inset=0, col=theCol$col, breaks = custom_labels, cex = 1.5)
    centroids_plot <- centroids(world4[!is.na(world4$p_med_new), ], inside = TRUE)
    text(centroids_plot, world4$ISO_A2_EH[!is.na(world4$p_med_new)])


    dev.off()
  }
}


# Usage:
# load(file = "mortality/Europe/v10_(by_sex)/result/result_all_age.rda")
# load(file = "result/result_all_age.rda")
load(file = "result/result_all_age_complete.rda")

plot_age_group_map(model_result_all, "Y20-39", "T")
plot_age_group_map(model_result_all, "Y40-59", "T")
plot_age_group_map(model_result_all, "Y60-79", "T")
plot_age_group_map(model_result_all, "Y_GE80", "T")


plot_age_group_map(model_result_all, "Y20-39", "F")
plot_age_group_map(model_result_all, "Y40-59", "F")
plot_age_group_map(model_result_all, "Y60-79", "F")
plot_age_group_map(model_result_all, "Y_GE80", "F")


plot_age_group_map(model_result_all, "Y20-39", "M")
plot_age_group_map(model_result_all, "Y40-59", "M")
plot_age_group_map(model_result_all, "Y60-79", "M")
plot_age_group_map(model_result_all, "Y_GE80", "M")
