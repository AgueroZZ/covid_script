.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)

load(file = "USA_monthly_result.rda")
state_name_to_code <- data.frame(
  full_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                "Colorado", "Connecticut", "Delaware", "District of Columbia",
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN",
           "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
           "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
           "VT", "VA", "WA", "WV", "WI", "WY")
)
wave_levels = c("initial", "alpha", "delta", "omicron")


# Map the full state names in your data to their two-letter codes
model_result_all <- left_join(model_result_all, state_name_to_code,
                              by = c("state" = "full_name"))
names(model_result_all)[8] <- "State_full"
names(model_result_all)[11] <- "state"

state_order_west_to_east <- data.frame(
  state = c("AK", "HI", "WA", "OR", "CA", "NV", "ID", "MT", "WY", "UT", "CO", "AZ", "NM",
            "ND", "SD", "NE", "KS", "OK", "TX", "MN", "IA", "MO", "AR", "LA", "WI",
            "IL", "MS", "MI", "IN", "KY", "TN", "AL", "OH", "WV", "SC", "GA",
            "FL", "NC", "VA", "PA", "NY", "VT", "NH", "ME", "MA", "RI", "CT", "NJ", "DE", "MD", "DC"),
  order_west_to_east = 1:51
)

# Merge this ordering into your main dataframe
model_result_all <- left_join(model_result_all, state_order_west_to_east, by = "state")

create_plot <- function(data, age_group, sex_group, states = NULL, TEXT = "") {
  if(is.null(states)){
    states <- unique(data$state)
  }
  # Filter data for the specified age group, sex group, and countries
  df_age <- data %>% filter(age == age_group, sex == sex_group, state %in% states)

  # Calculate the interval width and set the wave factor
  df_age <- df_age %>%
    mutate(interval_width = p_upper - p_lower,
           point_size = 1 / interval_width) %>%
    mutate(wave = factor(wave, levels = wave_levels))

  # Create the plot
  p <- ggplot(df_age, aes(x = state, y = p_med, color = wave)) +
    geom_point(aes(size = point_size), position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = p_lower, ymax = p_upper), position = position_dodge(width = 0.5), width = 0.2) +
    ylab("P-Score by waves") + xlab("State") +
    theme_minimal() +
    ggtitle(TEXT) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 20)) +
    geom_hline(yintercept = 0, col = "black") +
    coord_cartesian(ylim = c(-1, 1)) +
    scale_size_continuous(range = c(1, 5)) +
    guides(size = "none")

  # Return the plot object
  return(p)
}

### Save these plots
png("figure/pscore_bar/PS_age_0_44_F.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "0-44", sex_group = "F", TEXT = "0-44, F",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_0_44_M.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "0-44", sex_group = "M", TEXT = "0-44, M",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_45_64_F.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "45-64", sex_group = "F", TEXT = "45-64, F",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_45_64_M.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "45-64", sex_group = "M", TEXT = "45-64, M",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_65_84_F.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "65-84", sex_group = "F", TEXT = "65-84, F",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_65_84_M.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "65-84", sex_group = "M", TEXT = "65-84, M",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_85_F.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "Over 85", sex_group = "F", TEXT = "Over 85, F",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

png("figure/pscore_bar/PS_age_85_M.png", width = 10, height = 10, units = "in", res = 300)
create_plot(model_result_all, age_group = "Over 85", sex_group = "M", TEXT = "Over 85, M",
            states = c("TX", "CA", "FL", "NY", "PA", "WA", "GA", "OH", "MI", "NC"))
dev.off()

# For elderly people, initial wave has much higher death rate in male than in female.

create_plot(model_result_all, age_group = "0-44", sex_group = "M", TEXT = "0-44, M",
            states = c("MS", "AL", "FL", "SC", "AZ", "KS", "CO"))

create_plot(model_result_all, age_group = "0-44", sex_group = "F", TEXT = "0-44, F",
            states = c("MS", "AL", "FL", "SC", "AZ", "KS", "CO"))

