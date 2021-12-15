require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(plotly) # for data manipulation
require(tidyverse) # for interactive plots
require(car)

here("analysis")

amber <- read.csv(here("analysis/data/raw_data/reference_data/amber.csv"),
                             header = TRUE,
                             stringsAsFactors = TRUE,
                             sep = ";",
                             encoding = "UTF-8",
                             dec = '.',
                             na.strings = '')

amber$PH_skill <- dplyr::case_when(amber$skill_level == "low" ~   (amber$time * 0.0),
                                   amber$skill_level == "medium" ~ (amber$time * 0.4),
                                   amber$skill_level == "high" ~ (amber$time * 2.0))


amber$scarcity <- amber$total_graves/amber$total_material_grave

amber$amber_travel <- amber$source_distance_km/7

amber$PH_skill_scarc_travel <-
  amber$time_PH+
  amber$PH_skill+
  amber$scarcity+
  amber$amber_travel

