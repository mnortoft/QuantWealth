require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots

here("analysis")

source(here('analysis/scripts/stone.R'), echo = TRUE, encoding = "UTF-8")

stone_axe_predict <- read.csv(here("analysis/data/raw_data/stone_axe_predict.csv"),
                              header = TRUE,
                              stringsAsFactors = TRUE,
                              sep = ";",
                              encoding = "UTF-8",
                              dec = '.',
                              na.strings = '')

#if wanting to remove objects in grave fill:
#stone_axe_predict <- dplyr::filter(stone_axe_predict, is.na(grave_fill))

stone_axe_predict$PH_blank <- axe_blank_10kg

stone_axe_predict$mohs_tough <- log(stone_axe_predict$mohs_hardness_mean)+stone_axe_predict$toughness

# first number sets the starting point, exponent at the end sets the slope
stone_axe_predict$PH_raw <- dplyr::case_when(stone_axe_predict$objectType == "axe-hammer" ~
                                               2*median(stone_axe_ref$total_PH[stone_axe_ref$type_simple == "groundstone axe"])
                                             *stone_axe_predict$grind_perc*log(stone_axe_predict$mohs_tough),

                                             stone_axe_predict$objectType == "axe-hammer (faceted)" ~
                                               3*median(stone_axe_ref$total_PH[stone_axe_ref$type_simple == "groundstone axe"])
                                             *stone_axe_predict$grind_perc*log(stone_axe_predict$mohs_tough),

                                             stone_axe_predict$objectType == "axe" | stone_axe_predict$objectType == "flat axe"
                                             ~ median(stone_axe_ref$total_PH[stone_axe_ref$type_simple == "groundstone axe"])
                                             *stone_axe_predict$grind_perc*log(stone_axe_predict$mohs_tough),

                                             stone_axe_predict$objectType == "stone anvil" ~ (1*log(stone_axe_predict$mohs_tough)),
                                             stone_axe_predict$objectType == "whetstone" ~ (1.2*log(stone_axe_predict$mohs_tough)),

                                             stone_axe_predict$objectType == "pebble" | stone_axe_predict$objectType == "stone"
                                             ~ (0.5*log(stone_axe_predict$mohs_tough)))



stone_arch_plot <- ggplot(stone_axe_predict, aes(x = objectType, y = PH_raw, alpha=grind_perc))+
  geom_jitter(aes(color = raw_material), width = 0.1, size = 1)+
  #scale_shape_manual(values=1:nlevels(stone_axe_predict$objectType))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  scale_alpha(range=c(0.3,1), limits=c(0,1), na.value = 0)


#1 hour for axe-hammer handles (Informant MK), using flint axe handle variable for flat axes,
#and adding 1 hour for birch tar glue
stone_axe_predict$handle <- dplyr::case_when(
  stone_axe_predict$objectType == "axe-hammer" ~ 1+1,
  stone_axe_predict$objectType == "axe-hammer (faceted)" ~ 1+1,
  stone_axe_predict$objectType == "axe" ~ axeAdze_handleMedian+1,
  stone_axe_predict$objectType == "flat axe" ~ axeAdze_handleMedian+1)

#skill bonus *2.0 high level (the general level of crafts people in experiments and interviews)
stone_axe_predict$skill_bonus <- dplyr::case_when(
  stone_axe_predict$objectType == "axe-hammer" ~ stone_axe_predict$PH_raw*2.0,
  stone_axe_predict$objectType == "axe-hammer (faceted)" ~ stone_axe_predict$PH_raw*2.0,
  stone_axe_predict$objectType == "axe" ~ stone_axe_predict$PH_raw*2.0,
  stone_axe_predict$objectType == "flat axe" ~ stone_axe_predict$PH_raw*2.0)

stone_axe_predict$scarcity_bonus <- dplyr::case_when(
  stone_axe_predict$source_distance_km > 5 ~
    stone_axe_predict$total_graves/stone_axe_predict$total_material)

stone_axe_predict$travel_bonus <- stone_axe_predict$source_distance_km/7

#adding lenght of axe bonus multiplying extraordinarily long axes by 0.2
stone_axe_predict$length_bonus <- dplyr::case_when(
  stone_axe_predict$length_mm >= 130 ~ stone_axe_predict$PH_raw*0.2)

stone_axe_predict$total_PH <- rowSums(stone_axe_predict[,c(
  "PH_raw",
  "handle",
  "length_bonus"
)], na.rm = TRUE)


grave_total_stone_tools <- stone_axe_predict %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(total_PH = sum(total_PH),
                   skill_bonus = sum(skill_bonus),
                   scarcityBonus = sum(scarcity_bonus),
                   travelBonus = sum(travel_bonus))
