require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots
require(matrixStats) #for rowMedians function combining different value parameters

here("analysis")

source(here('analysis/scripts/PH_flint.R'), echo = TRUE, encoding = "UTF-8")

#load archaeological flint axe data:
flintAxe_predict <- read.csv(here("analysis/data/raw_data/flint_axe_predict.csv"),
                          header = TRUE,
                          stringsAsFactors = TRUE,
                          sep = ";",
                          encoding = "UTF-8",
                          dec = '.',
                          na.strings = '')

#if wanting to remove objects in the grave fill:
#flintAxe_predict <- dplyr::filter(flintAxe_predict, is.na(grave_fill))


PH_flint_axe_2_side = Flint_PH_all[Flint_PH_all$Object == "Flint Axe (two-sided)",]
PH_flint_axe_thinButt = Flint_PH_all[Flint_PH_all$Object == "thin-butted axe (four-sided)",]
PH_flint_axe_thickButt = Flint_PH_all[Flint_PH_all$Object == "thick-butted axe (four-sided)",]

flintAxe_predict$knapping <- dplyr::case_when(
  flintAxe_predict$object_type == "axe two-sided" ~ PH_flint_axe_2_side$knappingPH,
  flintAxe_predict$object_type == "axe four-sided simple" ~ PH_flint_axe_thickButt$knappingPH,
  flintAxe_predict$object_type == "axe four-sided simple" ~ PH_flint_axe_thickButt$knappingPH,
  flintAxe_predict$object_type == "axe thick-butted" ~ PH_flint_axe_thickButt$knappingPH,
  flintAxe_predict$object_type == "axe thin-butted" ~ PH_flint_axe_thinButt$knappingPH,
)

flintAxe_predict$grind <-
  thinButt_grind_median * flintAxe_predict$grind_perc

flintAxe_predict$sharpen <- flint_axe_sharp

flintAxe_predict$total_PH <-
  flintAxe_predict$knapping +
  flintAxe_predict$grind +
  flintAxe_predict$sharpen

flintAxe_predict$handle <- axeAdze_handleMedian

flintAxe_predict$scarcity_bonus <- flintAxe_predict$total_graves/flintAxe_predict$total_material_grave

flintAxe_predict$length_bonus <-  dplyr::case_when(
  flintAxe_predict$length_mm  > 270 & flintAxe_predict$length_mm < 300 ~ PH_flint_axe_2_side$grindPH*0.2,
  flintAxe_predict$length_mm  > 300 ~ PH_flint_axe_2_side$grindPH*0.5,
  flintAxe_predict$length_mm < 280 ~ PH_flint_axe_2_side$grindPH*0.0
  )

#Thin_bonus adds points to axes with thickness less than 5% of length using thickness/length (both in mm)
flintAxe_predict$skill_bonus <-  dplyr::case_when(
  (flintAxe_predict$thickness_mm/flintAxe_predict$length_mm) <= 0.055
  & (flintAxe_predict$thickness_mm/flintAxe_predict$length_mm) > 0.035 ~
    (flintAxe_predict$total_PH*2.0), #high skill
  (flintAxe_predict$thickness_mm/flintAxe_predict$length_mm)  <= 0.035 ~
    (flintAxe_predict$total_PH*5.0), #very high skill
  (flintAxe_predict$thickness_mm/flintAxe_predict$length_mm)  > 0.055 ~
    (flintAxe_predict$total_PH*0.4) #medium skill
  )

flintAxe_predict$travel_bonus <- flintAxe_predict$source_distance_km/7

flintAxe_predict$knap_length <- rowSums(flintAxe_predict[,c("knapping", "length_bonus")])

flintAxe_predict$PH_total <- rowSums(flintAxe_predict[,c(
  "knap_length",
  "grind",
  "sharpen",
  "handle")])

# flintAxe_predict$final_total <- rowSums(flintAxe_predict[,c(
#   "PH_raw",
#   "scarcity_bonus",
#   "skill_bonus",
#   "travel_bonus"
#   )])


grave_total_flint_axe <- flintAxe_predict %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(PH_total = sum(PH_total),
                   skill_bonus = sum(skill_bonus),
                   scarcityBonus = sum(scarcity_bonus),
                   travelBonus = sum(travel_bonus))




#### Archaeological other flint objects  ####
#set skill bonuses
other_flint$skill_factor = dplyr::case_when(
  other_flint$skill_level == "low" ~ 0.0,
  other_flint$skill_level == "low?" ~ 0.0,
  other_flint$skill_level == "medium" ~ 0.4,
  other_flint$skill_level == "medium?" ~ 0.4,
  other_flint$skill_level == "low-medium" ~ 0.2,
  other_flint$skill_level == "medium-high" ~ 0.6,
  other_flint$skill_level == "high" ~ 2.0,
  other_flint$skill_level == "high?" ~ 2.0,
  other_flint$skill_level == "very high" ~ 5.0,
  other_flint$skill_level == "expert" ~ 10.0,
)

#load other archaeological flint data:
flint_other_predict <- read.csv(here("analysis/data/raw_data/flint_other_predict.csv"),
                             header = TRUE,
                             stringsAsFactors = TRUE,
                             sep = ";",
                             encoding = "UTF-8",
                             dec = '.',
                             na.strings = '')

#if wanting to remove objects in the grave fill:
#flint_other_predict <- dplyr::filter(flint_other_predict, is.na(grave_fill))

#Extract reference times for various flint artefact types
flint_disc_scraper <- other_flint[other_flint$type == "disc scraper",]
backed_knife <- other_flint[other_flint$type == "backed knife",]
flake_axe <- other_flint[other_flint$type == "flake axe",]
blade_sickle <- other_flint[other_flint$type == "blade sickle",]
flake_burin <- other_flint[other_flint$type == "flake burin",]
flint_borer <- other_flint[other_flint$type == "borer",]
arrowhead_transverse <- other_flint[other_flint$type == "transverse arrowhead",]
blade <- other_flint[other_flint$type == "blade",]
BBC_knife <- other_flint[other_flint$type == "Bell Beaker knife",]
Nordic_LN_dagger <- other_flint[other_flint$type == "Late Neolithic Nordic dagger",]
blade_core <- other_flint[other_flint$type == "Blade core",]
arrowhead_barbTang <- other_flint[other_flint$type == "Arrowhead barbed-and-tanged",]
British_Neo_dagger <- other_flint[other_flint$type == "British Neolithic Dagger",]

#apply this to the archaeological data
flint_other_predict$PH_raw <- dplyr::case_when(
  flint_other_predict$object_type == "blade" ~ blade$knapping_PH,
  flint_other_predict$object_type == "flake" ~ flake_burin$knapping_PH,
  flint_other_predict$object_type == "endscraper" ~ flint_disc_scraper$knapping_PH,
  flint_other_predict$object_type == "scraper" ~ flint_disc_scraper$knapping_PH,
  flint_other_predict$object_type == "knife" ~ backed_knife$knapping_PH,
  flint_other_predict$object_type == "point" ~ flint_borer$knapping_PH,
  flint_other_predict$object_type == "splintered piece" ~ (flake_burin$knapping_PH*0.5),
  flint_other_predict$object_type == "arrowhead" ~ arrowhead_barbTang$knapping_PH)

#add time for handles from the reference data
flint_other_predict$handle <- dplyr::case_when(
  flint_other_predict$object_type == "blade" ~ blade$handle_PH,
  flint_other_predict$object_type == "flake" ~ flake_burin$handle_PH,
  flint_other_predict$object_type == "endscraper" ~ flint_disc_scraper$handle_PH,
  flint_other_predict$object_type == "scraper" ~ flint_disc_scraper$handle_PH,
  flint_other_predict$object_type == "knife" ~ backed_knife$handle_PH,
  flint_other_predict$object_type == "point" ~ flint_borer$handle_PH,
  flint_other_predict$object_type == "splintered piece" ~ flake_burin$handle_PH,
  flint_other_predict$object_type == "arrowhead" ~ arrowhead_barbTang$handle_PH)

#define retouch bonus
retouch_1 <- 0.17
retouch_2 <- 0.17*2

#add skill bonus
flint_other_predict$skill_bonus <- dplyr::case_when(
  flint_other_predict$object_type == "blade" ~ (blade$knapping_PH*blade$skill_factor),
  flint_other_predict$object_type == "flake" ~ (flake_burin$knapping_PH*flake_burin$skill_factor),
  flint_other_predict$object_type == "endscraper" ~ (flint_disc_scraper$knapping_PH*flint_disc_scraper$knapping_PH),
  flint_other_predict$object_type == "scraper" ~ (flint_disc_scraper$knapping_PH*flint_disc_scraper$skill_factor),
  flint_other_predict$object_type == "knife" ~ (backed_knife$knapping_PH*backed_knife$skill_factor),
  flint_other_predict$object_type == "point" ~ (flint_borer$knapping_PH*flint_borer$skill_factor),
  flint_other_predict$object_type == "splintered piece" ~ ((flint_borer$knapping_PH*0.5)*flint_borer$skill_factor),
  flint_other_predict$object_type == "arrowhead" ~ (arrowhead_barbTang$knapping_PH*arrowhead_barbTang$skill_factor)
  )

#apply retouch bonus to retouched artefacts
flint_other_predict$retouch_bonus <- dplyr::case_when(
  flint_other_predict$retouch_1_side == 0 ~ (flint_other_predict$length_mm*0.0),
  flint_other_predict$retouch_1_side == 1 ~ ((flint_other_predict$length_mm*0.2)/50),
  flint_other_predict$retouch_1_side == 2 ~ ((flint_other_predict$length_mm*0.4)/50))

#add scarcity bonus
flint_other_predict$scarcity_bonus <- flint_other_predict$total_graves/
  flint_other_predict$total_material

#summarize total manufacturing time, incl. retouch and hafting
flint_other_predict$PH_total <- rowSums(flint_other_predict[,c("PH_raw",
                                                            "retouch_bonus",
                                                            "handle")],
                                        na.rm = TRUE)

# flint_other_predict$final_total <- rowSums(flint_other_predict[, c(
#   "PH_total",
#   "retouch_bonus",
#   "handle",
#   "skill_bonus",
#   "scarcity_bonus")], na.rm = TRUE)


grave_total_flint_other <- flint_other_predict %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(PH_total = sum(PH_total),
                   skill_bonus = sum(skill_bonus))
