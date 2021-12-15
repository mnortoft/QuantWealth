require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(plotly) # for data manipulation
require(tidyverse) # for interactive plots
require(car)
require(lmtest)

#This script applies the PH system to archaeological grave data
here("analysis")


source(here('analysis/scripts/PH_ceramics.R'), echo = TRUE)

#source('ceramics_liter.R', echo = TRUE)


pot_predict <- read.csv(here("analysis/data/raw_data/pot_predict.csv"),
                        header = TRUE,
                        stringsAsFactors = TRUE,
                        sep = ",",
                        encoding = "UTF-8",
                        dec = '.',
                        na.strings = '')

#if to remove objects found in grave or barrow fill
#(to see if leaving them out changes result significantly):
#pot_predict <- dplyr::filter(pot_predict, is.na(grave_fill))

pot_predict$weight_predict <- dplyr::case_when(pot_predict$max_measure_cm < 10 ~ (pot_predict$weight_predict =  pot_predict$max_measure_cm * 10),
                                   pot_predict$max_measure_cm >= 10 ~ (pot_predict$weight_predict =  (pot_predict$max_measure_cm-9) * 120))


#plot reference weights against predicted weights of archaeological data
weight_size_arch <- ggplot()+
  geom_point(data = PH_pottery, aes(max_measure_cm, weight_g), color = "green")+
  geom_point(data = pot_predict, aes(max_measure_cm, weight_predict), color = "red")

#Now use the predicted weights (subtracting its temper percentage either from column or else a fixed 20%) in clay extraction calculations
pot_predict$PH_extract = ifelse(!is.na(pot_predict$temper_percent),
                                                   (PH_clay_extract*((pot_predict$weight_predict/1000)*(abs(pot_predict$temper_percent-1)))), #
                                                   PH_clay_extract*((pot_predict$weight_predict/1000)*0.80)
                                                   )

#internal diameter using reported thicknesses, otherwise standard set to 0.68
pot_predict$diam_int <- ifelse(!is.na(pot_predict$thickness_mm),
                               (pot_predict$max_width_cm-(pot_predict$thickness_mm/10)),
                               pot_predict$max_width_cm-0.68) #end of ifelse statement
pot_predict$diam_ext <- pot_predict$max_width_cm        #external diameter
pot_predict$Liter <- (pi*pot_predict$height_cm*((pot_predict$diam_ext^2)-(pot_predict$diam_int^2))/4)/1000

#then converting the specific temper percents to temper liter based on the objects Liter and the differently defined tempers
pot_predict$PH_temper = pot_predict$Liter * ifelse(!is.na(pot_predict$temper_percent), pot_predict$temper_percent *
                                                    (dplyr::case_when(
                                                      pot_predict$temper == "grog" ~ PH_temper_grog, #then applying to the different tempers
                                                      pot_predict$temper == "shell" ~ PH_temper_shell,
                                                      pot_predict$temper == "sand" ~ PH_temper_sand,
                                                      pot_predict$temper == "quartz" ~ PH_temper_quartz,
                                                      pot_predict$temper == "granite" ~ PH_temper_granite,
                                                      pot_predict$temper == "mica" ~ PH_temper_mica,
                                                      TRUE ~ 0)),  0.2) #end of ifelse statement (else = 0.2)

#using the coefficient of max_measure_cm vs. shaping time to predict shaping PH (with a lower slope for smaller pots)
pot_predict$PH_shape_predict = dplyr::case_when(pot_predict$max_measure_cm <= 10 ~
                                                  (pot_predict$PH_shape_predict =  pot_predict$max_measure_cm * (4/60)),
                                                pot_predict$max_measure_cm > 10 ~
                                                  (pot_predict$PH_shape_predict =  (pot_predict$max_measure_cm-8) * (15/60)))

#plot the reference shaping time against the predicted shaping time of the archaeological data:
shape_time_arch <- ggplot()+
  geom_point(data = PH_pottery, aes(max_measure_cm, PH_shape_time), color = "green")+
  geom_point(data = pot_predict, aes(max_measure_cm, PH_shape_predict), color = "red")

#predict PH for plastic and impressed decoration
pot_predict$PH_plastic = pot_predict$plastic_deco * PH_plastic_deco
pot_predict$PH_impress_deco = (pot_predict$PH_shape_predict) * (pot_predict$impressed_decorations_perc)

#Predict polishing time (1 polish on one side, 2 polish on both sides)
pot_predict$PH_polish_predict = dplyr::case_when(
  pot_predict$polish == 1 ~ (pot_predict$max_measure_cm * (4.3/60)), #polish on one side
  pot_predict$polish == 2 ~ (pot_predict$max_measure_cm * (4.3/60))*2) #polish on two sides

pot_predict$Sides_polished = Sides_polished = as.factor(pot_predict$polish)

#plot the reference polishing time against the predicted polishing time:
polish_time_arch <- ggplot()+
  geom_point(data = PH_pottery, aes(max_measure_cm, (polish_time_mins/60)), color = "green")+
  geom_point(data = pot_predict, aes(max_measure_cm, PH_polish_predict, shape = Sides_polished), color = "red")+
  labs(fill='Sides polished')

#Predict smoothing time (1 polish on one side, 2 polish on both sides)
#as 28% of polishing time (based on mean of reference smoothing times)
pot_predict$PH_smooth = dplyr::case_when(
  pot_predict$smooth_beat == 1 ~ (pot_predict$PH_polish_predict* 0.28), #smooth on one side
  pot_predict$smooth_beat == 2 ~ (pot_predict$PH_polish_predict* 0.28)*2) # smooth on two sides


#Slip/paint time derived from how much is treated (one side = 0.5, both sides = 1) * shaping time:
pot_predict$PH_slip_paint = pot_predict$PH_shape_predict * pot_predict$slip_paint_perc

# add firing
pot_predict$PH_fire = PH_firing

pot_predict$skill = dplyr::case_when(pot_predict$max_measure_cm >= 25 ~ (pot_predict$skill =  2.0), #high
                                     pot_predict$max_measure_cm >= 15 &
                                      pot_predict$max_measure_cm <25 ~ (pot_predict$skill = 0.6), #medium-high
                                     pot_predict$max_measure_cm < 15 ~ (pot_predict$skill = 0.4)) #medium

#Multiplying sum of shape, polish and firing hours by skill factor
pot_predict$PH_skill_bonus = (rowSums(pot_predict[,c("PH_shape_predict","PH_polish_predict","PH_fire")],
                                    na.rm = TRUE) * pot_predict$skill)


# summing up PH for all production stages:
pot_predict$PH_all <- rowSums(pot_predict[,c("PH_temper",
                                           "PH_shape_predict",
                                           "PH_plastic",
                                           "PH_impress_deco",
                                           "PH_smooth",
                                           "PH_polish_predict",
                                           "PH_slip_paint",
                                           "PH_fire")], na.rm = TRUE)


#summarising objects in each grave:
grave_total_pot <- pot_predict %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(PH_raw = sum(PH_all),
                   skill_bonus = sum(PH_skill_bonus))


## Other ceramics archaeological grave data
other_ceramic_predict <- read.csv(here("analysis/data/raw_data/other_ceramic_predict.csv"),
                                  header = TRUE,
                                  stringsAsFactors = TRUE,
                                  sep = ";",
                                  encoding = "UTF-8",
                                  dec = '.',
                                  na.strings = '')

PH_spindle_whorl = other_ceramic_ref[other_ceramic_ref$type == "spindle whorl",]
PH_loom_weight = other_ceramic_ref[other_ceramic_ref$type == "loom weight",]

other_ceramic_predict$PH_raw = dplyr::case_when(other_ceramic_predict$object_type == "spindle whorl" ~ PH_spindle_whorl$PH_raw,
                                                other_ceramic_predict$object_type == "loom weight" ~ PH_loom_weight$PH_raw)

other_ceramic_predict$skill_bonus = dplyr::case_when(other_ceramic_predict$object_type == "spindle whorl" ~ PH_spindle_whorl$skill_factor,
                                                other_ceramic_predict$object_type == "loom weight" ~ PH_loom_weight$skill_factor)

grave_total_ceramic_other <- other_ceramic_predict[c("Grave_ID", "PH_raw", "skill_bonus")]

# #summarising objects in each grave:
# grave_total_ceramic_other <- other_ceramic_predict %>%
#   group_by(Grave_ID) %>%
#   dplyr::summarise(PH_raw = sum(PH_raw),
#                    skill_bonus = sum(skill_bonus))
