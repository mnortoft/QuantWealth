# Flint axe knapping and grinding comparison

library(here) # for knitr to print out all used packages at the end of the RMarkdown
library(dplyr) # for data manipulation
library(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots

here("analysis")

flintAxe_grind <- read.csv(here('analysis/data/raw_data/reference_data/flintAxe_grinding.csv'),
                           header = TRUE,
                           stringsAsFactors = TRUE,
                           sep = ";",
                           encoding = "UTF-8",
                           dec = '.',
                           na.strings = '')

flintAxe_grind$flint_axe_surface <- #convert measures to cm, then get cm2 for each side
  case_when(
    flintAxe_grind$sides_ground == 4 ~
      ((flintAxe_grind$length_cm*flintAxe_grind$width_cm)*2)+
      ((flintAxe_grind$length_cm*flintAxe_grind$thickness_cm)*2),
    flintAxe_grind$sides_ground == 2 ~
      ((flintAxe_grind$length_cm*flintAxe_grind$width_cm)*2),
  )

#Andersen (after Olausson 1983a: 32) reports grinding time mean of 2 cm2/minute,
#but 1.3 cm2/minute fits the reference data better (see plot below):
flintAxe_grind$flint_axe_grind_time <-
  (((flintAxe_grind$flint_axe_surface)/60)*1.3)*flintAxe_grind$percent_surface_ground

flint_axe_thin_grind <- flintAxe_grind[flintAxe_grind$axe_type == "thin-butted"]
flint_axe_thin_grind2 <- na.omit(flintAxe_grind$flint_axe_grind_time)

flint_grind_fit <- coef(lm(PH_grindTime ~ flint_axe_surface, data = dplyr::filter(flintAxe_grind[-1,], percent_surface_ground == 1)))
flint_grind_predict <- coef(lm(flint_axe_grind_time ~ flint_axe_surface, data = dplyr::filter(flintAxe_grind, percent_surface_ground == 1)))
flint_grind_lm_diff <- rbind(flint_grind_fit, flint_grind_predict)
#geo_mean <- exp(mean(log(flint_grind_lm_diff[,2])))
flint_grind_median <- median(flint_grind_lm_diff[,2])


flint_grind_ref.p <- ggplot(flintAxe_grind, aes(flint_axe_surface, flint_axe_grind_time,
                                                shape = grinder))+
  geom_point(aes(alpha = percent_surface_ground), size = 3, color = "green")+
  geom_point(aes(flint_axe_surface, PH_grindTime,
                 alpha = percent_surface_ground), size = 3, color = "red")+
  geom_abline(aes(intercept = flint_grind_fit[1], slope = flint_grind_fit[2]), color = "red")+
  geom_abline(aes(intercept = flint_grind_predict[1], slope = flint_grind_predict[2]), color = "green")+
  geom_abline(aes(intercept = mean(flint_grind_lm_diff[,1]), slope = flint_grind_median), color = "gray")


#load flint knapping reference data
flint_knapp_time <- read.csv(here("analysis/data/raw_data/reference_data/flint_knapp_time.csv"),
                           header = TRUE,
                           stringsAsFactors = TRUE,
                           sep = ",",
                           encoding = "UTF-8",
                           dec = '.',
                           na.strings = '')

#get just the thin-butted axes and fit a model to time with exponential effect of axe_length_cm
thin_butt_knap <- filter(flint_knapp_time, object_type == "Thin-butted axe (four-sided)") #excluding objects that are not thin-butted axes
thin_butt_knap2 <- thin_butt_knap[,c(1,3)]
thin_butt_knap2 <- na.omit(thin_butt_knap2)

thin_knap.lm <- lm(time ~ axe_length_cm, data = thin_butt_knap2)
summary(thin_knap.lm)
thin_knap.lm_coef <- thin_knap.lm$coefficients[2]

thin_knap.p <- ggplot(thin_butt_knap2, aes(axe_length_cm, time))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

thick_butt_knap <- filter(flint_knapp_time, object_type == "Thick-butted axe (four-sided)") #excluding objects that are not thick-butted axes
thick_butt_knap_median <-median(thick_butt_knap$time) #getting the median of thick-butted axe knapping
thick_butt_knap_sd <- as.character(round(sd(thick_butt_knap$time), 2)) #stand.dev of thin-butted axe knapping
thick_butt_knap_sd_label <- paste("std.dev.", thick_butt_knap_sd)

#same as above but concatenated with text to one object to be used in knapp_plot

thick_knapp.p <- ggplot(thick_butt_knap, aes("knapping", y = time))+
  geom_boxplot()+
  geom_jitter(width = 0.01)+
  stat_summary(fun=mean, geom="point", shape="-", size=2, color="red", fill="red") +
  annotate(geom = "text", x = 1.2, y = 1.95,
           label = c(thick_butt_knap_sd_label),
           size = 3,
           hjust = 0.5,
           angle = 0)


#excluding vague and very old ethnographic reference, and thick-butted axes (less grinded):
# thinButt_grindPlot <- ggplot(flintAxe_grind[-c(1,6,7),], aes(grinding_method, PH_grindTime)) +
#   geom_boxplot()+
#   geom_jitter(width = 0.01, aes(color = source_type, shape = People), size = 3, alpha = 0.6)+
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

#Surface area of a hypothetical fully ground four-sided axe of 20 x 8 x 4 cm
flint_ground_surface_example <- ((20*8)*2)+((20*4)*2)
flint_grind_thinbutt_example <-  flint_ground_surface_example*flint_grind_median
thin_butt_knap_median <- 20*0.1 #20 cm long
thinButt_grind_median <- flint_grind_median
#thinButt_grind_median <- median(flintAxe_grind$PH_grindTime[-c(1,6,7,8)],)

## Formula for calculating prehistoric flint axe person-hours:
# 8000 kg raw material by 6-7 (6.5) persons for 6 months:
flint_raw_1 <- round(8000/6.5, 2)  #8000 kg raw material divided by 6.5 persons
flint_raw_2 <- round(flint_raw_1/6, 2)    # divided by 6 months
flint_raw_3 <- round(flint_raw_2/30, 2) # divided 30 days (per month)
flint_raw_4 <- round(flint_raw_3/8, 2)
flint_raw_5 <- round(5/flint_raw_4, 2)
axe_blank <- round(flint_raw_5+(9.5/60), 1)

flint_axe_sharp <- mean(15:25)/60

flint_travel_PH <- 14 # this should be calculated with the travel hours script using GIS

axeAdze_handle_ethno1 <- 6.33 #from Olausson 1984:43
axeAdze_handle_ethno2 <- 8.62 #from Olausson 1984:43
axeAdze_handle_inf <- mean(16:32) # from informant (MK)
axeAdze_handleMedian <- median(c(axeAdze_handle_ethno1, axeAdze_handle_ethno2, axeAdze_handle_inf))

#calculate PH for hypotehtical example
flint_axeAdze_total_PH <-
  axe_blank +
  thin_butt_knap_median +
  (thinButt_grind_median*flint_ground_surface_example) +
  flint_axe_sharp +
  flint_travel_PH +
  axeAdze_handleMedian

flint_long <- (axe_blank+thin_butt_knap_median+((1+thinButt_grind_median)*30))*0.5
flint_axe_thin <- (thin_butt_knap_median+((1+thinButt_grind_median)*30))*0.5
flint_shine <- ((1+thinButt_grind_median)*30)*0.5
flint_colour <- (axe_blank+(1+thinButt_grind_median)*30)*0.5

#Thick-butted flint axe grinding (rougher than thin-butted axes):
thick_butt_grind_median <- median(flintAxe_grind[flintAxe_grind$axe_type == "thick-butted", c("flint_axe_grind_time")])

#British Neolithic flint axe (two-sided) knapping:
Brit_neo_flint_axe <- median(flint_knapp_time[flint_knapp_time$object_type == "British Neolithic flint axe (two-sided)", c("time")])
#Brit_neo_flint_axe <- flint_knapp_time[8,]

#This dataframe will be used to estimate/calculate person-hours for archaeological axes:
Flint_PH_all <- data.frame("Object" = c("thin-butted axe (four-sided)",
                                        "thick-butted axe (four-sided)",
                                        "Flint Axe (two-sided)"),
                           "raw_material" = c(axe_blank, axe_blank, axe_blank),
                           "knappingPH" = c(thin_butt_knap_median,
                                            thick_butt_knap_median,
                                            Brit_neo_flint_axe),
                           "grindPH" = c(median(flint_axe_thin_grind$flint_axe_grind_time, na.rm = TRUE),
                                         thick_butt_grind_median,
                                         thick_butt_grind_median*0.6), #only two-sided so sides not ground = shorter time
                           "sharpening" = c(flint_axe_sharp,
                                            flint_axe_sharp,
                                            flint_axe_sharp),
                           "handle" = c(axeAdze_handleMedian,
                                        axeAdze_handleMedian,
                                        axeAdze_handleMedian),
                           "skill_level" = c("very high", "high", "medium"))
Flint_PH_all$total_PH <- round(rowSums(Flint_PH_all[2:6], na.rm = TRUE), 2)



HansenMadsen83_flakes <- read.csv(here("analysis/data/raw_data/reference_data/HansenMadsen83_otherflint.csv"),
                                  header = TRUE,
                                  stringsAsFactors = TRUE,
                                  sep = ";",
                                  encoding = "UTF-8",
                                  dec = '.',
                                  na.strings = 'NA')

other_flint <- read.csv(here("analysis/data/raw_data/reference_data/other_flint.csv"),
                                  header = TRUE,
                                  stringsAsFactors = TRUE,
                                  sep = ";",
                                  encoding = "UTF-8",
                                  dec = '.',
                                  na.strings = 'NA')

#sum knapping and handle PH into total PH:
other_flint$total_PH <- rowSums(other_flint[c("knapping_PH", "handle_PH")], na.rm = TRUE)

