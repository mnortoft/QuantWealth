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

flintAxe_grind_thinButt <- flintAxe_grind[-c(1,6,7,8),]

box_method <- ggplot(flintAxe_grind, aes(x = grinding_method, y = PH_grindTime, color = axe_type))+
  geom_boxplot()+
  geom_jitter(width = 0.1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

all_meth_grind_SD <- as.character(round(sd(flintAxe_grind_thinButt$PH_grindTime), 1)) #Std.dev for all grinding methods
all_meth_grind_SD_label <- paste("std.dev.", all_meth_grind_SD)
summary(flintAxe_grind_thinButt$PH_grindTime)

box_all <- ggplot(flintAxe_grind_thinButt, aes("all methods\n (experimental and informant)", y = PH_grindTime))+
  geom_boxplot()+
  geom_jitter(width = 0.01, aes(color = grinding_method))+
  stat_summary(fun=mean, geom="point", shape="-", size=2, color="red", fill="red")+
  annotate(geom = "text", x = 1.2, y = 13,
           label = c(all_meth_grind_SD_label),
           size = 3,
           hjust = 0.5,
           angle = 90)


flint_knapp_time <- read.csv(here("analysis/data/raw_data/reference_data/flint_knapp_time.csv"),
                           header = TRUE,
                           stringsAsFactors = TRUE,
                           sep = ",",
                           encoding = "UTF-8",
                           dec = '.',
                           na.strings = '')

thin_butt_knap <- flint_knapp_time[-8,] #excluding objects that are not thin-butted axes
thin_butt_knap_median <-median(thin_butt_knap$time) #getting the median of thin-butted axe knapping
thin_butt_knap_sd <- as.character(round(sd(thin_butt_knap$time), 2)) #stand.dev of thin-butted axe knapping
#same as above but concatenated with text to one object to be used in knapp_plot
thin_butt_knap_sd_label <- paste("std.dev.", thin_butt_knap_sd)

knapp_plot <- ggplot(thin_butt_knap, aes("knapping", y = time))+
  geom_boxplot()+
  geom_jitter(width = 0.01)+
  stat_summary(fun=mean, geom="point", shape="-", size=2, color="red", fill="red") +
  annotate(geom = "text", x = 1.2, y = 1.78,
           label = c(thin_butt_knap_sd_label),
           size = 3,
           hjust = 0.5,
           angle = 90)


#excluding vague and very old ethnographic reference, and thick-butted axes (less grinded):
thinButt_grindPlot <- ggplot(flintAxe_grind[-c(1,6,7),], aes(grinding_method, PH_grindTime)) +
  geom_boxplot()+
  geom_jitter(width = 0.01, aes(color = source_type, shape = People), size = 3, alpha = 0.6)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

thinButt_grind_median <- median(flintAxe_grind$PH_grindTime[-c(1,6,7,8)],)

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

flint_axeAdze_total_PH <-
  axe_blank +
  thin_butt_knap_median +
  thinButt_grind_median +
  flint_axe_sharp +
  flint_travel_PH +
  axeAdze_handleMedian

flint_long <- (thin_butt_knap_median+thinButt_grind_median)*0.5
flint_axe_thin <- (thin_butt_knap_median+thinButt_grind_median)*0.5
flint_shine <- thinButt_grind_median*0.5
flint_colour <- (axe_blank+thinButt_grind_median)*0.5

#Thick-butted flint axe grinding (rougher than thin-butted axes):
thick_butt_grind_median <- median(flintAxe_grind$PH_grindTime[c(6,7)])

#British Neolithic flint axe (two-sided) knapping:
Brit_neo_flint_axe <- flint_knapp_time[8,]


Flint_PH_all <- data.frame("Object" = c("thin-butted axe (four-sided)",
                                        "thick-butted axe (four-sided)",
                                        "Flint Axe (two-sided)"),
                           "raw_material" = c(axe_blank, axe_blank, axe_blank),
                           "knappingPH" = c(thin_butt_knap_median,
                                            thin_butt_knap_median,
                                            Brit_neo_flint_axe$time),
                           "grindPH" = c(thinButt_grind_median,
                                         thick_butt_grind_median,
                                         thinButt_grind_median),
                           "sharpening" = c(flint_axe_sharp,
                                            flint_axe_sharp,
                                            flint_axe_sharp),
                           "handle" = c(axeAdze_handleMedian,
                                        axeAdze_handleMedian,
                                        axeAdze_handleMedian),
                           "skill_level" = c("very high", "high", "very high"))
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
