require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots
require(DescTools)
require(psych)
require(gglorenz)


here("analysis")

source(here('analysis/scripts/labour_hour_total.R'), echo = FALSE)
source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE)
source(here('analysis/scripts/Prestige.R'), echo = FALSE)
source(here('analysis/scripts/animals.R'), echo = FALSE)


#first make Gini from each of the variables (PH_raw, skill, scarcity, travel, pitLabourHours),
#then make geometric mean of the Ginis (working thus as normalised inequality measures)
#calculate scarcity Gini:
# grave_scarcity <- data.frame(Grave_ID = grave_8_skelet$Grave_ID, scarcity = grave_8_skelet %>% select(contains('scarcity')))
# grave_scarcity$total_scarcity <- rowSums(grave_scarcity[,-1], na.rm = TRUE)
# scarcity_num <- unlist(grave_scarcity$total_scarcity)
# Gini_scarcity <- round(Gini(scarcity_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
# LC_scarcity <- Lc(scarcity_num)

hist_animal <- ggplot(animals_all, aes(total_animal))+
  geom_density()

#calculate PH_raw Gini:
grave_PH_raw <- data.frame(Grave_ID = grave_8_skelet$Grave_ID, grave_8_skelet %>% select(contains('PH_raw')))
grave_PH_raw$total_PH_raw <- rowSums(grave_PH_raw[,-1], na.rm = TRUE)

PH_raw_num <- unlist(grave_PH_raw$total_PH_raw)
#PH_raw_num[PH_raw_num < 1.1] <- 1.1
#logPH_num <- sqrt(PH_raw_num)
Gini_PH_raw <- round(Gini(PH_raw_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

hist_PH_raw <- ggplot(grave_PH_raw, aes(total_PH_raw))+
  geom_density()



#calculate TOT Gini:
# TOT_num <- unlist(CW_binary$TOT[-16])
# Gini_TOT <- round(Gini(TOT_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


#Script calculating changes to the Gini depending on number of added offset (0-10)
source(here('analysis/scripts/Gini_TOT_diff.R'), echo = FALSE)


sweet_spot <- filter(Gini_TOT_diff, Gini_drop > 0)
drop_perc_cap <- filter(Gini_TOT_diff[-c(1:2),], drop_perc <= 5.0)
sweet_drop_perc <- max(drop_perc_cap$drop_perc)
CI_gap_sweet_spot <- max(drop_perc_cap[-c(1:2),c("CI_gap")])
addition_sweet_spot <- min(sweet_spot[,c("addition")])
Gini_sweet_spot <- filter(sweet_spot, CI_gap == CI_gap_sweet_spot)


#plot the changes (Moravia) to the Gini and its CI depending on added values (0-10)
Gini_TOT_diff.p_prep <- Gini_TOT_diff[-c(1,12),]
Gini_TOT_diff.p_prep$addition <- as.factor(Gini_TOT_diff.p_prep$addition)
Gini_TOT_diff.p_prep$addition <- c("max mult.", "max+1", "max+2", "max+3", "max+4", "max+5", "max+6", "max+7", "max+8", "max+9")

TOT_Gini_stats.p <- ggplot(Gini_TOT_diff.p_prep, aes(addition, (drop_perc/100)))+
  geom_col()+
  #geom_line()+
  geom_hline(aes(yintercept = (sweet_drop_perc/100), color = "red"), show.legend = FALSE)+
  geom_point(aes(addition, gini))+
  #scale_x_discrete(breaks = Gini_TOT_diff.p_prep$addition)+
  #scale_x_discrete(breaks = c("x max", "1", "2", "3", "4", "5", "6", "7", "8", "9"))+
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5))+
  ylab("Gini drop % (bars) vs. Gini (dots)")+
  xlab("TOT added")+
  geom_text(aes(6.5, ((sweet_drop_perc/100)+0.02), hjust = 0, label=paste("Gini drop <5%: ", as.name(sweet_drop_perc))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc/100)+0.04), hjust = 0, label=paste("TOT added: ", as.name(addition_sweet_spot))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc/100)+0.06), hjust = 0, label=paste("Orig. Gini: ", as.name(Gini_TOT_diff$gini[1]))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc/100)+0.08), hjust = 0, label=paste("Adj. Gini: ", as.name(Gini_sweet_spot$gini))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc/100)+0.10), hjust = 0, label=paste("CI: ", as.name(CI_gap_sweet_spot))), size = 2.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sweet_spot_vli <- filter(Gini_diff_vli, Gini_drop > 0)
drop_perc_cap_vli <- filter(Gini_diff_vli[-c(1:2),], drop_perc <= 5.0)
sweet_drop_perc_vli <- max(drop_perc_cap_vli$drop_perc)
CI_gap_sweet_spot_vli <- max(drop_perc_cap_vli[-c(1:2),c("CI_gap")])
addition_sweet_spot_vli <- min(filter(sweet_spot_vli, drop_perc <= 5.0)[,c("addition")])
Gini_sweet_spot_vli <- filter(sweet_spot_vli, CI_gap == CI_gap_sweet_spot_vli)

#plot the changes to the Gini (Vlineves) and its CI depending on added values (0-10)
Gini_diff_vli.p_prep <- Gini_diff_vli[-c(1,12),]
Gini_diff_vli.p_prep$addition <- as.factor(Gini_diff_vli.p_prep$addition)
Gini_diff_vli.p_prep$addition <- c("max mult.", "max+1", "max+2", "max+3", "max+4", "max+5", "max+6", "max+7", "max+8", "max+9")

TOT_Gini_stats_vli.p <- ggplot(Gini_diff_vli.p_prep, aes(addition, (drop_perc/100)))+
  geom_col()+
  #geom_line()+
  geom_hline(aes(yintercept = (sweet_drop_perc_vli/100), color = "red"), show.legend = FALSE)+
  geom_point(aes(addition, gini))+
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5))+
  ylab("Gini drop % (bars) vs. Gini (dots)")+
  xlab("TOT added")+
  geom_text(aes(6.5, ((sweet_drop_perc_vli/100)+0.02), hjust = 0, label=paste("Gini drop <5%: ", as.name(sweet_drop_perc_vli))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc_vli/100)+0.04), hjust = 0, label=paste("TOT added: ", as.name(addition_sweet_spot_vli))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc_vli/100)+0.06), hjust = 0, label=paste("Orig. Gini: ", as.name(Gini_diff_vli$gini[1]))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc_vli/100)+0.08), hjust = 0, label=paste("Adj. Gini: ", as.name(Gini_sweet_spot_vli$gini))), size = 2.5)+
  geom_text(aes(6.5, ((sweet_drop_perc_vli/100)+0.10), hjust = 0, label=paste("CI: ", as.name(CI_gap_sweet_spot_vli))), size = 2.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


hist_TOT <- ggplot(CW_binary, aes(TOT))+
  geom_density()

LC_TOT.p <- ggplot(CW_binary, aes(TOT))+
  stat_lorenz(geom = "point", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("Total Object Types (TOT) %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(Gini_sweet_spot$gini))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")


#Calculate scarcity Gini:
grave_scarcity <- data.frame(Grave_ID = grave_8_skelet$Grave_ID, grave_8_skelet %>% select(contains('scarcity')))
grave_scarcity$total_scarcity <- rowSums(grave_scarcity[,-1], na.rm = TRUE)
scarcity_num <- unlist(grave_scarcity$total_scarcity)
Gini_scarcity <- round(Gini(scarcity_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


hist_scarcity <- ggplot(grave_scarcity, aes(total_scarcity))+
  geom_density()

LC_scarcity.p <- ggplot(grave_scarcity, aes(total_scarcity))+
  stat_lorenz(geom = "point", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("Scarcity points %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(Gini_scarcity))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")

ggplotly(LC_scarcity.p)

#combine travel_hours+scarcity+skill:
grave_travel <- data.frame(Grave_ID = grave_8_skelet$Grave_ID, grave_8_skelet %>% select(contains('travel')))
grave_travel$total_travel <- rowSums(grave_travel[,-1], na.rm = TRUE)
travel_total <- data.frame(Grave_ID = grave_travel$Grave_ID, total_travel = grave_travel$total_travel)
travel_num <- unlist(travel_total$total_travel)

hist_travel <- ggplot(grave_travel, aes(total_travel))+
  geom_density()

# scarcity_total <- data.frame(Grave_ID = grave_scarcity$Grave_ID, grave_scarcity$total_scarcity)
# travel_scarcity <- merge(x=travel_total, y=scarcity_total, by="Grave_ID", all = TRUE)
# names(travel_scarcity) <- c("Grave_ID", "total_travel", "total_scarcity")

grave_skill <- data.frame(Grave_ID = grave_8_skelet$Grave_ID, grave_8_skelet %>% select(contains('skill')))
grave_skill$total_skill <- rowSums(grave_skill[,-1], na.rm = TRUE)
skill_total <- data.frame(Grave_ID = grave_travel$Grave_ID, total_skill = grave_skill$total_skill)
skill_num <- unlist(skill_total$total_skill)

hist_skill <- ggplot(grave_skill, aes(total_skill))+
  geom_density()


#Calculate grave depth Gini (removing graves with missing depth data)
graveDepth <- na.omit(data.frame(Grave_ID = grave_8_skelet$Grave_ID, graveDepth = grave_8_skelet$graveDepth))
graveDepth_num <- unlist(graveDepth)
Gini_graveDepth <- round(Gini(graveDepth_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

hist_grave_depth <- ggplot(graveDepth, aes(graveDepth))+
  geom_density()

LC_graveDepth.p <- ggplot(graveDepth, aes(graveDepth))+
  stat_lorenz(geom = "point", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("Grave Depth %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(Gini_graveDepth))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")


#Calculate prestige Gini:
prestige_total <- data.frame(prestige_total = Prestige_values$prestige_total)
prestige_num <- unlist(prestige_total)
Gini_prestige <- round(Gini(prestige_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

hist_prestige <- ggplot(prestige_total, aes(prestige_total))+
  geom_density()


#add all value measures together in one table:
allGravesTotals1 <- merge(x=grave_PH_raw[,c(1,10)], y=Prestige_values[,c(1,20)], by="Grave_ID", all = TRUE)
allGravesTotals2 <- merge(x=allGravesTotals1, y=grave_scarcity[,c(1,10)], by="Grave_ID", all = TRUE)
allGravesTotals3 <- merge(x=allGravesTotals2, y=grave_travel[,c(1,6)], by="Grave_ID", all = TRUE)
allGravesTotals4 <- merge(x=allGravesTotals3, y=grave_skill[,c(1,10)], by="Grave_ID", all = TRUE)
allGravesTotals5 <- merge(x=allGravesTotals4, y=animals_all[,c(1,14)], by="Grave_ID", all = TRUE)



appendix_table <- merge(x=grave_id[1:2], y=allGravesTotals5, by="Grave_ID", all = TRUE)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#allGravesTotals5$mean <- rowMeans(allGravesTotals5[2:7])
#allGravesTotals5$median <- rowMedians(as.matrix(allGravesTotals5[2:7]))
#allGravesTotals5$sum <- rowSums(allGravesTotals5[2:7])
#grave_normed <- as.data.frame(lapply(allGravesTotals5[,2:7], min_max_norm))
grave_normed_ID <- data.frame(Grave_ID = allGravesTotals5[,1], lapply(allGravesTotals5[,2:7], min_max_norm))



#grave_scaled_num <- unlist(rowSumsallGravesTotals4$scaled)
grave_normed_sum <- unlist(rowSums(grave_normed_ID[,2:7]))
Gini_GG_normed <- round(Gini(grave_normed_sum, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_norm_single <- Gini_GG_normed[1]

# Normalized grave good values Lorenz curve
LC_GG_normed.p <- ggplot(as.data.frame(grave_normed_sum), aes(grave_normed_sum))+
  stat_lorenz(geom = "point", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("grave good values* %.")+
  geom_text(aes(0.2, 1, size = 25, label=paste("Gini ", as.name(Gini_norm_single))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")

