require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(DescTools)
require(gglorenz)

here("analysis")



source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE)

TOT_num <- data.frame(TOT_original = CW_binary$TOT[-16]) #without extreme outlier
#TOT is usually (for the Neolithic) somewhere between 0 and 10. The Gini is quite sensitive to these low numbers,
#so this script is used to see how much the Gini drops with every added TOT point to the TOT distribution
#in the Gini input, and how much the confidence intervals decrease.
TOT_num$TOT_x_max <- CW_binary$TOT[-16]*max(CW_binary$TOT) #without extreme outlier, problem gone if multiplying TOT with 10
#TOT_num <- data.frame(TOT = grave_scarcity$total_scarcity[-16])
#TOT_num <- data.frame(TOT = graveDepth[,-1])
TOT_num$TOTadd1 <- TOT_num$TOT_x_max+1
TOT_num$TOTadd2 <- TOT_num$TOT_x_max+2
TOT_num$TOTadd3 <- TOT_num$TOT_x_max+3
TOT_num$TOTadd4 <- TOT_num$TOT_x_max+4
TOT_num$TOTadd5 <- TOT_num$TOT_x_max+5
TOT_num$TOTadd6 <- TOT_num$TOT_x_max+6
TOT_num$TOTadd7 <- TOT_num$TOT_x_max+7
TOT_num$TOTadd8 <- TOT_num$TOT_x_max+8
TOT_num$TOTadd9 <- TOT_num$TOT_x_max+9
TOT_num$TOTadd10 <- TOT_num$TOT_x_max+10

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

TOT_norm <- data.frame(lapply(TOT_num, min_max_norm))

Gini_TOT_norm <- round(Gini(unlist(TOT_norm$TOT_original), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

#Load TOT data from Vlineves
TOT_vli <- read.csv(here("analysis/data/raw_data/TOT_additions_Vlineves.csv"),
                    header = TRUE,
                    sep = ",",
                    stringsAsFactors = TRUE)

TOT_vli_org <- filter(TOT_vli[,c(2:3)], addition == "TOT_original")
TOT_vli_org <- data.frame(TOT = TOT_vli_org[,2])
TOT_norm_vli <- data.frame(lapply(TOT_vli_org, min_max_norm))
Gini_TOT_vli <- round(Gini(unlist(TOT_norm_vli$TOT), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

Gini_TOT <- round(Gini(unlist(TOT_num$TOT_original), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- t(data.frame(Gini_TOT))
Gini_TOT_x_max <- round(Gini(unlist(TOT_num$TOT_x_max), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT_x_max)
Gini_TOT1 <- round(Gini(unlist(TOT_num$TOTadd1), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT1)
Gini_TOT2 <- round(Gini(unlist(TOT_num$TOTadd2), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT2)
Gini_TOT3 <- round(Gini(unlist(TOT_num$TOTadd3), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT3)
Gini_TOT4 <- round(Gini(unlist(TOT_num$TOTadd4), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT4)
Gini_TOT5 <- round(Gini(unlist(TOT_num$TOTadd5), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT5)
Gini_TOT6 <- round(Gini(unlist(TOT_num$TOTadd6), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT6)
Gini_TOT7 <- round(Gini(unlist(TOT_num$TOTadd7), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT7)
Gini_TOT8 <- round(Gini(unlist(TOT_num$TOTadd8), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT8)
Gini_TOT9 <- round(Gini(unlist(TOT_num$TOTadd9), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT9)
Gini_TOT10 <- round(Gini(unlist(TOT_num$TOTadd10), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- rbind(Gini_TOT_diff, Gini_TOT10)



addition <- data.frame(addition = c(0, max(CW_binary$TOT), (1:10)))
Gini_TOT_diff <- cbind(Gini_TOT_diff, addition)
rownames(Gini_TOT_diff) <- 1:12

Gini_drop <- abs(diff(Gini_TOT_diff$gini))
Gini_drop <- c(0, Gini_drop)
Gini_TOT_diff$Gini_drop <- Gini_drop

#calculate the Confidence interval gaps
Gini_TOT_diff$CI_gap <- Gini_TOT_diff$upr.ci-Gini_TOT_diff$lwr.ci

#calculate the percentage gini drops from the original gini
Gini_TOT_diff$drop_perc <- round((Gini_TOT_diff$Gini_drop/Gini_TOT_diff[1,c("gini")])*100, 2)

#rearrange
Gini_TOT_diff[ , c("addition", "gini", "Gini_drop", "drop_perc", "lwr.ci", "upr.ci", "CI_gap")]

TOT_add.long <- pivot_longer(TOT_num, cols = 1:length(TOT_num), names_to = "addition")

TOT_add.p <- ggplot(TOT_add.long, aes(value, color = addition))+
  geom_density()


TOT_norm_combined.p <- ggplot()+
  geom_density(aes(TOT_norm_vli$TOT))+
  geom_density(aes(TOT_norm$TOT_original))

TOT_add_vli.p <- ggplot(TOT_vli, aes(value, color = addition))+
  geom_density()


#Load Gini TOT data from Vlineves
Gini_diff_vli <- read.csv(here("analysis/data/raw_data/Gini_diffs_Vlineves.csv"),
                         header = TRUE,
                         sep = ",",
                         stringsAsFactors = TRUE)

LC_vli <- ggplot()+
  stat_lorenz(aes(TOT_vli$value), geom = "point", size = 0.5)+
  #stat_lorenz(aes(TOT_norm$TOT_original), geom = "point", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population (Vlineves) %")+
  ylab("TOT %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini Vli ", as.name(Gini_TOT_vli))))+
  geom_text(aes(0.2, 0.9, size = 36, label=paste("Gini Mor ", as.name(Gini_TOT_norm))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")

LC_comb <- ggplot()+
  stat_lorenz(aes(TOT_vli$value), color = "red", geom = "line", size = 0.5)+
  stat_lorenz(aes(TOT_norm$TOT_original), color = "blue", geom = "point", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("TOT %")+
  geom_text(aes(0.05, 1, size = 36, hjust = 0, label=paste("Gini Vli ", as.name(Gini_TOT_vli))), color = "red")+
  geom_text(aes(0.05, 0.9, size = 36, hjust = 0, label=paste("Gini Mor ", as.name(Gini_TOT_norm))), color = "blue")+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")

TOT_vli_trans <- filter(TOT_vli[,c(2:3)], addition == "TOTadd8")
TOT_vli_trans <- data.frame(TOT = TOT_vli_trans[,2])
Gini_TOT_trans_sweet_vli <- round(Gini(unlist(TOT_vli_trans$TOT), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

LC_comb_trans <- ggplot()+
  stat_lorenz(aes(TOT_vli_trans$TOT), geom = "line", color = "red", size = 0.5)+
  stat_lorenz(aes(TOT_num$TOTadd3), geom = "point", color = "blue", size = 0.5)+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("TOT %")+
  geom_text(aes(0.05, 1, size = 36, hjust = 0, label=paste("Gini Vli ", as.name(Gini_TOT_trans_sweet_vli))), color = "red")+
  geom_text(aes(0.05, 0.9, size = 36, hjust = 0, label=paste("Gini Mor ", as.name(Gini_TOT3))), color = "blue")+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")

