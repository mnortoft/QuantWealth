require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(DescTools)

here("analysis")



source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE)

#TOT_num_original <- unlist(CW_binary$TOT[-16]) #without extreme outlier
#TOT is usually (for the Neolithic) somewhere between 0 and 10. The Gini is quite sensitive to these low numbers,
#so this script is used to see how much the Gini drops with every added TOT point to the TOT distribution
#in the Gini input, and how much the confidence intervals decrease.
TOT_num <- data.frame(TOT = CW_binary$TOT*max(CW_binary$TOT)) #get TOT values from prestige boxplot script
#TOT_num <- data.frame(TOT = grave_scarcity$total_scarcity[-16])
#TOT_num <- data.frame(TOT = graveDepth[,-1])
TOT_num$TOTadd1 <- TOT_num$TOT+1
TOT_num$TOTadd2 <- TOT_num$TOT+2
TOT_num$TOTadd3 <- TOT_num$TOT+3
TOT_num$TOTadd4 <- TOT_num$TOT+4
TOT_num$TOTadd5 <- TOT_num$TOT+5
TOT_num$TOTadd6 <- TOT_num$TOT+6
TOT_num$TOTadd7 <- TOT_num$TOT+7
TOT_num$TOTadd8 <- TOT_num$TOT+8
TOT_num$TOTadd9 <- TOT_num$TOT+9
TOT_num$TOTadd10 <- TOT_num$TOT+10
Gini_TOT <- round(Gini(unlist(TOT_num$TOT), conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Gini_TOT_diff <- t(data.frame(Gini_TOT))
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

#add column showing how many TOT points (0:10) added before each Gini calculation
addition <- data.frame(addition = c(0, max(CW_binary$TOT), (1:10)))
Gini_TOT_diff <- cbind(Gini_TOT_diff, addition)
rownames(Gini_TOT_diff) <- 1:11

#Add column showing how much the Gini drops with each added TOT point
Gini_drop <- abs(diff(Gini_TOT_diff$gini))
Gini_drop <- c(0, Gini_drop)
Gini_TOT_diff$Gini_drop <- Gini_drop

#calculate the confidence intervals
Gini_TOT_diff$CI_gap <- Gini_TOT_diff$upr.ci-Gini_TOT_diff$lwr.ci

#calculate the percentage gini drops from the original gini
Gini_TOT_diff$drop_perc <- round((Gini_TOT_diff$Gini_drop/Gini_TOT_diff[1,c("gini")])*100, 2)

#rearrange
Gini_TOT_diff[ , c("addition", "gini", "Gini_drop", "drop_perc", "lwr.ci", "upr.ci", "CI_gap")]
