# load some packages idk
# do this EVERY TIME you open this script
library(datasets)
library(lme4)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(dplyr)
library(car)


# load data. redo if source file changed. 
# STICK TO UTF-8!!!
en03_results <- read.csv(file.choose(), 
                         header = TRUE, 
                         na.strings = "NA",
                         encoding = "UTF-8")


# open a panel to check the data file
View(en03_results)


# check summary of data
summary(en03_results)


# create some variables to manipulate
dur_clo <- en03_results$duration_CLO
dur_bur <- en03_results$duration_BUR
dur_voi <- en03_results$duration_VOI
proportion_voi <- en03_results$VOI.CLO
proportion_bur <- en03_results$BUR.CLO
prev_sound_type <- en03_results$prev.sound.type
following_sound_type <- en03_results$following.sound.type
target <- en03_results$target.sound


# check the dispersion. remember to REMOVE NA VALUES!!!
dispersion_en03 <- en03_results %>% 
  group_by(target.sound) %>%  
  summarise(mean_clo = mean(duration_CLO, na.rm = TRUE),
            sd_clo = sd(duration_CLO, na.rm = TRUE),
            mean_bur = mean(duration_BUR, na.rm = TRUE),
            sd_bur = sd(duration_BUR, na.rm = TRUE),
            mean_voi = mean(duration_VOI, na.rm = TRUE),
            sd_voi = mean(duration_VOI, na.rm = TRUE),
            mean_voiclo = mean(VOI.CLO, na.rm = TRUE),
            sd_voiclo = sd(VOI.CLO, na.rm = TRUE),
            mean_burclo = mean(BUR.CLO, na.rm = TRUE),
            sd_burclo = sd(BUR.CLO, na.rm = TRUE)
            )


# open a panel to check the dispersion table
View(dispersion_en03)


# export dispersion table to a csv file
write.csv(dispersion_en03, 
          file = "C:/Users/TYH/Desktop/R_working_dir/dispersion_en03.csv")


# make a box plot for some variables
boxplot(dur_voi, dur_bur, dur_clo,
        main = "test title",
        names = c("duration of voicing", "duration of burst", "duration of closure")
)


# make a scatter plot for two CONTINUOUS NUMERICAL variables 
# to check possible correlation
scatter.smooth(dur_voi, proportion_voi,
               main = "test title",
               xlab = "duration of voicing",
               ylab = "proportion of voicing"
  
)


# do a covariance between two CONTINUOUS NUMERICAL variables
# use Spearman here cuz Pearson just returns NA
# not sure why
rho1_spearman <- cor(en03_results$VOI.CLO, en03_results$word.syllable.total, 
                     use = "complete.obs",
                     method = "spearman")
print(rho1_spearman) 
# result: 0.05736205



# ~~~~~~~~~~~~~~for VOI/CLO analysis~~~~~~~~~~~~~~~~~
# step1: Levene’s test for the homogeneity of variance
# if p < 0.05, it is concluded that there is a difference 
# between the variances in the population
# and we can continue to do ANCOVA
leveneTest(VOI.CLO~target.sound, en03_results)
# results:
#   Levene's Test for Homogeneity of Variance (center = median)
#        Df F value    Pr(>F)    
# group   5  4.8825 0.0002516 ***
#       334                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# step2: ANCOVA test 
# to see which variable contributes significantly 
# to the variance of voicing proportion
AncovaModel_VOI.CLO <- aov(VOI.CLO ~ target.sound + prev.sound.type + following.sound.type, 
                          data = en03_results)
Anova(AncovaModel_VOI.CLO, type="III")
# results:
# Anova Table (Type III tests)
# Response: VOI.CLO
# Sum Sq  Df F value    Pr(>F)    
#   (Intercept)           1.4321   1 16.8057 5.233e-05 ***
#   target.sound          1.1876   5  2.7874   0.01756 *  
#   prev.sound.type       9.1958   4 26.9784 < 2.2e-16 ***
#   following.sound.type  0.5791   4  1.6990   0.14989    
# Residuals            27.7799 326                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# step3: pairwise comparison
# to see which pair of target sounds are diff from each other?
library(multcomp)
AncovaModel_VOI.CLO <- aov(VOI.CLO ~ target.sound + prev.sound.type + following.sound.type, 
                          data = en03_results) # MUST EXECUTE THIS!
postHocs_VOI.CLO <- glht(AncovaModel_VOI.CLO, linfct = mcp(target.sound = "Tukey"))
summary(postHocs_VOI.CLO)
# results:
#   Simultaneous Tests for General Linear Hypotheses
# Multiple Comparisons of Means: Tukey Contrasts
# Fit: aov(formula = VOI.CLO ~ target.sound + prev.sound.type + following.sound.type, 
#          data = en03_results)
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)  
# d - b == 0  0.13814    0.08555   1.615   0.5701  
# g - b == 0  0.04878    0.10583   0.461   0.9971  
# k - b == 0 -0.05567    0.07884  -0.706   0.9793  
# p - b == 0 -0.01337    0.07950  -0.168   1.0000  
# t - b == 0  0.07809    0.07590   1.029   0.9006  
# g - d == 0 -0.08936    0.09301  -0.961   0.9240  
# k - d == 0 -0.19380    0.06288  -3.082   0.0244 *
# p - d == 0 -0.15151    0.06485  -2.336   0.1697  
# t - d == 0 -0.06004    0.05707  -1.052   0.8917  
# k - g == 0 -0.10444    0.08703  -1.200   0.8250  
# p - g == 0 -0.06215    0.08952  -0.694   0.9808  
# t - g == 0  0.02931    0.08458   0.347   0.9993  
# p - k == 0  0.04229    0.05311   0.796   0.9650  
# t - k == 0  0.13376    0.04652   2.875   0.0446 *
# t - p == 0  0.09146    0.04653   1.966   0.3455  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)



# ~~~~~~~~~~~~~~~~for BUR/CLO analysis~~~~~~~~~~~~~~~~
# step1: Levene's test
leveneTest(BUR.CLO~target.sound, en03_results)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value    Pr(>F)    
# group   5  4.9637 0.0002127 ***
#       334                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# step2: ANCOVA test 
AncovaModel_BUR.CLO <- aov(BUR.CLO ~ target.sound + prev.sound.type + following.sound.type, 
                           data = en03_results)
Anova(AncovaModel_BUR.CLO, type="III") 
# Anova Table (Type III tests)
# 
# Response: BUR.CLO
# Sum Sq  Df F value    Pr(>F)    
# (Intercept)            3.880   1  7.3370  0.007112 ** 
#   target.sound          86.013   5 32.5268 < 2.2e-16 ***
#   prev.sound.type       74.376   4 35.1580 < 2.2e-16 ***
#   following.sound.type  19.025   4  8.9934 6.694e-07 ***
#   Residuals            172.413 326                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# step3: pairwise comparison
library(multcomp)
AncovaModel_BUR.CLO <- aov(BUR.CLO ~ target.sound + prev.sound.type + following.sound.type, 
                           data = en03_results) # MUST EXECUTE THIS!
postHocs_BUR.CLO <- glht(AncovaModel_BUR.CLO, linfct = mcp(target.sound = "Tukey"))
summary(postHocs_BUR.CLO)
# Simultaneous Tests for General Linear Hypotheses
# Multiple Comparisons of Means: Tukey Contrasts
# Fit: aov(formula = BUR.CLO ~ target.sound + prev.sound.type + following.sound.type, 
#          data = en03_results)
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)    
#   d - b == 0   0.1725     0.2131   0.809   0.9625    
#   g - b == 0   0.3049     0.2636   1.157   0.8465    
#   k - b == 0   1.3446     0.1964   6.846   <0.001 ***
#   p - b == 0   0.9720     0.1981   4.908   <0.001 ***
#   t - b == 0   1.5597     0.1891   8.249   <0.001 ***
#   g - d == 0   0.1324     0.2317   0.571   0.9921    
#   k - d == 0   1.1721     0.1567   7.482   <0.001 ***
#   p - d == 0   0.7995     0.1616   4.949   <0.001 ***
#   t - d == 0   1.3872     0.1422   9.758   <0.001 ***
#   k - g == 0   1.0397     0.2168   4.795   <0.001 ***
#   p - g == 0   0.6671     0.2230   2.991   0.0321 *  
#   t - g == 0   1.2548     0.2107   5.955   <0.001 ***
#   p - k == 0  -0.3726     0.1323  -2.816   0.0523 .  
#   t - k == 0   0.2151     0.1159   1.856   0.4117    
#   t - p == 0   0.5877     0.1159   5.070   <0.001 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)