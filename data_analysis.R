# TITLE: data_analysis
# VERSION: 0.4.1
# LAST EDIT: 2022/09/08


# IN CASE YOU NEED A CLEAN START:
rm(list=ls())


# LOAD SOME PACKAGES
library(tidyverse)
library(languageR)
library(dplyr)
library(tidyr)
library(lme4)
library(datasets)
library(ggpubr)
library(rstatix)
library(broom)
library(car)
library(forcats)


# DATA PREPARATION
## Load all data files in the data directory
## Then bind them all into one dataframe
library(dplyr)
datafiles <- list.files(path = "C:/Users/TYH/Desktop/R_working_dir/analysis_intermediate/data/",
           pattern = "*results.csv",
           full.names = TRUE)
datafiles

datafiles %>%
  lapply(read.csv) %>%
  bind_rows -> results_int

dim(results_int)
head(results_int)
tail(results_int)

View(results_int)

## Reorder factor level of target sounds
### factor() converts a variable into a factor variable
### With the c() argument, it can also reorder factor level
### "p t k b d g" in this case
results_int2 <- results_int
results_int2$target_sound <- factor(results_int2$target_sound,
                                        c("p", "t", "k", "b", "d", "g")
                                        )
results_int2$pre_s_type <- factor(results_int2$pre_s_type)
results_int2$foll_s_type <- factor(results_int2$foll_s_type)

## Check dispersion & write into a file.
### Remember to REMOVE NA VALUES!!!
disp_int <- results_int2 %>% 
  group_by(target_sound) %>%  
  summarise(mean_clo = mean(closure_dur, na.rm = TRUE),
            sd_clo = sd(closure_dur, na.rm = TRUE),
            mean_bur = mean(burst_dur, na.rm = TRUE),
            sd_bur = sd(burst_dur, na.rm = TRUE),
            mean_voi = mean(voicing_dur, na.rm = TRUE),
            sd_voi = sd(voicing_dur, na.rm = TRUE),
            mean_voiclo = mean(voi.clo, na.rm = TRUE),
            sd_voiclo = sd(voi.clo, na.rm = TRUE)
            )

## Open a panel to check the dispersion table
View(disp_int)

## Export dispersion table to a csv file
write.csv(disp_int, 
          file = "C:/Users/TYH/Desktop/R_working_dir/analysis_intermediate/disp_int.csv",
          row.names = TRUE)


# PLOTTING
## (forcats) Collapse factor levels into manually defined groups
library(forcats)
results_int2$target_sound2 <- fct_collapse(results_int2$target_sound, 
                                               voiceless = c("p", "t", "k"), 
                                               voiced = c("b", "d", "g")
                                               )
results_int2$target_sound2 <- factor(results_int2$target_sound2,
                                        c("voiceless", "voiced")
                                        )

View(results_int2) 
### A new column "target.sound2" is now added to the dataframe

## Use the new column as grouping factor to make a new dispersion table
disp_int2 <- results_int2 %>% 
  group_by(target_sound2) %>%  
  summarise(mean_clo = mean(closure_dur, na.rm = TRUE),
            sd_clo = sd(closure_dur, na.rm = TRUE),
            mean_bur = mean(burst_dur, na.rm = TRUE),
            sd_bur = sd(burst_dur, na.rm = TRUE),
            mean_voi = mean(voicing_dur, na.rm = TRUE),
            sd_voi = sd(voicing_dur, na.rm = TRUE),
            mean_voiclo = mean(voi.clo, na.rm = TRUE),
            sd_voiclo = sd(voi.clo, na.rm = TRUE)
            )

View(disp_int2)

## Export dispersion table to a csv file
write.csv(disp_int2, 
          file = "C:/Users/TYH/Desktop/R_working_dir/analysis_intermediate/disp_int2.csv",
          row.names = TRUE)


## (ggplot2) Make box plots to check dispersion for duration of closure.
## With variable box width to display sample size for each group
## The newly added factor "target_sound2" can be used for x-axis
## Prepare a special xlab with the number of obs for each group
xlab_withN <- paste(levels(results_int2$target_sound2),
                    "\n(N=", 
                    table(results_int2$target_sound2),
                    ")", 
                    sep="")

box_clo <- ggplot(results_int2, aes(x = target_sound2, y = closure_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of closure duration", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

ggsave("box_clo.png") 
### ggsave() only saves the latest plot, so do it right after a plot is created

box_clo

box_bur <- ggplot(results_int2, aes(x = target_sound2, y = burst_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of burst duration", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

ggsave("box_bur.png")

box_bur

box_voi <- ggplot(results_int2, aes(x = target_sound2, y = voicing_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of voicing duration", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

ggsave("box_voi.png")

box_voi

box_voiclo <- ggplot(results_int2, aes(x = target_sound2, y = voi.clo, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of VOI/CLO values", x = "Target sound", y = "voi.clo") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

ggsave("box_voiclo.png")

box_voiclo


# INTERSONORANT TARGETS: a closer look
## Check for factor level
### Make sure that the vector is a FACTOR first
levels(results_int2$pre_s_type)
levels(results_int2$foll_s_type)

## Create a subset for intersonorant targets
### First create new columns to collapse factor levels 
library(forcats)

results_int2$pre_s_type2 <- fct_collapse(results_int2$pre_s_type, 
                                               obstruent = c("fric", "stop"), 
                                               sonorant = c("lat", "nas", "vowel","vowel_hi")
                                               )

results_int2$foll_s_type2 <- fct_collapse(results_int2$foll_s_type, 
                                               obstruent = c("fric", "stop"), 
                                               sonorant = c("lat", "nas", "vowel","vowel_hi", "gli", "rho")
                                               )

results_int2_interso <- results_int2[results_int2$pre_s_type2 == "sonorant" & results_int2$foll_s_type2 == "sonorant",]

View(results_int2_interso)

disp_int2_interso <- results_int2_interso %>% 
  group_by(target_sound2) %>%  
  summarise(mean_clo = mean(closure_dur, na.rm = TRUE),
            sd_clo = sd(closure_dur, na.rm = TRUE),
            mean_bur = mean(burst_dur, na.rm = TRUE),
            sd_bur = sd(burst_dur, na.rm = TRUE),
            mean_voi = mean(voicing_dur, na.rm = TRUE),
            sd_voi = sd(voicing_dur, na.rm = TRUE),
            mean_voiclo = mean(voi.clo, na.rm = TRUE),
            sd_voiclo = sd(voi.clo, na.rm = TRUE)
            )

View(disp_int2_interso)

write.csv(disp_int2_interso, 
          file = "C:/Users/TYH/Desktop/R_working_dir/analysis_intermediate/disp_int2_interso.csv",
          row.names = TRUE)

## Box plots for intersonorant targets
xlab_withN_interso <- paste(levels(results_int2_interso$target_sound2),
                    "\n(N=", 
                    table(results_int2_interso$target_sound2),
                    ")", 
                    sep="")

box_clo_interso <- ggplot(results_int2_interso, aes(x = target_sound2, y = closure_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of closure duration", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN_interso)

box_clo_interso

ggsave("box_clo_interso.png") 

box_bur_interso <- ggplot(results_int2_interso, aes(x = target_sound2, y = burst_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of burst duration", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN_interso)

box_bur_interso

ggsave("box_bur_interso.png") 

box_voi_interso <- ggplot(results_int2_interso, aes(x = target_sound2, y = voicing_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of voicing duration", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN_interso)

box_voi_interso

ggsave("box_voi_interso.png") 

box_voiclo_interso <- ggplot(results_int2_interso, aes(x = target_sound2, y = voi.clo, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of VOI/CLO values", x = "Target sound", y = "VOI/CLO") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN_interso)

box_voiclo_interso

ggsave("box_voiclo_interso.png") 


# FACTOR EFFECT INSPECTION: closure duration.
## Use the whole data set ftm
## Factor 1: target syllable stress
box_clo_stress <- ggplot(results_int2, aes(x = target_sound2, y = closure_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of closure duration (unstressed vs stressed)", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

### Adding one factor into the picture
box_clo_stress <- box_clo_stress + facet_wrap(~ stressed)

box_clo_stress

ggsave("box_clo_stress.png") 

## Factor 2: at morpheme boundary
box_clo_boundary <- ggplot(results_int2, aes(x = target_sound2, y = closure_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of closure duration (at morpheme boundary?)", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

box_clo_boundary <- box_clo_boundary + facet_wrap(~ boundary)

box_clo_boundary

ggsave("box_clo_boundary.png") 


# FACTOR EFFECT INPECTION: burst duration.
## Factor 1: stressing
box_bur_stress <- ggplot(results_int2, aes(x = target_sound2, y = burst_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of burst duration (unstressed vs stressed)", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

box_bur_stress <- box_bur_stress + facet_wrap(~ stressed)

box_bur_stress

ggsave("box_bur_stress.png") 

## Factor 2: at morpheme boundary
box_bur_boundary <- ggplot(results_int2, aes(x = target_sound2, y = burst_dur, na.rm = TRUE)) + 
  stat_boxplot(geom = "errorbar", width = 0.25, na.rm = TRUE) + 
  geom_boxplot(varwidth = TRUE, fill = "gray", na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "black", fill = "black", na.rm = TRUE) +
  labs(title = "Dispersion of burst duration (at morpheme boundary)", x = "Target sound", y = "Duration (sec)") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = xlab_withN)

box_bur_boundary <- box_bur_boundary + facet_wrap(~ boundary)

box_bur_boundary

ggsave("box_bur_boundary.png") 


# MAYBE LATER?

## stacked bar plots to show target sound contextual info

## LME attempts
### Syntax: model = lmer(target \~ fixed1 + fixed2 + (1\|random1) + (1\|random2), data=data)
### Still need: interpretation, data from multiple speakers (for random effects controlling etc.)

# END OF SCRIPT.
