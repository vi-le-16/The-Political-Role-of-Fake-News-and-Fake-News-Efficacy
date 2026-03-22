library(ggplot2)
library(dplyr)
library(fastDummies) 
library(scales)
library(gridExtra)
library(texreg)
library(sjPlot)
library(sjmisc)
library(likert)
library(formattable)
library(tidyverse)
library(reshape2)
library(ordinal)
library(VGAM)
library(DescTools)
library(MASS)
library(nnet)
library(effects)
library(car)
library(ggpubr)
library(survey)
library(jtools)
library(sandwich)
library(huxtable)
library(emmeans)
library(modelsummary)
library(interactions)
library(modelsummary)
library(kableExtra)
library(gt)
library(stringi)
library(fixest) ### for ols regressions
library(marginaleffects) #for ols regressions
library(xtable)
library(sensemakr)
library(stargazer)
library(modelsummary)
library(insight)  # for latex()
library(patchwork)
library(ggplot2)
library(png)
library(grid)
library(cowplot)
library(sensemakr)

##Set working directory
setwd("~/Documents/For Laura/HU Fake News Efficacy Project/Downloaded")

## Read in Hungary survey from March 2022
HungarianSurvey <-read.csv("~/Documents/For Laura/HU Fake News Efficacy Project/Downloaded/UTXD0004_newparticipationfactor.csv")

## Create a clean date variable in %Y-%m-%d format, 
## based on respondent start time (sometimes, we see a small discrepancy between start and end date.)
HungarianSurvey$date<-as.Date(HungarianSurvey$starttime, format="%Y-%m-%d")

## First, let's recode most_liked_party as their categorical labels so we 
## don't need to keep remembering the associated integer values.
HungarianSurvey$most_liked_party[is.na(HungarianSurvey$most_liked_party)] <- "N/A" 

HungarianSurvey <- HungarianSurvey %>%
  mutate(
    mostlikedparty = case_when(
      most_liked_party == 1 ~ 1, #Fidesz
      most_liked_party == 2 ~ 2, #United Opposition 
      most_liked_party %in% c(3, 8) ~ NA, #Would not vote + skipped 
      TRUE ~ NA_real_  # handles other/missing values safely
    )
  )

table(HungarianSurvey$mostlikedparty)

### variables ### 

##basic demographics ## 

HungarianSurvey <- HungarianSurvey %>% 
  mutate(gender =
           dplyr::case_when(
             gender <= 1 ~ 0, #Male 
             gender > 1 ~ 1 #Female 
           )
  )

table(HungarianSurvey$gender) # 0 (male) = 970; 1 (female) = 1030

table(HungarianSurvey$agegroup) #not flipped, matches codebook 

table(HungarianSurvey$education_hu) #not flipped, matches codebook 

table(HungarianSurvey$gross_household_hu) #not flipped, matches codebook 

## additional variables ## 

HungarianSurvey <- HungarianSurvey %>% 
  mutate(turnout2018par =
           dplyr::case_when(
             turnout2018par <= 1 ~ 1, #Yes, voted 
             turnout2018par > 1 ~ 0 #No, did not vote 
           )
  )

table(HungarianSurvey$turnout2018par) # 0 (no) = 584; 1 (yes) = 1416

HungarianSurvey <- HungarianSurvey %>% 
  mutate(fakenews_ubiquity =
           dplyr::case_when(
             fakenews_c == 5 ~ 1, #disagree strongly
             fakenews_c == 4 ~ 2, #disagree
             fakenews_c == 3 ~ 3, #neither agree nor disagree
             fakenews_c == 2 ~ 4, #agree
             fakenews_c == 1 ~ 5 #agree strongly 
           )
  )

table(HungarianSurvey$fakenews_ubiquity) # 1 = 38; 2 = 66; 3 = 365; 4 = 771; 5 = 760

HungarianSurvey <- HungarianSurvey %>% 
  mutate(fnu_dummy =
           dplyr::case_when(
             fakenews_ubiquity >= 4 ~ 1, #high FNU 
             fakenews_ubiquity <  4 ~ 0 #low FNU
           ))

table(HungarianSurvey$fnu_dummy) #low = 469; high = 1531
summary(HungarianSurvey$fakenews_ubiquity)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(UbiquityDummy = 
           dplyr::case_when(
             fnu_dummy >= 1 ~ 1, 
             fnu_dummy <= 0 ~ 0
           ))

table(HungarianSurvey$UbiquityDummy)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(ccaware =
           dplyr::case_when(
             ccaware >= 4 ~ 1, #have never heard of: 70 
             ccaware >= 3 ~ 2, #not very aware: 712 
             ccaware >= 2 ~ 3, #somewhat aware: 
             ccaware <= 1 ~ 4, #very aware: 192
           )
  )

table(HungarianSurvey$ccaware) 
summary(HungarianSurvey$ccaware)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(politicalloyal =
           dplyr::case_when(
             party_close <= 8 ~ 1, #opposition
             party_close > 8 ~ 0, #fidesz 
           )
  )

table(HungarianSurvey$politicalloyal)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(democ_sat =
           dplyr::case_when(
             democ_sat >= 4 ~ 1, #very dissatisfied
             democ_sat >= 3 ~ 2, 
             democ_sat >= 2 ~ 3,  
             democ_sat <= 1 ~ 4, #very satisfied
           )
  )

table(HungarianSurvey$democ_sat)
summary(HungarianSurvey$democ_sat)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(EfficacyDummy =
           dplyr::case_when(
             fakenews_d <= 3 ~ 0, #Low Efficacy
             fakenews_d >= 4 ~ 1 #High efficacy
           )
  )

table(HungarianSurvey$fakenews_d)
table(HungarianSurvey$EfficacyDummy) # low (0) = 836; high (1) = 1164

HungarianSurvey <- HungarianSurvey %>% 
  mutate(confidence3_frac =
           dplyr::case_when(
             confidence_3 <= 1 ~ 1, #great deal of confidence 
             confidence_3 <= 2 ~ 0.5, #only some confidence
             confidence_3 <= 3 ~ 0, #hardly any confidence 
             confidence_3 <= 4 ~ NA #no opinion 
           )
  )

table(HungarianSurvey$confidence3_frac)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(confidence4_frac =
           dplyr::case_when(
             confidence_4 <= 1 ~ 1, #great deal of confidence 
             confidence_4 <= 2 ~ 0.5, #only some confidence
             confidence_4 <= 3 ~ 0, #hardly any confidence 
             confidence_4 <= 4 ~ NA #no opinion 
           )
  )

table(HungarianSurvey$confidence4_frac)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(confidence6_frac =
           dplyr::case_when(
             confidence_6 <= 1 ~ 1, #great deal of confidence 
             confidence_6 <= 2 ~ 0.5, #only some confidence
             confidence_6 <= 3 ~ 0, #hardly any confidence 
             confidence_6 <= 4 ~ NA #no opinion 
           )
  )

table(HungarianSurvey$confidence6_frac)

HungarianSurvey <- HungarianSurvey %>% 
  mutate(confidence10_frac =
           dplyr::case_when(
             confidence_10 <= 1 ~ 1, #great deal of confidence 
             confidence_10 <= 2 ~ 0.5, #only some confidence
             confidence_10 <= 3 ~ 0, #hardly any confidence 
             confidence_10 <= 4 ~ NA #no opinion 
           )
  )

table(HungarianSurvey$confidence10_frac)

## new variables for plotting 

HungarianSurvey <-  HungarianSurvey %>% 
  dplyr::mutate(EfficacyContinuous =
                  dplyr::case_when(
                    fakenews_d == 5 ~ 5,
                    fakenews_d == 4 ~ 4,
                    fakenews_d == 3 ~ 3,
                    fakenews_d == 2 ~ 2,
                    fakenews_d == 1 ~ 1))

table(HungarianSurvey$fakenews_d)
table(HungarianSurvey$EfficacyContinuous)

HungarianSurvey <-  HungarianSurvey %>% 
  dplyr::mutate(confidence_fidesz =
                  dplyr::case_when(
                    confidence3_frac == 1 ~ 1, #great deal of confidence 
                    confidence3_frac == 0.5 ~ 0.5, #only some confidence
                    confidence3_frac == 0 ~ 0, #hardly any confidence 
                    confidence3_frac == NA ~ NA #no opinion 
                  )
  )

table(HungarianSurvey$confidence_fidesz)
table(HungarianSurvey$confidence3_frac)

HungarianSurvey <-  HungarianSurvey %>% 
  dplyr::mutate(confidence_united =
                  dplyr::case_when(
                    confidence4_frac == 1 ~ 1, #great deal of confidence 
                    confidence4_frac == 0.5 ~ 0.5, #only some confidence
                    confidence4_frac == 0 ~ 0, #hardly any confidence 
                    confidence4_frac == NA ~ NA #no opinion 
                  )
  )

table(HungarianSurvey$confidence_united)
table(HungarianSurvey$confidence4_frac)

HungarianSurvey <-  HungarianSurvey %>% 
  dplyr::mutate(confidence_gov =
                  dplyr::case_when(
                    confidence6_frac == 1 ~ 1, #great deal of confidence 
                    confidence6_frac == 0.5 ~ 0.5, #only some confidence
                    confidence6_frac == 0 ~ 0, #hardly any confidence 
                    confidence6_frac == NA ~ NA #no opinion 
                  )
  )

table(HungarianSurvey$confidence_gov)
table(HungarianSurvey$confidence6_frac)

HungarianSurvey <-  HungarianSurvey %>% 
  dplyr::mutate(confidence_media =
                  dplyr::case_when(
                    confidence10_frac == 1 ~ 1, #great deal of confidence 
                    confidence10_frac == 0.5 ~ 0.5, #only some confidence
                    confidence10_frac == 0 ~ 0, #hardly any confidence 
                    confidence10_frac == NA ~ NA #no opinion 
                  )
  )

table(HungarianSurvey$confidence_media)
table(HungarianSurvey$confidence10_frac)

##other necessary items## 

# first let's clean income variable to remove don't knows and prefer not to say
HungarianSurvey$householdincome <- ifelse(HungarianSurvey$gross_household_hu > 17, NA, HungarianSurvey$gross_household_hu)
table(HungarianSurvey$householdincome)
table(HungarianSurvey$fakenews_ubiquity)
table(HungarianSurvey$democ_sat)

#Filling in NA variables for household income with median income for one's education level
education_income <- HungarianSurvey %>% group_by(education_hu) %>% dplyr::summarize(edinc = median(householdincome, na.rm = T))
HungarianSurvey <- left_join(HungarianSurvey,education_income, by = "education_hu")
HungarianSurvey$householdincome <- ifelse(is.na(HungarianSurvey$householdincome), HungarianSurvey$edinc, HungarianSurvey$householdincome)

### It is easy for you to identify news or information that you
### believe misrepresent reality or are even false
HungarianSurvey_fakenews_d<- HungarianSurvey 
HungarianSurvey_fakenews_d$Party <- factor(HungarianSurvey_fakenews_d$most_liked_party,
                                           levels = c(1,2,3),
                                           labels = c("Fidesz", "Opposition", "Neither"))
HungarianSurvey_fakenews_d$Party <- factor(HungarianSurvey_fakenews_d$Party,
                                           levels = c("Neither","Fidesz","Opposition"))
HungarianSurvey_fakenews_d$gender <- factor(HungarianSurvey_fakenews_d$gender, levels=c(0,1)) 
HungarianSurvey_fakenews_d$EfficacyDummy<- factor(HungarianSurvey_fakenews_d$EfficacyDummy,  levels=c(0,1), labels=c("Low", "High"))
HungarianSurvey_fakenews_d$UbiquityDummy<- factor(HungarianSurvey_fakenews_d$UbiquityDummy,  levels=c(0,1), labels=c("Low", "High"))

##Make sure that these controls are interpreted as numeric to preserve the order and meaning of their levels
HungarianSurvey_fakenews_d$education_hu<- as.numeric(HungarianSurvey_fakenews_d$education_hu)
HungarianSurvey_fakenews_d$ccaware<- as.numeric(HungarianSurvey_fakenews_d$ccaware)
HungarianSurvey_fakenews_d$fakenews_ubiquity<- as.numeric(HungarianSurvey_fakenews_d$fakenews_ubiquity)
HungarianSurvey_fakenews_d$democ_sat<- as.numeric(HungarianSurvey_fakenews_d$democ_sat)
HungarianSurvey_fakenews_d$householdincome<- as.numeric(HungarianSurvey_fakenews_d$householdincome)
HungarianSurvey_fakenews_d$agegroup<- as.numeric(HungarianSurvey_fakenews_d$agegroup)
HungarianSurvey_fakenews_d$politicalloyal<- as.numeric(HungarianSurvey_fakenews_d$politicalloyal)
HungarianSurvey_fakenews_d$turnout2018par <- as.numeric(HungarianSurvey_fakenews_d$turnout2018par)      

##### Main Text Figure 1 - FNU and Confidence in Institutions (pooled results) #####

## Confidence in Fidesz ##

mod_6 <- fixest::feglm(
  confidence3_frac ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

UbiquityConf_Fidesz <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +   scale_y_continuous(limits = c(-0.2, 0.1), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(x = "", y = "Confidence in Fidesz", title = "", subtitle = "")

print(UbiquityConf_Fidesz)

## Confidence in United Opposition ## 

mod_6 <- fixest::feglm(
  confidence4_frac ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

UbiquityConf_UnitedOpposition <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +   scale_y_continuous(limits = c(-0.2, 0.1), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(x = "", y = "Confidence in the United Opposition", title = "", subtitle = "")

print(UbiquityConf_UnitedOpposition)

## Confidence in the National Government ## 

mod_6 <- fixest::feglm(
  confidence6_frac ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

UbiquityConf_NationalGovernment <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +   scale_y_continuous(limits = c(-0.2, 0.1), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(x = "", y = "Confidence in the National Government", title = "", subtitle = "")

print(UbiquityConf_NationalGovernment)

## Confidence in the Media ## 

mod_6 <- fixest::feglm(
  confidence10_frac ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

UbiquityConf_Media <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +   scale_y_continuous(limits = c(-0.2, 0.1), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(x = "", y = "Confidence in the Media", title = "", subtitle = "")

print(UbiquityConf_Media)

## final faceted plot ## 

plots_to_use <- list(
  UbiquityConf_Fidesz,
  UbiquityConf_UnitedOpposition,
  UbiquityConf_NationalGovernment,
  UbiquityConf_Media
)

# Safety check
stopifnot(all(sapply(plots_to_use, inherits, "gg")))

# ---- Remove legends and harmonize theme ----
shared_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.position = "none"
  )

plots_clean <- lapply(plots_to_use, function(p) p + shared_theme)

# ---- Empty placeholders to complete 3×3 grid ----
empty <- ggplot() + theme_void()

# ---- Build strict 3×3 grid (all panels identical size) ----
panel_grid <-cowplot:: plot_grid(
  plots_clean[[1]], plots_clean[[2]],
  plots_clean[[3]], plots_clean[[4]], 
  ncol = 2,
  #labels = c("A","B","C","D","E","F","G","",""),
  label_size = 14,
  align = "hv"
)

# ---- Title, subtitle, caption ----
title <- ggdraw() +
  draw_label(
    "Fake News Ubiquity (FNU) and Confidence in Institutions",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Pooled comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Estimates from feglm models. Pooled comparisons. All panels share identical scales.",
    size = 8, x = 0, hjust = 0
  )

final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

final_plot

##### Main Text Figure 2 - FNU and Confidence in Institutions (w/ party split) #####

### visualization for confidence in Fidesz ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence3_frac~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

UbiquityConf_Fidesz <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in Fidesz",
    title = "",
    subtitle = ""
  )

print(UbiquityConf_Fidesz)

### visualizaiton for confidence in the United Opposition ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence4_frac~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

UbiquityConf_UnitedOpposition <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in the United Opposition",
    title = "",
    subtitle = ""
  )

print(UbiquityConf_UnitedOpposition)

### visualization for confidence in the national government ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence6_frac~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

UbiquityConf_NationalGovernment <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in the National Government",
    title = "",
    subtitle = ""
  )

print(UbiquityConf_NationalGovernment)

### visualization for confidence in the media ###

#With interaction logit
mod_6 <- fixest::feglm(confidence10_frac~UbiquityDummy*Party + education_hu +ccaware +democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

UbiquityConf_Media <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.33, 0.33), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in the Media",
    title = "",
    subtitle = ""
  )

print(UbiquityConf_Media)

### final faceted plot ###

plots_to_use <- list(
  UbiquityConf_Fidesz,
  UbiquityConf_UnitedOpposition,
  UbiquityConf_NationalGovernment,
  UbiquityConf_Media
)

# Safety check
stopifnot(all(sapply(plots_to_use, inherits, "gg")))

# ---- Remove legends and harmonize theme ----
shared_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.position = "none"
  )

plots_clean <- lapply(plots_to_use, function(p) p + shared_theme)

# ---- Empty placeholders to complete 3×3 grid ----
empty <- ggplot() + theme_void()

# ---- Build strict 3×3 grid (all panels identical size) ----
panel_grid <- cowplot::plot_grid(
  plots_clean[[1]], plots_clean[[2]],
  plots_clean[[3]], plots_clean[[4]], 
  ncol = 2,
  #labels = c("A","B","C","D","E","F","G","",""),
  label_size = 14,
  align = "hv"
)

# ---- Title, subtitle, caption ----
title <- ggdraw() +
  draw_label(
    "Fake News Ubiquity (FNU) and Confidence in Institutions",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Within-party comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Estimates from feglm models. Pairwise comparisons by party. All panels share identical scales.",
    size = 8, x = 0, hjust = 0
  )

final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

final_plot

##### Main Text Figure 3 - FNU and Electoral Engagement (pooled results) #####

### visualization for volunteer ###

mod_6 <- fixest::feglm(volunteer~UbiquityDummy + Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

# single comparisons() call at 95% (authoritative)
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     conf_level = 0.95)

# make a single data.frame and compute 90% CI from its SE so they align
ph_df <- as.data.frame(ph_95)


# --- Minimal diagnostic + plotting replacement (robust to CI column names) ---
# ph_df must already exist (as.data.frame(ph_95))
print("ph_df column names:")
print(names(ph_df))
print("ph_df head:")
print(head(ph_df))

# Robustly pick the first contrast row to plot
ph_row <- ph_df[1, , drop = FALSE]

# Find estimate column
est_col <- if ("estimate" %in% names(ph_row)) "estimate" else
  if ("Estimate" %in% names(ph_row)) "Estimate" else
    stop("No 'estimate' column found in ph_df; names(ph_df) printed above.")

# Find 95% CI columns (many possible names)
ci95_lo_name <- if ("conf.low" %in% names(ph_row)) "conf.low" else
  if ("lower.CL" %in% names(ph_row)) "lower.CL" else
    if ("conf_low" %in% names(ph_row)) "conf_low" else
      if ("lower" %in% names(ph_row)) "lower" else
        NA_character_
ci95_hi_name <- if ("conf.high" %in% names(ph_row)) "conf.high" else
  if ("upper.CL" %in% names(ph_row)) "upper.CL" else
    if ("conf_high" %in% names(ph_row)) "conf_high" else
      if ("upper" %in% names(ph_row)) "upper" else
        NA_character_

# Find SE column if present
se_col <- if ("std.error" %in% names(ph_row)) "std.error" else
  if ("SE" %in% names(ph_row)) "SE" else
    if ("se" %in% names(ph_row)) "se" else
      NA_character_

# If 95% CI present use it, else compute from SE (and if SE missing attempt compute from other CI)
if (!is.na(ci95_lo_name) & !is.na(ci95_hi_name)) {
  ci95_low <- as.numeric(ph_row[[ci95_lo_name]])
  ci95_high <- as.numeric(ph_row[[ci95_hi_name]])
} else if (!is.na(se_col)) {
  z95 <- 1.96
  est_val <- as.numeric(ph_row[[est_col]])
  se_val <- as.numeric(ph_row[[se_col]])
  ci95_low <- est_val - z95 * se_val
  ci95_high <- est_val + z95 * se_val
} else {
  stop("No 95% CI or SE found in ph_df; printed names above — please paste them if you need help.")
}

# Compute 90% CI from same SE if available; otherwise approximate from 95% CI
if (!is.na(se_col)) {
  z90 <- 1.645
  se_val <- as.numeric(ph_row[[se_col]])
  ci90_low  <- as.numeric(ph_row[[est_col]]) - z90 * se_val
  ci90_high <- as.numeric(ph_row[[est_col]]) + z90 * se_val
} else {
  # approximate 90% by shrinking 95% interval proportionally
  ci90_low <- (ci95_low + ci95_high)/2 - 1.645/1.96 * ( (ci95_high - ci95_low)/2 )
  ci90_high <- (ci95_low + ci95_high)/2 + 1.645/1.96 * ( (ci95_high - ci95_low)/2 )
}

# Numeric print to verify values
cat("\nFinal numbers to plot (response scale if you used type='response'):\n")
cat("estimate =", ph_row[[est_col]], "\n")
cat("90% CI = [", ci90_low, ",", ci90_high, "]\n")
cat("95% CI = [", ci95_low, ",", ci95_high, "]\n\n")

# Build a tiny plotting df with explicit numeric columns
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row[[est_col]]),
  ci90_low = ci90_low,
  ci90_high = ci90_high,
  ci95_low = ci95_low,
  ci95_high = ci95_high
)

# Plot 
Ubiquity_CampaignVolunteer <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +   # 90% (thicker)
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +  # 95% (thinner)
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +   scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Willingness to Volunteer for Campaign", title = "", subtitle = "")

Ubiquity_CampaignVolunteer

### visualization for demonstration ## 

mod_6 <- fixest::feglm(
  rally ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",    
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# then the same robust plotting block you used (uses ph_df freshly created)
# (diagnostic prints help confirm numbers)
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

Ubiquity_RallyProtest <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +   scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Willingness to Attend Campaign Rally or Protest", title = "", subtitle = "")

print(Ubiquity_RallyProtest)

###visualization for follow campaign### 

# fit model for campaignfollow
mod_6 <- fixest::feglm(
  campaignfollow ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df 
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

Ubiquity_CampaignFollow <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +    scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Willingness to Follow Electoral Campaign", title = "", subtitle = "")

print(Ubiquity_CampaignFollow)

###visualization for intention to vote### 

# fit model for voteintent
mod_6 <- fixest::feglm(
  voteintent ~ UbiquityDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(UbiquityDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as above
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

Ubiquity_Vote <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNU: High-Low") +  scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Intent to Vote", title = "", subtitle = "")

print(Ubiquity_Vote)

## final faceted plot ## 

# Remove legends (recommended for coefficient plots)
p1 <- Ubiquity_CampaignVolunteer + theme(legend.position = "none")
p2 <- Ubiquity_RallyProtest     + theme(legend.position = "none")
p3 <- Ubiquity_CampaignFollow     + theme(legend.position = "none")
p4 <- Ubiquity_Vote   + theme(legend.position = "none")

# Empty placeholder to keep grid structure
empty <- ggplot() + theme_void()

# Build a strict 2 x 3 grid (all panels same size)
panel_grid <- cowplot::plot_grid(
  p1, p2, 
  p3, p4,
  ncol = 2,
  align = "hv"
)

# Title, subtitle, caption
title <- ggdraw() +
  draw_label(
    "Fake News Ubiquity (FNU) and Electoral Engagement",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Pooled comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Pooled comparisons from feglm models. CIs: 90% & 95%.",
    size = 8, x = 0, hjust = 0
  )

# Stack everything vertically
final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

print(final_plot)

### Main Text Figure 4 - FNU and Electoral Engagement (w/ party split) ###

###visualization for volunteering### 

#With interaction logit
mod_6 <- fixest::feglm(volunteer~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Ubiquity_CampaignVolunteer <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Willingness to Volunteer for Campaign",
    title = "",
    subtitle = ""
  )

print(Ubiquity_CampaignVolunteer)

###visualization for demonstrations##

#With interaction logit
mod_6 <- fixest::feglm(rally~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Ubiquity_RallyProtest <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.4, 0.4), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Willingness to Attend Campaign Rally or Protest",
    title = "",
    subtitle = ""
  )

print(Ubiquity_RallyProtest)

###visualization for follow campaign### 

#With interaction logit
mod_6 <- fixest::feglm(campaignfollow~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Ubiquity_CampaignFollow <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Willingness to Follow Electoral Campaign",
    title = "",
    subtitle = ""
  )

print(Ubiquity_CampaignFollow)

###visualization for intention to vote### 

#With interaction logit
mod_6 <- fixest::feglm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party

ph_party_95 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(UbiquityDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Ubiquity_Vote <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Intent to Vote",
    title = "",
    subtitle = ""
  )

print(Ubiquity_Vote)

### Main Text Table 1 - two-sample t-tests with equal variances for FNE ###

df <- HungarianSurvey_fakenews_d

vars <- c(
  gender = "Gender (0=male,1=female)",
  agegroup = "Age cohort (1=youngest..4=oldest)",
  education_hu = "Level of education (1-10 lowest->highest)",
  householdincome = "Gross household income (1-17 lowest->highest)",
  turnout2018par = "2018 electoral turnout (1=voted,0=abstained)",
  ccaware = "Awareness of Constitutional Court (1-4 least->most aware)",
  politicalloyal = "Political loyalty (1=party preference,0=no pref)",
  democ_sat = "Satisfaction with Hungarian democracy (1-4 least->most)"
)

# helper to coerce a variable to numeric safely
coerce_to_numeric <- function(x) {
  # if already numeric, return as is
  if(is.numeric(x)) return(x)
  # if logical, convert to numeric 0/1
  if(is.logical(x)) return(as.numeric(x))
  # try to convert character/factor to numeric via as.character -> as.numeric
  x_char <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_char))
  # if conversion produced any non-NA values, assume numeric conversion is valid
  if(!all(is.na(x_num))) return(x_num)
  # otherwise, check if it is a two-level (binary) factor/character: map to 0/1
  uniq <- unique(na.omit(x_char))
  if(length(uniq) == 2) {
    # deterministic mapping: sorted levels -> 0/1
    levels_sorted <- sort(uniq)
    mapped <- ifelse(x_char == levels_sorted[1], 0,
                     ifelse(x_char == levels_sorted[2], 1, NA_real_))
    return(mapped)
  }
  # not convertible (multi-level non-numeric)
  return(NULL)
}

sig_stars <- function(p){
  if (is.na(p)) return("")
  if (p < 0.01) return("\\textsuperscript{***}")
  if (p < 0.05) return("\\textsuperscript{**}")
  if (p < 0.10) return("\\textsuperscript{*}")
  return("")
}

test_one_var <- function(df, varname, label){
  # make local copy of relevant cols
  d <- df %>% dplyr::select(EfficacyDummy, dplyr::all_of(varname))
  # coerce EfficacyDummy to binary numeric 0/1 if needed
  E_num <- coerce_to_numeric(d$EfficacyDummy)
  if(is.null(E_num)) {
    # cannot coerce EfficacyDummy -> abort with informative NA row
    return(tibble(
      variable = varname, label = label,
      mean_high = NA_real_, mean_low = NA_real_,
      diff = NA_real_, n_high = 0L, n_low = 0L,
      t_statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_
    ))
  }
  d <- d %>% mutate(.E = E_num)
  
  # coerce target variable to numeric safely
  target_num <- coerce_to_numeric(d[[varname]])
  if(is.null(target_num)) {
    # not sensible to run t-test (e.g., multi-level non-numeric factor)
    return(tibble(
      variable = varname, label = label,
      mean_high = NA_real_, mean_low = NA_real_,
      diff = NA_real_, n_high = 0L, n_low = 0L,
      t_statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_
    ))
  }
  
  d <- d %>% mutate(.X = target_num) %>% filter(!is.na(.E) & !is.na(.X))
  
  # compute group summaries safely
  means_tbl <- d %>%
    group_by(.E) %>%
    summarise(
      n = n(),
      mean = mean(.X, na.rm = TRUE),
      sd = ifelse(n()>1, sd(.X, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  
  mean_high <- means_tbl %>% filter(.E == 0) %>% pull(mean) %>% { if(length(.)==0) NA_real_ else . } ##
  mean_low  <- means_tbl %>% filter(.E == 1) %>% pull(mean) %>% { if(length(.)==0) NA_real_ else . } ##
  n_high <- means_tbl %>% filter(.E == 0) %>% pull(n) %>% { if(length(.)==0) 0L else . }
  n_low  <- means_tbl %>% filter(.E == 1) %>% pull(n) %>% { if(length(.)==0) 0L else . }
  
  # if either group has <2 obs, t.test will warn/produce NA; still try to run but handle errors
  tt <- tryCatch(
    t.test(.X ~ .E, data = d, var.equal = TRUE),
    error = function(e) NULL,
    warning = function(w) {
      # still capture t.test output even with warning
      invokeRestart("muffleWarning")
    }
  )
  
  if(is.null(tt)){
    tibble(
      variable = varname, label = label,
      mean_high = mean_high, mean_low = mean_low,
      diff = mean_high - mean_low, n_high = n_high, n_low = n_low,
      t_statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_
    )
  } else {
    tt_tidy <- broom::tidy(tt)
    tibble(
      variable = varname, label = label,
      mean_high = mean_high, mean_low = mean_low,
      diff = mean_high - mean_low, n_high = n_high, n_low = n_low,
      t_statistic = tt_tidy$statistic, df = tt_tidy$parameter,
      p_value = tt_tidy$p.value, conf_low = tt_tidy$conf.low, conf_high = tt_tidy$conf.high
    )
  }
}

# Run tests
results <- purrr::map2_dfr(names(vars), vars, ~test_one_var(df, .x, .y))

# add formatted columns
results <- results %>%
  mutate(
    star = purrr::map_chr(p_value, sig_stars),
    mean_high_r = ifelse(is.na(mean_high), NA, round(mean_high, 2)),
    mean_low_r = ifelse(is.na(mean_low), NA, round(mean_low, 2)),
    diff_r = ifelse(is.na(diff), NA, round(diff, 2)),
    conf_low_r = ifelse(is.na(conf_low), NA, round(conf_low, 2)),
    conf_high_r = ifelse(is.na(conf_high), NA, round(conf_high, 2))
  )

# Print a table 

table_df <- results %>%
  dplyr::select(label, mean_high_r, mean_low_r, diff_r, p_value, star) %>%
  dplyr::rename(
    Variable = label,
    Mean_High = mean_high_r,
    Mean_Low  = mean_low_r,
    Difference = diff_r,
    P_value = p_value,
    Signif = star
  ) %>%
  dplyr::mutate(
    Difference = ifelse(
      is.na(Difference),
      NA_character_,
      paste0(sprintf("%.2f", Difference), Signif)
    )
  ) %>%
  dplyr::select(-Signif)

# show table
kable(table_df, caption = "High vs Low Efficacy comparisons") %>%
  kable_styling(full_width = FALSE)

# print numeric results for debugging
print(results)

### Main Text Table 2 - Logistic Regression Models - FNE as DV ###

df <- HungarianSurvey_fakenews_d  

# Make sure EfficacyDummy exists and is 0/1
if(!"EfficacyDummy" %in% names(df)) stop("EfficacyDummy not found in data.")
df <- df %>%
  mutate(
    EfficacyDummy = dplyr::case_when(
      EfficacyDummy %in% c(1, "1", "High", "high", "HIGH", TRUE) ~ 1,
      EfficacyDummy %in% c(0, "0", "Low", "low", "LOW", FALSE)   ~ 0,
      TRUE ~ NA_real_
    )
  )

# Check for politicalloyal variable name
# Some datasets use 'party_close' or 'politicalloyal' — pick whichever exists.
if("politicalloyal" %in% names(df)) {
  pol_name <- "politicalloyal"
} else if("party_close" %in% names(df)) {
  pol_name <- "party_close"
} else {
  # if neither exists, create a placeholder and warn (or stop)
  stop("Neither 'politicalloyal' nor 'party_close' found in data. Please rename or supply the variable.")
}

# For safety, coerce key vars to numeric where appropriate
coerce_if_possible <- function(x) {
  if(is.numeric(x)) return(x)
  if(is.logical(x)) return(as.numeric(x))
  # if factor/char looks numeric, coerce; else leave as-is (so model will error if inappropriate)
  x_char <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_char))
  if(!all(is.na(x_num))) return(x_num)
  return(x) # leave
}

vars_to_coerce <- c("gender", "agegroup", "education_hu", "householdincome",
                    "turnout2018par", "fakenews_ubiquity", "ccaware", pol_name, "democ_sat")
for(v in intersect(vars_to_coerce, names(df))) {
  df[[v]] <- coerce_if_possible(df[[v]])
}

# Basic missing-value note (models will use na.omit by default)
sapply(df[c("EfficacyDummy", intersect(vars_to_coerce, names(df)))], function(x) sum(is.na(x)))

# ====== Model formulas (incremental additions) ======
# Basic demographics (Model 1)
f1 <- as.formula("EfficacyDummy ~ gender + agegroup + education_hu + householdincome")

# Add turnout (Model 2)
f2 <- update(f1, . ~ . + turnout2018par)

# Add constitutional court awareness (Model 3)
f3 <- update(f2, . ~ . + ccaware)

# Add political loyalty (Model 5)  -- uses pol_name determined above
# we build this formula programmatically to insert the correct name
f4 <- as.formula(paste0(". ~ . + ", pol_name))
f4 <- update(f3, f4)

# Add democracy satisfaction (Model 5)
f5 <- update(f4, . ~ . + democ_sat)

# ====== Fit logistic models (GLM binomial) ======
# use na.action = na.omit so each model drops rows with missing values in its covariates
m1 <- glm(f1, data = df, family = binomial(link = "logit"), na.action = na.omit)
m2 <- glm(f2, data = df, family = binomial(link = "logit"), na.action = na.omit)
m3 <- glm(f3, data = df, family = binomial(link = "logit"), na.action = na.omit)
m4 <- glm(as.formula(paste0("EfficacyDummy ~ gender + agegroup + education_hu + householdincome + turnout2018par + ccaware + ", pol_name)),
          data = df, family = binomial(link = "logit"), na.action = na.omit)
m5 <- glm(f5, data = df, family = binomial(link = "logit"), na.action = na.omit)

# ====== Create a tidy table of Odds Ratios (OR) with CIs for display ======
# We'll use modelsummary to print exponentiated coefficients (OR) and CI.
# Provide a named vector to map raw term names to pretty labels (exact labels you provided)
coef_map <- c(
  "gender" = "Gender (0=male,1=female)",
  "agegroup" = "Age cohort (1=youngest..4=oldest)",
  "education_hu" = "Level of education (1-10 lowest->highest)",
  "householdincome" = "Gross household income (1-17 lowest->highest)",
  "turnout2018par" = "2018 electoral turnout (1=voted,0=abstained)",
  "ccaware" = "Awareness of Constitutional Court (1-4 least->most aware)",
  pol_name = "Political loyalty (1=party preference,0=no pref)",
  "democ_sat" = "Satisfaction with Hungarian democracy (1-4 least->most satisfied)"
)

# Model list in order (m1..m6)
models <- list("Model 1" = m1, "Model 2" = m2, "Model 3" = m3,
               "Model 4" = m4, "Model 5" = m5)

# Tidy options: show OR rounded to 2 decimals and CI in parentheses
fmt <- function(x) formatC(x, format = "f", digits = 2)

# Make table: Odds Ratios with 95% CI and stars
msummary(models,
         estimate = "{estimate} [{conf.low}, {conf.high}]",
         statistic = "p.value",
         exponentiate = TRUE,
         coef_map = coef_map,
         stars = TRUE,
         digits = 2,
         gof_omit = ".*",           
         title = "Logistic regressions: Odds Ratios (95% CI)",
         output = "markdown") %>%  # change to "latex" or "html" or "docx" as needed
  print()

##code for latex##

outfile <- "test_logit_table_standalone.tex"

# Ensure models m1..m6 exist
model_names <- c("m1","m2","m3","m4","m5")
missing_models <- setdiff(model_names, ls())
if(length(missing_models)) stop("Missing models in environment: ", paste(missing_models, collapse = ", "))

models <- mget(model_names)

# Map term -> left label (LaTeX friendly)
coef_map <- c(
  gender = "Gender (0=male,1=female)",
  agegroup = "Age cohort (1=youngest..4=oldest)",
  education_hu = "Level of education (1--10 lowest \\rightarrow highest)",
  householdincome = "Gross household income (1--17 lowest \\rightarrow highest)",
  turnout2018par = "2018 electoral turnout (1=voted,0=abstained)",
  ccaware = "Awareness of Constitutional Court (1--4 least \\rightarrow most aware)",
  politicalloyal = "Political loyalty (1=party preference,0=no pref)",
  party_close = "Political loyalty (1=party preference,0=no pref)", # fallback
  democ_sat = "Satisfaction with Hungarian democracy (1--4 least \\rightarrow most)"
)

display_order <- c("gender","agegroup","education_hu","householdincome",
                   "turnout2018par","ccaware","politicalloyal","democ_sat")

# ---- escape_label ----

escape_label <- function(x){
  if(is.na(x) || x == "") return("")
  temp <- stringi::stri_replace_all_regex(x, "(\\\\[A-Za-z]+)", "<<SAFE:$1>>")
  specials <- c("$", "%", "#", "&", "_", "{", "}", "~", "^")
  repl     <- c("\\\\$", "\\\\%", "\\\\#", "\\\\&", "\\\\_", "\\\\{", "\\\\}", "\\\\textasciitilde{}", "\\\\textasciicircum{}")
  for(i in seq_along(specials)) {
    temp <- stringi::stri_replace_all_fixed(temp, specials[i], repl[i], vectorize_all = FALSE)
  }
  temp <- stringi::stri_replace_all_regex(temp, "<<SAFE:(\\\\[A-Za-z]+)>>", "$1")
  temp
}

# star label (LaTeX)
star_label <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("\\textsuperscript{***}")
  if (p < 0.05) return("\\textsuperscript{**}")
  if (p < 0.10) return("\\textsuperscript{*}")
  return("")
}

# Get tidy results (exponentiated)
tidy_models <- lapply(models, function(m) {
  td <- tryCatch(broom::tidy(m, exponentiate = TRUE, conf.int = TRUE),
                 error = function(e) broom::tidy(m, exponentiate = TRUE))
  td$term <- as.character(td$term)
  td
})

# Helper to produce two-line cell: OR on first line, CI on second line
cell_two_line <- function(term_key, tidy_df) {
  row <- tidy_df %>% filter(term == term_key)
  if(nrow(row) == 0) return("")  # empty cell
  or <- row$estimate[1]
  lo <- if("conf.low" %in% names(row)) row$conf.low[1] else NA_real_
  hi <- if("conf.high" %in% names(row)) row$conf.high[1] else NA_real_
  p  <- if("p.value" %in% names(row)) row$p.value[1] else NA_real_
  or_s <- sprintf("%.2f", or)
  ci_s <- if(!is.na(lo) && !is.na(hi)) sprintf("(%.2f, %.2f)", lo, hi) else ""
  stars <- star_label(p)
  if(ci_s == "") {
    paste0(or_s, stars)
  } else {
    paste0("\\makecell{", or_s, stars, " \\\\ ", ci_s, "}")
  }
}

# Build rows
rows <- lapply(display_order, function(key) {
  keys_try <- if(key == "politicalloyal") c("politicalloyal","party_close") else key
  cells <- sapply(tidy_models, function(td) {
    # pick first matching key
    found <- ""
    for(k in keys_try) {
      tmp <- cell_two_line(k, td)
      if(tmp != "") { found <- tmp; break }
    }
    found
  }, USE.NAMES = FALSE)
  list(key = key, label = coef_map[[key]], cells = cells)
})

# nobs
nobs <- sapply(models, function(m) {
  g <- tryCatch(broom::glance(m)$nobs, error = function(e) NA_integer_)
  if(is.null(g) || is.na(g)) g2 <- tryCatch(nrow(model.frame(m)), error = function(e) NA_integer_) else g2 <- g
  as.integer(g2)
})

# Now write a complete LaTeX document
tex <- c()
tex <- c(tex, "\\documentclass[11pt]{article}")
tex <- c(tex, "\\usepackage[utf8]{inputenc}")
tex <- c(tex, "\\usepackage{geometry}")
tex <- c(tex, "\\geometry{margin=1in}")
tex <- c(tex, "\\usepackage{booktabs}")
tex <- c(tex, "\\usepackage{tabularx}")
tex <- c(tex, "\\usepackage{threeparttable}")
tex <- c(tex, "\\usepackage{makecell}")
tex <- c(tex, "\\usepackage{caption}")
tex <- c(tex, "\\begin{document}")
tex <- c(tex, "\\begin{table}[t]")
tex <- c(tex, "\\centering")
tex <- c(tex, "\\small")
tex <- c(tex, "\\setlength{\\tabcolsep}{8pt}")
tex <- c(tex, "\\begin{threeparttable}")
tex <- c(tex, "\\caption{Logistic regressions: Predictors of High Fake-News Efficacy (Odds Ratios)}")
tex <- c(tex, "\\label{tab:logit_or}")
# slightly wider model columns
tex <- c(tex, "\\begin{tabularx}{\\textwidth}{@{}X *{6}{>{\\centering\\arraybackslash}p{2.2cm}}@{}}")
tex <- c(tex, "\\toprule")
tex <- c(tex, "\\multicolumn{1}{l}{} & \\multicolumn{6}{c}{\\textbf{Models}} \\\\")
tex <- c(tex, "\\cmidrule(lr){2-7}")
tex <- c(tex, "\\multicolumn{1}{l}{\\textbf{Effect of independent variable}} & \\textbf{Model 1} & \\textbf{Model 2} & \\textbf{Model 3} & \\textbf{Model 4} & \\textbf{Model 5} & \\textbf{Model 6} \\\\")
tex <- c(tex, "\\midrule")

for(r in rows) {
  left <- escape_label(r$label)
  left_cell <- paste0("\\makecell[l]{", left, "}")
  filled <- r$cells
  if(length(filled) < 6) filled <- c(filled, rep("", 6 - length(filled)))
  right_line <- paste0(sapply(filled, function(x) if(x=="" ) "" else x), collapse = " & ")
  tex <- c(tex, paste0(left_cell, " & ", right_line, " \\\\ [6pt]"))
}

tex <- c(tex, "\\midrule")
tex <- c(tex, paste0("\\multicolumn{1}{l}{\\textbf{N}} & \\multicolumn{1}{c}{", paste(nobs, collapse = "} & \\multicolumn{1}{c}{"), "} \\\\"))
tex <- c(tex, "\\bottomrule")
tex <- c(tex, "\\end{tabularx}")
tex <- c(tex, "\\begin{tablenotes}")
tex <- c(tex, "\\footnotesize")
tex <- c(tex, "\\item \\textit{Notes.} Odds ratios; 95\\% confidence intervals in parentheses (on second line). Stars: $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$, $^{\\dagger}p<0.1$.")
tex <- c(tex, "\\end{tablenotes}")
tex <- c(tex, "\\end{threeparttable}")
tex <- c(tex, "\\end{table}")
tex <- c(tex, "\\end{document}")

writeLines(tex, con = outfile)
message("Standalone LaTeX file written to: ", normalizePath(outfile))
message("Compile this file directly (pdflatex/xelatex) or upload to Overleaf and compile.")

##### Main Text Figure 5 - FNE and Confidence in Institutions (pooled results) ##### 

### visualization for confidence in Fidesz ### 

mod_6 <- fixest::feglm(
  confidence3_frac ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

EfficacyConf_Fidesz <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +   scale_y_continuous(limits = c(-0.1, 0.1), labels = label_number(accuracy = 0.05)) +
  theme_classic() +
  labs(x = "", y = "Confidence in Fidesz", title = "", subtitle = "")

print(EfficacyConf_Fidesz)

### visualization for confidence in United Opposition ### 

mod_6 <- fixest::feglm(
  confidence4_frac ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

EfficacyConf_UnitedOpposition <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +   scale_y_continuous(limits = c(-0.1, 0.1), labels = label_number(accuracy = 0.05)) +
  theme_classic() +
  labs(x = "", y = "Confidence in the United Opposition", title = "", subtitle = "")

print(EfficacyConf_UnitedOpposition)

### visualization for confidence in the national government ### 

mod_6 <- fixest::feglm(
  confidence6_frac ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

EfficacyConf_NationalGovernment <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +   scale_y_continuous(limits = c(-0.1, 0.1), labels = label_number(accuracy = 0.05)) +
  theme_classic() +
  labs(x = "", y = "Confidence in the National Government", title = "", subtitle = "")

print(EfficacyConf_NationalGovernment)

### visualization for confidence in the media ### 

mod_6 <- fixest::feglm(
  confidence10_frac ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

EfficacyConf_Media <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +   scale_y_continuous(limits = c(-0.1, 0.1), labels = label_number(accuracy = 0.05)) +
  theme_classic() +
  labs(x = "", y = "Confidence in the Media", title = "", subtitle = "")

print(EfficacyConf_Media)

### final faceted plot ###

plots_to_use <- list(
  EfficacyConf_Fidesz,
  EfficacyConf_UnitedOpposition,
  EfficacyConf_NationalGovernment,
  EfficacyConf_Media
)

# Safety check
stopifnot(all(sapply(plots_to_use, inherits, "gg")))

# ---- Remove legends and harmonize theme ----
shared_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.position = "none"
  )

plots_clean <- lapply(plots_to_use, function(p) p + shared_theme)

# ---- Empty placeholders to complete 3×3 grid ----
empty <- ggplot() + theme_void()

# ---- Build strict 3×3 grid (all panels identical size) ----
panel_grid <- cowplot::plot_grid(
  plots_clean[[1]], plots_clean[[2]],
  plots_clean[[3]], plots_clean[[4]], 
  ncol = 2,
  #labels = c("A","B","C","D","E","F","G","",""),
  label_size = 14,
  align = "hv"
)

# ---- Title, subtitle, caption ----
title <- ggdraw() +
  draw_label(
    "Fake News Efficacy (FNE) and Confidence in Institutions",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Pooled comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Estimates from feglm models. Pooled comparisons. All panels share identical scales.",
    size = 8, x = 0, hjust = 0
  )

final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

print(final_plot)

##### Main Text Figure 6 - FNE and Confidence in Institutions (w/ party split) #####

### visualization for confidence in Fidesz ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence3_frac~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

EfficacyConf_Fidesz <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in Fidesz",
    title = "",
    subtitle = ""
  )

print(EfficacyConf_Fidesz)

### visualization for confidence in United Opposition ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence4_frac~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

EfficacyConf_UnitedOpposition <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in the United Opposition",
    title = "",
    subtitle = ""
  )

print(EfficacyConf_UnitedOpposition)

### visualization for confidence in the national government ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence6_frac~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

EfficacyConf_NationalGovernment <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in the National Government",
    title = "",
    subtitle = ""
  )

print(EfficacyConf_NationalGovernment)

### visualization for confidence in the media ### 

#With interaction logit
mod_6 <- fixest::feglm(confidence10_frac~EfficacyDummy*Party + education_hu +ccaware +democ_sat+ politicalloyal +
                         householdincome + agegroup + gender,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

EfficacyConf_Media <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Confidence in the Media",
    title = "",
    subtitle = ""
  )

print(EfficacyConf_Media)

### final faceted plot ### 

plots_to_use <- list(
  EfficacyConf_Fidesz,
  EfficacyConf_UnitedOpposition,
  EfficacyConf_NationalGovernment,
  EfficacyConf_Media
)

# Safety check
stopifnot(all(sapply(plots_to_use, inherits, "gg")))

# ---- Remove legends and harmonize theme ----
shared_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.position = "none"
  )

plots_clean <- lapply(plots_to_use, function(p) p + shared_theme)

# ---- Empty placeholders to complete 3×3 grid ----
empty <- ggplot() + theme_void()

# ---- Build strict 3×3 grid (all panels identical size) ----
panel_grid <- cowplot::plot_grid(
  plots_clean[[1]], plots_clean[[2]],
  plots_clean[[3]], plots_clean[[4]], 
  ncol = 2,
  #labels = c("A","B","C","D","E","F","G","",""),
  label_size = 14,
  align = "hv"
)

# ---- Title, subtitle, caption ----
title <- ggdraw() +
  draw_label(
    "Fake News Efficacy (FNE) and Confidence in Institutions",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Within-party comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Estimates from feglm models. Pairwise comparisons by party. All panels share identical scales.",
    size = 8, x = 0, hjust = 0
  )

final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

print(final_plot)

##### Main Text Figure 7 - FNE and Electoral Engagement (pooled results) #####

### visualization for volunteer ## 

# 1) Fit model without the interaction

mod_6 <- fixest::feglm(volunteer~EfficacyDummy + Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

# single comparisons() call at 95% (authoritative)
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     conf_level = 0.95)

# make a single data.frame and compute 90% CI from its SE so they align
ph_df <- as.data.frame(ph_95)

# --- Minimal diagnostic + plotting replacement (robust to CI column names) ---
# ph_df must already exist (as.data.frame(ph_95))
print("ph_df column names:")
print(names(ph_df))
print("ph_df head:")
print(head(ph_df))

# Robustly pick the first contrast row to plot
ph_row <- ph_df[1, , drop = FALSE]

# Find estimate column
est_col <- if ("estimate" %in% names(ph_row)) "estimate" else
  if ("Estimate" %in% names(ph_row)) "Estimate" else
    stop("No 'estimate' column found in ph_df; names(ph_df) printed above.")

# Find 95% CI columns (many possible names)
ci95_lo_name <- if ("conf.low" %in% names(ph_row)) "conf.low" else
  if ("lower.CL" %in% names(ph_row)) "lower.CL" else
    if ("conf_low" %in% names(ph_row)) "conf_low" else
      if ("lower" %in% names(ph_row)) "lower" else
        NA_character_
ci95_hi_name <- if ("conf.high" %in% names(ph_row)) "conf.high" else
  if ("upper.CL" %in% names(ph_row)) "upper.CL" else
    if ("conf_high" %in% names(ph_row)) "conf_high" else
      if ("upper" %in% names(ph_row)) "upper" else
        NA_character_

# Find SE column if present
se_col <- if ("std.error" %in% names(ph_row)) "std.error" else
  if ("SE" %in% names(ph_row)) "SE" else
    if ("se" %in% names(ph_row)) "se" else
      NA_character_

# If 95% CI present use it, else compute from SE (and if SE missing attempt compute from other CI)
if (!is.na(ci95_lo_name) & !is.na(ci95_hi_name)) {
  ci95_low <- as.numeric(ph_row[[ci95_lo_name]])
  ci95_high <- as.numeric(ph_row[[ci95_hi_name]])
} else if (!is.na(se_col)) {
  z95 <- 1.96
  est_val <- as.numeric(ph_row[[est_col]])
  se_val <- as.numeric(ph_row[[se_col]])
  ci95_low <- est_val - z95 * se_val
  ci95_high <- est_val + z95 * se_val
} else {
  stop("No 95% CI or SE found in ph_df; printed names above — please paste them if you need help.")
}

# Compute 90% CI from same SE if available; otherwise approximate from 95% CI
if (!is.na(se_col)) {
  z90 <- 1.645
  se_val <- as.numeric(ph_row[[se_col]])
  ci90_low  <- as.numeric(ph_row[[est_col]]) - z90 * se_val
  ci90_high <- as.numeric(ph_row[[est_col]]) + z90 * se_val
} else {
  # approximate 90% by shrinking 95% interval proportionally
  ci90_low <- (ci95_low + ci95_high)/2 - 1.645/1.96 * ( (ci95_high - ci95_low)/2 )
  ci90_high <- (ci95_low + ci95_high)/2 + 1.645/1.96 * ( (ci95_high - ci95_low)/2 )
}

# Numeric print so you can verify values
cat("\nFinal numbers to plot (response scale if you used type='response'):\n")
cat("estimate =", ph_row[[est_col]], "\n")
cat("90% CI = [", ci90_low, ",", ci90_high, "]\n")
cat("95% CI = [", ci95_low, ",", ci95_high, "]\n\n")

# Build a tiny plotting df with explicit numeric columns
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row[[est_col]]),
  ci90_low = ci90_low,
  ci90_high = ci90_high,
  ci95_low = ci95_low,
  ci95_high = ci95_high
)

# Plot using your original style but DO NOT set y-limits (prevents clipping)
Efficacy_CampaignVolunteer <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +   # 90% (thicker)
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +  # 95% (thinner)
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +    scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Willingness to Volunteer for Campaign", title = "", subtitle = "")

Efficacy_CampaignVolunteer

### visualization for demonstration ## 

# fit model for rally 
mod_6 <- fixest::feglm(
  rally ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",    # recommended for readable scale; drop if you want log-odds
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# then the same robust plotting block you used (uses ph_df freshly created)
# (diagnostic prints help confirm numbers)
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]
# ... compute ci90/ci95 as you already do (or use the robust snippet you had) ...
# minimal plotting (keeps your original aesthetic)
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

Efficacy_RallyProtest <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +    scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Willingness to Attend Campaign Rally or Protest", title = "", subtitle = "")

print(Efficacy_RallyProtest)

###visualization for follow campaign### 

# fit model for campaignfollow
mod_6 <- fixest::feglm(
  campaignfollow ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as in your voteintent block
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

Efficacy_CampaignFollow <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +    scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Willingness to Follow Electoral Campaign", title = "", subtitle = "")

print(Efficacy_CampaignFollow)

###visualization for intention to vote### 

# fit model for voteintent
mod_6 <- fixest::feglm(
  voteintent ~ EfficacyDummy + Party + education_hu + ccaware + democ_sat + politicalloyal +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# **CRITICAL**: compute comparisons for THIS model and convert to dataframe
ph_95 <- comparisons(mod_6,
                     variables = list(EfficacyDummy = "pairwise"),
                     type = "response",
                     conf_level = 0.95)
ph_df <- as.data.frame(ph_95)

# diagnostics
print(names(ph_df)); print(head(ph_df))
ph_row <- ph_df[1, , drop = FALSE]

# build plotting df exactly as above
plot_df <- data.frame(
  x = 1,
  estimate = as.numeric(ph_row$estimate),
  ci90_low = as.numeric(ph_row$estimate) - 1.645 * as.numeric(ph_row$std.error),
  ci90_high = as.numeric(ph_row$estimate) + 1.645 * as.numeric(ph_row$std.error),
  ci95_low = as.numeric(ph_row$conf.low),
  ci95_high = as.numeric(ph_row$conf.high)
)

Efficacy_Vote <- ggplot(plot_df, aes(x = x, y = estimate)) +
  geom_linerange(aes(x = x, ymin = ci90_low, ymax = ci90_high), size = 1.2) +
  geom_linerange(aes(x = x, ymin = ci95_low, ymax = ci95_high), size = 0.35) +
  geom_point(aes(x = x, y = estimate), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1, labels = "FNE: High-Low") +   scale_y_continuous(limits = c(-0.05, 0.05), labels = label_number(accuracy = 0.025)) +
  theme_classic() +
  labs(x = "", y = "Intent to Vote", title = "", subtitle = "")

print(Efficacy_Vote)

### final faceted plot ###

# Remove legends (recommended for coefficient plots)
p1 <- Efficacy_CampaignVolunteer + theme(legend.position = "none")
p2 <- Efficacy_RallyProtest     + theme(legend.position = "none")
p3 <- Efficacy_CampaignFollow     + theme(legend.position = "none")
p4 <- Efficacy_Vote   + theme(legend.position = "none")

# Empty placeholder to keep grid structure
empty <- ggplot() + theme_void()

# Build a strict 2 x 3 grid (all panels same size)
panel_grid <- cowplot::plot_grid(
  p1, p2, 
  p3, p4,
  ncol = 2,
  align = "hv"
)

# Title, subtitle, caption
title <- ggdraw() +
  draw_label(
    "Fake News Efficacy (FNE) and Electoral Engagement",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Pooled comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Pooled comparisons from feglm models. CIs: 90% & 95%.",
    size = 8, x = 0, hjust = 0
  )

# Stack everything vertically
final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

print(final_plot)

##### Main Text Figure 8 - FNE and Electoral Engagement (w/ party split) ##### 

###visualization for volunteering### 

#With interaction logit
mod_6 <- fixest::feglm(volunteer~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Efficacy_CampaignVolunteer <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Willingness to Volunteer for Campaign",
    title = "",
    subtitle = ""
  )

print(Efficacy_CampaignVolunteer)

###visualization for demonstrations## 

#With interaction logit
mod_6 <- fixest::feglm(rally~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Efficacy_RallyProtest <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Willingness to Attend Campaign Rally or Protest",
    title = "",
    subtitle = ""
  )

print(Efficacy_RallyProtest)

###visualization for follow campaign### 

#With interaction logit
mod_6 <- fixest::feglm(campaignfollow~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party
# Compute comparisons with both 95% and 90% CIs
ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Efficacy_CampaignFollow <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Willingness to Follow Electoral Campaign",
    title = "",
    subtitle = ""
  )

print(Efficacy_CampaignFollow)

###visualization for intention to vote### 

#With interaction logit
mod_6 <- fixest::feglm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal +
                         householdincome + agegroup + gender + turnout2018par,
                       vcov = "hetero",
                       family = quasibinomial(link = "logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Comparing within party

ph_party_95 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.95)

ph_party_90 <- comparisons(mod_6,
                           variables = list(EfficacyDummy = "pairwise"),
                           newdata = datagrid(Party = c("Fidesz", "Opposition", "Neither")),
                           conf_level = 0.90)

Efficacy_Vote <- ggplot(ph_party_95, aes(x = Party, y = estimate)) +
  # 90% CI: thicker line
  geom_linerange(data = ph_party_90, aes(ymin = conf.low, ymax = conf.high), size = 1.2) +
  # 95% CI: thinner line
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-0.3, 0.3), labels = label_number(accuracy = 0.1)) +
  theme_classic() +
  labs(
    x = "Within Party Comparison",
    y = "Intent to Vote",
    title = "",
    subtitle = ""
  )

print(Efficacy_Vote)

### final faceted plot ### 

# Remove legends (recommended for coefficient plots)
p1 <- Efficacy_CampaignVolunteer + theme(legend.position = "none")
p2 <- Efficacy_RallyProtest     + theme(legend.position = "none")
p3 <- Efficacy_CampaignFollow     + theme(legend.position = "none")
p4 <- Efficacy_Vote   + theme(legend.position = "none")

# Empty placeholder to keep grid structure
empty <- ggplot() + theme_void()

# Build a strict 2 x 3 grid (all panels same size)
panel_grid <- cowplot::plot_grid(
  p1, p2, 
  p3, p4, 
  ncol = 2,
  align = "hv"
)

# Title, subtitle, caption
title <- ggdraw() +
  draw_label(
    "Fake News Efficacy (FNE) and Electoral Engagement",
    fontface = "bold", size = 14, x = 0.5, hjust = 0.5
  )

subtitle <- ggdraw() +
  draw_label(
    "Within-party comparisons (90% and 95% CIs)",
    size = 10, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Notes: Pairwise comparisons from feglm models. CIs: 90% & 95%.",
    size = 8, x = 0, hjust = 0
  )

# Stack everything vertically
final_plot <- cowplot::plot_grid(
  title,
  subtitle,
  panel_grid,
  caption,
  ncol = 1,
  rel_heights = c(0.06, 0.04, 1, 0.04)
)

print(final_plot)

##### Appendix D - FNU as a binary variable, confidence in institutions (pooled results) #####

# ---- table dictionary ----
dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty",
  confidence_fidesz = "Confidence in Fidesz"
)

#### Table D.1 - Confidence in Fidesz ####

# ---- OLS (no interaction) ----
mod_1 <- fixest::feols(
  confidence_fidesz ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_fidesz ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# ---- Fractional logit (no interaction) ----
mod_4 <- fixest::feglm(
  confidence_fidesz ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_fidesz ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# ---- models list: only four models (two OLS, two Fractional Logit) ----
models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with Confidence in Fidesz DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

## Figure D. 1 plotting code ##

# 1) get link-scale predictions
pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)   # only two levels
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

# 2) compute symmetric y limits (same logic you used)
absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym

n_breaks <- 8
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = n_breaks)

# 3) plot (single group, not split by Party)
p_no_party_fidesz <- ggplot(
  pred_plot_no_party,
  aes(x = fnu_label, y = estimate)
) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.35,
                  position = position_dodge(width = 0)) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym))
# print or save the plot
print(p_no_party_fidesz)

#### Table D.2 - Confidence in United Opposition ####

# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_united ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_united ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_united ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_united ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with Confidence in United Opposition DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure D. 2 plotting code ###
pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_no_party_united <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym))

print(p_no_party_united)

#### Table D.3 - Confidence in National Government ####

# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_gov ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_gov ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_gov ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_gov ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with with Confidence in the National Government DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure D. 3 plotting code ###
pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_no_party_gov <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym))

print(p_no_party_gov)

#### Table D.4 - Confidence in Media ####

# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_media ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_media ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_media ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_media ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with with Confidence in the Media DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure D. 4 plotting code ###
pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_no_party_media <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym))

print(p_no_party_media)

##### Appendix E - FNU as a continuous variable, confidence in institutions (pooled results) #####

dict <- c(
  fakenews_ubiquity = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty",
  confidence_fidesz = "Confidence in Fidesz"
)

# --------------------------
# Table E.1 - Confidence in Fidesz
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_fidesz ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_fidesz ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_fidesz ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_fidesz ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# ensure DV label present (already included for confidence_fidesz in dict)
models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Confidence in Fidesz DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure E. 1 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_g1_fidesz <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none") 

print(p_g1_fidesz)

# --------------------------
# Table E.2 - Confidence in United Opposition
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_united ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_united ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_united ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_united ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Use same base dict + add DV label for this table
dict["confidence_united"] <- "Confidence in United Opposition"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Confidence in United Opposition DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure E. 2 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_g2_united <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_g2_united)

# --------------------------
# Table E.3 - Confidence in National Government
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_gov ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_gov ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_gov ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_gov ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["confidence_gov"] <- "Confidence in the National Government"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Confidence in the National Government DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure E. 3 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_g3_gov <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_g3_gov)

# --------------------------
# Table E.4 - Confidence in the Media
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_media ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_media ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_media ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_media ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["confidence_media"] <- "Confidence in the Media"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Confidence in the Media DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure E. 4 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_g4_media <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_g4_media)

##### Appendix F code - FNU as a binary variable, confidence in institutions (w/ party split) ######

dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty",
  confidence_fidesz = "Confidence in Fidesz"
)

#### Table F.1 - Confidence in Fidesz ####

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_fidesz~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_fidesz~ fnu_dummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_fidesz~ fnu_dummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)


#No interaction binary Logit
mod_4<- fixest::feglm(confidence_fidesz~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_fidesz~ fnu_dummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_fidesz~ fnu_dummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)

modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary with Confidence in Fidesz DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure F. 1 plotting code ###

# 1) get link-scale predictions
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz", "Opposition", "Neither"),
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  # be defensive about types
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  )

# 2) compute a symmetric y limit based on the largest absolute value in the preds
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)

# add small padding and round up to 1 decimal for clean tick values
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym

# optional: create sensible breaks (you can adjust n_breaks)
n_breaks <- 8
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = n_breaks)

# 3) plot on the logit (link) scale with dynamic symmetric framing
p2_interaction_fidesz <- ggplot(
  pred_plot_interaction,
  aes(x = factor(fnu_dummy), y = estimate, shape = Party, group = Party)
) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks,  # tick placement
                     name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +   # dynamic symmetric frame
  labs(x = "FNU")

p2_interaction_fidesz

#### Table F.2 - Confidence in United Opposition ####

mod_1 <- fixest::feols(confidence_united~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_united~ fnu_dummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_united~ fnu_dummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_united~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_united~ fnu_dummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_united~ fnu_dummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary with Confidence in United Opposition DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure F. 2 plotting code ###

# 1) get link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz", "Opposition", "Neither"),
    fnu_dummy = c(0, 1)
  ),
  type = "link"   # <<-- logit (link) scale
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz", "Opposition", "Neither")),
    fnu_dummy = factor(as.character(fnu_dummy), levels = c("0", "1"))
  )

# 2) compute dynamic symmetric limits (based on the largest absolute value)
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2                              # small visual padding
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# 3) plot on logit scale with dynamic symmetric framing
p2_interaction_united <- ggplot(pred_plot_interaction,
                                aes(x = factor(fnu_dummy),
                                    y = estimate,
                                    shape = Party,
                                    group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +   # dynamic symmetric frame, does not drop data
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# Print the plot
p2_interaction_united


#### Table F.3 - Confidence in National Government ####

mod_1 <- fixest::feols(confidence_gov~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_gov~ fnu_dummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_gov~ fnu_dummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_gov~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_gov~ fnu_dummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_gov~ fnu_dummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary with Confidence in the National Government DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure F. 3 plotting code ###

# 1) get link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz", "Opposition", "Neither"),
    fnu_dummy = c(0, 1)
  ),
  type = "link"   # logit (link) scale
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz", "Opposition", "Neither")),
    fnu_dummy = factor(as.character(fnu_dummy), levels = c("0", "1"))
  )

# 2) dynamic symmetric y-limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# 3) plot on logit scale with symmetric framing
p2_interaction_gov <- ggplot(pred_plot_interaction,
                             aes(x = factor(fnu_dummy),
                                 y = estimate,
                                 shape = Party,
                                 group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +   # dynamic symmetric frame (no data dropped)
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_interaction_gov

#### Table F.4 - Confidence in Media ####

mod_1 <- fixest::feols(confidence_media~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_media~ fnu_dummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_media~ fnu_dummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_media~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_media~ fnu_dummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_media~ fnu_dummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary with Confidence in the Media DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure F. 4 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fnu_dummy = c(0,1)
  ),
  type = "link"
)

# Ensure numeric + stable factor ordering
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    fnu_dummy = factor(as.character(fnu_dummy),
                       levels = c("0","1"))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_interaction_media <- ggplot(
  pred_plot_interaction,
  aes(y = estimate,
      x = factor(fnu_dummy),
      shape = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_x_discrete(labels = c("0" ="Low",
                              "1" ="High")) +
  scale_y_continuous(
    breaks = y_breaks,
    name = "Confidence in the Media (Log-Odds)"
  ) +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x ="FNU")

p2_interaction_media

##### Appendix G code - FNU as a continous variable, confidence in institutions (w/ party split) ######

dict <- c(
  fakenews_ubiquity = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty",
  confidence_fidesz = "Confidence in Fidesz"
)

### Table G.1 - Confidence in Fidesz ### 

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_fidesz~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_fidesz~fakenews_ubiquity +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_fidesz~fakenews_ubiquity*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_fidesz~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_fidesz~fakenews_ubiquity +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_fidesz~fakenews_ubiquity*Party + education_hu +ccaware+politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Confidence in Fidesz DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure G. 1 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Clean + enforce ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither"))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_fidesz_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = factor(fakenews_ubiquity),
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(
    breaks = y_breaks,
    name = "Confidence in Fidesz (Log-Odds)"
  ) +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x = "FNU")

p2_fidesz_continuous

### Table G.2 - Confidence in United Opposition ### 

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_united~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_united~fakenews_ubiquity +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_united~fakenews_ubiquity*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_united~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_united~fakenews_ubiquity +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_united~fakenews_ubiquity*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Confidence in United Opposition DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure G. 2 plotting code ###

# link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# ensure numeric + stable ordering (use explicit namespace for dplyr)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity, levels = as.character(seq(1,5,1)))
  )

# dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_rally_united <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x = "FNU")

p2_rally_united

### Table G.3 - Confidence in National Government ### 

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_gov~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_gov~fakenews_ubiquity +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_gov~fakenews_ubiquity*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_gov~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_gov~fakenews_ubiquity +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_gov~fakenews_ubiquity*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Confidence in the National Government DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure G. 3 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + stable ordering (explicit dplyr namespace to avoid masking issues)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits (based on max absolute value in estimates/CI)
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing; no probability axis)
p2_gov_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# Print
p2_gov_continuous

### Table G. 4 - Confidence in the Media ###

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_media~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_media~fakenews_ubiquity +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_media~fakenews_ubiquity*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_media~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_media~fakenews_ubiquity +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_media~fakenews_ubiquity*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Confidence in the Media DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure G. 4 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + stable ordering (explicit dplyr namespace)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits based on the largest absolute value among estimates and CI bounds
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing; color palette preserved)
p2_media_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_media_continuous

##### Appendix H Code - FNU as a binary variable, electoral participation (pooled results) ##### 

# Shared dict base
dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty"
)

# --------------------------
# Table H.1 - Volunteer for Campaign
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  volunteer ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  volunteer ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  volunteer ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  volunteer ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict <- dict
dict["volunteer"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with Volunteer for Campaign DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure H. 1 plotting code ###

pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_j1_volunteer <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_j1_volunteer)

# --------------------------
# Table H.2 - Attend Campaign Rally or Protest
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  rally ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  rally ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  rally ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  rally ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict <- dict
dict["rally"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with Attend Campaign Rally or Protest DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure H. 2 plotting code ###

pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_j2_rally <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_j2_rally)

# --------------------------
# Table H.3 - Follow Electoral Campaign
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  campaignfollow ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  campaignfollow ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  campaignfollow ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  campaignfollow ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict <- dict
dict["campaignfollow"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with Follow Electoral Campaign DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure H. 3 plotting code ###

pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_j3_campaignfollow <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Follow Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_j3_campaignfollow)

# --------------------------
# Table H.4 - Intent to Vote in Election
# --------------------------
# OLS (no interaction)
mod_1 <- fixest::feols(
  voteintent ~ fnu_dummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  voteintent ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  voteintent ~ fnu_dummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  voteintent ~ fnu_dummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict <- dict
dict["voteintent"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as a Binary IV with Intent to Vote in Election DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure H. 4 plotting code ###

pred_plot_no_party <- predictions(
  mod_5,
  newdata = datagrid(
    fnu_dummy = c(0, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fnu_label = factor(fnu_dummy, levels = c(0, 1), labels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot_no_party$estimate,
                    pred_plot_no_party$conf.low,
                    pred_plot_no_party$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 8)

p_j4_voteintent <- ggplot(pred_plot_no_party, aes(x = fnu_label, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNU") +
  scale_y_continuous(breaks = y_breaks, name = "Intend to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_j4_voteintent)

##### Appendix I Code - FNU as a continuous variable, electoral participation (pooled results) ##### 

dict <- c(
  fakenews_ubiquity = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty",
  confidence_fidesz = "Confidence in Fidesz"
)

# --------------------------
# I.1 - Volunteer for Campaign (FNU as ordinal IV, continuous predictor)
# --------------------------

# OLS (no interaction)
mod_1 <- fixest::feols(
  volunteer ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  volunteer ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  volunteer ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  volunteer ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Volunteer for Electoral Campaign DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure I. 1 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_i1_volunteer <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_i1_volunteer)

# --------------------------
# I.2 - Attend Campaign Rally or Protest
# --------------------------

mod_1 <- fixest::feols(
  rally ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  rally ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_4 <- fixest::feglm(
  rally ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  rally ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["rally"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Attend Campaign Rally or Protest DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure I. 2 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_i2_rally <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_i2_rally)

# --------------------------
# I.3 - Follow the Electoral Campaign
# --------------------------

mod_1 <- fixest::feols(
  campaignfollow ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  campaignfollow ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_4 <- fixest::feglm(
  campaignfollow ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  campaignfollow ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["campaignfollow"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Follow Electoral Campaign DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure I. 3 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_i3_campaignfollow <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Follow the Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_i3_campaignfollow)

# --------------------------
# I.4 - Intention to Vote
# --------------------------

mod_1 <- fixest::feols(
  voteintent ~ fakenews_ubiquity,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  voteintent ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_4 <- fixest::feglm(
  voteintent ~ fakenews_ubiquity,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  voteintent ~ fakenews_ubiquity + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["voteintent"] <- "Participation"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNU as Ordinal IV with Intention to Vote DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure I. 4 plotting code ###

pred_plot_continuous <- predictions(
  mod_5,
  newdata = datagrid(
    fakenews_ubiquity = seq(1, 5, 1)
  ),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    fakenews_ubiquity = as.numeric(as.character(fakenews_ubiquity))
  )

absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_i4_voteintent <- ggplot(pred_plot_continuous, aes(x = fakenews_ubiquity, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNU", breaks = seq(1, 5, 1)) +
  scale_y_continuous(breaks = y_breaks, name = "Intent to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_i4_voteintent)

##### Appendix J Code - FNU as a binary variable, electoral participation (w/ party split) ##### 

dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty"
)

#### Table J.1 - Volunteer for Campaign ####

#No interaction binary OLS
mod_1 <- fixest::feols(volunteer~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(volunteer~ fnu_dummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(volunteer~ fnu_dummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)


#No interaction binary Logit
mod_4<- fixest::feglm(volunteer~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(volunteer~ fnu_dummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(volunteer~ fnu_dummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary IV with Volunteer for Campaign DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure J. 1 plotting code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fnu_dummy = c(0, 1)
  ),
  type = "link"
)

# ensure numeric + stable factor ordering (use explicit dplyr namespace to avoid masking)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fnu_dummy = factor(as.character(fnu_dummy), levels = c("0","1"))
  )

# dynamic symmetric limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_interaction_volunteer <- ggplot(pred_plot_interaction,
                                   aes(x = factor(fnu_dummy),
                                       y = estimate,
                                       shape = Party,
                                       group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_interaction_volunteer

#### Table J.2 - Attend Campaign Rally or Protest ####

mod_1 <- fixest::feols(rally~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(rally~ fnu_dummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(rally~ fnu_dummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(rally~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(rally~ fnu_dummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(rally~ fnu_dummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary IV with Attend Campaign Rally or Protest DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure J. 2 plotting code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fnu_dummy = c(0, 1)   # numeric, must match model
  ),
  type = "link"
)

# ensure numeric + stable factor ordering (explicit dplyr namespace)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fnu_dummy = factor(as.character(fnu_dummy), levels = c("0","1"))
  )

# dynamic symmetric limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_interaction_rally <- ggplot(pred_plot_interaction,
                               aes(x = factor(fnu_dummy),
                                   y = estimate,
                                   shape = Party,
                                   group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_interaction_rally

#### Table J.3 - Follow Electoral Campaign ####

mod_1 <- fixest::feols(campaignfollow~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(campaignfollow~ fnu_dummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(campaignfollow~ fnu_dummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(campaignfollow~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(campaignfollow~ fnu_dummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(campaignfollow~ fnu_dummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary IV with Follow Electoral Campaign DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure J. 3 plotting code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fnu_dummy = c(0, 1)
  ),
  type = "link"
)

# ensure numeric + stable factor ordering (explicit dplyr namespace)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fnu_dummy = factor(as.character(fnu_dummy), levels = c("0","1"))
  )

# dynamic symmetric limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_interaction_campaignfollow <- ggplot(pred_plot_interaction,
                                        aes(x = factor(fnu_dummy),
                                            y = estimate,
                                            shape = Party,
                                            group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Follow Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_interaction_campaignfollow

#### Table J.4 - Intent to Vote in Election ####

mod_1 <- fixest::feols(voteintent~ fnu_dummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(voteintent~ fnu_dummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(voteintent~ fnu_dummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(voteintent~ fnu_dummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(voteintent~ fnu_dummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(voteintent~ fnu_dummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Binary IV with Intent to Vote in Election DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure J. 4 plotting code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fnu_dummy = c(0, 1)
  ),
  type = "link"
)

# coerce numerics and fix factor ordering (explicit dplyr namespace to avoid masking)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fnu_dummy = factor(as.character(fnu_dummy), levels = c("0","1"))
  )

# dynamic symmetric y-limits (based on the largest absolute value in estimates/CIs)
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot on logit (link) scale with dynamic symmetric framing
p2_interaction_voteintent <- ggplot(pred_plot_interaction,
                                    aes(x = factor(fnu_dummy),
                                        y = estimate,
                                        shape = Party,
                                        group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  scale_y_continuous(breaks = y_breaks, name = "Intend to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_interaction_voteintent

##### Appendix K Code - FNU as a continous variable, electoral participation (w/ party split) #####

dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty"
)

### Table K.1 - volunteer for a campaign ### 

#No interaction binary OLS
mod_1 <- fixest::feols(volunteer~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(volunteer~fakenews_ubiquity +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(volunteer~fakenews_ubiquity*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(volunteer~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(volunteer~fakenews_ubiquity +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(volunteer~fakenews_ubiquity*Party + education_hu +ccaware+politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Volunteer for Electoral Campaign DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure K. 1 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing)
p2_volunteer_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNU")

p2_volunteer_continuous

### Table K.2 - attend campaign rally or protest ### 

#No interaction binary OLS
mod_1 <- fixest::feols(rally~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(rally~fakenews_ubiquity +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(rally~fakenews_ubiquity*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(rally~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(rally~fakenews_ubiquity +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(rally~fakenews_ubiquity*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Political Participation DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure K. 2 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_rally_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNU")

p2_rally_continuous

### Table K.3 - follow the electoral campaign ### 

#No interaction binary OLS
mod_1 <- fixest::feols(campaignfollow~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(campaignfollow~fakenews_ubiquity +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(campaignfollow~fakenews_ubiquity*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(campaignfollow~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(campaignfollow~fakenews_ubiquity +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(campaignfollow~fakenews_ubiquity*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Political Participation DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure K. 3 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + enforce ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits from data
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing)
p2_campaignfollow_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Follow the Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNU")

# print
p2_campaignfollow_continuous

### Table K. 4 - intention to vote ###

#No interaction binary OLS
mod_1 <- fixest::feols(voteintent~fakenews_ubiquity,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(voteintent~fakenews_ubiquity +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(voteintent~fakenews_ubiquity*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(voteintent~fakenews_ubiquity,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(voteintent~fakenews_ubiquity +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(voteintent~fakenews_ubiquity*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNU as Ordinal IV with Political Participation DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure K. 4 plotting code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    fakenews_ubiquity = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    fakenews_ubiquity = factor(fakenews_ubiquity,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_voteintent_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = fakenews_ubiquity,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Intent to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNU")

p2_voteintent_continuous

### Appendix L - Figure L.1: Marginal Effects Plot for FNE ###

df <- HungarianSurvey_fakenews_d  # your data frame

df <- df %>%
  mutate(
    EfficacyDummy = dplyr::case_when(
      EfficacyDummy %in% c(1, "1", "High", "high", "HIGH", TRUE) ~ 1,
      EfficacyDummy %in% c(0, "0", "Low", "low", "LOW", FALSE)   ~ 0,
      TRUE ~ NA_real_
    )
  )

# Check for politicalloyal variable name: user mentioned 'politicalloyal' earlier.
if("politicalloyal" %in% names(df)) {
  pol_name <- "politicalloyal"
} else if("party_close" %in% names(df)) {
  pol_name <- "party_close"
} else {
  # if neither exists, create a placeholder and warn (or stop)
  stop("Neither 'politicalloyal' nor 'party_close' found in data. Please rename or supply the variable.")
}

# For safety, coerce key vars to numeric where appropriate (do not coerce arbitrary factors)
coerce_if_possible <- function(x) {
  if(is.numeric(x)) return(x)
  if(is.logical(x)) return(as.numeric(x))
  # if factor/char looks numeric, coerce; else leave as-is (so model will error if inappropriate)
  x_char <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_char))
  if(!all(is.na(x_num))) return(x_num)
  return(x) # leave
}

vars_to_coerce <- c("gender", "agegroup", "education_hu", "householdincome",
                    "turnout2018par", "fakenews_ubiquity", "ccaware", pol_name, "democ_sat")
for(v in intersect(vars_to_coerce, names(df))) {
  df[[v]] <- coerce_if_possible(df[[v]])
}

# Basic missing-value note (models will use na.omit by default)
sapply(df[c("EfficacyDummy", intersect(vars_to_coerce, names(df)))], function(x) sum(is.na(x)))

# ====== Model formulas (incremental additions) ======
# Basic demographics (Model 1)
f1 <- as.formula("EfficacyDummy ~ gender + agegroup + education_hu + householdincome")

# Add turnout (Model 2)
f2 <- update(f1, . ~ . + turnout2018par)

# Add constitutional court awareness (Model 3)
f3 <- update(f2, . ~ . + ccaware)

# Add political loyalty (Model 5)  -- uses pol_name determined above
# we build this formula programmatically to insert the correct name
f4 <- as.formula(paste0(". ~ . + ", pol_name))
f4 <- update(f3, f4)

# Add democracy satisfaction (Model 5)
f5 <- update(f4, . ~ . + democ_sat)

# ====== Fit logistic models (GLM binomial) ======
# use na.action = na.omit so each model drops rows with missing values in its covariates
m1 <- glm(f1, data = df, family = binomial(link = "logit"), na.action = na.omit)
m2 <- glm(f2, data = df, family = binomial(link = "logit"), na.action = na.omit)
m3 <- glm(f3, data = df, family = binomial(link = "logit"), na.action = na.omit)
m4 <- m4 <- glm(as.formula(paste0("EfficacyDummy ~ gender + agegroup + education_hu + householdincome + turnout2018par + ccaware + ", pol_name)),
                data = df, family = binomial(link = "logit"), na.action = na.omit)
m5 <- glm(f5, data = df, family = binomial(link = "logit"), na.action = na.omit)

# Put models in a named list
models <- list(
  "M1" = m1,
  "M2" = m2,
  "M3" = m3,
  "M4" = m4,
  "M5" = m5
)

# Compute AMEs for each model
ame_df <- lapply(names(models), function(mname) {
  m <- models[[mname]]
  avg_slopes(m) %>%
    mutate(Model = mname)
}) %>%
  bind_rows()

# Label map (same logic as your regression table)
label_map <- c(
  gender = "Gender",
  agegroup = "Age",
  education_hu = "Education",
  householdincome = "Household Income",
  turnout2018par = "Voted (2018)",
  ccaware = "CC Awareness",
  politicalloyal = "Political Loyalty",
  party_close = "Political Loyalty",
  democ_sat = "Democratic Satisfaction"
)

ame_df_plot <- ame_df %>%
  filter(term %in% names(label_map)) %>%
  mutate(
    Predictor = label_map[term],
    Model = factor(Model, levels = names(models))
  )

predictor_order <- c(
  "Gender",
  "Age",
  "Education",
  "Household Income",
  "Voted (2018)",             
  "CC Awareness",
  "Political Loyalty",
  "Democratic Satisfaction"
)

ame_df_plot <- ame_df_plot %>%
  mutate(
    Predictor = factor(Predictor, levels = predictor_order)
  )

p_standard <- ggplot(ame_df_plot,
                     aes(x = estimate, y = Predictor, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  labs(
    x = "Average marginal effect on high FNE",
    y = NULL,
    title = "Average Marginal Effects Across Logistic Regression Models",
    subtitle = "Bars represent 95% confidence intervals"
  ) +
  theme_classic(base_size = 12) +
  scale_color_brewer(palette = "Dark2")

print(p_standard)

### Appendix L - Table L.1 - FNU Two Sample t-test ###

# --- VARIABLES ---------------------------------------------------------------
fnu_hungarian_survey <- HungarianSurvey_fakenews_d   # your dataframe
group_var <- "fnu_dummy"           # grouping variable: 1 = High FNU, 0 = Low FNU

# original 'vars' mapping but remove fakenews_ubiquity (we're using that as grouping)
vars <- c(
  gender = "Gender (0=male,1=female)",
  agegroup = "Age cohort (1-4 scale youngest to oldest)",
  education_hu = "Level of education (1-10 scale lowest to highest)",
  householdincome = "Gross household income (1-17 scale lowest to highest)",
  turnout2018par = "2018 electoral turnout (1=voted,0=abstained)",
  ccaware = "Awareness of Constitutional Court (1-4 from least to most aware)",
  politicalloyal = "Political loyalty (1=party preference,0=no pref)",
  democ_sat = "Satisfaction with Hungarian democracy (1-4 scale least to most satisfied)"
)

# --- helper: coerce to numeric safely ---------------------------------------
coerce_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.numeric(x))
  x_char <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_char))
  if (!all(is.na(x_num))) return(x_num)
  uniq <- unique(na.omit(x_char))
  if (length(uniq) == 2) {
    levels_sorted <- sort(uniq)
    mapped <- ifelse(x_char == levels_sorted[1], 0,
                     ifelse(x_char == levels_sorted[2], 1, NA_real_))
    return(mapped)
  }
  return(NULL)
}

# --- helper: significance stars (for formatted table) -----------------------
sig_stars <- function(p){
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

# --- single-variable test function (predictor -> difference by fnu_dummy) ---
test_one_var_fnu <- function(fnu_hungarian_survey, varname, label, group_var = "fnu_dummy") {
  d <- fnu_hungarian_survey %>% dplyr::select(all_of(c(group_var, varname)))
  G_num <- coerce_to_numeric(d[[group_var]])
  if (is.null(G_num)) {
    return(tibble(
      variable = varname, label = label,
      mean_high = NA_real_, mean_low = NA_real_,
      diff = NA_real_, n_high = 0L, n_low = 0L,
      t_statistic = NA_real_, fnu_hungarian_survey = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_
    ))
  }
  d <- d %>% mutate(.G = G_num)
  X_num <- coerce_to_numeric(d[[varname]])
  if (is.null(X_num)) {
    return(tibble(
      variable = varname, label = label,
      mean_high = NA_real_, mean_low = NA_real_,
      diff = NA_real_, n_high = 0L, n_low = 0L,
      t_statistic = NA_real_, fnu_hungarian_survey = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_
    ))
  }
  d <- d %>% mutate(.X = X_num) %>% filter(!is.na(.G) & !is.na(.X))
  means_tbl <- d %>% group_by(.G) %>% summarise(
    n = n(),
    mean = mean(.X, na.rm = TRUE),
    sd = ifelse(n()>1, sd(.X, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )
  mean_high <- means_tbl %>% filter(.G == 1) %>% pull(mean) %>% { if(length(.)==0) NA_real_ else . }
  mean_low  <- means_tbl %>% filter(.G == 0) %>% pull(mean) %>% { if(length(.)==0) NA_real_ else . }
  n_high <- means_tbl %>% filter(.G == 1) %>% pull(n) %>% { if(length(.)==0) 0L else . }
  n_low  <- means_tbl %>% filter(.G == 0) %>% pull(n) %>% { if(length(.)==0) 0L else . }
  tt <- tryCatch(
    t.test(.X ~ .G, data = d, var.equal = TRUE),
    error = function(e) NULL,
    warning = function(w) { invokeRestart("muffleWarning") }
  )
  if (is.null(tt)) {
    tibble(
      variable = varname, label = label,
      mean_high = mean_high, mean_low = mean_low,
      diff = mean_high - mean_low, n_high = n_high, n_low = n_low,
      t_statistic = NA_real_, fnu_hungarian_survey = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_
    )
  } else {
    tt_tidy <- broom::tidy(tt)
    tibble(
      variable = varname, label = label,
      mean_high = mean_high, mean_low = mean_low,
      diff = mean_high - mean_low, n_high = n_high, n_low = n_low,
      t_statistic = tt_tidy$statistic, fnu_hungarian_survey = tt_tidy$parameter,
      p_value = tt_tidy$p.value, conf_low = tt_tidy$conf.low, conf_high = tt_tidy$conf.high
    )
  }
}

# --- run tests across variables ---------------------------------------------
results <- purrr::map2_dfr(names(vars), vars, ~ test_one_var_fnu(fnu_hungarian_survey, .x, .y, group_var = group_var))

# --- add formatted columns & stars ------------------------------------------
results <- results %>%
  mutate(
    star = purrr::map_chr(p_value, sig_stars),
    mean_high_r = ifelse(is.na(mean_high), NA, round(mean_high, 2)),
    mean_low_r  = ifelse(is.na(mean_low),  NA, round(mean_low,  2)),
    diff_r = ifelse(is.na(diff), NA, round(diff, 2)),
    conf_low_r = ifelse(is.na(conf_low), NA, round(conf_low, 2)),
    conf_high_r = ifelse(is.na(conf_high), NA, round(conf_high, 2))
  )

# --- build table_for_tex like your example ----------------------------------
table_for_tex <- results %>%
  mutate(
    Mean_High = ifelse(is.na(mean_high), "NA", sprintf("%.2f", round(mean_high, 2))),
    Mean_Low  = ifelse(is.na(mean_low),  "NA", sprintf("%.2f", round(mean_low, 2))),
    Diff_num  = diff,
    Diff_fmt  = ifelse(is.na(diff), NA_character_, sprintf("%.2f", round(diff, 2)))
  ) %>%
  mutate(
    Signif = case_when(
      is.na(p_value) ~ "",
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1  ~ "*",
      TRUE           ~ ""
    ),
    Difference = ifelse(is.na(Diff_fmt), "", paste0(Diff_fmt, Signif))
  ) %>%
  transmute(
    Variable = label,
    Mean_High,
    Mean_Low,
    `Difference in Means` = Difference
  ) %>%
  mutate(across(-Variable, ~ ifelse(. == "" | is.na(.), "--", .)))

# --- make LaTeX table with 2-line header and write to file ------------------
colnames(table_for_tex) <- c("Variable", "Mean", "Mean", "Difference in Means")

latex_tbl <- table_for_tex %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "High vs Low Fake-News Ubiquity comparisons",
    align = c("l", "c", "c", "c"),
    escape = FALSE
  ) %>%
  add_header_above(c(" " = 1, "High FNU" = 1, "Low FNU" = 1, " " = 1), bold = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = "*** p<0.01, ** p<0.05, * p<0.1", general_title = "", threeparttable = TRUE)

# write to disk
out_file <- "table_high_low_fnu.tex"
cat(latex_tbl, file = out_file)
message("Wrote LaTeX table to: ", file.path(getwd(), out_file))

### Appendix L - Figure L.2: FNU marginal effects plot ###

# --------------- Data ---------------
df <- HungarianSurvey_fakenews_d   # your data frame (must contain fnu_dummy)

# --------------- Ensure fnu_dummy 0/1 -------------------------------------
if(!"fnu_dummy" %in% names(df)) stop("fnu_dummy not found in data. Please add fnu_dummy (0/1).")
df <- df %>%
  mutate(fnu_dummy = case_when(
    fnu_dummy %in% c(1, "1", "High", "high", "HIGH", TRUE) ~ 1,
    fnu_dummy %in% c(0, "0", "Low", "low", "LOW", FALSE)  ~ 0,
    TRUE ~ NA_real_
  ))

# --------------- Detect political-loyalty variable --------------------------
pol_name <- if("politicalloyal" %in% names(df)) "politicalloyal" else
  if("party_close" %in% names(df)) "party_close" else NULL
if(is.null(pol_name)) warning("No political-loyalty variable found ('politicalloyal' or 'party_close'). It will be skipped.")

# --------------- Safe coercion helper --------------------------------------
coerce_if_possible <- function(x){
  if(is.numeric(x)) return(x)
  if(is.logical(x)) return(as.numeric(x))
  ch <- as.character(x)
  num <- suppressWarnings(as.numeric(ch))
  if(!all(is.na(num))) return(num)
  x
}

vars_to_coerce <- c("gender","agegroup","education_hu","householdincome",
                    "turnout2018par","ccaware",pol_name,"democ_sat")
vars_to_coerce <- vars_to_coerce[!is.null(vars_to_coerce)]
for(v in intersect(vars_to_coerce, names(df))) df[[v]] <- coerce_if_possible(df[[v]])

# --------------- Model formulas (drop fakenews_ubiquity) --------------------
f1 <- as.formula("fnu_dummy ~ gender + agegroup + education_hu + householdincome")
f2 <- update(f1, . ~ . + turnout2018par)
f3 <- update(f2, . ~ . + ccaware)
if(!is.null(pol_name)) {
  f4 <- as.formula(paste0(". ~ . + ", pol_name)); f4 <- update(f3, f4)
} else {
  f4 <- f3
}
f5 <- update(f4, . ~ . + democ_sat)
formulas <- list(M1 = f1, M2 = f2, M3 = f3, M4 = f4, M5 = f5)

# --------------- Fit helper (glm then fallback to brglm2 if needed) ----------
fit_model_safe <- function(formula, data) {
  # try glm with higher maxit
  glm_try <- tryCatch(
    glm(formula, data = data, family = binomial(link = "logit"), control = glm.control(maxit = 200)),
    error = function(e) e,
    warning = function(w) w
  )
  need_firth <- FALSE
  if(inherits(glm_try, "error")) {
    need_firth <- TRUE
    warning("glm error: ", glm_try$message)
  } else {
    # check for NA coefs or known convergence messages in capture.output
    coefs_na <- any(is.na(coef(glm_try)))
    conv_msgs <- any(grepl("algorithm did not converge|fitted probabilities numerically 0 or 1", capture.output(glm_try)))
    if(coefs_na || conv_msgs) need_firth <- TRUE
  }
  
  if(need_firth) {
    # attempt brglm2::brglmFit (Firth)
    if(!requireNamespace("brglm2", quietly = TRUE)) install.packages("brglm2")
    library(brglm2)
    m_firth <- tryCatch(
      brglm2::brglmFit(formula, data = data, family = binomial("logit"), type = "AS_mean"),
      error = function(e) e,
      warning = function(w) w
    )
    if(inherits(m_firth, "error")) {
      warning("brglm2 also failed; returning glm result (may be problematic).")
      return(glm_try)
    } else {
      message("Used brglm2 (Firth) for: ", deparse(formula))
      return(m_firth)
    }
  }
  glm_try
}

# --------------- Fit all models --------------------------------------------
models <- map(formulas, ~ fit_model_safe(.x, df))

# Expectation: `models` is a named list: names(models) == c("Model 1", "Model 2", ...)
if(!exists("models") || !is.list(models)) stop("Please provide a named list `models` (e.g. list('Model 1'=m1, ...)).")

# -------------------- label map and predictor order (drop fakenews_ubiquity) --------------------
label_map <- c(
  gender = "Gender",
  agegroup = "Age",
  education_hu = "Education",
  householdincome = "Household Income",
  turnout2018par = "Voted (2018)",
  ccaware = "CC Awareness",
  politicalloyal = "Political Loyalty",
  democ_sat = "Democratic Satisfaction"
)

predictor_order <- c(
  "Gender",
  "Age",
  "Education",
  "Household Income",
  "Voted (2018)",
  "CC Awareness",
  "Political Loyalty",
  "Democratic Satisfaction"
)

# -------------------- compute AMEs for each model (avg_slopes from marginaleffects) --------------------
# Use tryCatch so single model failures won't break everything
ame_df <- lapply(names(models), function(mname) {
  m <- models[[mname]]
  out <- tryCatch({
    marginaleffects::avg_slopes(m) %>%
      as_tibble() %>%
      mutate(Model = mname)
  }, error = function(e) {
    warning("avg_slopes failed for ", mname, ": ", conditionMessage(e))
    NULL
  })
  out
}) %>% bind_rows()

# Keep only terms we care about and map labels
ame_df_plot <- ame_df %>%
  filter(term %in% names(label_map)) %>%
  mutate(
    Predictor = label_map[term],
    Model = factor(Model, levels = names(models))
  ) %>%
  filter(!is.na(Predictor))

# apply predictor order
ame_df_plot <- ame_df_plot %>%
  mutate(Predictor = factor(Predictor, levels = predictor_order))

# -------------------- STANDARD horizontal plot (all AMEs on one axis) --------------------
p_standard <- ggplot(ame_df_plot, aes(x = estimate, y = Predictor, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.6), size = 2.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.6),
                 height = 0.18) +
  labs(
    x = "Average marginal effect on high FNU",
    y = NULL,
    title = "Average Marginal Effects Across Logistic Regression Models",
    subtitle = "Bars represent 95% confidence intervals"
  ) +
  theme_classic(base_size = 13) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right", axis.text.y = element_text(size = 10))

# print or save
print(p_standard)

### Appendix M - logistic regression for FNU ###

# --------------- Extract tidy (exp) results safely --------------------------
get_tidy_exp <- function(m) {
  td <- tryCatch(
    broom::tidy(m, exponentiate = TRUE, conf.int = TRUE),
    error = function(e) {
      # fallback: exponentiated without conf.int (we'll treat CI as NA)
      td2 <- tryCatch(broom::tidy(m, exponentiate = TRUE), error = function(e2) NULL)
      if(!is.null(td2)) {
        td2$conf.low <- NA_real_; td2$conf.high <- NA_real_
      }
      td2
    }
  )
  if(!is.null(td)) td$term <- as.character(td$term)
  td
}
tidy_list <- lapply(models, get_tidy_exp)

# --------------- Build coefficient display order & label map -----------------
# Preferred row order: gender, agegroup, education_hu, householdincome,
# turnout2018par, ccaware, political loyalty (pol_name), democ_sat
desired_keys <- c("gender","agegroup","education_hu","householdincome","turnout2018par","ccaware")
if(!is.null(pol_name)) desired_keys <- c(desired_keys, pol_name)
desired_keys <- c(desired_keys, "democ_sat")

# Labels
coef_labels <- list(
  gender = "Gender (0=male,1=female)",
  agegroup = "Age cohort (1..4)",
  education_hu = "Education (1--10)",
  householdincome = "Household income (1--17)",
  turnout2018par = "2018 turnout (1=voted,0=abstained)",
  ccaware = "Constitutional Court awareness (1--4)",
  democ_sat = "Democratic Satisfaction (1--4)"
)
if(!is.null(pol_name)) coef_labels[[pol_name]] <- "Political loyalty (1=party pref,0=no pref)"

# Keep only keys that exist in coef_labels and in the tidy data
display_keys <- intersect(desired_keys, names(coef_labels))

# --------------- Helper: safe scalar and star labels ------------------------
safe_get <- function(x, col, i) {
  if(is.null(x) || !(col %in% names(x)) || nrow(x) < i) return(NA_real_)
  val <- x[[col]][i]
  if(is.infinite(val) || is.nan(val)) return(NA_real_)
  val
}
star_label <- function(p) {
  if(is.na(p)) return("")
  if(p < 0.01) return("\\textsuperscript{***}")
  if(p < 0.05) return("\\textsuperscript{**}")
  if(p < 0.10) return("\\textsuperscript{*}")
  ""
}

# --------------- Compose LaTeX rows ----------------------------------------
# For each display key, for each model, format OR and CI (two-line cell)
format_cell <- function(key, tidy_df) {
  if(is.null(tidy_df)) return("")
  ix <- which(tidy_df$term == key)
  if(length(ix) == 0) return("")
  i <- ix[1]
  or <- safe_get(tidy_df, "estimate", i)
  lo <- safe_get(tidy_df, "conf.low", i)
  hi <- safe_get(tidy_df, "conf.high", i)
  p  <- safe_get(tidy_df, "p.value", i)
  stars <- star_label(p)
  if(is.na(or)) return("")
  or_s <- sprintf("%.2f", or)
  if(!is.na(lo) && !is.na(hi)) {
    ci_s <- sprintf("(%.2f, %.2f)", lo, hi)
    paste0("\\makecell{", or_s, stars, " \\\\ ", ci_s, "}")
  } else {
    paste0(or_s, stars)
  }
}

rows <- lapply(display_keys, function(k) {
  label <- coef_labels[[k]]
  cells <- sapply(tidy_list, function(td) format_cell(k, td), USE.NAMES = FALSE)
  list(key = k, label = label, cells = cells)
})

# --------------- Compute N (observations) for each model --------------------
nobs <- sapply(models, function(m) {
  n1 <- tryCatch(broom::glance(m)$nobs, error = function(e) NA_real_)
  if(!is.na(n1) && !is.null(n1)) return(as.integer(n1))
  n2 <- tryCatch(nrow(stats::model.frame(m)), error = function(e) NA_integer_)
  as.integer(n2)
})

# --------------- Build LaTeX document (matches the aesthetic you wanted) ----
outfile <- "logit_fnu_table.tex"
tex <- c()
tex <- c(tex, "\\documentclass[11pt]{article}")
tex <- c(tex, "\\usepackage[utf8]{inputenc}")
tex <- c(tex, "\\usepackage[margin=1in]{geometry}")
tex <- c(tex, "\\usepackage{pdflscape}")
tex <- c(tex, "\\usepackage{booktabs}")
tex <- c(tex, "\\usepackage{tabularx}")
tex <- c(tex, "\\usepackage{threeparttable}")
tex <- c(tex, "\\usepackage{makecell}")
tex <- c(tex, "\\usepackage{caption}")
tex <- c(tex, "\\begin{document}")
tex <- c(tex, "\\begin{landscape}")
tex <- c(tex, "\\begin{table}[t]")
tex <- c(tex, "\\centering")
tex <- c(tex, "\\footnotesize")
tex <- c(tex, "\\setlength{\\tabcolsep}{8pt}")
tex <- c(tex, "\\begin{threeparttable}")
tex <- c(tex, "\\caption{Logistic Regression: Predictors of High Fake News Ubiquity (Odds ratios)}")
tex <- c(tex, "\\label{tab:logit_fnu}")

# table header: left wide col and 5 model cols
tex <- c(tex, "\\begin{tabular}{@{}p{8.2cm} *{5}{>{\\centering\\arraybackslash}p{2.1cm}}@{}}")
tex <- c(tex, "\\toprule")
tex <- c(tex, "\\multicolumn{1}{l}{} & \\multicolumn{5}{c}{\\textbf{Models}} \\\\")
tex <- c(tex, "\\cmidrule(lr){2-6}")
tex <- c(tex, "\\multicolumn{1}{l}{\\textbf{Effect of IV}} & \\textbf{Model 1} & \\textbf{Model 2} & \\textbf{Model 3} & \\textbf{Model 4} & \\textbf{Model 5} \\\\")
tex <- c(tex, "\\midrule")

# add rows
for(r in rows) {
  left_escaped <- stringi::stri_replace_all_regex(r$label, "(\\\\[A-Za-z]+)", "<<SAFE:$1>>")
  specials <- c("$","%","#","&","_","{","}","~","^")
  repl     <- c("\\\\$","\\\\%","\\\\#","\\\\&","\\\\_","\\\\{","\\\\}","\\\\textasciitilde{}","\\\\textasciicircum{}")
  for(i in seq_along(specials)) left_escaped <- stringi::stri_replace_all_fixed(left_escaped, specials[i], repl[i], vectorize_all = FALSE)
  left_escaped <- stringi::stri_replace_all_regex(left_escaped, "<<SAFE:(\\\\[A-Za-z]+)>>", "$1")
  left_cell <- paste0("\\makecell[l]{", left_escaped, "}")
  filled <- r$cells
  if(length(filled) < length(models)) filled <- c(filled, rep("", length(models) - length(filled)))
  right_line <- paste0(sapply(filled, function(x) if(x=="" ) "" else x), collapse = " & ")
  tex <- c(tex, paste0(left_cell, " & ", right_line, " \\\\ [6pt]"))
}

# N row
tex <- c(tex, "\\midrule")
n_line <- paste0("\\multicolumn{1}{l}{\\textbf{N}} & ",
                 paste0("\\multicolumn{1}{c}{", nobs, "}", collapse = " & "), " \\\\")
tex <- c(tex, n_line)

tex <- c(tex, "\\bottomrule")
tex <- c(tex, "\\end{tabular}")
tex <- c(tex, "\\begin{tablenotes}")
tex <- c(tex, "\\footnotesize")
tex <- c(tex, "\\item \\textit{Notes.} Odds ratios; 95\\% confidence intervals in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.")
tex <- c(tex, "\\end{tablenotes}")
tex <- c(tex, "\\end{threeparttable}")
tex <- c(tex, "\\end{table}")
tex <- c(tex, "\\end{landscape}")
tex <- c(tex, "\\end{document}")

writeLines(tex, outfile)
message("Wrote LaTeX table to: ", normalizePath(outfile))

##### Appendix N code - FNE as a binary variable, confidence in institutions (pooled results) ######

dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty"
)

# --------------------------
# N.1 - Confidence in Fidesz (FNE binary IV)
# --------------------------

# OLS (no interaction)
mod_1 <- fixest::feols(
  confidence_fidesz ~ EfficacyDummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_fidesz ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

# Fractional logit (no interaction)
mod_4 <- fixest::feglm(
  confidence_fidesz ~ EfficacyDummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_fidesz ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNE as Binary IV with Confidence in Fidesz DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure N. 1 Plotting Code ###

pred_plot <- predictions(
  mod_5,
  newdata = datagrid(EfficacyDummy = c("Low", "High")),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot$estimate, pred_plot$conf.low, pred_plot$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_n1_fidesz <- ggplot(pred_plot, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_n1_fidesz)

# --------------------------
# N.2 - Confidence in United Opposition
# --------------------------

mod_1 <- fixest::feols(
  confidence_united ~ EfficacyDummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_united ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_4 <- fixest::feglm(
  confidence_united ~ EfficacyDummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_united ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["confidence_united"] <- "Confidence in United Opposition"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNE as Binary IV with Confidence in United Opposition DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure N. 2 Plotting Code ###

pred_plot <- predictions(
  mod_5,
  newdata = datagrid(EfficacyDummy = c("Low", "High")),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot$estimate, pred_plot$conf.low, pred_plot$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_n2_united <- ggplot(pred_plot, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_n2_united)

# --------------------------
# N.3 - Confidence in National Government
# --------------------------

mod_1 <- fixest::feols(
  confidence_gov ~ EfficacyDummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_gov ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_4 <- fixest::feglm(
  confidence_gov ~ EfficacyDummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_gov ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["confidence_gov"] <- "Confidence in the National Government"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNE as Binary IV with Confidence in the National Government DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure N. 3 Plotting Code ###

pred_plot <- predictions(
  mod_5,
  newdata = datagrid(EfficacyDummy = c("Low", "High")),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot$estimate, pred_plot$conf.low, pred_plot$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_n3_gov <- ggplot(pred_plot, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_n3_gov)

# --------------------------
# N.4 - Confidence in the Media
# --------------------------

mod_1 <- fixest::feols(
  confidence_media ~ EfficacyDummy,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_2 <- fixest::feols(
  confidence_media ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_4 <- fixest::feglm(
  confidence_media ~ EfficacyDummy,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

mod_5 <- fixest::feglm(
  confidence_media ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
    householdincome + agegroup + gender + turnout2018par,
  vcov = "hetero",
  family = quasibinomial(link = "logit"),
  data = HungarianSurvey_fakenews_d,
  weights = HungarianSurvey_fakenews_d$weight
)

dict["confidence_media"] <- "Confidence in the Media"

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(
  models,
  coef_rename = dict,
  title = "FNE as Binary IV with Confidence in the Media DV",
  statistic = "(p= {p.value})",
  output = "latex",
  gof_map = c("nobs", "r.squared"),
  notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses.")
)

### Figure N. 4 Plotting Code ###

pred_plot <- predictions(
  mod_5,
  newdata = datagrid(EfficacyDummy = c("Low", "High")),
  type = "link"
) %>%
  mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High"))
  )

absmax <- max(abs(c(pred_plot$estimate, pred_plot$conf.low, pred_plot$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

p_n4_media <- ggplot(pred_plot, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "none")

print(p_n4_media)

##### Appendix O code - FNE as a continuous variable, confidence in institutions (pooled results) ######

dict <- c(
  EfficacyContinuous = "FNE",
  PartyOpposition     = "United Opposition",
  PartyNeither        = "Neither Party",
  education_hu        = "Education",
  ccaware             = "CC Aware",
  householdincome     = "Household Income",
  democ_sat           = "Satisfaction with Democracy",
  agegroup            = "Age",
  gender1             = "Gender",
  turnout2018par      = "Voted (2018)",
  politicalloyal      = "Political Loyalty",
  confidence_fidesz   = "Confidence in Fidesz",
  confidence_united   = "Confidence in United Opposition",
  confidence_gov      = "Confidence in the National Government",
  confidence_media    = "Confidence in the Media"
)

# small helper to compute symmetric y-limits given a preds data.frame
make_sym_limits <- function(pred_df, pad = 0.2, n_breaks = 7) {
  absmax <- max(abs(c(pred_df$estimate, pred_df$conf.low, pred_df$conf.high)), na.rm = TRUE)
  ymax_sym <- ceiling((absmax + pad) * 10) / 10
  ymin_sym <- -ymax_sym
  y_breaks <- pretty(c(ymin_sym, ymax_sym), n = n_breaks)
  list(ymin = ymin_sym, ymax = ymax_sym, breaks = y_breaks)
}

# --------------------------
# O. 1 - Confidence in Fidesz
# --------------------------

# OLS (no interaction)
mod_1 <- feols(confidence_fidesz ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(confidence_fidesz ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

# Fractional logit (no interaction)
mod_4 <- feglm(confidence_fidesz ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(confidence_fidesz ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Confidence in Fidesz DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure O. 1 Plotting Code ###

pred_o1 <- predictions(mod_5,
                       newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                       type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = as.numeric(as.character(EfficacyContinuous)))

lims <- make_sym_limits(pred_o1)

p_o1_fidesz <- ggplot(pred_o1, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNE", breaks = seq(1,5,1)) +
  scale_y_continuous(breaks = lims$breaks, name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_o1_fidesz)

# --------------------------
# O. 2 - Confidence in United Opposition
# --------------------------

# OLS (no interaction)
mod_1 <- feols(confidence_united ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(confidence_united ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(confidence_united ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(confidence_united ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Confidence in United Opposition DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure O. 2 Plotting Code ###

pred_o2 <- predictions(mod_5,
                       newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                       type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = as.numeric(as.character(EfficacyContinuous)))

lims <- make_sym_limits(pred_o2)
p_o2_united <- ggplot(pred_o2, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNE", breaks = seq(1,5,1)) +
  scale_y_continuous(breaks = lims$breaks, name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_o2_united)

# --------------------------
# O. 3 - Confidence in National Government
# --------------------------

# OLS (no interaction)
mod_1 <- feols(confidence_gov ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(confidence_gov ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(confidence_gov ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(confidence_gov ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Confidence in the National Government DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure O. 3 Plotting Code ###

pred_o3 <- predictions(mod_5,
                       newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                       type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = as.numeric(as.character(EfficacyContinuous)))

lims <- make_sym_limits(pred_o3)
p_o3_gov <- ggplot(pred_o3, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNE", breaks = seq(1,5,1)) +
  scale_y_continuous(breaks = lims$breaks, name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_o3_gov)

# --------------------------
# O. 4 - Confidence in the Media
# --------------------------

# OLS (no interaction)
mod_1 <- feols(confidence_media ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(confidence_media ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(confidence_media ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(confidence_media ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Confidence in the Media DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure O. 4 Plotting Code ###

pred_o4 <- predictions(mod_5,
                       newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                       type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = as.numeric(as.character(EfficacyContinuous)))

lims <- make_sym_limits(pred_o4)
p_o4_media <- ggplot(pred_o4, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.35) +
  theme_classic() +
  scale_x_continuous(name = "FNE", breaks = seq(1,5,1)) +
  scale_y_continuous(breaks = lims$breaks, name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_o4_media)

##### Appendix P code - FNE as a binary variable, confidence in institutions (w/ party split)######

dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty"
)

#### Table P.1 - Confidence in Fidesz ####

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_fidesz~ EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_fidesz~ EfficacyDummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_fidesz~ EfficacyDummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)


#No interaction binary Logit
mod_4<- fixest::feglm(confidence_fidesz~ EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_fidesz~ EfficacyDummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_fidesz~ EfficacyDummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Confidence in Fidesz DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure P. 1 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numerics + stable factor ordering
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_interaction_fidesz <- ggplot(pred_plot_interaction,
                                aes(x = factor(EfficacyDummy),
                                    y = estimate,
                                    shape = Party,
                                    group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_fidesz

#### Table P.2 - Confidence in United Opposition ####

mod_1 <- fixest::feols(confidence_united~ EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_united~ EfficacyDummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_united~ EfficacyDummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_united~ EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_united~ EfficacyDummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_united~ EfficacyDummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Confidence in United Opposition DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure P. 2 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numerics + stable factor ordering
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot on logit (link) scale with dynamic symmetric framing
p2_interaction_united <- ggplot(pred_plot_interaction,
                                aes(x = factor(EfficacyDummy),
                                    y = estimate,
                                    shape = Party,
                                    group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks, name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_united

#### Table P.3 - Confidence in National Government ####

mod_1 <- fixest::feols(confidence_gov~ EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_gov~ EfficacyDummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_gov~ EfficacyDummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_gov~ EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_gov~ EfficacyDummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_gov~ EfficacyDummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Confidence in the National Government DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure P. 3 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numeric + enforce stable factor ordering
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy),
                           levels = c("Low","High"))
  )

# dynamic symmetric limits based on the data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_interaction_gov <- ggplot(pred_plot_interaction,
                             aes(x = factor(EfficacyDummy),
                                 y = estimate,
                                 shape = Party,
                                 group = Party)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge2(width = 0.5, preserve = "single"),
    size = .35
  ) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x = "FNE")

p2_interaction_gov

#### Table P.4 - Confidence in Media ####

mod_1 <- fixest::feols(confidence_media~ EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_media~ EfficacyDummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_media~ EfficacyDummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_media~ EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_media~ EfficacyDummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_media~ EfficacyDummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Confidence in the Media DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure P. 4 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numeric + enforce stable factor ordering (explicit dplyr namespace)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on the data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)),
              na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot on link (logit) scale with dynamic symmetric framing
p2_interaction_media <- ggplot(pred_plot_interaction,
                               aes(x = factor(EfficacyDummy),
                                   y = estimate,
                                   shape = Party,
                                   group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_media

##### Appendix Q code - FNE as a continous variable, confidence in institutions (w/ party split) ######

dict <- c(
  fnu_dummyHigh = "FNU",
  PartyOpposition = "United Opposition",
  PartyNeither = "Neither Party",
  education_hu = "Education",
  ccaware = "CC Aware",
  householdincome = "Household Income",
  democ_sat = "Democratic Satisfaction",
  agegroup = "Age",
  gender1 = "Gender",
  turnout2018par = "Voted (2018)",
  politicalloyal = "Political Loyalty"
)

### Figure Q.1 - Confidence in Fidesz ### 

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_fidesz~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_fidesz~EfficacyContinuous +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#With interaction OLS
mod_3 <- fixest::feols(confidence_fidesz~EfficacyContinuous*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_fidesz~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_fidesz~EfficacyContinuous +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_fidesz~EfficacyContinuous*Party + education_hu +ccaware+politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Confidence in Fidesz DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure Q. 1 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_fidesz_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in Fidesz (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNE")

p2_fidesz_continuous

### Figure Q.2 - Confidence in United Opposition ### 

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_united~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_united~EfficacyContinuous +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_united~EfficacyContinuous*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_united~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_united~EfficacyContinuous +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_united~EfficacyContinuous*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Confidence in United Opposition DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure Q. 2 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_continuous_united <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in United Opposition (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNE")

p2_continuous_united

### Figure Q.3 - Confidence in National Government ### 

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_gov~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_gov~EfficacyContinuous +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_gov~EfficacyContinuous*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_gov~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_gov~EfficacyContinuous +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_gov~EfficacyContinuous*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Confidence in the National Government DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure Q. 3 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + enforce stable ordering (explicit dplyr namespace)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits (based on largest absolute value among estimate/CI)
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing; colorblind palette preserved)
p2_gov_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in the National Government (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_gov_continuous

### Figure Q. 4 - Confidence in the Media ###

#No interaction binary OLS
mod_1 <- fixest::feols(confidence_media~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(confidence_media~EfficacyContinuous +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(confidence_media~EfficacyContinuous*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(confidence_media~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(confidence_media~EfficacyContinuous +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(confidence_media~EfficacyContinuous*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Confidence in the Media DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure Q. 4 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + enforce stable ordering (explicit dplyr namespace)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits from the data
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing; palette kept)
p2_media_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Confidence in the Media (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_media_continuous

##### Appendix R Code - FNE as a binary variable, electoral participation (pooled results) #####

dict <- c(
  EfficacyDummyHigh = "FNE",
  PartyOpposition   = "United Opposition",
  PartyNeither      = "Neither Party",
  education_hu      = "Education",
  ccaware           = "CC Aware",
  householdincome   = "Household Income",
  democ_sat         = "Democratic Satisfaction",
  agegroup          = "Age",
  gender1           = "Gender",
  turnout2018par    = "Voted (2018)",
  politicalloyal    = "Political Loyalty",
  volunteer         = "Participation",
  rally             = "Participation",
  campaignfollow    = "Participation",
  voteintent        = "Participation"
)

make_sym_limits <- function(pred_df, pad = 0.2, n_breaks = 7) {
  absmax <- max(abs(c(pred_df$estimate, pred_df$conf.low, pred_df$conf.high)), na.rm = TRUE)
  ymax_sym <- ceiling((absmax + pad) * 10) / 10
  ymin_sym <- -ymax_sym
  y_breaks <- pretty(c(ymin_sym, ymax_sym), n = n_breaks)
  list(ymin = ymin_sym, ymax = ymax_sym, breaks = y_breaks)
}

##### Appendix R - Table R.1 - Volunteer for Campaign #####

# OLS (no interaction)
mod_1 <- feols(volunteer ~ EfficacyDummy,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(volunteer ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

# Fractional logit (no interaction)
mod_4 <- feglm(volunteer ~ EfficacyDummy,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(volunteer ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Binary IV with Volunteer for Campaign DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure R. 1 Plotting Code ###

pred_volunteer <- predictions(mod_5,
                              newdata = datagrid(EfficacyDummy = c("Low", "High")),
                              type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High")))

lims <- make_sym_limits(pred_volunteer)
p_r1_volunteer <- ggplot(pred_volunteer, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_r1_volunteer)


##### Appendix R - Table R.2 - Attend Campaign Rally or Protest #####

mod_1 <- feols(rally ~ EfficacyDummy,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(rally ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(rally ~ EfficacyDummy,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(rally ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Binary IV with Attend Campaign Rally or Protest DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure R. 2 Plotting Code ###

pred_rally <- predictions(mod_5,
                          newdata = datagrid(EfficacyDummy = c("Low", "High")),
                          type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High")))

lims <- make_sym_limits(pred_rally)
p_r2_rally <- ggplot(pred_rally, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_r2_rally)

##### Appendix R - Table R.3 - Follow Electoral Campaign #####

mod_1 <- feols(campaignfollow ~ EfficacyDummy,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(campaignfollow ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(campaignfollow ~ EfficacyDummy,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(campaignfollow ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Binary IV with Follow Electoral Campaign DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure R. 3 Plotting Code ###

pred_campaignfollow <- predictions(mod_5,
                                   newdata = datagrid(EfficacyDummy = c("Low", "High")),
                                   type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High")))

lims <- make_sym_limits(pred_campaignfollow)
p_r3_campaignfollow <- ggplot(pred_campaignfollow, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Follow Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_r3_campaignfollow)

##### Appendix R - Table R.4 - Intent to Vote in Election #####

mod_1 <- feols(voteintent ~ EfficacyDummy,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(voteintent ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(voteintent ~ EfficacyDummy,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(voteintent ~ EfficacyDummy + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Binary IV with Intent to Vote in Election DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure R. 4 Plotting Code ###

comparisons(mod_5,
            newdata = datagrid(EfficacyDummy = c("Low", "High")),
            type = "link")

pred_voteintent <- predictions(mod_5,
                               newdata = datagrid(EfficacyDummy = c("Low", "High")),
                               type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low", "High")))

lims <- make_sym_limits(pred_voteintent)
p_r4_voteintent <- ggplot(pred_voteintent, aes(x = EfficacyDummy, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Intend to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_r4_voteintent)

##### Appendix S Code - FNE as a continuous variable, electoral participation (pooled results) #####

# Shared dict with updated labels
dict <- c(
  EfficacyDummyHigh = "FNE",
  EfficacyContinuous = "FNE",
  PartyOpposition   = "United Opposition",
  PartyNeither      = "Neither Party",
  education_hu      = "Education",
  ccaware           = "CC Aware",
  householdincome   = "Household Income",
  democ_sat         = "Democratic Satisfaction",
  agegroup          = "Age",
  gender1           = "Gender",
  turnout2018par    = "Voted (2018)",
  politicalloyal    = "Political Loyalty",
  volunteer         = "Participation",
  rally             = "Participation",
  campaignfollow    = "Participation",
  voteintent        = "Participation"
)

# Helper for symmetric y-limits (your make_sym_limits)
make_sym_limits <- function(pred_df, pad = 0.2, n_breaks = 7) {
  absmax <- max(abs(c(pred_df$estimate, pred_df$conf.low, pred_df$conf.high)), na.rm = TRUE)
  ymax_sym <- ceiling((absmax + pad) * 10) / 10
  ymin_sym <- -ymax_sym
  y_breaks <- pretty(c(ymin_sym, ymax_sym), n = n_breaks)
  list(ymin = ymin_sym, ymax = ymax_sym, breaks = y_breaks)
}

### Appendix S - Figure S.1 - Volunteer for Campaign (continuous FNE) ###
# OLS no interaction (simple)
mod_1 <- feols(volunteer ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

# OLS adjusted (Party as control, not interaction)
mod_2 <- feols(volunteer ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

# Fractional logit (simple)
mod_4 <- feglm(volunteer ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

# Fractional logit (adjusted)
mod_5 <- feglm(volunteer ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Volunteer for Electoral Campaign DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure S. 1 Plotting Code ###

pred_volunteer <- predictions(mod_5,
                              newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                              type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = factor(as.character(EfficacyContinuous), levels = as.character(seq(1,5,1))))

lims <- make_sym_limits(pred_volunteer)
p_s1_volunteer <- ggplot(pred_volunteer, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35, position = position_dodge2(width = 0)) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_s1_volunteer)


### Appendix S - Figure S.2 - Attend Campaign Rally or Protest (continuous FNE) ###
mod_1 <- feols(rally ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(rally ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(rally ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(rally ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Attend Campaign Rally or Protest DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure S. 2 Plotting Code ###

pred_rally <- predictions(mod_5,
                          newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                          type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = factor(as.character(EfficacyContinuous), levels = as.character(seq(1,5,1))))

lims <- make_sym_limits(pred_rally)
p_s2_rally <- ggplot(pred_rally, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35, position = position_dodge2(width = 0)) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_s2_rally)


### Appendix S - Figure S.3 - Follow Electoral Campaign (continuous FNE) ###
mod_1 <- feols(campaignfollow ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(campaignfollow ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(campaignfollow ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(campaignfollow ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Follow Electoral Campaign DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure S. 3 Plotting Code ###

pred_campaignfollow <- predictions(mod_5,
                                   newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                                   type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = factor(as.character(EfficacyContinuous), levels = as.character(seq(1,5,1))))

lims <- make_sym_limits(pred_campaignfollow)
p_s3_campaignfollow <- ggplot(pred_campaignfollow, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35, position = position_dodge2(width = 0)) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Follow Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_s3_campaignfollow)


### Appendix S - Figure S.4 - Intent to Vote (continuous FNE) ###
mod_1 <- feols(voteintent ~ EfficacyContinuous,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_2 <- feols(voteintent ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_4 <- feglm(voteintent ~ EfficacyContinuous,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

mod_5 <- feglm(voteintent ~ EfficacyContinuous + Party + education_hu + ccaware + politicalloyal + democ_sat +
                 householdincome + agegroup + gender + turnout2018par,
               vcov = "hetero",
               family = quasibinomial(link = "logit"),
               data = HungarianSurvey_fakenews_d,
               weights = HungarianSurvey_fakenews_d$weight)

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) Fractional Logit" = mod_4,
  "(4) Fractional Logit" = mod_5
)

modelsummary(models,
             coef_rename = dict,
             title = "FNE as Ordinal IV with Intent to Vote DV",
             statistic = "(p= {p.value})",
             output = "latex",
             gof_map = c("nobs", "r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure S. 4 Plotting Code ###

pred_voteintent <- predictions(mod_5,
                               newdata = datagrid(EfficacyContinuous = seq(1, 5, 1)),
                               type = "link") %>%
  mutate(estimate = as.numeric(estimate),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high),
         EfficacyContinuous = factor(as.character(EfficacyContinuous), levels = as.character(seq(1,5,1))))

lims <- make_sym_limits(pred_voteintent)
p_s4_voteintent <- ggplot(pred_voteintent, aes(x = EfficacyContinuous, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35, position = position_dodge2(width = 0)) +
  theme_classic() +
  scale_x_discrete(name = "FNE") +
  scale_y_continuous(breaks = lims$breaks, name = "Intent to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(lims$ymin, lims$ymax)) +
  theme(legend.position = "none")

print(p_s4_voteintent)

##### Appendix T Code - FNE as a binary variable, electoral participation (w/ party split)#####

dict <- c(EfficacyDummyHigh ="FNE", PartyOpposition ="United Opposition",
          PartyNeither ="Neither Party", education_hu ="Education", ccaware="CC Aware", 
          householdincome ="Household Income",  democ_sat="Democratic Satisfaction",
          agegroup ="Age", gender1 ="Gender", turnout2018par ="Voted (2018)",  politicalloyal="Political Loyalty",
          volunteer ="Participation")

#### Table T.1 - Volunteer for Campaign ####

#No interaction binary OLS
mod_1 <- fixest::feols(volunteer~EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(volunteer~EfficacyDummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(volunteer~EfficacyDummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)


#No interaction binary Logit
mod_4<- fixest::feglm(volunteer~EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(volunteer~EfficacyDummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(volunteer~EfficacyDummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Volunteer for Campaign DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure T. 1 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numerics + stable factor ordering
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot (logit scale, dynamic symmetric framing)
p2_interaction_volunteer <- ggplot(pred_plot_interaction,
                                   aes(x = factor(EfficacyDummy),
                                       y = estimate,
                                       shape = Party,
                                       group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks, name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_volunteer

#### Table T.2 - Attend Campaign Rally or Protest ####

mod_1 <- fixest::feols(rally~EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(rally~EfficacyDummy +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(rally~EfficacyDummy*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(rally~EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(rally~EfficacyDummy +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(rally~EfficacyDummy*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Attend Campaign Rally or Protest DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure T. 2 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numerics + stable factor ordering (explicit dplyr namespace)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate  = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party     = factor(as.character(Party),     levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on the data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot on link (logit) scale with dynamic symmetric framing
p2_interaction_rally <- ggplot(pred_plot_interaction,
                               aes(x = factor(EfficacyDummy),
                                   y = estimate,
                                   shape = Party,
                                   group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_rally

#### Table T.3 - Follow Electoral Campaign ####

mod_1 <- fixest::feols(campaignfollow~EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(campaignfollow~EfficacyDummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(campaignfollow~EfficacyDummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(campaignfollow~EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(campaignfollow~EfficacyDummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(campaignfollow~EfficacyDummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Follow Electoral Campaign DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure T. 3 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numerics + stable factor ordering (explicit dplyr namespace)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate  = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party     = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on the data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot on link (logit) scale with dynamic symmetric framing
p2_interaction_campaignfollow <- ggplot(pred_plot_interaction,
                                        aes(x = factor(EfficacyDummy),
                                            y = estimate,
                                            shape = Party,
                                            group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Follow Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_campaignfollow

#### Table T.4 - Intent to Vote in Election ####

mod_1 <- fixest::feols(voteintent~EfficacyDummy,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(voteintent~EfficacyDummy +Party + education_hu + ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(voteintent~EfficacyDummy*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(voteintent~EfficacyDummy,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(voteintent~EfficacyDummy +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(voteintent~EfficacyDummy*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Binary IV with Intent to Vote in Election DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure T. 4 Plotting Code ###

# link-scale predictions (log-odds)
pred_plot_interaction <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyDummy = c("Low","High")
  ),
  type = "link"
)

# coerce numerics + stable factor ordering (explicit dplyr namespace)
pred_plot_interaction <- pred_plot_interaction |>
  dplyr::mutate(
    estimate  = as.numeric(estimate),
    conf.low  = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party     = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyDummy = factor(as.character(EfficacyDummy), levels = c("Low","High"))
  )

# dynamic symmetric limits based on the data
absmax <- max(abs(c(pred_plot_interaction$estimate,
                    pred_plot_interaction$conf.low,
                    pred_plot_interaction$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# plot on link (logit) scale with dynamic symmetric framing
p2_interaction_voteintent <- ggplot(pred_plot_interaction,
                                    aes(x = factor(EfficacyDummy),
                                        y = estimate,
                                        shape = Party,
                                        group = Party)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge2(width = 0.5, preserve = "single"),
                  size = .35) +
  theme_classic() +
  scale_x_discrete(labels = c("Low" = "Low", "High" = "High")) +
  scale_y_continuous(breaks = y_breaks,
                     name = "Intend to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_interaction_voteintent

##### Appendix U Code - FNE as a continous variable, electoral participation (w/ party split) #####

dict <- c(EfficacyContinuous ="FNE", PartyOpposition ="United Opposition",
          PartyNeither ="Neither Party", education_hu ="Education", ccaware="CC Aware", 
          householdincome ="Household Income", democ_sat="Democratic Satisfaction",
          agegroup ="Age", gender1 ="Gender", turnout2018par ="Voted (2018)",  politicalloyal="Political Loyalty",
          volunteer ="Participation")

### Table U.1 - volunteer for a campaign ### 

#No interaction binary OLS
mod_1 <- fixest::feols(volunteer~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(volunteer~EfficacyContinuous +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(volunteer~EfficacyContinuous*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(volunteer~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(volunteer~EfficacyContinuous +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(volunteer~EfficacyContinuous*Party + education_hu +ccaware+politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Volunteer for Electoral Campaign DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure U. 1 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_volunteer_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Volunteer for Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNE")

p2_volunteer_continuous

### Table U. 2 Plotting Code ###

#No interaction binary OLS
mod_1 <- fixest::feols(rally~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(rally~EfficacyContinuous +Party + education_hu + ccaware+ politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(rally~EfficacyContinuous*Party + education_hu +  ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(rally~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(rally~EfficacyContinuous +Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(rally~EfficacyContinuous*Party + education_hu +ccaware+ politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table
dict <- c(EfficacyContinuous ="FNE", PartyOpposition ="United Opposition",
          PartyNeither ="Neither Party", education_hu ="Education", ccaware="CC Aware", 
          householdincome ="Household Income", democ_sat="Democratic Satisfaction",
          agegroup ="Age", gender1 ="Gender", turnout2018par ="Voted (2018)",  politicalloyal="Political Loyalty",
          rally ="Participation")


models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Political Participation DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure U. 2 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Ensure numeric + stable ordering
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party),
                   levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous,
                                levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)),
              na.rm = TRUE)

pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot
p2_rally_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Attend Campaign Rally or Protest (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position ="right",
        legend.box.margin = ggplot2::margin(-10,-10,-10,-10)) +
  labs(x ="FNE")

p2_rally_continuous

### Table U.3 - follow the electoral campaign ### 

#No interaction binary OLS
mod_1 <- fixest::feols(campaignfollow~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(campaignfollow~EfficacyContinuous +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(campaignfollow~EfficacyContinuous*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(campaignfollow~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(campaignfollow~EfficacyContinuous +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(campaignfollow~EfficacyContinuous*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Political Participation DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure U. 3 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + enforce stable ordering (explicit dplyr namespace)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits based on the largest absolute value among estimate/CI
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing; palette preserved)
p2_campaignfollow_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Follow the Electoral Campaign (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_campaignfollow_continuous

### Table U. 4 - intention to vote ###

#No interaction binary OLS
mod_1 <- fixest::feols(voteintent~EfficacyContinuous,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)

#No interaction OLS
mod_2 <- fixest::feols(voteintent~EfficacyContinuous +Party + education_hu + ccaware+   politicalloyal+ democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction OLS
mod_3 <- fixest::feols(voteintent~EfficacyContinuous*Party + education_hu +  ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#No interaction binary Logit
mod_4<- fixest::feglm(voteintent~EfficacyContinuous,
                      vcov ="hetero",
                      family = quasibinomial(link ="logit"),
                      data = HungarianSurvey_fakenews_d,
                      weights = HungarianSurvey_fakenews_d$weight)

#No interaction Logit
mod_5 <- fixest::feglm(voteintent~EfficacyContinuous +Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#With interaction logit
mod_6 <- fixest::feglm(voteintent~EfficacyContinuous*Party + education_hu +ccaware+  politicalloyal+democ_sat+
                         householdincome + agegroup + gender + turnout2018par,
                       vcov ="hetero",
                       family = quasibinomial(link ="logit"),
                       data = HungarianSurvey_fakenews_d,
                       weights = HungarianSurvey_fakenews_d$weight)
#Creating table

models <- list(
  "(1) OLS" = mod_1,
  "(2) OLS" = mod_2,
  "(3) OLS" = mod_3,
  "(4) Fractional Logit" = mod_4,
  "(5) Fractional Logit" = mod_5,
  "(6) Fractional Logit" = mod_6
)


modelsummary(models,
             #vcov ="robust",
             coef_rename = dict,
             title ="FNE as Ordinal IV with Political Participation DV",
             statistic ="(p= {p.value})", 
             output ="latex",
             gof_map = c("nobs","r.squared"),
             notes = list("Pvalues calculated from heteroskedasticity robust standard errors in parentheses."))

### Figure U. 4 Plotting Code ###

# Link-scale predictions (log-odds)
pred_plot_continuous <- predictions(
  mod_6,
  newdata = datagrid(
    Party = c("Fidesz","Opposition","Neither"),
    EfficacyContinuous = seq(1,5,1)
  ),
  type = "link"
)

# Coerce numeric + enforce stable ordering (explicit dplyr namespace)
pred_plot_continuous <- pred_plot_continuous |>
  dplyr::mutate(
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    Party = factor(as.character(Party), levels = c("Fidesz","Opposition","Neither")),
    EfficacyContinuous = factor(EfficacyContinuous, levels = as.character(seq(1,5,1)))
  )

# Dynamic symmetric limits based on the largest absolute value among estimate/CI
absmax <- max(abs(c(pred_plot_continuous$estimate,
                    pred_plot_continuous$conf.low,
                    pred_plot_continuous$conf.high)), na.rm = TRUE)
pad <- 0.2
ymax_sym <- ceiling((absmax + pad) * 10) / 10
ymin_sym <- -ymax_sym
y_breaks <- pretty(c(ymin_sym, ymax_sym), n = 7)

# Plot (logit scale, dynamic symmetric framing; palette preserved)
p2_voteintent_continuous <- ggplot(
  pred_plot_continuous,
  aes(y = estimate,
      x = EfficacyContinuous,
      shape = Party,
      color = Party,
      group = Party)
) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "black",
    size = .35,
    position = position_dodge2(width = 0.5, preserve = "single")
  ) +
  theme_classic() +
  scale_y_continuous(breaks = y_breaks,
                     name = "Intent to Vote (Log-Odds)") +
  coord_cartesian(ylim = c(ymin_sym, ymax_sym)) +
  theme(legend.position = "right",
        legend.box.margin = ggplot2::margin(-10, -10, -10, -10)) +
  labs(x = "FNE")

# print
p2_voteintent_continuous

##### Appendix V - Sensitivity Analysis #####

##Now let's just remove the under 18s in 2018 to make sure it doesn't affect the sensitivity analyses

HungarianSurvey_fakenews_d<- subset(HungarianSurvey_fakenews_d, birthyear < 2001)


### EFFICACY ####

OLS_volunteer <- lm(volunteer~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                      turnout2018par,
                    data = HungarianSurvey_fakenews_d,
                    weights = HungarianSurvey_fakenews_d$weight)

OLS_volunteer

# runs sensemakr for sensitivity analysis
sensitivity_volunteer <- sensemakr(model = OLS_volunteer, 
                                   treatment = "EfficacyDummyHigh:PartyFidesz",
                                   benchmark_covariates = "turnout2018par",
                                   kd = 1:3)

# short description of results
sensitivity_volunteer
# long description of results
summary(sensitivity_volunteer)
# plot bias contour of point estimate
plot(sensitivity_volunteer)
# plot bias contour of t-value
plot(sensitivity_volunteer, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_volunteer, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_volunteer)


OLS_rally <- lm(rally~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                  turnout2018par,
                data = HungarianSurvey_fakenews_d,
                weights = HungarianSurvey_fakenews_d$weight)
OLS_rally

# runs sensemakr for sensitivity analysis
sensitivity_rally <- sensemakr(model = OLS_rally, 
                               treatment = "EfficacyDummyHigh:PartyFidesz",
                               benchmark_covariates = "turnout2018par",
                               kd = 1:3)

# short description of results
sensitivity_rally
# long description of results
summary(sensitivity_rally)
# plot bias contour of point estimate
plot(sensitivity_rally)
# plot bias contour of t-value
plot(sensitivity_rally, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_rally, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_rally)


OLS_campaignfollow <- lm(campaignfollow~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                           turnout2018par,
                         data = HungarianSurvey_fakenews_d,
                         weights = HungarianSurvey_fakenews_d$weight)
OLS_campaignfollow

# runs sensemakr for sensitivity analysis
sensitivity_campaignfollow <- sensemakr(model = OLS_campaignfollow, 
                                        treatment = "EfficacyDummyHigh:PartyFidesz",
                                        benchmark_covariates = "turnout2018par",
                                        kd = 1:3)

# short description of results
sensitivity_campaignfollow
# long description of results
summary(sensitivity_campaignfollow)
# plot bias contour of point estimate
plot(sensitivity_campaignfollow)
# plot bias contour of t-value
plot(sensitivity_campaignfollow, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_campaignfollow, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_campaignfollow)



OLS_voteintent <- lm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "EfficacyDummyHigh:PartyFidesz",
                                    benchmark_covariates = "turnout2018par",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)

#education
OLS_voteintent <- lm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "EfficacyDummyHigh:PartyFidesz",
                                    benchmark_covariates = "education_hu",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)


#ccaware

OLS_voteintent <- lm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "EfficacyDummyHigh:PartyFidesz",
                                    benchmark_covariates = "ccaware",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)



OLS_volunteer <- lm(volunteer~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                      turnout2018par,
                    data = HungarianSurvey_fakenews_d,
                    weights = HungarianSurvey_fakenews_d$weight)

OLS_volunteer

# runs sensemakr for sensitivity analysis
sensitivity_volunteer <- sensemakr(model = OLS_volunteer, 
                                   treatment = "EfficacyDummyHigh:PartyOpposition",
                                   benchmark_covariates = "turnout2018par",
                                   kd = 1:3)

# short description of results
sensitivity_volunteer
# long description of results
summary(sensitivity_volunteer)
# plot bias contour of point estimate
plot(sensitivity_volunteer)
# plot bias contour of t-value
plot(sensitivity_volunteer, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_volunteer, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_volunteer)


OLS_rally <- lm(rally~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                  turnout2018par,
                data = HungarianSurvey_fakenews_d,
                weights = HungarianSurvey_fakenews_d$weight)
OLS_rally

# runs sensemakr for sensitivity analysis
sensitivity_rally <- sensemakr(model = OLS_rally, 
                               treatment = "EfficacyDummyHigh:PartyOpposition",
                               benchmark_covariates = "turnout2018par",
                               kd = 1:3)

# short description of results
sensitivity_rally
# long description of results
summary(sensitivity_rally)
# plot bias contour of point estimate
plot(sensitivity_rally)
# plot bias contour of t-value
plot(sensitivity_rally, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_rally, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_rally)


OLS_campaignfollow <- lm(campaignfollow~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                           turnout2018par,
                         data = HungarianSurvey_fakenews_d,
                         weights = HungarianSurvey_fakenews_d$weight)
OLS_campaignfollow

# runs sensemakr for sensitivity analysis
sensitivity_campaignfollow <- sensemakr(model = OLS_campaignfollow, 
                                        treatment = "EfficacyDummyHigh:PartyOpposition",
                                        benchmark_covariates = "turnout2018par",
                                        kd = 1:3)

# short description of results
sensitivity_campaignfollow
# long description of results
summary(sensitivity_campaignfollow)
# plot bias contour of point estimate
plot(sensitivity_campaignfollow)
# plot bias contour of t-value
plot(sensitivity_campaignfollow, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_campaignfollow, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_campaignfollow)



OLS_voteintent <- lm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "EfficacyDummyHigh:PartyOpposition",
                                    benchmark_covariates = "turnout2018par",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)


##what about education?
OLS_voteintent <- lm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "EfficacyDummyHigh:PartyOpposition",
                                    benchmark_covariates = "education_hu",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)

##ccaware
OLS_voteintent <- lm(voteintent~EfficacyDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "EfficacyDummyHigh:PartyOpposition",
                                    benchmark_covariates = "ccaware",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)

### Ubiquity ####

OLS_volunteer <- lm(volunteer~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                      turnout2018par,
                    data = HungarianSurvey_fakenews_d,
                    weights = HungarianSurvey_fakenews_d$weight)

OLS_volunteer

# runs sensemakr for sensitivity analysis
sensitivity_volunteer <- sensemakr(model = OLS_volunteer, 
                                   treatment = "UbiquityDummyHigh:PartyFidesz",
                                   benchmark_covariates = "turnout2018par",
                                   kd = 1:3)

# short description of results
sensitivity_volunteer
# long description of results
summary(sensitivity_volunteer)
# plot bias contour of point estimate
plot(sensitivity_volunteer)
# plot bias contour of t-value
plot(sensitivity_volunteer, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_volunteer, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_volunteer)


OLS_rally <- lm(rally~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                  turnout2018par,
                data = HungarianSurvey_fakenews_d,
                weights = HungarianSurvey_fakenews_d$weight)
OLS_rally

# runs sensemakr for sensitivity analysis
sensitivity_rally <- sensemakr(model = OLS_rally, 
                               treatment = "UbiquityDummyHigh:PartyFidesz",
                               benchmark_covariates = "turnout2018par",
                               kd = 1:3)

# short description of results
sensitivity_rally
# long description of results
summary(sensitivity_rally)
# plot bias contour of point estimate
plot(sensitivity_rally)
# plot bias contour of t-value
plot(sensitivity_rally, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_rally, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_rally)


OLS_campaignfollow <- lm(campaignfollow~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                           turnout2018par,
                         data = HungarianSurvey_fakenews_d,
                         weights = HungarianSurvey_fakenews_d$weight)
OLS_campaignfollow

# runs sensemakr for sensitivity analysis
sensitivity_campaignfollow <- sensemakr(model = OLS_campaignfollow, 
                                        treatment = "UbiquityDummyHigh:PartyFidesz",
                                        benchmark_covariates = "turnout2018par",
                                        kd = 1:3)

# short description of results
sensitivity_campaignfollow
# long description of results
summary(sensitivity_campaignfollow)
# plot bias contour of point estimate
plot(sensitivity_campaignfollow)
# plot bias contour of t-value
plot(sensitivity_campaignfollow, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_campaignfollow, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_campaignfollow)



OLS_voteintent <- lm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "UbiquityDummyHigh:PartyFidesz",
                                    benchmark_covariates = "turnout2018par",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)

#education
OLS_voteintent <- lm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "UbiquityDummyHigh:PartyFidesz",
                                    benchmark_covariates = "education_hu",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)


#ccaware

OLS_voteintent <- lm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "UbiquityDummyHigh:PartyFidesz",
                                    benchmark_covariates = "ccaware",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)



OLS_volunteer <- lm(volunteer~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                      turnout2018par,
                    data = HungarianSurvey_fakenews_d,
                    weights = HungarianSurvey_fakenews_d$weight)

OLS_volunteer

# runs sensemakr for sensitivity analysis
sensitivity_volunteer <- sensemakr(model = OLS_volunteer, 
                                   treatment = "UbiquityDummyHigh:PartyOpposition",
                                   benchmark_covariates = "turnout2018par",
                                   kd = 1:3)

# short description of results
sensitivity_volunteer
# long description of results
summary(sensitivity_volunteer)
# plot bias contour of point estimate
plot(sensitivity_volunteer)
# plot bias contour of t-value
plot(sensitivity_volunteer, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_volunteer, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_volunteer)


OLS_rally <- lm(rally~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                  turnout2018par,
                data = HungarianSurvey_fakenews_d,
                weights = HungarianSurvey_fakenews_d$weight)
OLS_rally

# runs sensemakr for sensitivity analysis
sensitivity_rally <- sensemakr(model = OLS_rally, 
                               treatment = "UbiquityDummyHigh:PartyOpposition",
                               benchmark_covariates = "turnout2018par",
                               kd = 1:3)

# short description of results
sensitivity_rally
# long description of results
summary(sensitivity_rally)
# plot bias contour of point estimate
plot(sensitivity_rally)
# plot bias contour of t-value
plot(sensitivity_rally, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_rally, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_rally)


OLS_campaignfollow <- lm(campaignfollow~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                           turnout2018par,
                         data = HungarianSurvey_fakenews_d,
                         weights = HungarianSurvey_fakenews_d$weight)
OLS_campaignfollow

# runs sensemakr for sensitivity analysis
sensitivity_campaignfollow <- sensemakr(model = OLS_campaignfollow, 
                                        treatment = "UbiquityDummyHigh:PartyOpposition",
                                        benchmark_covariates = "turnout2018par",
                                        kd = 1:3)

# short description of results
sensitivity_campaignfollow
# long description of results
summary(sensitivity_campaignfollow)
# plot bias contour of point estimate
plot(sensitivity_campaignfollow)
# plot bias contour of t-value
plot(sensitivity_campaignfollow, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_campaignfollow, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_campaignfollow)



OLS_voteintent <- lm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "UbiquityDummyHigh:PartyOpposition",
                                    benchmark_covariates = "turnout2018par",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)


##what about education?
OLS_voteintent <- lm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "UbiquityDummyHigh:PartyOpposition",
                                    benchmark_covariates = "education_hu",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)

##ccaware
OLS_voteintent <- lm(voteintent~UbiquityDummy*Party + education_hu +ccaware+democ_sat+ politicalloyal + householdincome + agegroup + gender +
                       turnout2018par,
                     data = HungarianSurvey_fakenews_d,
                     weights = HungarianSurvey_fakenews_d$weight)
OLS_voteintent

# runs sensemakr for sensitivity analysis
sensitivity_voteintent <- sensemakr(model = OLS_voteintent, 
                                    treatment = "UbiquityDummyHigh:PartyOpposition",
                                    benchmark_covariates = "ccaware",
                                    kd = 1:3)

# short description of results
sensitivity_voteintent
# long description of results
summary(sensitivity_voteintent)
# plot bias contour of point estimate
plot(sensitivity_voteintent)
# plot bias contour of t-value
plot(sensitivity_voteintent, sensitivity.of = "t-value")
# plot extreme scenario
plot(sensitivity_voteintent, type = "extreme")
# latex code for sensitivity table
ovb_minimal_reporting(sensitivity_voteintent)
