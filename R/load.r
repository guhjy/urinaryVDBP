#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################
install.packages(c("tidyverse", "neprho", "dplyr", "msm"))

library(nephro)
library(carpenter)
library(ggplot2)
library(knitr)
library(plyr)
library(dplyr)
library(tidyr)
library(pander)
library(captioner)
library(knitr)
library(mason)
library(msm)
library(gee)
library(magrittr)
library(lme4)
library(lmerTest)
#library(geepack)


# No need to run unless data has changed

ds <- PROMISE::PROMISE %>%
  dplyr::filter(UDBP < 10000) %>% 
  dplyr::mutate(UDBP = ifelse(UDBP == 0.01, NA, UDBP),
         UrineCreatinine = ifelse(SID == 2028, 9, UrineCreatinine),
         ACR = round(UrineMicroalbumin/UrineCreatinine, digits = 2),
         Ethnicity = as.character(Ethnicity),
         isAfrican = ifelse(Ethnicity == 'African', 1, 0), 
         Ethnicity = ifelse(Ethnicity %in% c('African', 'First Nations', 'Other'), 'Other', Ethnicity),
         fVN = factor(VN, levels=c(1, 3, 6), labels = c("Baseline", "3Year", "6Year"), ordered = TRUE), 
         fMedsBP = factor(MedsBloodPressure, levels = c(0, 1),
                          labels = c("No", "Yes"), ordered = TRUE), 
         dm_status = ifelse(DM ==1, 'DM',
                            ifelse(IFG == 1 | IGT == 1, 'Prediabetes',
                                   'NGT')),
         acr_status = ifelse(ACR < 2, 'Normoalbuminuria',
                             ifelse(ACR > 20, 'Macroalbuminuria',
                                    "Microalbuminuria")),
         acr_status2 = ifelse(acr_status == "Normoalbuminuria", "Normoalbuminuria",
                              "Albuminuria"),
         creat.mgdl = Creatinine * 0.011312,
         eGFR = nephro::CKDEpi.creat(creat.mgdl, as.numeric(Sex)-1, Age, isAfrican),
         eGFR_status = ifelse(eGFR>=90, 'Normal',
                              ifelse(eGFR >= 60 & eGFR < 90, 'Mild',
                                     'Moderate')),
         UDBP_status = ifelse(UDBP < 1.23, 'Trace',
                              ifelse(UDBP > 60, 'High', 'Normal')),
         udbpCrRatio = UDBP/UrineCreatinine)%>% 
  dplyr::filter(eGFR<200) %>% 
  dplyr::filter(Creatinine<200) %>% 
  dplyr::filter(!(SID == 1131 & VN == 1)) %>%
  dplyr::filter(!(SID == 1444 & VN == 6)) %>%
  dplyr::filter(!(SID == 2042 & VN == 3)) %>%
  dplyr::filter(!(SID == 2124 & VN == 1)) %>%
  dplyr::filter(!(SID == 3025 & VN == 6)) %>%
  dplyr::filter(!(SID == 4016 & VN == 1)) %>%
  dplyr::mutate(eGFR_status=factor(eGFR_status, 
                            levels = c('Normal', 'Mild', 'Moderate'), 
                            ordered = TRUE),
         acr_status = factor(acr_status,
                             levels = c("Normoalbuminuria", "Microalbuminuria", "Macroalbuminuria"),
                             ordered = TRUE),
         acr_status2 = factor(acr_status2,
                              levels = c("Normoalbuminuria", "Albuminuria"),
                              ordered = TRUE),
         dm_status = factor(dm_status, 
                          levels = c('NGT', 'Prediabetes', 'DM'), 
                          ordered = TRUE),
         UDBP_status = factor(UDBP_status, 
                            levels = c('Trace', 'Normal', 'High'), 
                            ordered = TRUE)) %>% 
  dplyr::select(SID, BMI, Waist, Age, Sex, Ethnicity, VN, fVN,
       Glucose0, Glucose120, DM, IFG, IGT,
       dm_status, acr_status, acr_status2, eGFR_status, UDBP_status,
       MicroalbCreatRatio, eGFR, UrineMicroalbumin, UrineCreatinine, Creatinine, ACR,
       UDBP, udbpCrRatio, VitaminD,
       MeanArtPressure, Systolic, Diastolic, PTH, ALT,
       UrinaryCalcium, fMedsBP, SmokeCigs, Canoe, dplyr::matches("meds"))

## 
  
# Progression Data =================================================================================

dsProgress <- ds %>% 
  dplyr::select(SID, VN, acr_status, eGFR_status, UDBP_status) %>% 
  

# Categorize acr status =============================fix this up so ugly============================

ds_VN1 <- ds %>% 
  dplyr::filter(VN == 1) %>% 
  dplyr::mutate(acr_VN1 = ifelse(acr_status == "Normoalbuminuria", 0,
                                  ifelse(acr_status == "Microalbuminuria", 1,
                                         3))) %>% 
  select(SID, acr_VN1)

ds <- plyr::join(ds, ds_VN1, type = "full")

ds_VN3 <- ds %>% 
  dplyr::filter(VN == 3) %>% 
  dplyr::mutate(acr_VN3 = ifelse(acr_status == "Normoalbuminuria", 0,
                                  ifelse(acr_status == "Microalbuminuria", 1,
                                         3))) %>% 
  select(SID, acr_VN3)

ds <- plyr::join(ds, ds_VN3, type = "full")

ds_VN6 <- ds %>% 
  dplyr::filter(VN == 6) %>% 
  dplyr::mutate(acr_VN6 = ifelse(acr_status == "Normoalbuminuria", 0,
                                  ifelse(acr_status == "Microalbuminuria", 1,
                                         3))) %>% 
  select(SID, acr_VN6)

ds <- plyr::join(ds, ds_VN6, type = "full")


# Make variable that shows progression ---------------------fix------------------------------------

# ds <- ds %>%
#   dplyr::mutate(acrDiff1_3 = acr_VN3 - acr_VN1,
#                 acrDiff3_6 = acr_VN6 - acr_VN3,
#                 acrProg1_3 = ifelse(acrDiff1_3 == 0, "No change",
#                                     ifelse(acrDiff1_3 == 1 | acrDiff1_3 == 2 | acrDiff1_3 == 3,
#                                            "Progress", "Regress")),
#                 acrProg3_6 = ifelse(acrDiff3_6 == 0, "No change",
#                                     ifelse(acrDiff3_6 == 1 | acrDiff3_6 == 2 | acrDiff3_6 == 3,
#                                            "Progress", "Regress"))) %>%
#   mutate(n = ifelse(is.na(acr_VN1) & !is.na(acr_VN3), 1,
#                     ifelse(is.na(acr_VN1) & !is.na(acr_VN6), 1, 0)))

## Key
## 0 = no change
## 1 = normo to micro
## 2 = micro to macro
## 3 = normo to macro
## -1 = micro to normo
## -2 = macro to micro
## -3 = macro to normo

ds <- ds %>% 
  dplyr::mutate(acrDiff1_3 = acr_VN3 - acr_VN1,
                acrDiff3_6 = acr_VN6 - acr_VN3,
                acrProg1_3 = ifelse(acrDiff1_3 == 0, "No change",
                                    ifelse(acrDiff1_3 == 1, "Norm to Micro", 
                                           ifelse(acrDiff1_3 == 2, "Micro to Macro",
                                                  ifelse(acrDiff1_3 == 3, "Norm to Macro",
                                                         ifelse(acrDiff1_3 == -1, "Micro to Norm",
                                                                ifelse(acrDiff1_3 == -2, "Macro to Micro",
                                                                       "Macro to Norm")))))),
                acrProg3_6 = ifelse(acrDiff3_6 == 0, "No change",
                                    ifelse(acrDiff3_6 == 1, "Norm to Micro", 
                                           ifelse(acrDiff3_6 == 2, "Micro to Macro",
                                                  ifelse(acrDiff3_6 == 3, "Norm to Macro",
                                                         ifelse(acrDiff3_6 == -1, "Micro to Norm",
                                                                ifelse(acrDiff3_6 == -2, "Macro to Micro",
                                                                       "Macro to Norm")))))),
                acrProgress1 = ifelse(acrDiff1_3 == 0, "No change",
                                      ifelse(acrDiff1_3 == 1 | acrDiff1_3 == 2 | acrDiff1_3 == 3,
                                             "Progress", "Regress")),
                acrProgress2 = ifelse(acrDiff3_6 == 0, "No change",
                                    ifelse(acrDiff3_6 == 1 | acrDiff3_6 == 2 | acrDiff3_6 == 3,
                                           "Progress", "Regress")),
                acrProgress = ifelse(acrProgress1 == "Progress" | acrProgress2 == "Progress",
                                     "Progress",
                                     ifelse(acrProgress1 == "Regress" | acrProgress2 == "Regress",
                                            "Regress", "No change"))) %>% 
  dplyr::mutate(acrProg1_3 = factor(acrProg1_3,
                                    levels = c("No change", 
                                               "Norm to Micro", "Micro to Macro",
                                               "Norm to Macro",
                                               "Micro to Norm", "Macro to Micro",
                                               "Macro to Norm"),
                                    ordered = TRUE),
                acrProg3_6 = factor(acrProg3_6,
                                    levels = c("No change", 
                                               "Norm to Micro", "Micro to Macro",
                                               "Norm to Macro",
                                               "Micro to Norm", "Macro to Micro",
                                               "Macro to Norm"),
                                    ordered = TRUE),
                acrProgress = factor(acrProgress,
                                     levels = c("No change",
                                                "Progress",
                                                "Regress"),
                                     ordered = TRUE))

## To find how many people have missing baseline but subsequent follow up, use
## `sum(ds$n/3)`

rm(ds_VN1)
rm(ds_VN3)
rm(ds_VN6)

  
# Save the data ===================================================================================

saveRDS(ds, file='data/ds.Rds')


ds_old <- PROMISE::PROMISE_data
saveRDS(ds, file='data/ds_old.Rds')
