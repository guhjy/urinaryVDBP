#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################

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


# No need to run unless data has changed

ds <- PROMISE::PROMISE_data %>%
  mutate(UDBP = ifelse(UDBP>0 & UDBP<1.23, 0.62,
                       ifelse(UDBP == 0, 0.01,
                              UDBP)),
         Ethnicity = as.character(Ethnicity),
         isAfrican = ifelse(Ethnicity == 'African', 1, 0), 
         Ethnicity = ifelse(Ethnicity %in% c('African', 'First Nations', 'Other'), 'Other', Ethnicity),
         fVN = factor(VN, levels=c(1, 3, 6), labels = c("Baseline", "3Year", "6Year"), ordered = TRUE), 
         fMedsBP = factor(MedsBloodPressure, levels = c(0, 1),
                          labels = c("No", "Yes"), ordered = TRUE), 
         dm_status = ifelse(DM ==1, 'DM',
                            ifelse(IFG == 1 | IGT == 1, 'Prediabetes',
                                   'NGT')),
         mcr_status = ifelse(MicroalbCreatRatio < 2, 'Normoalbuminuria',
                             ifelse(MicroalbCreatRatio > 20, 'Macroalbuminuria',
                                    'Microalbuminuria')),
         creat.mgdl = Creatinine * 0.011312,
         eGFR = CKDEpi.creat(creat.mgdl, as.numeric(Sex)-1, Age, isAfrican),
         eGFR_status = ifelse(eGFR>=90, 'Normal',
                              ifelse(eGFR >= 60 & eGFR < 90, 'Mild',
                                     'Moderate')),
         UDBP_status = ifelse(UDBP == 0.01, 'Undetected',
                              ifelse(UDBP < 1.23, 'Low',
                                     ifelse(UDBP > 60, 'High', 'Normal'))),
         udbpCrRatio = UDBP/UrineCreatinine,
         CaCrRatio = UrinaryCalcium/UrineCreatinine) %>% 
  dplyr::mutate(eGFR_status=factor(eGFR_status, 
                            levels = c('Normal', 'Mild', 'Moderate'), 
                            ordered = TRUE),
         mcr_status = factor(mcr_status,
                             levels = c("Normoalbuminuria", "Microalbuminuria", "Macroalbuminuria"),
                             ordered = TRUE),
         dm_status = factor(dm_status, 
                          levels = c('NGT', 'Prediabetes', 'DM'), 
                          ordered = TRUE),
         UDBP_status = factor(UDBP_status, 
                            levels = c('Undetected', 'Low', 'Normal', 'High'), 
                            ordered = TRUE),
         Ethnicity = factor(Ethnicity,
                            levels = c("European", "Latino/a", "South Asian", "Other"),
                            ordered = TRUE)) %>%
  dplyr::select(SID, BMI, Waist, Age, Sex, Ethnicity, VN, fVN,
       Glucose0, Glucose120, DM, IFG, IGT, 
       dm_status, mcr_status, eGFR_status, UDBP_status,
       MicroalbCreatRatio, eGFR, UrineMicroalbumin, UrineCreatinine, Creatinine,  
       UDBP, udbpCrRatio, VitaminD,
       MeanArtPressure, Systolic, Diastolic, PTH, ALT,
       CaCrRatio, UrinaryCalcium, matches("meds"), SmokeCigs)

# Categorize mcr status =============================fix this up so ugly============================

ds_VN1 <- ds %>% 
  dplyr::filter(VN == 1) %>% 
  dplyr::mutate(mcr_VN1 = ifelse(mcr_status == "Normoalbuminuria", 0,
                                  ifelse(mcr_status == "Microalbuminuria", 1,
                                         3))) %>% 
  select(SID, mcr_VN1)

ds <- plyr::join(ds, ds_VN1, type = "full")

ds_VN3 <- ds %>% 
  dplyr::filter(VN == 3) %>% 
  dplyr::mutate(mcr_VN3 = ifelse(mcr_status == "Normoalbuminuria", 0,
                                  ifelse(mcr_status == "Microalbuminuria", 1,
                                         3))) %>% 
  select(SID, mcr_VN3)

ds <- plyr::join(ds, ds_VN3, type = "full")

ds_VN6 <- ds %>% 
  dplyr::filter(VN == 6) %>% 
  dplyr::mutate(mcr_VN6 = ifelse(mcr_status == "Normoalbuminuria", 0,
                                  ifelse(mcr_status == "Microalbuminuria", 1,
                                         3))) %>% 
  select(SID, mcr_VN6)

ds <- plyr::join(ds, ds_VN6, type = "full")


# Make variable that shows progression ---------------------fix------------------------------------

ds <- ds %>% 
  dplyr::mutate(mcrDiff1_3 = mcr_VN3 - mcr_VN1,
                mcrDiff3_6 = mcr_VN6 - mcr_VN3,
                mcrProg1_3 = ifelse(mcrDiff1_3 == 0, "No change",
                                    ifelse(mcrDiff1_3 == 1 | mcrDiff1_3 == 2 | mcrDiff1_3 == 3,
                                           "Progress", "Regress")),
                mcrProg3_6 = ifelse(mcrDiff3_6 == 0, "No change",
                                    ifelse(mcrDiff3_6 == 1 | mcrDiff3_6 == 2 | mcrDiff3_6 == 3,
                                           "Progress", "Regress")))

## Key
## 0 = no change
## 1 = normo to micro
## 2 = micro to macro
## 3 = normo to macro
## -1 = micro to normo
## -2 = macro to micro
## -3 = macro to normo




  
# Save the data ===================================================================================

saveRDS(ds, file='data/ds.Rds')
