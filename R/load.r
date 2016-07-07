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
library(reshape2)


# No need to run unless data has changed

ds_init <- PROMISE::PROMISE_data %>%
  filter(UDBP < 50000)

ds <- ds_init %>% 
  mutate(UDBP = ifelse(UDBP>0 & UDBP<1.23, 0.62, UDBP),
         Ethnicity = as.character(Ethnicity),
         isAfrican = ifelse(Ethnicity == 'African', 1, 0), 
         Ethnicity = ifelse(Ethnicity %in% c('African', 'First Nations', 'Other'), 'Other', Ethnicity),
         fVN = factor(VN, levels=c(1, 3, 6), labels = c("Baseline", "3Year", "6Year"), ordered = TRUE), 
         fMedsBP = factor(MedsBloodPressure, levels = c(0, 1),
                          labels = c("No", "Yes"), ordered = TRUE), 
         dm_status = ifelse(DM ==1, 'DM',
                            ifelse(IFG == 1 | IGT == 1, 'Prediabetes',
                                   'NGT')),
         mcr_status = ifelse(MicroalbCreatRatio < 2, 'Normal',
                             ifelse(MicroalbCreatRatio > 20, 'Macroalbuminuria',
                                    'Microalbuminuria')),
         creat.mgdl = Creatinine * 0.011312,
         eGFR = CKDEpi.creat(creat.mgdl, as.numeric(Sex)-1, Age, isAfrican),
         eGFR_status = ifelse(eGFR>=90 & eGFR<=125, 'Normal',
                              ifelse(eGFR >= 60 & eGFR < 90, 'Mild',
                                     ifelse(eGFR>125, 'Hyperfiltration',
                                            'Moderate'))),
         UDBP_status = ifelse(UDBP == 0, 'Undetected',
                              ifelse(UDBP < 1.23, 'Low',
                                     ifelse(UDBP > 60, 'High', 'Normal'))),
         udbpCrRatio = UDBP/Creatinine,
         CaCrRatio = UrinaryCalcium/UrineCreatinine) %>% 
  filter(eGFR<200) %>% 
  filter(Creatinine<200) %>% 
  filter(!(SID == 1131 & VN == 1)) %>%
  filter(!(SID == 1444 & VN == 6)) %>%
  filter(!(SID == 2042 & VN == 3)) %>%
  filter(!(SID == 2124 & VN == 1)) %>%
  filter(!(SID == 3025 & VN == 6)) %>%
  filter(!(SID == 4016 & VN == 1)) %>%
  mutate(eGFR_status=factor(eGFR_status, 
                            levels=c('Normal', 'Mild', 'Moderate', 'Hyperfiltration'), 
                            ordered=TRUE),
         mcr_status = factor(mcr_status,
                             levels = c("Normal", "Microalbuminuria", "Macroalbuminuria"),
                             ordered=TRUE),
         dm_status=factor(dm_status, 
                          levels=c('NGT', 'Prediabetes', 'DM'), 
                          ordered=TRUE),
         UDBP_status=factor(UDBP_status, 
                            levels=c('Undetected', 'Low', 'Normal', 'High'), 
                            ordered=TRUE)) %>%
  select(SID, BMI, Waist, Age, Sex, Ethnicity, VN, fVN, 
       Glucose0, Glucose120, DM, IFG, IGT, 
       dm_status, mcr_status, eGFR_status, UDBP_status,
       MicroalbCreatRatio, eGFR, UrineMicroalbumin, UrineCreatinine, Creatinine,  
       UDBP, udbpCrRatio, VitaminD,
       MeanArtPressure, Systolic, Diastolic, PTH, ALT,
       CaCrRatio, UrinaryCalcium, matches("meds"), SmokeCigs, CRP)

#####################################################
# UDBP with only three groups (low group not sub-divided)
ds_UDBP3 <- ds %>% 
  mutate(UDBP_status = ifelse(UDBP < 1.23, 'Low',
                              ifelse(UDBP > 60, 'High', 'Normal'))) %>% 
  mutate(UDBP_status=factor(UDBP_status,
                            levels=c('Low', 'Normal', 'High'),
                            ordered=TRUE)) %>% 
  arrange(UDBP_status)

###################################################
# Save the data
saveRDS(ds, file='ds.Rds')
