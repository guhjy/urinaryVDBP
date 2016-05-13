#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################

# No need to run unless data has changed

ds_init <- PROMISE::PROMISE_data %>%
  filter(VN == 1, UDBP < 5000) #UDBP < 5000

ds <- ds_init %>% 
  mutate(UDBP = ifelse(UDBP <= 1.23, 0.62, UDBP),
         Ethnicity = as.character(Ethnicity),
         isAfrican = ifelse(Ethnicity == 'African', 1, 0), 
         Ethnicity = ifelse(Ethnicity %in% c('African', 'First Nations', 'Other'), 'Other', Ethnicity),
         SexNum = ifelse(Sex == 'F', 0, 1), 
         dm_status = ifelse(DM ==1, 'DM',
                            ifelse(IFG == 1 | IGT == 1, 'Prediabetes',
                                   'NGT')),
         mcr_status = ifelse(MicroalbCreatRatio < 2, 'Normal',
                             ifelse(MicroalbCreatRatio > 20, 'Macroalbuminuria',
                                    'Microalbuminuria')),
         creat.mgdl = Creatinine * 0.011312,
         eGFR = CKDEpi.creat(creat.mgdl, SexNum, Age, isAfrican),
         eGFR_status = ifelse(eGFR>=90 & eGFR<=125, 'Normal',
                              ifelse(eGFR >= 60 & eGFR < 90, 'Mild',
                                     ifelse(eGFR>125, 'Hyperfiltration',
                                            'Moderate'))),
         UDBP_status = ifelse(UDBP < 1.23, 'Low',
                              ifelse(UDBP > 60, 'High',
                                     'Normal')),
         UDBP_cr = UDBP/Creatinine,
         UDBP_ln = log(UDBP),
         UDBP_cr_ln = log(UDBP_cr),
         VitaminD_ln = log(VitaminD),
         eGFR_ln = log(eGFR),
         mcr_ln = log(MicroalbCreatRatio)) %>% 
  filter(eGFR < 200) %>%
  mutate(eGFR_status=factor(eGFR_status, 
                            levels=c('Normal', 'Mild', 'Moderate', 'Hyperfiltration'), 
                            ordered=TRUE)) %>%
  arrange(eGFR_status) %>% 
  mutate(dm_status=factor(dm_status, 
                          levels=c('NGT', 'Prediabetes', 'DM'), 
                          ordered=TRUE)) %>%
  arrange(dm_status) %>% 
  mutate(UDBP_status=factor(UDBP_status, 
                            levels=c('Low', 'Normal', 'High'), 
                            ordered=TRUE)) %>%
  arrange(UDBP_status) %>% 
  select(SID, BMI, Waist, Age, Sex, Ethnicity, 
         Glucose0, Glucose120, 
         dm_status, mcr_status, eGFR_status, UDBP_status,
         MicroalbCreatRatio, eGFR, UrineMicroalbumin, UrineCreatinine, Creatinine,  
         UDBP, UDBP_cr, UDBP_ln, UDBP_cr_ln, 
         eGFR_ln, mcr_ln, 
         VitaminD, VitaminD_ln,
         SmokeCigs, MeanArtPressure, Systolic, Diastolic, PTH)

saveRDS(ds, file='ds.Rds')
