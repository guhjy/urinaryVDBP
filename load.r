#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################

# No need to run unless data has changed

ds_init <- PROMISE::PROMISE_data %>%
  filter(VN == 1, UDBP < 5000)

ds <- ds_init %>% 
  mutate(UDBP = ifelse(UDBP <= 1.23, 0.62, UDBP),
         Ethnicity = as.character(Ethnicity),
         isAfrican = ifelse(Ethnicity == 'African', 1, 0), 
         Ethnicity = ifelse(Ethnicity %in% c('African', 'First Nations', 'Other'), 'Other', Ethnicity),
         SexNum = ifelse(Sex == 'F', 0, 1), 
         dm_status = ifelse(DM ==1, 'DM',
                            ifelse(IFG == 1, 'IFG',
                                   ifelse(IGT == 1, 'IGT',
                                          'NGT'))),
         mcr_status = ifelse(MicroalbCreatRatio < 2, 'normal',
                             ifelse(MicroalbCreatRatio > 20, 'macroalbuminuria',
                                    'microalbuminuria')),
         creat.mgdl = Creatinine * 0.011312,
         eGFR = CKDEpi.creat(creat.mgdl, SexNum, Age, isAfrican),
         eGFR_status = ifelse(eGFR>=90 & eGFR<150, 'normal',
                              ifelse(eGFR >= 60 & eGFR < 90, 'mild',
                                     'moderate')),
         UDBP_status = ifelse(UDBP < 1.23, 'low',
                              ifelse(UDBP > 60, 'high',
                                     'normal')),
         UDBP_cr = UDBP/Creatinine,
         UDBP_ln = log(UDBP),
         UDBP_cr_ln = log(UDBP_cr),
         VitaminD_ln = log(VitaminD),
         eGFR_ln = log(eGFR),
         mcr_ln = log(MicroalbCreatRatio)) %>% 
  # filter(eGFR < 200) %>% 
  select(SID, BMI, Waist, Age, Sex, Ethnicity, 
         Glucose0, Glucose120, 
         dm_status, mcr_status, eGFR_status, UDBP_status,
         MicroalbCreatRatio, eGFR, UrineMicroalbumin, UrineCreatinine, Creatinine,  
         UDBP, UDBP_cr, UDBP_ln, UDBP_cr_ln, 
         eGFR_ln, mcr_ln, 
         VitaminD, VitaminD_ln,
         SmokeCigs, MeanArtPressure, Systolic, Diastolic)

saveRDS(ds, file='ds.Rds')
