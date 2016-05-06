#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################
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
                             ifelse(ds$MicroalbCreatRatio > 20, 'macroalbuminuria',
                                    'microalbuminuria')),
         creat.mgdl = Creatinine * 0.011312,
         eGFR = CKDEpi.creat(creat.mgdl, SexNum, Age, isAfrican),
         eGFR_status = ifelse(eGFR < 60, 'moderate',
                              ifelse(eGFR >= 60 & eGFR < 90, 'mild',
                                     'normal')),
         UDBP_status = ifelse(ds$UDBP < 1.23, 'low',
                              ifelse(ds$UDBP > 60, 'high',
                                     'normal')),
         UDBP_cr = UDBP/Creatinine,
         UDBP_ln = log(UDBP),
         UDBP_cr_ln = log(UDBP_cr),
         VitaminD_ln = log(VitaminD)) %>% 
  select(SID, BMI, Age, Sex, Ethnicity,
         dm_status, mcr_status, eGFR_status, UDBP_status,
         UDBP, UDBP_cr, UDBP_ln, UDBP_cr_ln,
         VitaminD, VitaminD_ln)

saveRDS(ds, file='ds.Rds')
