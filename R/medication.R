# GOAL: CATEGORIZE BLOOD PRESSURE DATA INTO SPECIFIC TYPES


molded.meds <- mold_fetch_data('form121med') %>% 
  mold_meds_diabetes() %>% 
  mold_meds_blood_glucose() %>% 
  mold_meds_blood_pressure() %>% 
  mold_meds_lipids_chol() %>% 
  mold_fix_date('StartDate') %>% 
  mold_fix_date('StopDate')
#> These are the datasets that will be imported: 
#> - C:/Users/Hanley/Documents/PROMISE/promise/inst/extdata/form121med.Rds

data.Medications <- mold_meds_usage(molded.meds) %>%  
  check_duplicates(c('SID', 'VN')) %>% 
  check_glance_at_data()
#> visit.date.dataset is defaulting to form013base.
#> These are the datasets that will be imported: 
#> - C:/Users/Hanley/Documents/PROMISE/promise/inst/extdata/form013base.Rds
#> There are 3 duplicates in this dataset.
#> ... Removed last duplicate.
#> These are the datasets that will be imported: 
#> - C:/Users/Hanley/Documents/PROMISE/promise/inst/extdata/form121.Rds
#> There are 436 duplicates in this dataset.
#> ... Removed last duplicate.
#> There are no duplicate rows.
#> Observations: 752
#> Variables: 8
#> $ AID               (int) 2, 2, 2, 3, 3, 3, 6, 6, 14, 14, 14, 14, 15, ...
#> $ SID               (int) 1002, 1002, 1002, 1003, 1003, 1003, 1006, 10...
#> $ VN                (int) 3, 6, 9, 6, 9, 16, 3, 6, 1, 3, 6, 9, 1, 3, 1...
#> $ VisitCount        (dbl) 2, 3, 4, 4, 5, 3, 2, 3, 1, 2, 3, 4, 1, 2, 1,...
#> $ MedsBloodPressure (dbl) 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0,...
#> $ MedsDiabetes      (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
#> $ MedsGlucose       (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
#> $ MedsLipidsChol    (dbl) 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1,...

data.PreFinal <- mold_combine_datasets(data.PreFinal, data.Medications)
#> There are 2 datasets to combine. Combining data.PreFinal, data.Medications together.