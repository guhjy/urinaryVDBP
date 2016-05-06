#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################
library(dplyr)
library(nephro)

ds <- PROMISE::PROMISE_data %>%
  filter(VN == 1, UDBP < 5000)

ds2 <- ds %>% 
  mutate(UDBP = ifelse(UDBP <= 1.23, 0.62, UDBP),
         Ethnicity = as.character(Ethnicity),
         Ethnicity = ifelse(Ethnicity %in% c('African', 'First Nations', 'Other'), 'Other', Ethnicity),
         dm_status = ifelse(DM ==1, 'DM',
                            ifelse(IFG == 1, 'IFG',
                                   ifelse(IGT == 1, 'IGT',
                                          'NGT')))) %>% 
  select(SID, BMI, UDBP, dm_status, Ethnicity)



ds$UDBP[ds$UDBP <= 1.23] <- 0.62 # account for below detection limit

# Condense ethnicity to be only European, South Asian, Latino, Other
ds$group_ethn <- ifelse(
  ds$Ethnicity == 'South Asian', 'South Asian',
  ifelse(ds$Ethnicity == 'European', 'European',
          ifelse(ds$Ethnicity == 'Latino/a', 'Latino',
                'Other')))

# Create new column with DIABETIC STATUS
# ds$dm_status <- ifelse(
#   ds$DM == 1, 'DM',
#   ifelse(ds$IFG == 1 | ds$IGT == 1, 'prediabetes',
#          'normal'))

ds$dm_status <- ifelse(
  ds$DM == 1, 'DM',
  ifelse(ds$IFG == 1, 'IFG',
         ifelse(ds$IGT == 1, 'IGT',
                'normal')))

# Create new column with MICROALBUMIN/CREATININE RATIO
ds$mcr_status <- ifelse(ds$MicroalbCreatRatio < 2, 'normal',
                        ifelse(ds$MicroalbCreatRatio > 20, 'macroalbuminuria',
                               'microalbuminuria'))
ds$mcr_status <- as.factor(ds$mcr_status)

# Create new colum with eGFR value
ds$creat.mgdl <- ds$Creatinine * 0.011312 # Convert serum creatinine units to mg/dL
ds$isAfrican <- ifelse(ds$Ethnicity == 'African', 1, 0) # conditions for function
ds$SexNum <- ifelse(ds$Sex == 'F', 0, 1) # conditions for function

ds$eGFR <- CKDEpi.creat(ds$creat.mgdl, ds$SexNum, ds$Age, ds$isAfrican) # mL/min/1.73m^2

# Create new column with eGFR STATUS
ds$eGFR_status <- ifelse(ds$eGFR < 60, 'moderate',
                         ifelse(ds$eGFR >= 60 & ds$eGFR < 90, 'mild',
                                'normal'))

# Create new column with UDBP_status
ds$UDBP_status <- ifelse(ds$UDBP < 1.23, 'low',
                         ifelse(ds$UDBP > 60, 'high',
                                'normal'))

# Create new column with normalized UDBP (UDBP:Cr)
ds$UDBP_cr <- ds$UDBP / ds$UrineCreatinine

# # Transforming UDBP using natural log
ds$UDBP_ln <- log(ds$UDBP)

# Transform UDBP:Cr using natural log
ds$UDBP_cr_ln <- log(ds$UDBP_cr)

# Transform MCR
ds$MCR_ln <- log(ds$MicroalbCreatRatio)

# Transform eGFR
ds$eGFR_ln <- log(ds$eGFR)

saveRDS(ds2, file='ds.Rds')
