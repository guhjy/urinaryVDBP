#######################################################################################
## Load and Clean Data [https://bitbucket.org/promise-cohort/promise]
#######################################################################################

ds <- PROMISE::PROMISE_data %>%
  filter(VN == 1, UDBP < 5000) # limit to baseline, remove upper outliers
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

#######################################################################################

ds %>% 
  group_by(dm_status) %>% 
  summarize(gluc0=max(Glucose0),
            gluc120=max(Glucose120))

# Check n
table(ds$dm_status)

# Luke's method
ds %>%
  tbl_df()%>%
  select(SID, UDBP, eGFR, matches('Glucose'), Sex, Ethnicity, Age, BMI, dm_status) %>%
  gather(Measure, Value, -SID) %>%
  na.omit() %>%
  group_by(Measure) %>%
  summarise(n = n())


# Check normal distribution
ggplot(ds, aes(x=UDBP_cr_ln)) +
  geom_histogram(binwidth=0.5,
                 colour='black', fill='white') +
  xlab('log UDBP:Creatinine')
theme_minimal()