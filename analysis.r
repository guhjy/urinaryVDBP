library(plyr)
library(dplyr)
library(nephro)
library(ggplot2)

ds <- readRDS(file='ds.Rds')

## Check n ##
table(ds$dm_status)

## Check n of multiple variables ##
ds %>%
  tbl_df()%>%
  select(SID, UDBP, eGFR, matches('Glucose'), Sex, Ethnicity, Age, BMI, dm_status) %>%
  gather(Measure, Value, -SID) %>%
  na.omit() %>%
  group_by(Measure) %>%
  summarise(n = n())

# Check normal distribution
histo_plot(ds$UDBP_cr, .5, 'UDBP:Creatinine (ng/mL)')

# Check continous distribution
scatter_plot(ds$Diastolic, ds$UDBP_cr_ln, 
             'Diastolic blood pressure (mmHg)', 'log UDBP:Creatinine') +
geom_smooth(se=TRUE, colour='black')

cor.test(ds$Diastolic, ds$UDBP_cr_ln, method='spearman', exact=FALSE)

###########################################################################################
## TABLE 1 ##
###########################################################################################
tb1 <-
  ds %>%
  select(UDBP_status, dm_status, Age, Sex, Ethnicity, BMI, Waist,
         UrineMicroalbumin, UrineCreatinine, MicroalbCreatRatio, eGFR,
         Glucose0, Glucose120,
         MeanArtPressure, VitaminD) %>%  # Form 015 on dataDictionary
  na.omit()  # Omit the missing values ""

# dplyr method
ds %>% 
  group_by(UDBP_status) %>% 
  # na.omit() %>% 
  summarise(mean=mean(Diastolic), sd=sd(Diastolic))

prop.table(table(tb1$Sex, tb1$UDBP_status), 2) # column proportions (change 1 to 2 for row/column)
prop.table(table(tb1$group_ethn, tb1$UDBP_status), 2)
prop.table(table(tb1$dm_status, tb1$UDBP_status), 2)

# aggregate method
aggregate(tb1$Age ~ tb1$UDBP_status, data=ds, FUN=mean)

# ANOVA for table 1
summary(aov(ds$Diastolic~ds$UDBP_status))
aov.BMI <- (aov(tb1$BMI~tb1$UDBP_status))
aov.WC <- (aov(tb1$Waist~tb1$UDBP_status))
summary(aov(tb1$UrineMicroalbumin~tb1$UDBP_status))
aov.creat <- (aov(tb1$UrineCreatinine~tb1$UDBP_status))
aov.eGFR <- (aov(tb1$eGFR~tb1$UDBP_status))
summary(aov(tb1$Glucose0~tb1$UDBP_status))
summary(aov(tb1$Glucose120~tb1$UDBP_status))

TukeyHSD(aov.WC)

# Chi-square for table 1
chisq.test(table(tb1$Sex, tb1$UDBP_status), correct = TRUE)
chisq.test(table(tb1$group_ethn, tb1$UDBP_status), correct = TRUE)
chisq.test(table(tb1$dm_status, tb1$UDBP_status), correct = TRUE)

#####################################################################################
## Table 2 ##
#####################################################################################

aggregate(tb2$UrineMicroalbumin ~ tb2$eGFR_status, data=ds, FUN=median)
aggregate(tb2$UDBP_cr_ln ~ tb2$eGFR_status, data=ds, FUN=quantile) # Take the (25%, 75%)
aggregate(tb2$UDBP_cr ~ tb2$mcr_status, data=ds, FUN=median)
aggregate(tb2$UDBP_cr ~ tb2$mcr_status, data=ds, FUN=quantile)
aggregate(tb2$UDBP_cr_ln ~ tb2$dm_status, data=ds, FUN=median)
aggregate(tb2$UDBP_cr_ln ~ tb2$dm_status, data=ds, FUN=quantile)

# ANOVA for table 2
tb2.aovMCR <- aov(tb2$UDBP_cr_ln~tb2$mcr_status)
tb2.aovGFR <- aov(tb2$UDBP_cr_ln~tb2$eGFR_status)

summary(aov(tb2$UDBP_cr_ln~tb2$eGFR_status))
summary(aov(tb2$UDBP_cr_ln~tb2$mcr_status))
summary(aov(tb2$UDBP_cr_ln~tb2$dm_status))

TukeyHSD(tb2.aovGFR)

# Boxplots
ggplot(ds, aes(x=ds$eGFR_status, y=ds$UDBP_cr_ln)) +
  geom_boxplot() +
  scale_x_discrete(limits=c('normal','mild','moderate')) +
  xlab('Estimated GFR') +
  ylab('log UDBP:Creatinine') +
  theme_bw()

ggplot(ds, aes(x=ds$mcr_status, y=ds$UDBP_cr_ln)) +
  geom_boxplot(fill='#00c5b9', colour='#2f3848') +
  scale_x_discrete(limits=c('normal','microalbuminuria','macroalbuminuria')) +
  xlab('Albuminuria') +
  ylab('log UDBP:Creatinine') +
  theme_set(theme_minimal())

ggplot(ds, aes(x=ds$dm_status, y=ds$UDBP_cr_ln)) +
  geom_boxplot(fill='#00c5b9', colour='#2f3848') +
  scale_x_discrete(limits=c('NGT','IFG', 'IGT','DM')) +
  xlab('Diabetic Status') +
  ylab('log UDBP:Creatinine') +
  theme_set(theme_minimal())

# Correlations
# geom_point(colour = '#number')
#f05768 pink
#00c5b9 turquoise
#2f3848 dark grey

ggplot(ds, aes(x=ds$eGFR_ln, y=ds$UDBP_cr_ln)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour='#2f3848') +
  xlab('log Estimated GFR (mL/min/1.73m^2)') +
  ylab('log UDBP:Creatinine') +
  theme_bw()

ggplot(tb2, aes(x=tb2$mcr_ln, y=tb2$UDBP_cr_ln)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour='#2f3848') +
  xlab('log Microalbumin:Creatinine (mg/mmol)') +
  ylab('log UDBP:Creatinine') +
  theme_bw()


# Correlation coefficient
cor.test(tb2$eGFR_ln, tb2$UDBP_cr_ln, method='spearman', exact=FALSE)
cor.test(tb2$MCR_ln, tb2$UDBP_cr_ln, method='spearman', exact=FALSE)

# # LOESS <- can do this using ggplot2 above (take out method=lm)
# scatter.smooth(x=tb2$eGFR, xlim=range(tb2$eGFR), xlab='Kidney Disease',
#                y=tb2$UDBP_cr_ln, ylim=range(tb2$UDBP_cr_ln), ylab='ln(UDBP:Creatinine)',
#                col="#f05768")
#
# scatter.smooth(x=tb2$MicroalbCreatRatio, xlim=range(tb2$MicroalbCreatRatio),
#                xlab='Proteinuria Status',
#                y=tb2$UDBP_cr_ln, ylim=range(tb2$UDBP_cr_ln),
#                ylab='ln(UDBP:Creatinine)',
#                col="#f05768")

########################################################################################
## Table 3 ##
########################################################################################

# Checking for covariants
# SmokeCigs
# dxa_subtot_ffm (muscle mass would be better for creatinine)

pairs(tb3)
plot(tb3$eGFR, tb3$dxa_subtot_ffm)
cor.test(ds$VitaminD, ds$UDBP, method='spearman', exact=FALSE)
cov(tb3$UDBP_cr_ln, tb3$Age)

# Homogeneity of Variance (between outcome and predictor)
leveneTest(tb3$UDBP_cr_ln, tb3$mcr_status, center='median')

# Test independence (between predictor and covariate) (DV~IV)
summary(aov(Age~mcr_status, data=tb3))
summary(aov(Sex~mcr_status, data=tb3))
summary(aov(Ethnicity~mcr_status, data=tb3))
summary(aov(Waist~mcr_status, data=tb3))
summary(aov(Smoking~mcr_status, data=tb3))

# ANCOVA
#contrasts(tb3$mcr_status) <- contr.poly(3)
contrasts(tb3$mcr_status) <- cbind(c(-2,1,1), c(0,-1,1))
model.1 <- aov(UDBP_cr_ln~Age + mcr_status,  data=tb3)
Anova(model.1, type="III") # View results (need 'car' package)
summary.lm(model.1) # get R^2

model.2 <- aov(UDBP_cr_ln~Age + Sex + mcr_status,  data=tb3)
Anova(model.2, type="III") # View results (need 'car' package)
summary.lm(model.2) # get R^2

model.3 <- aov(UDBP_cr_ln~Age + Sex + Ethnicity + mcr_status,  data=tb3)
Anova(model.3, type="III") # View results (need 'car' package)
summary.lm(model.3) # get R^2

model.4 <- aov(UDBP_cr_ln~Age + Sex + Ethnicity + Waist + mcr_status,  data=tb3)
Anova(model.4, type="III") # View results (need 'car' package)
summary.lm(model.4) # get R^2

model.5 <- aov(UDBP_cr_ln~Age + Sex + Ethnicity + Waist + SmokeCigs + mcr_status,  data=tb3)
Anova(model.5, type="III") # View results (need 'car' package)
summary.lm(model.5) # get R^2

# Adjusted means
summary(effect('mcr_status', model.1))

# Post-hoc using Tukey's
posth.1 <- glht(model.1, linfct=mcp(mcr_status='Tukey'))
summary(posth.1) # shows the output in a nice format
confint(posth.1) # computes CI

# Effect size calculated by hand

# Plot
ggplot(ds, aes(x=Age, y=UDBP_cr_ln, colour=mcr_status)) +
  geom_point() +
  xlab('Age') +
  ylab('ln(UDBP:Creatinine)') +
  theme_minimal()

######################################################################
## Serum 25(OHD) Correlation ##
######################################################################

## Check distribution of serum 25(OH)D
histo_plot(ds$VitaminD, 0.5, 'Serum 25(OH)D Concentration (nmol/L)')

## Check normality
shapiro.test(tb4$VitaminD_ln)

## Correlation
cor.test(tb4$UDBP_cr_ln, tb4$VitaminD_ln, method='spearman', exact=FALSE)

## Plots
scatter_plot(ds$VitaminD_ln, ds$UDBP_cr_ln,
             'log Serum 25(OH)D (nmol/L)', 'log UDBP:Creatinine')