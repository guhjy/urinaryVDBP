library(plyr)
library(dplyr)
library(tidyr)
library(nephro)
library(ggplot2)
library(knitr)
library(pander)
library(digest)

ds <- readRDS(file='data/ds.Rds')
source('functions.r')

ds_canoe <- ds %>% 
  select(Canoe, n) %>% 
  filter(n == 1)

## Check n ##
ds %>%
  filter(VN == 1) %>% 
  select(MicroalbCreatRatio) %>% 
  na.omit() %>% 
  summarise(n = n())

## Check n of multiple variables ##
ds %>%
  tbl_df() %>%
  filter(VN == 1) %>% 
  select(SID, Age, Sex, Ethnicity, BMI, Waist, 
         UrineMicroalbumin, UrineCreatinine, MicroalbCreatRatio, eGFR,
         VitaminD, Creatinine, PTH,
         MeanArtPressure, Systolic, Diastolic,
         Glucose0, Glucose120) %>%
  gather(Measure, Value, -SID) %>%
  na.omit() %>%
  group_by(Measure) %>%
  summarise(n = n())

# Check normal distribution
histo_plot(ds, UrinaryCalcium, 0.1, 'UDBP')

# Check continous distribution
scatter.plot(ds$Creatinine, ds$UDBP, 
             'Serum Creatinine', 'UDBP') +
geom_smooth(se=TRUE, colour='black') # method=lm for linear line

cor.test(ds_base$UDBP, ds_base$UDBP/ds_base$UrineCreatinine, method='pearson', exact=FALSE)
cor(ds$PTH, ds$CaCrRatio, use="complete.obs") %>% round(2)

ds %>% 
  filter(VN == 1, UDBP < 5000) %>% 
  scatter_plot("log(UDBP)", "log(udbpCrRatio)", 
               "log UDBP", "log UDBP:Creatinine") +
  geom_smooth(colour = "black")

# Missing from baseline but have follow-up ---------------------------------------------------------

ds %>%
  mutate(n = ifelse(is.na(mcr_VN1) & !is.na(mcr_VN3) | !is.na(mcr_VN6), 1, 0))



# TABLE 1 ==========================================================================================

# USE CARPENTER TO GENERATE ENTIRE TABLE

# dplyr method
ds %>% 
  group_by(fVN) %>% 
  na.omit() %>%
  summarise(meansd=paste0(round(mean(UDBP), 1), " (",round(sd(UDBP), 1), ")")) %>% 
  spread(fVN, meansd)

prop.table(table(tb1$Sex, tb1$UDBP_status), 2) # column proportions (1 to 2 for row/column)

# aggregate method (old)
aggregate(tb1$Age ~ tb1$eGFR_status, data=ds, FUN=mean)

# ANOVA for table 1
anova <- aov(ds$Glucose120~ds$eGFR_status)
summary(anova)
TukeyHSD(anova)

# Chi-square for table 1
chisq.test(table(ds$dm_status, ds$eGFR_status), correct = TRUE)



#####################################################################################
## Table 2 ##
#####################################################################################

aggregate(tb2$UrineMicroalbumin ~ tb2$eGFR_status, data=ds, FUN=median)
aggregate(ds$UDBP ~ ds$eGFR_status, data=ds, FUN=quantile) # Take the (25%, 75%)
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
ds %>% 
  filter(VN == 1) %>% 
  histo_plot("VitaminD", 2, 'Serum 25(OH)D Concentration (nmol/L)')

## Check normality
shapiro.test(tb4$VitaminD_ln)

## Correlation
cor.test(tb4$UDBP_cr_ln, tb4$VitaminD_ln, method='spearman', exact=FALSE)

## Plots
scatter_plot(ds$VitaminD_ln, ds$UDBP_cr_ln,
             'log Serum 25(OH)D (nmol/L)', 'log UDBP:Creatinine')

ds_base <- ds %>% 
  filter(VN == 3)

ds_base %>% 
  bar_plot(ds_base$dm_status, ds_base$VitaminD,
           'Diabetic Status', 'Vitamin D')

ds %>% 
  ggplot(aes(x = fVN, y = UDBP, fill = fVN)) +
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(1)) +
  stat_summary(fun.ymin = sd, fun.ymax = sd, geom = "errorbar", 
               color = "black", position = position_dodge(1), width = 0.2)
#######
ds %>% 
  select(dm_status, fVN, UDBP) %>% 
  na.omit() %>% 
  ggplot(aes(x = dm_status, y = UDBP, fill = dm_status)) +
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(1)) +
  scale_fill_discrete(name = "Glycemic Status") +
  theme(axis.text.x = element_blank(), 
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) +
  stat_summary(fun.data = mean_se, geom="errorbar",
               color = "grey80", position = position_dodge(1), width = 0.2) +
  xlab("Visit Number") +
  ylab("Urinary vitamin D binding protein (ng/mL)") +
  facet_grid(~fVN, switch = "x")
#########

ds %>% 
  ggplot(aes(x = eGFR, y = UDBP, color = fVN))