#################################################################################
## Tables
#################################################################################
rename_table_rows <- function(x) {
  gsub('Age', 'Age (years)', x) %>% 
  gsub('Waist', 'Waist Circumference (cm)', .) %>% 
  gsub('eGFR', 'Estimated GFR (ml/min/1.73m^2)', .) %>% 
  gsub('MicroalbCreatRatio', 'Microalbumin:Creatinine', .) %>%
  gsub('UrineCreatinine', 'Urinary Creatinine (mmol/L)', .) %>% 
  gsub('UrineMicroalbumin', 'Urinary Microalbumin (mg/L)', .) %>% 
  gsub('VitaminD', 'Serum 25(OH)D (nmol/L)', .) %>% 
  gsub('UDBP', 'Urinary VDBP (ng/mL)', .) %>% 
  gsub('Diastolic', 'Diastolic Blood Pressure (mmHg)', .) %>% 
  gsub('MeanArtPressure', 'Mean Arterial Pressure (mmHg)', .) %>% 
  gsub('Systolic', 'Systolic Blood Pressure (mmHg)', .) %>% 
  gsub('PTH', 'Parathyroid Hormone (pmol/L)', .) %>% 
  gsub('ALT', 'Serum ALT (U/L)', .) %>% 
  gsub('Glucose0', 'Fasting', .) %>% 
  gsub('Glucose120', '2h OGTT', .) %>% 
  gsub('dm_status', 'Diabetic Status', .) %>% 
  gsub('DM', 'Diabetes', .) %>% 
  gsub('NGT', 'Normal', .)
}

## Subject Characterization

table_baseline <- function(data, byfactor = '', caption) {
  data %>%
    mutate(
      eGFR_status = factor(eGFR_status, ordered = FALSE),
      dm_status = factor(dm_status, ordered = FALSE),
      UDBP_status = factor(UDBP_status, ordered = FALSE),
      fVN = factor(VN, levels=c(1, 3, 6), labels=c('Baseline', 'Year3', 'Year6'))
    ) %>%
    carpenter::outline_table(
      c(
        'Age',
        'Sex',
        'Ethnicity',
        'BMI',
        'Waist',
        'UrineMicroalbumin',
        'UrineCreatinine',
        'MicroalbCreatRatio',
        'eGFR',
        'VitaminD',
        'UDBP',
        'Creatinine',
        'MeanArtPressure',
        'Systolic',
        'Diastolic',
        'PTH',
        'ALT',
        'Glucose0',
        'Glucose120',
        'dm_status'
      ),
      byfactor
      
    ) %>%
    carpenter::add_rows(c('Age'),
                        carpenter::stat_meanSD, digits = 1) %>%
    carpenter::add_rows(c('Sex', 'Ethnicity'),
                        carpenter::stat_nPct, digits = 1) %>%
    carpenter::add_rows(c('BMI', 'Waist'),
                        carpenter::stat_meanSD, digits = 1) %>%
    carpenter::add_rows(
      c(
        'UrineMicroalbumin',
        'UrineCreatinine',
        'MicroalbCreatRatio',
        'eGFR',
        'UDBP'
      ),
      carpenter::stat_meanSD,
      digits = 1
    ) %>%
    carpenter::add_rows(c('VitaminD', 'Creatinine'),
                        carpenter::stat_meanSD,
                        digits = 1) %>%
    carpenter::add_rows(c('MeanArtPressure', 'Systolic', 'Diastolic'),
                        carpenter::stat_meanSD,
                        digits = 1) %>%
    carpenter::add_rows(c('PTH'),
                        carpenter::stat_meanSD, digits = 1) %>%
    carpenter::add_rows(c('ALT'),
                        carpenter::stat_meanSD, digits = 1) %>%
    carpenter::add_rows(c('Glucose0', 'Glucose120'),
                        carpenter::stat_meanSD,
                        digits = 1) %>%
    carpenter::add_rows(c('dm_status'),
                        carpenter::stat_nPct, digits = 1) %>% 
    carpenter::rename_rows(rename_table_rows) %>%
    # carpenter::rename_columns('', 'Undetectable (n=12)', 'Below Limit (n=57)',
    #                           'Normal (n=360)', 'Above Limit (n=310)')
    carpenter::construct_table(caption=caption)
}


#################################################################################
## Plots
#################################################################################

## Scatterplot ##
scatter_plot = function(data, xvar, yvar, xlab='', ylab='') {
  ggplot(data, aes_string(x=xvar, y=yvar)) +
    geom_point() + #mapping=aes(color=mcr_status)
    theme(panel.background = element_blank()) +
    xlab(xlab) +
    ylab(ylab)
}

# EXAMPLE:
# scatter_plot(ds$VitaminD_ln, ds$UDBP_cr_ln,
#              'log Serum 25(OH)D', 'log UDBP:Creatinine')
# 
# With line of best fit - 
# scatter_plot(ds$VitaminD_ln, ds$UDBP_cr_ln,
#              'log Serum 25(OH)D', 'log UDBP:Creatinine') + 
# geom_smooth(method=lm, se=FALSE, colour='black')


## Boxplot ##
box_plot2 <- function(data, xvar, yvar, xlab="", ylab="") {
  ggplot(data, aes_string(x = xvar, y = yvar)) +
    geom_boxplot(aes_string(colour = xvar, fill = xvar)) +
    stat_summary(geom = "crossbar", width = 0.65, fatten = 0, color = "white",
                 fun.data = function(x){
                   return(c(y = median(x), ymin = median(x), ymax = median(x)))
                 }) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(), 
          axis.line.y = element_blank(),
          axis.text.y = element_text(colour = "grey"),
          axis.ticks.y = element_line(colour = "grey"),
          axis.text.x = element_text(colour = "grey 30", angle = 45),
          axis.title = element_text(size = 10)) +
    xlab(xlab) +
    ylab(ylab)
}

# scale_x_discrete(labels = paste(levels(ds_base$xvar), 
#                                 "\n(N=", table(ds_base$xvar), ")", sep = "")) +



box_plot = function(data, xvar, yvar, xlab='', ylab='') {
  ggplot(data, aes(x=xvar, y=yvar)) +
    geom_boxplot() +
    # scale_x_discrete(limits=xcat) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
}

## Bargraph ##
bar_plot = function(data, xvar, yvar, xlab='', ylab='') {
  ggplot(data, aes(x=xvar, y=yvar)) +
    geom_bar(stat='identity') +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal()
}

# EXAMPLE:
# box_plot(ds$eGFR_status, ds$UDBP_cr_ln,
#          c('normal','mild','moderate'),
#          'Estimated GFR','log UDBP:Creatinine')

## Histogram ##
histo_plot = function(data, variable, bin, xlab='') {
  ggplot(data, aes(x=variable)) +
    geom_histogram(binwidth=bin,
                   colour='black', fill='white') +
    xlab(xlab) +
    theme_minimal()
}

# EXAMPLE:
# histo_plot(ds$VitaminD, 2, 'Serum 25(OH)D')
