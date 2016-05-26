#################################################################################
## Tables
#################################################################################
rename_table_rows <- function(x) {
  gsub('Age', 'Age (years)', x) %>% 
  gsub('Waist', 'Waist Circumference (cm)', .)
}

## Characerization by eGFR status

table_baseline <- function(data, byfactor = '') {
  data %>%
    mutate(
      eGFR_status = factor(eGFR_status, ordered = FALSE),
      dm_status = factor(dm_status, ordered = FALSE),
      UDBP_status = factor(UDBP_status, ordered = FALSE)
    ) %>%
    carpenter::outline_table(
      c(
        'Age',
        'Sex',
        'Ethnicity',
        'BMI',
        'Waist',
        'UDBP',
        'UrineMicroalbumin',
        'UrineCreatinine',
        'MicroalbCreatRatio',
        'eGFR',
        'VitaminD',
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
        'UDBP',
        'UrineMicroalbumin',
        'UrineCreatinine',
        'MicroalbCreatRatio',
        'eGFR'
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
    carpenter::construct_table()
}


#################################################################################
## Plots
#################################################################################

## Scatterplot ##
scatter_plot = function(data, xvar, yvar, xlab='', ylab='') {
  ggplot(data, aes_string(x=xvar, y=yvar)) +
    geom_point() +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal()
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
