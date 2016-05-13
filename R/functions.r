

#################################################################################
## Tables
#################################################################################
# rename_table_rows <- function(x) {
#   gsub()
# }

table.baseline <- function(data, caption) {
  data %>%
    mutate(eGFR_status = factor(eGFR_status, ordered = FALSE),
           dm_status = factor(dm_status, ordered = FALSE),
           UDBP_status = factor(UDBP_status, ordered = FALSE)) %>%
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
        'Creatinine',
        'MeanArtPressure',
        'Systolic',
        'Diastolic',
        'PTH',
        'Glucose0',
        'Glucose120',
        'dm_status'
      ),
      'eGFR_status'
      
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
    carpenter::add_rows(c('Glucose0', 'Glucose120'),
                        carpenter::stat_meanSD,
                        digits = 1) %>%
    carpenter::add_rows(c('dm_status'),
                        carpenter::stat_nPct, digits = 1) %>% 
  # carpenter::rename_rows(rename_table_rows) %>%
  # carpenter::rename_columns('Measure', 'Baseline', '3-yr',
  #                           '6-yr') %>%
  carpenter::construct_table(caption = caption)
}


#################################################################################
## Plots
#################################################################################

## Scatterplot ##
scatter.plot = function(xvar, yvar, xlab='', ylab='') {
  ggplot(ds, aes(x=xvar, y=yvar)) +
    geom_point() +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
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
box.plot = function(xvar, yvar, xcat, xlab='', ylab='') {
  ggplot(ds, aes(x=xvar, y=yvar)) +
    geom_boxplot() +
    scale_x_discrete(limits=xcat) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
}

## Bargraph ##
bar.plot = function(xvar, yvar, xlab='', ylab='') {
  ggplot(ds, aes(x=xvar, y=yvar)) +
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
histo.plot = function(variable, bin, xlab='') {
  ggplot(ds, aes(x=variable)) +
    geom_histogram(binwidth=bin,
                   colour='black', fill='white') +
    xlab(xlab) +
    theme_minimal()
}

# EXAMPLE:
# histo_plot(ds$VitaminD, 2, 'Serum 25(OH)D')
