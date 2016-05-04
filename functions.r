scatter_plot = function(ds, xvar, yvar, xlab='', ylab='') {
  ggplot(data=ds, aes(x=xvar, y=yvar)) +
    geom_point() +
    geom_smooth(method=lm, se=FALSE, colour='black') +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
}

# EXAMPLE:
# scatter_plot(tb4, tb4$VitaminD_ln, tb4$UDBP_cr_ln,
#              'log Serum 25(OH)D', 'log UDBP:Creatinine')

box_plot = function(ds, xvar, yvar, xcat, xlab='', ylab='') {
  ggplot(ds, aes(x=xvar, y=yvar)) +
    geom_boxplot() +
    scale_x_discrete(limits=xcat) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw()
}

# EXAMPLE:
# box_plot(tb2, tb2$eGFR_status, tb2$UDBP_cr_ln,
#          c('normal','mild','moderate'),
#          'Estimated GFR','log UDBP:Creatinine')