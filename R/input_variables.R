.onLoad <- function(libname, pkgname){
  utils::globalVariables(c('Q_cms', 'Q_cms_aggregate',
                           'day_of_year', 'day', 'month', 'year',
                           'gauge_id',
                           'colorBin',
                           'percentile',
                           'Q_min',
                           'Q_10%',
                           'Q_25%',
                           'Q_75%',
                           'Q_90%',
                           'Q_max',
                           'Q_current_year',
                           'object_id', 'pr','tasmin','tasmax','hurs',
                           'discharge_spec_obs'))
}
