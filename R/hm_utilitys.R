
#' Filter HydroMonitor ObservationWell data on year.
#'
#' @param hm HydroMonitor ObservationWell data as read by \code{\link{hm_read_export_csv}}
#' @param minyear Minimal year to read data from (integer)
#' @param maxyear Maximal year to read data from (integer)
#' @return Filtered HydroMonitor ObservationWell data \code{\link{hm_read_export_csv}}
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm <- hm_read_export_csv( fname )
#' hm_filtered <- hm_filter_on_year( hm, minyear=2000)
#' @export
hm_filter_on_year <- function( hm, minyear=1900, maxyear=3000 ) {
  hm$xd$YEAR <- lubridate::year(hm$xd$DATE)
  hm$xd$MONTH <- lubridate::month(hm$xd$DATE)
  #Filter stijghoogte gegevens
  hm$xd %<>% dplyr::filter(YEAR >= minyear & YEAR <= maxyear)

  #Verwijder peilbuizen waar geen stijghoogten bekend zijn in de gefilterde stijghoogte gegevens
  hm$xm %<>% dplyr::semi_join(unique(hm$xd[,c('NAME','FILTER')]))
  return(hm)
}

#' Ratio's (# observations in filter) / (average # of observations in monitoring well)
#'
#' @inheritParams hm_filter_on_year
#' @return tible with fields:
#' * NAME Name of observationwell (character vector)
#' * FILTER Filter number (integer)
#' * RATIO  Ratio (# observations in filter) / (average # of observations in monitoring well) (numeric)
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm <- hm_read_export_csv( fname )
#' r <- nr_obs_ratio(hm)
#' @export
nr_obs_ratio <- function (hm) {
  nf <-
    hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(nf = dplyr::n())
  n <-
    hm$xd %>%  dplyr::group_by(NAME) %>% dplyr::summarise(nmean = dplyr::n() /
                                                         dplyr::n_distinct(FILTER))
  x <- dplyr::left_join(nf, n)
  x$RATIO <- x$nf / x$nmean
  x$nf <- NULL
  x$nmean <- NULL
  return( x )
}

#' Calculate GxG's of HydroMonitor ObservationWell data.
#'
#' Percentile values of observed groundwater heads are calculated according to:
#'
#' \href{https://edepot.wur.nl/175881}{'Een alternatieve GHG analyse' Drs. D.H. Edelman, Ir. A.S. Burger
#' Stromingen 15 (2009) nummer 3 p29-34.}
#' @inheritParams hm_filter_on_year
#' @return Characteristics of monitoring well (meta data, \code{\link{hm_read_export_csv}}) with the following fields added:
#' * AHG 99,85 % value of observed heads.
#' * MHG 97,7 % value of observed heads.
#' * GHG 84,1 % value of observed heads.
#' * GG 50% value of observed heads.
#' * GLG 15,9% value of observed heads.
#' * MLG 2,3% value of observed heads.
#' * ALG 0,15% value of observed heads.
#' * n Number of observations used to calculate percentile values.
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm <- hm_read_export_csv( fname )
#' gxg <- hm_calc_gxg(hm)
#' @export
hm_calc_gxg <- function(hm) {
  # Bereken GxG's en voeg de waarden toe aan de gegevens van de peilbuizen.
  AHG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(AHG=quantile(HEAD,.9985,na.rm = TRUE))
  MHG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(MHG=quantile(HEAD,.977,na.rm = TRUE))
  GHG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(GHG=quantile(HEAD,.841,na.rm = TRUE))
  GG  <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(GG=quantile(HEAD,.5,na.rm = TRUE))
  GLG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(GLG=quantile(HEAD,.159,na.rm = TRUE))
  MLG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(MLG=quantile(HEAD,.023,na.rm = TRUE))
  ALG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(ALG=quantile(HEAD,.0015,na.rm = TRUE),n=n())
  hm$xm %<>% dplyr::left_join(AHG) %>% dplyr::left_join(MHG) %>% dplyr::left_join(GHG) %>% dplyr::left_join(GG) %>% dplyr::left_join(GLG) %>%
    dplyr::left_join(MLG) %>% dplyr::left_join(ALG)

  hm$xm %<>% dplyr::arrange(NAME, FILTER)
  return(hm$xm)
}

#' Plot HydroMonitor ObservationWell data.
#'
#' Create a list of timeseries plots of all HydroMonitor Observationwell data.
#'
#' @inheritParams hm_filter_on_year
#' @return tibble. Fields:
#'
#' * NAME Name of observationwell (character vector)
#' * plots List of timeseries plots (ggplot object)
#'
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm <- hm_read_export_csv( fname )
#' x <- hm_plot(hm)
#' x$NAME[1]
#' x$plots[[1]]
#' @export
hm_plot <- function(hm) {
  hm$xd$FILTER %<>% as.factor()
  suppressWarnings(
    hm$xd %>% dplyr::group_by(NAME) %>% dplyr::do(
      plots = ggplot2::ggplot(data = .) +
        ggplot2::aes(x = DATE, y = HEAD, color = FILTER) + ggplot2::geom_point() + ggplot2::ggtitle(unique(.$NAME))
    )
  )
}

#' Merge two HydroMonitor ObservationWell data objects.
#'
#' Bind rows of two HydroMonitor ObservationWell data objects (\code{\link{hm_read_export_csv}}).
#'
#' @param hm1 First HydroMonitor ObservationWell data object to be merged.
#' @param hm2 Second HydroMonitor ObservationWell data object to be merged.
#' @return HydroMonitor ObservationWell data object.
#'
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm1 <- hm_read_export_csv( fname )
#'
#' @export
hm_rbind <-function(hm1, hm2) {
  hm <- list()
  hm$xm <- rbind(hm1$xm,hm2$xm)
  hm$xd <- rbind(hm1$dm,hm2$dm)
  return(hm)
}


