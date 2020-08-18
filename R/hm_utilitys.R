#' Remove meta data of filters if there are no observations available.
#'
#' @param hm HydroMonitor Observation Well data as read by \code{\link{hm_read_export_csv}}
#' @return HydroMonitor ObservationWell data where in meta data filters with no observations are removed.
#' @examples
#' hm <- hm1
#' hm_clean <- hm_rm_fltrs_with_no_obs( hm )
#' @export
hm_rm_fltrs_with_no_obs <- function( hm ) {
  hm$xm %<>% dplyr::semi_join(unique(hm$xd[,c('NAME','FILTER')]))
  return(hm)
}

#' Filter HydroMonitor ObservationWell data on year.
#'
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @param minyear Minimal year to read data from (integer)
#' @param maxyear Maximal year to read data from (integer)
#' @return Filtered HydroMonitor ObservationWell data
#' @examples
#' hm <- hm1
#' hm_filtered <- hm_filter_on_year( hm, minyear=2000)
#' @export
hm_filter_on_year <- function( hm, minyear=1900, maxyear=3000 ) {
  hm$xd$YEAR <- lubridate::year(hm$xd$DATE)
  hm$xd$MONTH <- lubridate::month(hm$xd$DATE)
  #Filter stijghoogte gegevens
  hm$xd %<>% dplyr::filter(YEAR >= minyear & YEAR <= maxyear)

  #Verwijder peilbuizen uit meta data waar geen stijghoogten bekend zijn in de gefilterde stijghoogte gegevens.
  hm %<>% hm_rm_fltrs_with_no_obs()
  return(hm)
}

#' Filter HydroMonitor ObservationWell data on extent.
#'
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @param e Extent object or a Raster* or Spatial* object
#' @return Filtered HydroMonitor ObservationWell data
#' @examples
#' hm <- hm1
#' xmin <- min(hm$xm$X)
#' xmax <- max(hm$xm$X)
#' ymin <- min(hm$xm$Y)
#' ymax <- max(hm$xm$Y)
#' dx <- (xmax-xmin)/10
#' dy <- (ymax-ymin)/10
#' e <- raster::extent(c(xmin+dx, xmax-dx, ymin+dy, ymax-dy))
#' hm_filtered <- hm_filter_on_extent(hm, e)
#' @export
hm_filter_on_extent <- function(hm, e) {
  if (!is.null(e)) {
    #Filter meta data gegevens op extent
    hm$xm %<>% dplyr::filter(X >= e@xmin &
                               X <= e@xmax & Y >= e@ymin & Y <= e@ymax)
    #Filter stijghoogte gegevens
    hm$xd %<>% dplyr::semi_join(hm$xm, by = "NAME")
  }
  return(hm)
}

#' Remove double filter information in meta data part of HydroMonitor ObservationWell data
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @return HydroMonitor ObservationWell data with double filter information removed from meta data part of
#'   HydroMonitor ObservationWell data \code{\link{hm_read_export_csv}}
#' @examples
#' hm <- hm1
#' hm_clean <- hm_rm_dble_fltrs( hm )
#' @export
hm_rm_dble_fltrs <- function( hm ){
  hm$xm %<>% dplyr::distinct(NAME,FILTER, .keep_all = TRUE)
  return(hm)
}

#' Remove double observations in HydroMonitor ObservationWell data.
#'
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @return HydroMonitor ObservationWell data with double records removed.
#' @examples
#' hm <- hm1
#' hm_clean <- hm_rm_dble_obs( hm )
#' @export
hm_rm_dble_obs <- function(hm) {
  hm$xd %<>% dplyr::distinct(NAME,FILTER,DATE, .keep_all = TRUE)
  #Verwijder peilbuizen uit meta data waar geen stijghoogten bekend zijn in de gefilterde stijghoogte gegevens
  hm$xm %<>% dplyr::semi_join(unique(hm$xd[,c('NAME','FILTER')]))
  return(hm)
}

#' Ratio's (# observations in filter) / (average # of observations in monitoring well)
#'
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @return tible with fields:
#' * NAME Name of observationwell (character vector)
#' * FILTER Filter number (integer)
#' * RATIO  Ratio (# observations in filter) / (average # of observations in monitoring well) (numeric)
#' @examples
#' hm <- hm1
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
#' @seealso \code{link{hm_gxg_table}}
#' \href{https://edepot.wur.nl/175881}{'Een alternatieve GHG analyse' Drs. D.H. Edelman, Ir. A.S. Burger
#' Stromingen 15 (2009) nummer 3 p29-34.}
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @return Characteristics of monitoring well (meta data, \code{\link{hm_read_export_csv}}) with the following fields added:
#' * AHG & AHG_MV 99,85 % value of observed heads (relative to REF, relative to soil surface level).
#' * MHG & MHG_MV 97,7 % value of observed heads (relative to REF, relative to soil surface level).
#' * GHG & GHG_MV  84,1 % value of observed head (relative to REF, relative to soil surface level).
#' * GG & GG_MV 50% value of observed heads (relative to REF, relative to soil surface level).
#' * GLG & GLG_MV 15,9% value of observed heads (relative to REF, relative to soil surface level).
#' * MLG & MLG_MV 2,3% value of observed heads (relative to REF, relative to soil surface level).
#' * ALG & ALG_MV 0,15% value of observed heads (relative to REF, relative to soil surface level).
#' * n Number of observations used to calculate percentile values.
#' @details GxG's of observations of every filter is calculated.
#' @examples
#' hm <- hm1
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
  ALG <- hm$xd %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(ALG=quantile(HEAD,.0015,na.rm = TRUE),n=dplyr::n())
  hm$xm %<>% dplyr::left_join(AHG) %>% dplyr::left_join(MHG) %>% dplyr::left_join(GHG) %>% dplyr::left_join(GG) %>% dplyr::left_join(GLG) %>%
    dplyr::left_join(MLG) %>% dplyr::left_join(ALG)
  hm$xm %<>% dplyr::mutate(AHG_MV = MV-AHG)
  hm$xm %<>% dplyr::mutate(MHG_MV = MV-MHG)
  hm$xm %<>% dplyr::mutate(GHG_MV = MV-GHG)
  hm$xm %<>% dplyr::mutate(GG_MV = MV-GG)
  hm$xm %<>% dplyr::mutate(GLG_MV = MV-GLG)
  hm$xm %<>% dplyr::mutate(MLG_MV = MV-MLG)
  hm$xm %<>% dplyr::mutate(ALG_MV = MV-ALG)

  hm$xm %<>% dplyr::arrange(NAME, FILTER)
  return(hm$xm)
}

#' Calculate GxG table based on head observations in top filters of HydroMonitor ObservationWell data.
#'
#' @seealso \code{link{hm_calc_gxg}}
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @return Tibble with parameters as specified in \code{link{hm_calc_gxg}}.
#' @examples
#' hm <- hm1
#' gxg <- hm_gxg_table(hm)
#' @export
hm_gxg_table <- function(hm) {
  gxg_s <- hm %>% hm_calc_gxg() %>% dplyr::group_by(NAME) %>% dplyr::slice(which.min(FILTER))
  return( gxg_s[,!names(gxg_s) %in% c("X","Y","TOP","BOT","MV")])
}

#' Plot HydroMonitor ObservationWell data and optionally save plots to specified folder.
#'
#' Create a list of timeseries plots of all HydroMonitor Observationwell data
#' (\code{\link{hm_read_export_csv}}).
#'
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @param output_dir folder name to write plot (character). If NULL, plots are not written do disk.
#' @return tibble. Fields:
#'
#' * NAME Name of observationwell (character vector)
#' * plots List of timeseries plots (ggplot object)
#'
#' @examples
#' hm <- hm1
#' x <- hm_plot(hm)
#' NAME <- x$NAME[3]
#' x$plots[[which(x$NAME==NAME)]]
#' @export
hm_plot <- function(hm, output_dir = NULL) {
  hm$xd %<>% dplyr::full_join(hm$xm, by = c("NAME", "FILTER"))
  hm$xd$FILTER %<>% as.factor()
  x <- suppressWarnings(
    hm$xd %>% dplyr::group_by(NAME) %>% dplyr::do(
      plots = ggplot2::ggplot(data = .) +
        ggplot2::aes(x = DATE, y = HEAD, color = FILTER) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = .$MV[1], linetype = "longdash", colour = "brown") +
        ggplot2::ggtitle(unique(.$NAME))
    )
  )
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    n <- length(x$NAME)
    for (i in 1:n) {
      mypath <- file.path(output_dir, paste(x$NAME[i], ".jpg", sep = ""))
      ggplot2::ggsave(
        filename = mypath,
        x$plots[[i]],
        width = 16,
        height = 8,
        units = "cm"
      )
    }
  }
  return(x)
}

#' Merge HydroMonitor ObservationWell data objects.
#'
#' Merge HydroMonitor ObservationWell data objects (\code{\link{hm_read_export_csv}}).
#'
#' @param hm_list List of HydroMonitor ObservationWell (HMOW) data objects.
#' @return HydroMonitor ObservationWell data object.
#' @details Double observations and double filters are removed from the HMOW data.
#' @examples
#' hm <- hm_rbind(list(hm1, hm2))
#' @export
hm_rbind <- function(hm_list) {
  hm <- NA
  n <- length(hm_list)
  if (n>0) {
    hm <- hm_list[[1]]
    for (i in 2:n) {
      hm$xm %<>% rbind(hm_list[[i]]$xm)
      hm$xd %<>% rbind(hm_list[[i]]$xd)
    }
  }
  hm %<>% hm_rm_dble_obs() %>% hm_rm_dble_fltrs()
  return(hm)
}

#' Create a point shape file from HydroMonitor ObservationWell data object.
#'
#' Create a summarizing point shape file from the meta data part of HydroMonitor ObservationWell data object.
#'
#' (\code{\link{hm_read_export_csv}}).
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @param filename (character)
#' @param crs Character or object of class 'CRS'. PROJ.4 description of the coordinate reference system.
#'        Default CRS is "Amersfoort / RD new".
#' @return  Nothing is returned when writing a shapeﬁle.
#' @details Fields in the attribute table of the resulting shapefile are:
#'
#' * NAME: Name of observationwell.
#' * X: X-coordinate of the observationwell.
#' * Y: Y-coordinate of the observationwell.
#' * NFILTERS: Number of filters.
#' * TOP: Level of the top of the highest filter.
#' * BOT: Level of the bottom of the lowest filter.
#' * MV: Surface level.
#'
#' @examples
#' hm <- hm1
#' filename <- file.path(path.expand("~"),"filename.shp")
#' hm_create_shp(hm, filename)
#' @export
hm_create_shp <- function(hm, filename, crs="+init=epsg:28992") {
  gxg_table <- hm %>% hm_gxg_table()

  x <- hm$xm %>% dplyr::group_by(NAME) %>% dplyr::summarise(
    X = mean(X),
    Y = mean(Y),
    NFILTRS = dplyr::n(),
    TOP = max(TOP),
    BOT = min(BOT),
    MV = mean(MV, na.rm = TRUE)
  )
  x %<>% dplyr::left_join(gxg_table,by="NAME")
  sp::coordinates(x) <- ~ X + Y
  sp::proj4string(x) <- crs
  raster::shapefile(x, filename, overwrite = TRUE)
}

#' Filter HydroMonitor ObservationWell data with polygon shape.
#'
#' @inheritParams hm_create_shp
#' @param p Polygon shape
#' @return HydroMonitor ObservationWell data within polygon shape.
#' @details Only polygon shapes of length=1 can be used.
#' @examples
#' hm <- hm1
#' p <- polygn
#' hm_filtered_on_polygon <- hm_filter_on_poly(hm, p)
#' @export
hm_filter_on_poly <- function(hm, p, crs="+init=epsg:28992") {
  if (length(p)==1) { # single polygon
    # Create a shape file from HydroMonitor ObservationWell data object.
    filename <-  file.path(path.expand("~"),"tmp.shp")
    hm_create_shp( hm, filename)
    hmpointshape <- raster::shapefile(filename)

    # Make sure the point shape and polygon shape have the same CRS
    p %<>% sp::spTransform(crs)
    hmpointshape %<>% sp::spTransform(crs)

    # Spatial overlay
    i <- sp::over(hmpointshape, p, returnList = FALSE, fn = NULL)
    sel_names <- hmpointshape@data$NAME[!is.na(i)]

    #Filter meta data gegevens en stijghoogte gegevens
    hm$xm %<>% dplyr::filter(NAME %in% sel_names )
    hm$xd %<>% dplyr::semi_join(hm$xm, by = "NAME")
  }
  return(hm)
}

#' Create a data frame with observation period for each filter.
#'
#' @inheritParams hm_rm_fltrs_with_no_obs
#' @return Data frame with fields "min_date" and "max_date" (POSIXct)
#' @examples
#' hm_obs_periods( hm1 )
#' @export
hm_obs_periods <- function(hm) {
  obs_periods <-
    hm$xd %>% group_by(NAME, FILTER) %>% summarise(min_date = min(DATE), max_date =
                                                     max(DATE)) %>% as.data.frame()
  return(obs_periods)
}

