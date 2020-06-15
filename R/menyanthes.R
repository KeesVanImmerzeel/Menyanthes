#' menyanthes: A package read / write / manipulate \href{https://www.kwrwater.nl/tools-producten/hydromonitor/}{HydroMonitor} Observationwell data.
#'
#' This package exports the following functions:
#'
#' * \code{\link{hm_read_export_csv}}
#' * \code{\link{hm_read_export_csv2}}
#' * \code{\link{hm_filter_on_year}}
#' * \code{\link{hm_calc_gxg}}
#' * \code{\link{nr_obs_ratio}}
#'
#' @docType package
#' @name menyanthes
#'
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#'
#' @importFrom utils read.csv
#' @importFrom utils read.csv2
#'
#' @importFrom lubridate dmy_hm
#' @importFrom lubridate dmy_hms
#' @importFrom lubridate year
#' @importFrom lubridate month
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr semi_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr do
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggtitle
#'
#' @importFrom stats quantile
NULL
