#' menyanthes: A package read / write / manipulate \href{https://www.kwrwater.nl/tools-producten/hydromonitor/}{HydroMonitor} Observationwell data.
#'
#' Code to recreate internal data in R/sysdata.rda.
#'
#' crsAfoort <- sp::CRS("+init=epsg:28992") # epsg projection 28992 - amersfoort
#' hm_filtered_on_polygon <- hm_filter_on_poly( hm1, polygn )
#' use_data(crsAfoort, hm_filtered_on_polygon, internal=TRUE, overwrite=TRUE)
#'
#' This package exports the following functions:
#'
#' * \code{\link{hm_read_export_csv}}
#' * \code{\link{hm_read_export_csv2}}
#' * \code{\link{hm_read_dino}}
#' * \code{\link{hm_read_dino_path}}
#' * \code{\link{hm_read_dino_zip}}
#' * \code{\link{hm_filter_on_year}}
#' * \code{\link{hm_filter_on_extent}}
#' * \code{\link{hm_filter_on_poly}}
#' * \code{\link{hm_rm_fltrs_with_no_obs}}
#' * \code{\link{hm_rbind}}
#' * \code{\link{hm_rm_dble_fltrs}}
#' * \code{\link{hm_rm_dble_obs}}
#' * \code{\link{hm_plot}}
#' * \code{\link{hm_calc_gxg}}
#' * \code{\link{hm_create_shp}}
#' * \code{\link{nr_obs_ratio}}
#'
#' This package exports the following sample data sets:
#' hm1, ...,hm5: HydroMonitor ObservationWell Data.
#' polygn: Polygon shape used in examples.
#'
#' @docType package
#' @name menyanthes
#'
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#'
#' @importFrom utils read.csv
#' @importFrom utils read.csv2
#' @importFrom utils glob2rx
#' @importFrom utils unzip
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
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr slice
#' @importFrom dplyr full_join
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggsave
#'
#' @importFrom stats quantile
#'
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp spTransform
#' @importFrom sp over
#'
#' @importFrom raster shapefile
#' @importFrom raster plot
#'
NULL
