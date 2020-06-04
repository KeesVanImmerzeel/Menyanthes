#' Read export HydroMonitor file with ObservationWell data.
#'
#' @param fname Filename of export of HydroMonitor file with ObservationWell data (csv file, character)
#' @return List with two elements:
#'         xm: Characteristics of monitoring well (meta data in data frame)
#'         xd: Measured heads (data frame)
#' @importFrom utils read.csv
#' @importFrom utils read.csv2
#' @importFrom lubridate dmy_hm
#' @importFrom lubridate dmy_hms
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm1 <- hm_read_export_csv( fname )
#' @export
hm_read_export_csv <- function(fname) {
  # Read Header
  # Check if this is not a export HydroMonitor - open data exchange file
  con <- file(fname, "r")
  x <- readLines(con, 1)
  if (!(grepl("HydroMonitor - open data exchange file", x, fixed = TRUE))) {
    close(con)
    stop("This is not a export HydroMonitor - open data exchange file.")
  }
  # Check if this is not a export HydroMonitor - open data exchange file
  i <- 1

  is_obs_well_file <- FALSE
  while (TRUE) {
    x = readLines(con, 1)
    found <- grepl("ObservationWell", x, fixed = TRUE)
    if ((i == 100) || (found)) {
      break
    }
    i <- i + 1
  }
  if (!found) {
    stop("This is not a export HydroMonitor file with ObservationWell data.")
  }
  close(con)

  # Read all lines in the file to determine
  con <- file(fname, "r")
  x <- readLines(con, warn = FALSE)
  close(con)

  # Determine start end end line number of metadata and data.
  i <- which(grepl("^;;", x))

  # Determine column names of metadata and data
  names_xm <- x[i[1] + 1] %>% strsplit(";") %>% unlist()
  names_xd <- x[i[2] + 1] %>% strsplit(";") %>% unlist()

  # Read Metadata
  xm <- read.csv2(
    fname,
    header = FALSE,
    sep = ";",
    quote = "\"",
    fill = TRUE,
    skip = i[1] + 2,
    row.names = NULL
  )
  xm$V19 <- NULL
  names(xm) <- names_xm
  xm$FilterNo <- suppressWarnings(as.integer(xm$FilterNo))
  xm <- xm[!is.na(xm$FilterNo), ]
  xm$StartDateTime <- lubridate::dmy_hm(xm$StartDateTime)

  # Filter meta data on essential information
  xm <- data.frame(NAME=xm$Name, FILTER=xm$FilterNo, X=xm$XCoordinate, Y=xm$YCoordinate,
                   TOP= xm$FilterTopLevel, BOT=xm$FilterBottomLevel, MV=xm$SurfaceLevel )
  xm %<>% dplyr::arrange(NAME, FILTER)

  # Read Data
  xd <- read.csv2(
    fname,
    header = FALSE,
    sep = ";",
    quote = "\"",
    fill = TRUE,
    skip = i[2] + 2,
    row.names = NULL
  )
  xd <- xd[, c(1, 2, 3, 4)]
  names(xd) <- c("NAME", "FILTER", "DATE", "HEAD")
  xd <- xd[!is.na(xd$FILTER), ]
  xd$DATE <- lubridate::dmy_hms(xd$DATE)

  hm <- list()
  hm$xm <- xm
  hm$xd <- xd
  return(hm)
}

#' Read export HydroMonitor file with ObservationWell data.
#'
#' @inheritParams hm_read_export_csv
#' @return List with two elements:
#'         xm: Characteristics of monitoring well (meta data in data frame)
#'         xd: Measured heads (data frame)
#' @importFrom utils read.csv
#' @importFrom utils read.csv2
#' @importFrom lubridate dmy_hm
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @examples
#' fname <- system.file("extdata","Topsoil1.csv",package="menyanthes")
#' hm2 <- hm_read_export_csv2( fname )
#' @export
hm_read_export_csv2 <- function(fname) {
  # Lees gegevens van peilbuizen
  xm <-
    read.csv(
      fname,
      header = TRUE,
      skip = 1,
      dec = ",",
      sep = ";",
      stringsAsFactors = FALSE
    )
  colnames(xm) <-
    c("NAME",
      "FILTER",
      "X",
      "Y",
      "MV",
      "TOP",
      "BOT",
      "MEASLEV",
      "SUMPlEN",
      "STARTDATE")
  suppressWarnings( xm$FILTER %<>% as.integer(.) )
  xm <- xm[!is.na(xm$FILTER),]
  suppressWarnings( xm$X %<>% as.numeric(.) )
  xm <- xm[!is.na(xm$X),]
  suppressWarnings( xm$Y %<>% as.numeric(.) )
  xm <- xm[!is.na(xm$Y),]
  xm$STARTDATE %<>% lubridate::dmy_hm(.)

  # Filter meta data on essential information
  xm <- data.frame(NAME=xm$NAME, FILTER=xm$FILTER, X=xm$X, Y=xm$Y, TOP=xm$TOP, BOT=xm$BOT, MV=xm$MV )
  xm %<>% dplyr::arrange(NAME, FILTER)

  # Lees stijghoogte gegevens
  # Bepaal regelnummer van eerste blanco regel
  s <- readLines(fname)
  skip <- which(s == "")[1]
  xd <-
    read.csv(
      fname,
      header = TRUE,
      skip = skip,
      dec = ",",
      sep = ";",
      stringsAsFactors = FALSE
    )
  colnames(xd) <- c("NAME", "FILTER", "DATE", "HEAD")
  xd$DATE %<>% lubridate::dmy_hm(.)

  hm <- list()
  hm$xm <- xm
  hm$xd <- xd
  return(hm)
}

#' Filter HydroMonitor ObservationWell data on year.
#'
#' @param hm HydroMonitor ObservationWell data as read by \code{\link{hm_read_export_csv}}
#' @param minyear Minimal year to read data from (integer)
#' @param maxyear Maximal year to read data from (integer)
#' @return hm HydroMonitor ObservationWell data as read by \code{\link{hm_read_export_csv}}
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
#' @importFrom dplyr semi_join
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

#' Calculate GxG's of HydroMonitor ObservationWell data
#'
#' @inheritParams hm_filter_on_year
#' @return xm: Characteristics of monitoring well (meta data in data frame) with GxG information added.
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom stats quantile
#' @importFrom dplyr arrange
#' @examples
#' fname <- system.file("extdata","export_data_menyanthes.csv",package="menyanthes")
#' hm <- hm_read_export_csv( fname )
#' gxg <- hm_calc_gxg(hm)
#' @export
hm_calc_gxg <- function(hm) {
  # Bereken GxG's en voeg de waarden toe aan de gegevens van de peilbuizen.
  # Ref 'Een alternatieve GHG analyse' Drs. D.H. Edelman, Ir. A.S. Burger
  # Stromingen 15 (2009) nummer 3 p29-34.
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

