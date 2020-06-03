#' Read Menyanthes csv data and calculate GxG's of specified years
#'
#' @param fname Filename of Menyanthes csv file (character)
#' @param minyear Minimal year to read data from (integer)
#' @param maxyear Maximal year to read data from (integer)
#' @return List with two elements:
#'         x: characteristics of monitoring well and GxG's of specified years (data frame)
#'         xv: Measured heads (data frame)
#' @importFrom lubridate dmy_hm
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom dplyr semi_join
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @examples
#' x <- read_menyanthes_csv( system.file("extdata","Topsoil1.csv",package="menyanthes"))
#' @export
read_menyanthes_csv <- function(fname, minyear=1900, maxyear=3000) {
  # Lees gegevens van peilbuizen
  x <-
    read.csv(
      fname,
      header = TRUE,
      skip = 1,
      dec = ",",
      sep = ";",
      stringsAsFactors = FALSE
    )
  colnames(x) <-
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
  suppressWarnings( x$FILTER %<>% as.numeric(.) )
  x <- x[!is.na(x$FILTER),]
  suppressWarnings( x$X %<>% as.numeric(.) )
  x <- x[!is.na(x$X),]
  suppressWarnings( x$Y %<>% as.numeric(.) )
  x <- x[!is.na(x$Y),]
  x$STARTDATE %<>% lubridate::dmy_hm(.)

  # Lees stijghoogte gegevens
  # Bepaal regelnummer van eerste blanco regel
  s <- readLines(fname)
  skip <- which(s == "")[1]
  xv <-
    read.csv(
      fname,
      header = TRUE,
      skip = skip,
      dec = ",",
      sep = ";",
      stringsAsFactors = FALSE
    )
  colnames(xv) <- c("NAME", "FILTER", "DATE", "HEAD")
  xv$DATE %<>% lubridate::dmy_hm(.)
  xv$YEAR <- lubridate::year(xv$DATE)
  xv$MONTH <- lubridate::month(xv$DATE)

  #Filter stijghoogte gegevens
  xv %<>% dplyr::filter(YEAR >= minyear & YEAR <= maxyear)

  #Verwijder peilbuizen waar geen stijghoogten bekend zijn in de gefilterde stijghoogte gegevens
  x %<>% dplyr::semi_join(unique(xv[,c('NAME','FILTER')]))

  # Bereken GxG's en voeg de waarden toe aan de gegevens van de peilbuizen.
  # Ref 'Een alternatieve GHG analyse' Drs. D.H. Edelman, Ir. A.S. Burger
  # Stromingen 15 (2009) nummer 3 p29-34.
  AHG <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(AHG=quantile(HEAD,.9985))
  MHG <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(MHG=quantile(HEAD,.977))
  GHG <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(GHG=quantile(HEAD,.841))
  GG  <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(GG=quantile(HEAD,.5))
  GLG <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(GLG=quantile(HEAD,.159))
  MLG <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(MLG=quantile(HEAD,.023))
  ALG <- xv %>% dplyr::group_by(NAME, FILTER) %>% dplyr::summarise(ALG=quantile(HEAD,.0015),n=n())
  x %<>% dplyr::left_join(AHG) %>% dplyr::left_join(MHG) %>% dplyr::left_join(GHG) %>% dplyr::left_join(GG) %>% dplyr::left_join(GLG) %>%
    dplyr::left_join(MLG) %>% dplyr::left_join(ALG)

  x %<>% dplyr::arrange(NAME, FILTER)

  dino <- list()
  dino$x <- x
  dino$xv <- xv
  return(dino)
}
