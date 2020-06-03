#' Read Menyanthes csv data and calculate GxG's of specified years
#'
#' @param fname Filename of Menyanthes csv file (character)
#' @param minyear Minimal year to read data from (integer)
#' @param maxyear Maximal year to read data from (integer)
#' @return List with two elements:
#'         x: characteristics of monitoring well and GxG's of specified years (data frame)
#'         xv: Measured heads (data frame)
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
  xv$YEAR <- year(xv$DATE)
  xv$MONTH <- month(xv$DATE)

  #Filter stijghoogte gegevens
  xv %<>% filter(YEAR >= minyear & YEAR <= maxyear)

  #Verwijder peilbuizen waar geen stijghoogten bekend zijn in de gefilterde stijghoogte gegevens
  x %<>% dplyr::semi_join(unique(xv[,c('NAME','FILTER')]))

  # Bereken GxG's en voeg de waarden toe aan de gegevens van de peilbuizen.
  # Ref 'Een alternatieve GHG analyse' Drs. D.H. Edelman, Ir. A.S. Burger
  # Stromingen 15 (2009) nummer 3 p29-34.
  AHG <- xv %>% group_by(NAME, FILTER) %>% summarise(AHG=quantile(HEAD,.9985))
  MHG <- xv %>% group_by(NAME, FILTER) %>% summarise(MHG=quantile(HEAD,.977))
  GHG <- xv %>% group_by(NAME, FILTER) %>% summarise(GHG=quantile(HEAD,.841))
  GG  <- xv %>% group_by(NAME, FILTER) %>% summarise(GG=quantile(HEAD,.5))
  GLG <- xv %>% group_by(NAME, FILTER) %>% summarise(GLG=quantile(HEAD,.159))
  MLG <- xv %>% group_by(NAME, FILTER) %>% summarise(MLG=quantile(HEAD,.023))
  ALG <- xv %>% group_by(NAME, FILTER) %>% summarise(ALG=quantile(HEAD,.0015),n=n())
  x %<>% left_join(AHG) %>% left_join(MHG) %>% left_join(GHG) %>% left_join(GG) %>% left_join(GLG) %>%
    left_join(MLG) %>% left_join(ALG)

  x %<>% arrange(NAME, FILTER)

  dino <- list()
  dino$x <- x
  dino$xv <- xv
  return(dino)
}
