#' Read export Dino *_1.csv file with measured heads.
#'
#' @inherit hm_read_export_csv
#' @examples
#' fname <- system.file("extdata","B38E0108001_1.csv",package="menyanthes")
#' hm3 <- hm_read_dino( fname )
#' @export
hm_read_dino <-  function(fname) {
  lns <- readLines(fname)
  i <- which(lns=="")
  xm <- read.csv2(
    fname,
    header = TRUE,
    sep = ",",
    quote = "\"",
    fill = TRUE,
    skip = i[2],
    nrows = i[3]-i[2]-2,
    row.names = NULL
  )
  xm <-
    data.frame(
      NAME = xm$Locatie,
      FILTER = xm$Filternummer,
      X = xm$X.coordinaat,
      Y = xm$Y.coordinaat,
      TOP = xm$Bovenkant.filter..cm.t.o.v..NAP.,
      BOT = xm$Onderkant.filter..cm.t.o.v..NAP.,
      MV = xm$Maaiveld..cm.t.o.v..NAP.
    )
  xm %<>% dplyr::mutate(TOP=TOP/100)
  xm %<>% dplyr::mutate(BOT=BOT/100)
  xm %<>% dplyr::mutate(MV=MV/100)
  xm$X %<>% as.numeric(.)
  xm$Y %<>% as.numeric(.)
  xm %<>% dplyr::arrange(NAME, FILTER)

  # Read Data
  xd <- read.csv2(
    fname,
    header = FALSE,
    sep = ",",
    quote = "\"",
    fill = TRUE,
    skip = i[4] + 1,
    row.names = NULL
  )
  xd <- xd[, c(1, 2, 3, 6)]
  names(xd) <- c("NAME", "FILTER", "DATE", "HEAD")
  xd <- xd[!is.na(xd$FILTER),]
  xd$DATE <- lubridate::dmy(xd$DATE) %>% as.POSIXct()
  #xd$NAME %<>% remove_trailing_letter(.)
  xd <- xd[!is.na(xd$HEAD),] # Remove NA values
  xd$HEAD %<>% as.numeric(.)
  xd %<>% dplyr::mutate(HEAD=HEAD/100)

  hm <- list()
  hm$xm <- xm
  hm$xd <- xd
  # Remove double filters and observations
  hm %<>% hm_rm_dble_fltrs()
  hm %<>% hm_rm_dble_obs()
  return(hm)
}

#' Read all export Dino *_1.csv files with measured heads in specified folder.
#'
#' @param path a character vector containing a single path name. Tilde expansion (see \code{\link{path.expand}}) is done.
#' @examples
#' path <- system.file("extdata","Grondwaterstanden_Put",package="menyanthes")
#' hm4 <- hm_read_dino_path( path )
#' @export
hm_read_dino_path <-  function(path) {
  path %<>% path.expand(.)
  if (!dir.exists(path)) {
    stop('Specified path does not exist.')
  }
  # Retreive all *.csv filenames
  fnames <- list.files(
    path = path,
    pattern = glob2rx("*_1.csv"),
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  ) %>% as.matrix()
  if (nrow(fnames)<1) {
    stop('No filename matching the pattern *_1.csv found in specified folder.')
  }
  x <- apply(fnames, MARGIN = 1, hm_read_dino) %>% hm_rbind()
  return(x)
}
