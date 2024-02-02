#' Standardise Opensky Network naming conventions
#'
#' @param .df OSN data tibble
#'
#' @return
#' @export
#'
#' @examples
make_nice_names_osn <- function(.df){
  df <- .df  |> 
    dplyr::rename(
      TIME   = time
      ,ICAO24 = icao24
      ,   LAT = lat, LON = lon
      , FLTID = callsign
      , ALT   = baroaltitude
      , ALT_G = geoaltitude
    )
  return(df)
}