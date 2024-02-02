#' Standardise OSN naming conventions and data types
#'
#' @param .df 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
clean_osn_statevectors <- function(.df, ...){
  clean_states <- .df |> 
    make_nice_names_osn() |> 
    dplyr::mutate(
       across(.cols = c("ALT","ALT_G"), .fns = coerce_meter_to_feet)
      ) |> 
    coerce_unixepoch_to_datetime(...)
}