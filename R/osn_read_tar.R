#' Read downloaded tar files from Opensky Network
#'
#' @param .fn filename
#' @param .exdir temporary directory to extract tar files
#' @param .remove_exdir remove the temporary directory
#' @param .mouthy FALSE to quiet read_csv, set TRUE to troubleshoot
#'
#' @return tibble of extracted OSN tars
#' @export
#'
#' @examples
#' \dontrun{}
read_osn_tar_file <- function(.fn, .exdir="tmp_tar", .remove_exdir=TRUE, .mouthy = FALSE){
  # inspired by: https://stackoverflow.com/questions/7151145/unzip-a-tar-gz-file
  
  # identify gz file name from tarball 
  content <- untar(tarfile = .fn, list = TRUE)
  payload <- content[grepl(pattern = ".csv.gz", x = content)]
  
  # extract payload data file
  untar(tarfile = .fn, files = payload, exdir = .exdir)
  
  df <- readr::read_csv(paste0("./", .exdir, "/", payload), show_col_types = .mouthy)
  
  if(.remove_exdir == TRUE){  # --- clean-up by removing temp dir ----------
    unlink(.exdir, recursive = TRUE)
  }
  return(df)
}
