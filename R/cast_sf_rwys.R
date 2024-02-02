# RUNWAY UTILITY FUNCTIONS

# setup sf objects
#arp_pt   <- arp_egll |> cast_latlon_to_pts()
#rwys_pts <- rwys_egll |> cast_latlon_to_pts()

# utility function to coerce data frame of runway points to sf linestring
cast_rwy_ls <- function(.rwys_df, .rwy_name = NAME){
  rwys_ls  <- .rwys_df |> 
    dplyr::group_by({{.rwy_name}}) |>
    dplyr::group_modify(.f = ~ cast_latlon_to_ls(.) ) |> 
    sf::st_as_sf()
  return(rwys_ls)
}

#' utility function to create centerline
#' 
#' @param .rwy_df tibble of thresholds
#' @param .ctrl_length length of center line in NM
#' 
#' @return
rwy_ctr_line <- function(.rwy_df, .ctrl_length = 10){
  df <- .rwy_df |> 
    dplyr::filter(REF != "ARP") |> 
    dplyr::select(REF, NAME, LAT, LON) |> 
    dplyr::group_by(NAME) |> 
    dplyr::mutate( 
             LAT2 = dplyr::case_when(
                              !is.na(dplyr::lag(LAT))  ~ dplyr::lag(LAT)
                             ,!is.na(dplyr::lead(LAT)) ~ dplyr::lead(LAT))
            ,LON2 = dplyr::case_when(
                               !is.na(dplyr::lag(LON))  ~ dplyr::lag(LON)
                              ,!is.na(dplyr::lead(LON)) ~ dplyr::lead(LON))
    ) |> 
    # calculate "reverse" runway bearing with geosphere::bearingRhumb
    dplyr::mutate( 
      RBRG  = geosphere::bearingRhumb( p1 = cbind(LON2, LAT2)
                                      ,p2 = cbind(LON, LAT)
                                      )
    )
  
  # determine "endpoint" of extended centerline at d = .ctrl_length in meters
  ## coerce NM to meters
  .ctrl_length = .ctrl_length * 1852   # 1NM = 1852m
  ## determine endpoints
  tmp <- with(df, geosphere::destPointRhumb(cbind(LON, LAT), b= RBRG, d = .ctrl_length)) |> 
    dplyr::as_tibble() |> 
    dplyr::rename(LON3 = lon, LAT3 = lat)
  
  # combine and return
  df <- df |> dplyr::bind_cols(tmp)
  return(df)
}

# utility function to coerce extension points
rwy_ctr_line_pts <- function(.rwy_ctr_line, .debug = FALSE){
  # could not get pivot_longer work with multiple cols
  tmp1 <- .rwy_ctr_line |> dplyr::select("REF":"LON")
  
  # include opposite runway threshold
  tmp2 <- .rwy_ctr_line |> dplyr::select("REF","NAME", "LAT2","LON2") |>
    dplyr::rename(LAT = LAT2, LON = LON2)
  
  # centerline end point determined beforehand
  tmp3 <- .rwy_ctr_line |> dplyr::select("REF","NAME", "LAT3","LON3") |>
    dplyr::rename(LAT = LAT3, LON = LON3)
  
  if(.debug == TRUE){ 
    df <- dplyr::bind_rows(tmp1, tmp2, tmp3)
  }else{
    df <- dplyr::bind_rows(tmp1, tmp3)
  }
  
  df <- df |> dplyr::arrange(REF, NAME)
  return(df)  
}

#' Establish extended runway centerlines
#' 
#' @param .rwys_df tibble of runway thresholds
#' @param .ctrl_length length of centerline in NM, defaults to 10NM
cast_rwy_ctr_line_ls <- function(.rwys_df, .ctrl_length = 10, ...){
  # TODO - make this nicer and allow sf
  ## check for non-sf, i.e. rwys_df, and drop geometry (will cause wrong result)
  if("sf" %in% class(.rwys_df)) .rwys_df <- rwys_df |> sf::st_drop_geometry()
  ## cast LAT/LON points to centerline linestring
  .rwys_df |> 
    rwy_ctr_line(.ctrl_length, ...) |> 
    rwy_ctr_line_pts(...) |> 
    cast_latlon_to_pts(...) |> 
    cast_pts_to_ls(.group_var = REF)
}


# utility function to build box around buffered runway thresholds
airport_threshold_box <- function(.rwys_df, .thr_buffer = 500, ...){
  # cast to sf-pts and add buffer
  rwys_pts_buf <- .rwys_df |> cast_latlon_to_pts() |> 
    sf::st_buffer(dist = .thr_buffer)
  # combine and add convex hull to encompass all thresholds
  rwys_box <- rwys_pts_buf |> sf::st_union() |> sf::st_convex_hull()
  
  return(rwys_box)
}

airport_centerline_box <- function(.rwys_df, .thr_buffer = 500, .ctrline_buffer = 500, ...){
  airport_box  <- airport_threshold_box(.rwys_df, .thr_buffer)
  ctr_line_buf <- cast_rwy_ctr_line_ls(.rwys_df, ...) |> 
    sf::st_buffer(dist = .ctrline_buffer)
  
  combo_box    <- sf::st_union(airport_box, ctr_line_buf) |> 
    sf::st_convex_hull() |> 
    sf::st_union()
  return(combo_box)
}


#' construct a cone-like shape defined by runway thresholds and a point on the extended centerline
#' 
#' @param .rwy_thresholds tibble comprising LAT/LON for runway thresholds
#' @param .threshold_buffer approximate circle around threshold in meters (not projection accurate)
#' @param .ctrline_buffer approximate circle around point on extended centerline in meters
#' 
#' @return tibble of {sf}polygon shape(s) defined by threshold(s) and centerline point(s)
#' 
centerline_cone <- function(.rwy_thresholds, .threshold_buffer = 100, .ctrline_buffer = 500){
  # Create circles around each runway threshold
  thres_buf <- .rwy_thresholds |> 
    dplyr::select(REF, NAME, TYPE, LAT, LON) |> 
    cast_latlon_to_pts(.drop_coord = FALSE) |> 
    sf::st_buffer(dist = .threshold_buffer)
  
  # extended centerlines
  ctr_line_buf <- .rwy_thresholds |> 
    rwy_ctr_line() |> dplyr::ungroup() |> 
    dplyr::select(REF, NAME, LAT = LAT3, LON = LON3) |> 
    cast_latlon_to_pts(.drop_coord = FALSE) |> 
    sf::st_buffer(dist = .ctrline_buffer) |> 
    dplyr::mutate(TYPE = "EXT")
  
  tmp <- dplyr::bind_rows(thres_buf, ctr_line_buf) |> 
    dplyr::group_by(REF, NAME) |>  
    dplyr::summarise(do_union = FALSE, .groups = "drop") |>  
    sf::st_convex_hull()
  return(tmp)
}