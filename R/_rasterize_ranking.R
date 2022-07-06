#' @description This function converts a dataframe of ranked ocean pixels into a raster
#' @param
#' @keywords
#' @export
#' @examples
#' make_priority_maps()


rasterize_ranking <- function(rank_df){

  rank_raster <- ocean_df %>%
    filter(f_eez > 0) %>%
    left_join(rank_df) %>%
    replace(is.na(.), 0) %>%
    arrange(pick_order) %>%
    mutate(rank = 1 - scales::rescale(base::rank(pick_order, ties.method = "first", na.last = F)),) %>%
    select(lon, lat, rank, f_highly_mpa) %>%
    rasterFromXYZ(crs = crs(ocean_low_res_moll))

  return(rank_raster)
}
