get_sfc_for_fflecsi_locations <- function(){

  details_and_timetables <- get_details_and_timetables_for_fflecsi_locations()

  location_geoms <- geometry_for_details(details_and_timetables$details)

  osm_style_hours <- timetables_as_osm_opening_hours(details_and_timetables$timetables)

  location_geoms |>
    dplyr::left_join(details_and_timetables$details, by=c("tabpanel_id", "location_slug")) |>
    dplyr::left_join(osm_style_hours, by=c("tabpanel_id", "location_slug")) |>
    dplyr::mutate(
      url = paste0("https://www.fflecsi.wales/locations/", location_slug, "/")
    ) |>
    dplyr::rename(
      service_hours = OpeningHoursOSM
      ) |>
    dplyr::arrange(url, tabpanel_id) |>
    dplyr::select(
      name,
      url,
      service_hours,
      kml_url,
      tabpanel_id
      )
}