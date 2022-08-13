get_details_for_fflecsi_location_url <- function(fflecsi_location_url){

  fflecsi_location_html <- rvest::read_html(fflecsi_location_url)

  location_slug <- fflecsi_location_url |> stringr::str_match("^.+/(.+?)/?$")
  location_slug <- location_slug[[2]]

  script_elements <- fflecsi_location_html |> rvest::html_elements("script")

  maps_to_kml <- tibble::tibble(kml_url = character(0), map_id = character(0))

  for (el in script_elements) {
    maps_to_kml <- maps_to_kml |> dplyr::bind_rows(get_details_for_script_element(el))
  }


  timetables <- tibble::tibble(tabpanel_id = character(0))

  timetable_elements <- fflecsi_location_html |> rvest::html_elements(".tabpanel-panel[id*=timetable]")

  for (el in timetable_elements) {
    timetables <- timetables |> dplyr::bind_rows(get_details_for_tabpanel_timetable(el))
  }


  tabpanel_details <- tibble::tibble(tabpanel_id = character(0), map_id = character(0), name = character(0))

  tabpanels <- fflecsi_location_html |> rvest::html_elements(".tabpanel-panel:not([id*=timetable])")

  for (el in tabpanels) {
    tabpanel_details <- tabpanel_details |> dplyr::bind_rows(get_details_for_tabpanel(el))
  } 

  details <- tabpanel_details |> dplyr::left_join(maps_to_kml, by="map_id")

  timetables$location_slug <- location_slug
  details$location_slug <- location_slug

  return(list(timetables = timetables, details = details))

}
