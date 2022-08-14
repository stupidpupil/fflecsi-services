get_details_and_timetables_for_fflecsi_locations <- function(){
  urls <- get_all_fflecsi_location_urls()

  timetables <- tibble::tibble()
  details <- tibble::tibble()

  for (url in urls) {

    d <- get_details_and_timetable_for_fflecsi_location_url(url)

    timetables <- timetables |> bind_rows(d$timetables)
    details <- details |> bind_rows(d$details)

  }

  return(list(details = details, timetables = timetables))
}