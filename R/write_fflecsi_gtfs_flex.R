write_fflecsi_gtfs_flex <- function(out_folder){

  details_and_timetables <- get_details_and_timetables_for_fflecsi_locations()
  geometry <- details_and_timetables$details |> geometry_for_details()
  gtfs_flex_tables <- details_and_timetables$timetables |> timetables_as_gtfs_flex_tables()

  fs::dir_create(out_folder)

  geometry |> write_geometry_as_otp_safe_geojson(fs::path(out_folder, "locations", ext="geojson"))

  for(tn in names(gtfs_flex_tables)){
    gtfs_flex_tables[[tn]] |> 
      readr::write_csv(fs::path(out_folder, tn, ext="txt"))
  }
}