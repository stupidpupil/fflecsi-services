write_geometry_as_otp_safe_geojson <- function(in_sfc, out_path) {

  checkmate::assert_path_for_output(out_path, overwrite = TRUE, extension = "geojson")
  checkmate::assert_subset("stop_id", colnames(in_sfc))

  unlink(out_path)

  in_sfc |> 
    select(stop_id) |>
    sf::write_sf(out_path)

  readr::read_file(out_path) |>
    stringr::str_replace('"name": ".+?",\n', '') |>
    stringr::str_replace_all('"properties": \\{ "stop_id": "(.+?)" \\}', '"id": "\\1", "properties": {}') |>
    readr::write_file(out_path)


  check_sfc <- sf::read_sf(out_path)

  stopifnot(setequal(check_sfc$id, in_sfc$stop_id))

  return(out_path)
}