get_details_for_script_element <- function(script_element){

  # Example:
  # src = 'https://www.fflecsi.wales/app/uploads/2021/09/Pembs_region.kml';
  # if (!document.getElementById('map-1555'))

  id_and_kml_regex <- "src = '(https://.+\\.kml)';?\\n?\\s+.+'map-(\\d+)'"

  results <- script_element |> rvest::html_text() |> stringr::str_match_all(id_and_kml_regex)


  if(length(results[[1]]) == 0){
    return(tibble::tibble())
  }

  results[[1]] |> 
    tibble::as_tibble(.name_repair) |> 
    dplyr::rename(map_id=V3, kml_url =V2) |> 
    dplyr::select(-V1)
}
