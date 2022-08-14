get_all_fflecsi_location_urls <- function(){
  locations_url <- "https://www.fflecsi.wales/all-locations/"

  locations_html <- rvest::read_html(locations_url)

  location_urls <- locations_html |> rvest::html_elements("a.locationsblock-list-location-link") |> rvest::html_attr("href")
  
  location_urls |> xml2::url_absolute(locations_url)
}
