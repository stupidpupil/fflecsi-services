geometry_for_details <- function(details){

	geom_temp <- sf::st_sfc(crs="EPSG:4326")

	for (i in 1:nrow(details)) {
		src_url <- details[[i, "kml_url"]]

		tgeo <- sf::st_read(src_url)

		tgeo <- tgeo |> dplyr::mutate(
			src_url = src_url,
			tabpanel_id = details[[i, "tabpanel_id"]],
			location_slug = details[[i, "location_slug"]]
		) |> select(src_url, tabpanel_id, location_slug)

		geom_temp <- geom_temp |> rbind(tgeo)
	}


	geom_temp |> 
		group_by(tabpanel_id, location_slug) |>
		summarise() |>
		ungroup() |> 
		mutate(stop_id = paste(location_slug, tabpanel_id, sep="_"))

}