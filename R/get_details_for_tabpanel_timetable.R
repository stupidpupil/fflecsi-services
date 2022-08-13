get_details_for_tabpanel_timetable <- function(tabpanel_el){

	tibble(
		tabpanel_id = tabpanel_el |> rvest::html_attr("data-set")
	) |>
	dplyr::left_join(
		by = character(0),
		tabpanel_el |> 
			rvest::html_element("table") |> 
			rvest::html_table() |> 
			rename(DayOfWeekName = 1, Times = 2) 
	)

}
