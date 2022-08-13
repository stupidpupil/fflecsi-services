get_details_for_tabpanel <- function(tabpanel_el){

	tibble(
		tabpanel_id = tabpanel_el |> rvest::html_attr("data-set"),
		map_id = tabpanel_el |> rvest::html_element("div[id^=map]") |> rvest::html_attr("id") |> stringr::str_sub(start=5L),
		name = tabpanel_el |> rvest::html_element("h2") |> rvest::html_text()
	)

}