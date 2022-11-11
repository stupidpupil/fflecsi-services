timetables_as_gtfs_flex_tables <- function(timetables){

	timespan_regex <- "^(\\d{2}:\\d{2})\\s+-\\s+(\\d{2}:\\d{2})$"


	timetables <- timetables |> 
		separate_rows(Times, sep="[\n]+") |>
		mutate(Times = Times |> stringr::str_trim()) |>
    	filter(stringr::str_detect(Times, timespan_regex)) |>
		distinct() |> 
		mutate(active=1L, DayOfWeekName = stringr::str_to_lower(DayOfWeekName)) |> 
		pivot_wider(names_from=DayOfWeekName,values_from=active, values_fill=0L) |>
		mutate(stop_id = paste(location_slug, tabpanel_id, sep="_"), route_id = stop_id ) |>
		mutate(
			start_pickup_dropoff_window = Times |> stringr::str_replace_all(timespan_regex, "\\1:00"),
			end_pickup_dropoff_window = Times |> stringr::str_replace_all(timespan_regex, "\\2:00")
			) |>
		group_by(stop_id) |>
		mutate(service_id = paste(stop_id, row_number(), sep="_"), trip_id = service_id) |>
		ungroup()

	calendar <- timetables |>
		select(service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday) |>
		mutate(start_date = lubridate::today(), end_date = start_date + lubridate::weeks(4)) |>
		mutate(start_date = start_date |> strftime("%Y%m%d"), end_date = end_date |> strftime("%Y%m%d"))

	trips <- timetables	 |>
		select(route_id, service_id, trip_id)

	routes <- timetables |>
		select(route_id) |>
		mutate(
			route_short_name = route_id, # HACK
			route_type = 715 # "Demand and Response Bus Service"
		) |> distinct()

	# TODO: Consider duplicating stop_times, despite
	# https://github.com/opentripplanner/OpenTripPlanner/pull/3720
	stop_times <- timetables |>
		select(trip_id, stop_id, start_pickup_dropoff_window, end_pickup_dropoff_window) |>
		mutate(
			pickup_type = 2,
			drop_off_type = 2,
			stop_sequence = 1
		)

	return(list(calendar=calendar, trips=trips, routes=routes, stop_times=stop_times))
}