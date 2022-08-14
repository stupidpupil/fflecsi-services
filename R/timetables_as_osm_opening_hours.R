timetables_as_osm_opening_hours <- function(timetables) {

  days_of_week_tfw <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  days_of_week_osm <- c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")

  timetables <- timetables |> dplyr::mutate(
      DayOfWeekIndex = match(DayOfWeekName, days_of_week_tfw)
    )

  temp_out <- tibble(DayOfWeek = character(0), Hours = character(0), tabpanel_id = character(0), location_slug = character(0))

  timespan_regex <- "^(\\d{2}:\\d{2})\\s+-\\s+(\\d{2}:\\d{2})$"

  # TODO - Public Holidays

  timetables |> 
    distinct() |>
    separate_rows(Times, sep="[\n]+") |>
    mutate(Times = Times |> stringr::str_trim()) |>
    filter(stringr::str_detect(Times, timespan_regex)) |>
    group_by(tabpanel_id, location_slug, Times) |> 
    summarise(
      DayOfWeekOSM = case_when(
        setequal(DayOfWeekIndex, 1:5) ~ "Mo-Fr",
        setequal(DayOfWeekIndex, 1:6) ~ "Mo-Sa",
        setequal(DayOfWeekIndex, 1:7) ~ "Mo-Su",
        setequal(DayOfWeekIndex, 6:7) ~ "Sa-Su",
        all(DayOfWeekIndex %in% 1:7) ~ days_of_week_osm[DayOfWeekIndex] |> paste0(collapse = ","),
        TRUE ~ NA_character_
      )
    ) |>
    mutate(Times = Times |> stringr::str_replace(timespan_regex, "\\1-\\2")) |>
    group_by(tabpanel_id, location_slug, DayOfWeekOSM) |>
    arrange(tabpanel_id, location_slug, Times) |>
    summarise(TimesOSM = paste0(Times, collapse=",")) |>
    mutate(DayPlusTime = paste(DayOfWeekOSM, TimesOSM, sep=" ")) |>
    group_by(tabpanel_id, location_slug) |>
    summarise(OpeningHoursOSM = paste0(DayPlusTime, collapse="; "))
}
