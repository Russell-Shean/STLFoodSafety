
# create a data directory if it doesn't already exist


facilities_df <- get_facilities() |>

                 # Because of the way pagination works
                 # some facilities may have been pulled twice
                 # let's get rid of duplicates
                 # then save the file
                 distinct() |>
                 readr::write_csv(file = "data/facilities_list.csv")






violations <- purrr::map_dfr(facilities_list$link, get_inspections) |>
              readr::write_csv(file = "data/violations.csv")

violations2 <- violations |>

               dplyr::mutate(
                 # extract the zip code from the address
                 zip_code = stringr::str_extract(address2, "(?<=MO )63\\d+"),

                 # convert the date to a date
                 date = lubridate::mdy(date),

                 # convert scores to a number
                 score = readr::parse_number(score),

                 # Make sure the inspection ids don't have spaces
                 inspection_id = stringr::str_squish(inspection_id),

                 # create some additional date columns
                 month = lubridate::month(date),
                 year = lubridate::year(date),
                 month_year = lubridate::floor_date(date, "month"))


# De-duplicate inspections
# We're assuming any inspection with identical, date, facility, score, etc
# Is a mistake where the inspector submitted the report twice
inspections2 <- violations2 |>
  dplyr::distinct(date,inspection_type,score,facility_name, address1, address2,zip_code, .keep_all = TRUE) |>

  # remove violation specific columns
  dplyr::select(-violation_code, -violation_text)


# Record duplicated inspections so that the team can review
duplicated_inspections <- violations2 |>
  dplyr::group_by(date, inspection_type, score, facility_name, address1, address2, zip_code) |>
  dplyr::filter(dplyr::n_distinct(inspection_id) > 1) |>
  dplyr::distinct(date, inspection_type, score, facility_name, address1, address2, zip_code, inspection_id, .keep_all = TRUE) |>
  dplyr::ungroup()

# Record scores over 100 or equal to zero (max score is 100...)
incorrect_scores <- inspections2 |>
                    dplyr::filter(score == 0 | score > 100)


# Remove incorrect scores
inspections2 <- inspections2 |>
                dplyr::filter(score > 0,  score < 101)
