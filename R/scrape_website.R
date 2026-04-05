get_facility_links <- function(page_url){


  # Request the webpage
  response <- httr::GET(page_url)

  # load the html as an rvest object
  # for parsing later
  page_html <- rvest::read_html(response)

  # get the facility's address
  addresses <- page_html |>
               rvest::html_elements(".bodytext > td:nth-child(2)") |>
               rvest::html_text() |>
               stringr::str_remove_all("\r\n") |>
               stringr::str_squish() |>

               # fix commas
               stringr::str_replace(" ,", ",")


  # name and link
  names <- page_html |>
           rvest::html_elements(".bodytext > td:nth-child(1)") |>
           rvest::html_text()|>
           stringr::str_remove_all("\r\n") |>
           stringr::str_squish()

  links <- page_html |>
           rvest::html_elements(".bodytext > td:nth-child(1) > a") |>
           rvest::html_attr("href")

  # extract the facility id from the link
  facility_ids <- links |>
                 stringr::str_extract("(?<=facid\\=).*")


  # build a dataframe
  facilities_df <- data.frame(name = names,
                              address = addresses,
                              facility_id = facility_ids,
                              link = links)


  facilities_df


}


get_facilities <- function(agency = "stl", zip_code = "63" ) {


  # Define URLS for later use
  # In theory this will work with any agency that uses envision connect
  base_url <- paste0('https://pressagent.envisionconnect.com/results.phtml?agency=',
                     agency,
                     '&violsortfield=TB_CORE_INSPECTION_VIOL.VIOLATION_CODE')

  first_page_search_params <-  paste0('&offset=0&businessname=&businessstreet=&city=&zip=',
                           zip_code,
                           '&facilityid=&FTS=&soundslike=&sort=FACILITY_NAME')



  # This function searches Saint Louis County's food inspection website
  # for all facilities, and then loops through all the pages of results
  # to return a list of facilities

  # Build the first page using the base url
  # and search param the user provided
  first_page_url <- paste0(base_url, first_page_search_params)

  # Request the webpage
  response <- httr::GET(first_page_url)

  # load the html as an rvest object
  # for parsing later
  first_page_html <- rvest::read_html(response)

  # find the two top rows

  # Find how many facilities were return for the search
  top_two_rows <- first_page_html |>

                   # Find the top two rows
                   rvest::html_elements(xpath = "//td[@valign='top']")


  # Extract the text from the second row
  # which contains the number of facilities
  n_facilities <- top_two_rows[[2]] |>
    rvest::html_text(trim = TRUE) |>
    stringr::str_extract("[0-9]+(?= matches)") |>
    as.numeric()



  print(paste0("Found ", n_facilities, " facilities for your search"))


  # Determine how many pages of results there are
  # there are 50 results per page
  n_pages <- ceiling(n_facilities / 50)

  # create a vector of offsets which correspond to each page of results
  offsets <- seq(from=0, to=n_pages, by=1) * 50


  # Create a list of pages to visit
  page_urls <-  paste0(base_url,
                       '&offset=',
                       offsets,
                       '&businessname=&businessstreet=&city=&zip=',
                       zip_code,
                       '&facilityid=&FTS=&soundslike=&sort=FACILITY_NAME')



  # Use the get_facility_links function from above
  # to get names, adresses and links
  # for all the facilities on each page of results
  facilities_df <- purrr::map_dfr(page_urls, get_facility_links)



}





get_inspections <- function(endpoint_url){


  base_url <- "https://pressagent.envisionconnect.com/"

  url <- paste0(base_url, endpoint_url)

  page_html <- url |>
    rvest::read_html()


  tables <- page_html |>
    rvest::html_table()


  # if there's only one column in the second table
  # it means there are multiple permits
  if(ncol(tables[[2]]) == 1){


    links <- page_html |>
      rvest::html_nodes("table:nth-child(2)") |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")


    violations <- purrr::map_dfr(links, get_violations)


  } else {

    violations <- get_violations(url)


  }


  # return the dataframe
  violations



}




get_violations <- function(url){


  page_html <- url |>
    rvest::read_html()


  tables <- page_html |>
    rvest::html_table()


  facility_id <- stringr::str_extract(url, "(?<=facid\\=).*")

  print(url)
  print(facility_id)





  # the second table is a header with the facility's name and address
  name_address <- tables[[2]] |>
    dplyr::pull(X2)




  # the third table contains all the inspection data
  violations <- tables[[3]] |>

    # move column names out of first row
    janitor::row_to_names(row_number = 1) |>

    # Extract the html tag for all of the inspection's violations
    # and use it to get inspection id number
    dplyr::mutate(inspection_id = stringr::str_extract(`Inspection Type`, "(?<=spanId = ).*(?=;)")) |>
    dplyr::mutate(inspection_id = stringr::str_extract(inspection_id,"[A-Z0-9]+")) |>

    # Fill up to add a tag to all rows
    tidyr::fill(inspection_id, .direction = "up")  |>

    # remove blank rows
    dplyr::filter(!is.na(Score)) |>

    # Add data from other tables
    dplyr::mutate(facility_name =  stringr::str_squish(name_address[2]),
                  address1 = stringr::str_squish(name_address[3]),
                  address2 = stringr::str_squish(name_address[4]),
                  facility_id = facility_id
    ) |>

    dplyr::select(date = Date,
                  inspection_type = `Inspection Type`,
                  score = Score,
                  facility_name,
                  address1,
                  address2,
                  facility_id,
                  inspection_id) |>
    # For each inspection, extract violations
    dplyr::mutate(
      violations = purrr::map(inspection_id, function(inspection_id) {


        print(inspection_id)


        violation_elements <- page_html |>
          rvest::html_elements(paste0("#", inspection_id))

        # Extract violations
        violation_codes <-  violation_elements |>
          rvest::html_node("ul") |>

          # the violations are hidden by default
          # so we have to extract the text from the js function
          rvest::html_nodes("script") |>
          rvest::html_text() |>
          stringr::str_squish() |>

          # extract violations based on the common code pattern
          stringr::str_extract_all("F0\\d\\d -.*(?=</li>)") |>
          unlist() |>
          stringr::str_squish()





        # Extract violation text (or set NA if none)
        violation_texts <- if(length(violation_codes) > 0) {

          # Violation comments are stored inside a li with this style attribute
          violation_elements |>
            rvest::html_nodes("li[style=' list-style:none;']") |>
            rvest::html_text() |>

            # clean text
            stringr::str_remove_all("\r\n") |>
            stringr::str_squish()

        } else {
          NA_character_
        }


        # If violation codes is an empty vector
        # that means that no violations were cited
        if(length(violation_codes) < 1) {
          violation_codes <- NA
          violation_texts <- "No violations cited"
        }


        # Return a data.frame with violation type and text
        data.frame(

          violation_code = violation_codes,
          violation_text = violation_texts
        )
      })
    ) |>
    # Unnest so each violation gets its own row
    tidyr::unnest(violations)


  violations



}
