tw_data_download <- function(params,
                             client_id,
                             client_secret,
                             start_date,
                             end_date,
                             ...) {
  ##  Make Initial API request:
  res <- httr::GET(url = url_alert,
                   httr::add_headers(client_id = client_id,
                                     client_secret = client_secret),
                   query = params) 
  ##  Potential Error Handling 
  if (httr::status_code(res) != 200){
    shiny::showModal(modalDialog(paste("Request failed with status code: ", 
                                       httr::status_code(res)),
                                 "API Request Error",
                                 footer = shiny::tagList(actionButton("ok","OK"))))
    shiny::observeEvent(input$ok, {
      shiny::removeModal()
    })
  }else{
    ##  Retrieve the content
    foo_dis_dat <- res %>%
      ##  Retrieve content:
      httr::content() %>%
      purrr::pluck("items") %>% 
      dplyr::bind_rows()
    ##  Make 2nd or more request if the original request is equal to
    ##  the limit, i.e. check if there are more records to retrieve
    if (nrow(foo_dis_dat) == 1000) {
      ##  Set a flag that there is possibly more records
      maybe_more <- TRUE
      ##  Set out API query offset parameter
      off_set <- nrow(foo_dis_dat)
      
      while (maybe_more) {
        ##    Set query parameters to retrieve the observations up to 48 
        ##    hours from the present time:
        params <- paste("limit=1000&",
                        sprintf("offset=%s&", off_set),
                        sprintf(tw_date_query_temp_start, 
                                "1", "1", "1",
                                start_date),
                        sprintf(tw_date_query_temp_end,
                                "2", "2", "2",
                                end_date),
                        sep="&")
        ##  Make additional API request:
        foo_res <- httr::GET(url = url_alert,
                             httr::add_headers(client_id = client_id,
                                               client_secret = client_secret),
                             query = params)
        ##  Check if the response is valid and nonempty
        ##  Potential Error Handling 
        if (httr::status_code(foo_res) != 200) {
          maybe_more <- FALSE
          showModal(modalDialog(paste("Additional requests failed with status code: ", 
                                      httr::status_code(foo_res)),
                                "API Request Error",
                                footer = shiny::tagList(actionButton("ok","OK"))))
          observeEvent(input$ok, {
            removeModal()
          })
        }else{
          ##  If valid check for non empty response:
          if (!is.null(httr::content(foo_res) %>% 
                       purrr::pluck("items"))) {
            ##  Process the additional data:
            foo_dis_dat <- foo_dis_dat %>%
              rbind({httr::content(foo_res) %>% 
                  purrr::pluck("items") %>% 
                  dplyr::bind_rows()})
            ##  Repeat to see if there are more by changing 
            ##  the offset:
            off_set <- nrow(foo_dis_dat)
          }else{maybe_more <- FALSE}
        }
      }
    }
    ##  return the data:
    return(foo_dis_dat)
  }
}
