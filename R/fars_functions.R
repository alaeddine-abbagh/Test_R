#' -------------------------------------------------------------------------
#' fars_read
#' -------------------------------------------------------------------------
#'
#' This function reads dataset.
#'
#' @param filename path of the dataset
#' 
#' @return This function returns a dataset in the tbl form
#'
#' @examples
#' x <- fars_read("accident_2015.csv")
#' x <- fars_read("accident_205.csv") : returns an error
#'
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' -------------------------------------------------------------------------
#' make_filename
#' -------------------------------------------------------------------------
#'
#' This function prints filename with a given year.
#'
#' @param year the year with which  the filename should be created 
#' 
#' @return prints the created filename
#'
#' @examples
#' x <- make_filename("2015")

#'
#' @export


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' -------------------------------------------------------------------------
#' fars_read_years
#' -------------------------------------------------------------------------
#'
#' This function read multiple datasets with different given years, and adds a colomn containing the input year. 
#'
#' @param years a vector of years
#' 
#' @return this function returns datasets corresponding to given years, 
#'  after adding a year colomn, and selecting month and year colomn
#'
#' @examples
#' x <- make_filename("2015")
#' x <- make_filename("2015")
#' x <- make_filename("205"): returns an error because there is not corresponding dataset to year 205

#'
#' @export


fars_read_years <- function(years) {
  lapply(years, function(year) {
                        file <- make_filename(year)
                        tryCatch({
                          dat <- fars_read(file)
                          dplyr::mutate(dat, year = year) %>% 
                            dplyr::select(MONTH, year)
                        }, error = function(e) {
                          warning("invalid year: ", year)
                          return(NULL)
                        })
                      })
}


#' -------------------------------------------------------------------------
#' fars_read_years
#' -------------------------------------------------------------------------
#'
#' This function counts number of observations for given years 
#'
#' @param years a vector of years for which number of observations will be counted  
#' 
#' @return dataframe containing all given years as colomns and a count of observations per month for each year
#'
#' @examples
#' x <- fars_summarize_years(c( "2015", "2014)) 
#' x <- fars_summarize_years(c( "2015", "204)): error because 204 is not valid

#' @imports(dplyr,%>%)  
#' @export



fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' -------------------------------------------------------------------------
#' fars_map_state
#' -------------------------------------------------------------------------
#'
#' This function prints filename with a given year.
#'
#' @param state.num number of a state in the US
#' @param year a given year
#' 
#' @return a plot of accidents' locations of a state
#'
#' @examples
#' x <- fars_map_state(26, "2015")
#' x <- fars_map_state(26, "201"): error because of invalid year


fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  # is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  # is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}