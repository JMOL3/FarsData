#' Reading Fars Data
#'
#' \code{fars_read} is a function that reads in Fatality Analysis Reporting System data.
#'  You can select the file wanted (using the \code{filename} argument)
#'
#' @param filename A character string giving the file name of a .csv file that you wish to read into the data
#'
#' @return  A tibble data frame object containing the FARS dara.
#'
#' @note If the specified filename does not exist,
#'  returns "file '" \code{filename}"' does not exist".
#'
#' @export
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' \dontrun{
#' fars_read("doesnt_exist.csv") # error
#' }
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make FARS Filename
#'
#' \code{make_filename} formats a filename for Fatality Analysis Reporting System data, given a year as input
#'
#' @param year A number (integer, numeric or string) that can be coerced into a string to represent the year of the data,
#'  can be any number.
#'
#' @return The function will return a string of the filename in propper FARS data form, for the given year.
#'
#' @export
#'
#' @examples
#' make_filename(year = '2013')
#' make_filename(year = 2013)
#'
#' \dontrun{
#' make_filename(year="twenty eighteen") # error
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS Files over multiple years
#'
#' \code{fars_read_years} returns a list of data frames containing Fatality Analysis Reporting System data,
#'  each data frame containing the year and month data of observations, but not the obsevations themselves
#'
#'
#' @param years A vector of strings, each containing a number (integer, numeric or string) that
#'  can be coerced into a string, to represent the years of the data required
#'
#' @return A list of data frames, each containing the month and year data for the Fars data sets requested.
#'
#' @note  if an invalid year is given, returns \code{"invalid year"}
#'
#' @importFrom dplyr mutate select
#'
#' @export
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
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

#' FARS Summary by Year
#'
#' \code{fars_summarize_years} presents a summary of
#'  Fatality Analysis Reporting System data by year and month
#'  over a given vector of years
#'
#' @param years A vector of strings, each containing a number (integer, numeric or string) that
#'  can be coerced into a string, to represent the years of the data required
#'
#' @return Returns dataframe of month by year values for FARS data, will only return years with valid FARS data
#'
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows group_by summarize
#'
#' @export
#'
#' @examples
#' fars_summarise_years(c(2013,2014,2015))
#' \dontrun{
#' fars_summarise_years("3009")# error
#' }
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map FARS Data
#'
#' \code{fars_map_state} maps Fatality Analysis Reporting System data for a given state and year,
#'  plotting individual vehicle fatality sites over a map of the state for a given year.
#'
#' @param state.num A number (integer, numeric or string) that can be coerced into a string,
#'  between 1 and 56 (excluding3,7 and 14), representing alphabetically the 50 us states,
#'  DC, Puerto rico, the Virgin islands.
#' @param year A number (integer, numeric or string) that can be coerced into a string to
#'  represent the year of the data, can be any number.
#'
#' @return Returns a map of fatalities in the given state for the given year,
#'  fatality sites are indicated in black.
#'
#' @note If the state number provided is outside of the range of possible states and territories returns:
#'  "invalid STATE number: "\code{state.num}.
#'
#' @note If there are no fatalities found, returns:
#'  no accidents to plot"
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
#' @examples
#' fars_map_state(1,2015)
#' fars_map_state(28,2013)
#'
#' \dontrun{
#' fars_map_state(Alabama,2015)# error
#' }
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
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

