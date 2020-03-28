
# Refining the dates of the total output file -----------------------------

#' Function to refine the output data
#'
#' @description There are some modifications to the extracted data which make
#'   sense to calculate once that data is all put together. This function
#'   handles the following:
#'     - Infilling missing payment dates from file names
#'     - Fixing the incorrect years of some of the date (3 rows at last check)
#'
#' @param input_ds
#'
#' @return
#' @export
#'
#' @examples
csr_PurRefineOutputData <- function(input_ds){
        input_ds %>% 
        mutate(inferred.date = csr_PurDateParseFromFileName(Source.url)) %>% 
                # Infilling the missing dates
        mutate(Payment.date.infill = coalesce(Payment.date, inferred.date)) %>% 
        mutate(infilled.date.flag = is.na(Payment.date) & !is.na(inferred.date)) %>% 
                # Handling the incorrect year data
        mutate(Payment.date.infill = case_when(
                lubridate::year(Payment.date.infill) < "2010" 
                |  lubridate::year(Payment.date.infill) >  lubridate::year(Sys.Date()) 
                ~ lubridate::`year<-`(Payment.date.infill, lubridate::year(inferred.date))
                , T ~ Payment.date.infill
        )) %>% 
        select(-inferred.date, -Payment.date) %>% 
        rename(Payment.date = Payment.date.infill)
} 
