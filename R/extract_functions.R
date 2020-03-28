library(magrittr)

# Link Retrieval Wrapper --------------------------------------------------



#' Link Getter
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
csr_ImpGenerateLinksFromPage <- function(url){
        url %>% 
                xml2::read_html(encoding = "UTF-8") %>% 
                rvest::html_nodes("a") %>% 
                rvest::html_attr("href") %>%
                tibble::enframe(name = NULL,value = "link")
}




# Data Extraction wrapper -------------------------------------------------



#' Custom reader function for south glos payment data
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
csr_ImpCustomDataReader <- function(URL, .missing_threshold = 0.8) {
        # print(URL)
        temp <- tempfile()
        
        URL %>% 
                {`Encoding<-`(., "latin1")} %>% 
                {gsub("Â", "", .)} %>% 
                download.file(temp, method = "libcurl", mode = "wb")
        
        if(!is.na(readxl::excel_format(temp))){
                print("Excel Formatting :|")
                output <- readxl::read_excel(temp) %>% 
                        dplyr::select_if(function(x) csr_PurPropNA(x) < .missing_threshold)
        } else {
                
                output <- readr::read_csv(
                        temp
                        , guess_max = 5000
                ) %>% 
                        dplyr::select_if(function(x) csr_PurPropNA(x) < .missing_threshold)
                }
        
        output %>% 
                mutate(Source.URL = URL)
}

#' NA values indexer
#'
#' @description Function to determine the row indicies where there are above a
#'   certain threshold of missing values in that row.
#'
#' @param .input_ds
#' @param .threshold
#'
#' @return
#' @export
#'
#' @examples
csr_PurNAIndexer <- function(.input_ds, .threshold = 2){
        output_slice <- dplyr::mutate_all(.input_ds, is.na) %>% 
                rowSums() %>% 
                {. < .threshold}
        print(paste0("Slicing out ", sum(!output_slice), " rows with too many missings"))
        output_slice
}

#' NA Proportion
#'
#' @description Function to determine what proportion of the input vector is
#'   made up of missing values.
#'
#' @param .input_vect
#'
#' @return
#' @export
#'
#' @examples
csr_PurPropNA <- function(.input_vect) {
        sum(is.na(.input_vect))/length(.input_vect)
        }

#' Reader output refinement
#'
#' @description Function to refine the output of the reader function into a
#'   standardised format which can be brought together in the drake plan.
#'
#'   In the process it does the following
#'
#'   1. Use the NA values indexer to determine which rows have too many missing
#'   values
#'   2. Modifying all the column names to have a consistent formatting
#'   3. Unifies the multiple aliases for the same value
#'   4. Adds in any missing columns
#'   5. Handles any rogue characters in the dates
#'   6. Transforms all the dates into date type
#'
#' @param .raw_data
#'
#' @return
#' @export
#'
#' @examples
csr_PurRefineReaderOutput <- function(.raw_data){
        output <- .raw_data %>% 
                {dplyr::filter(., csr_PurNAIndexer(.))} %>% 
                `colnames<-`(toTitleCase(tolower(make.names(colnames(.))))) %>% 
                `colnames<-`(str_replace(colnames(.), "^Ref$", "Ref.no")) %>%  
                `colnames<-`(str_replace(colnames(.), "^Ref.no.$", "Ref.no")) %>%  
                `colnames<-`(str_replace(colnames(.), "^Ref.number$", "Ref.no")) %>%  
                `colnames<-`(str_replace(colnames(.), "^Gl.code.net.amount$", "Net.amount"))
        if(!"Payment.date" %in% colnames(output)){
                output <- tibble::add_column(output, Payment.date = as.Date(NA))
        }
        if(!"Gross.amount" %in% colnames(output)){
                output <- tibble::add_column(output, Gross.amount = as.double(NA))
        }
        if(!"Net.amount" %in% colnames(output)){
                output <- tibble::add_column(output, Net.amount = as.double(NA))
        }
        output <-output %>% 
                dplyr::mutate_at(dplyr::vars(Payment.date), str_replace,  pattern = "^\\[\\]\\s", replacement = "") %>% 
                dplyr::mutate_at(dplyr::vars(Payment.date), lubridate::dmy) %>% 
                dplyr::mutate_at(dplyr::vars(Gross.amount), as.numeric) %>% 
                dplyr::mutate_at(dplyr::vars(Net.amount), as.numeric) %>% 
                # Introduced since one month in particular was saved with an extra set of data not present in any other month's files
                select(-one_of(c("Level.3", "Voucher.no", "Invoice.number", "Ledger.code", "Sortkey", "Cost.centre", "Creditor.code", "Creditor.address")))
        output
        
        ## There are some records with dates well outside the reasonable range, which it would be sensible to repair.
        # At present I have found only 3, but the represent a significant expenditure.
        
}


# Munging functions -------------------------------------------------------


#' Link restriction wrapper
#'
#' @param tibble_of_links 
#' @param control 
#'
#' @return
#' @export
#'
#' @examples
csr_PurRestrictLinks <- function(tibble_of_links, control = T){
        output <- tibble_of_links %>% 
                dplyr::filter(grepl("councilpayments|documents", link)) 
        
        if (control) {
                head(output, 2)
        } else {
                output
        }
}



