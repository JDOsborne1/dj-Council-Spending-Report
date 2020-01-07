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
generateLinksFromPage <- function(url){
        url %>% 
                xml2::read_html(encoding = "UTF-8") %>% 
                rvest::html_nodes("a") %>% 
                rvest::html_attr("href") %>%
                tibble::enframe(name = NULL,value = "link")
}




# Data Extraction wrapper -------------------------------------------------

#' Data extraction function 
#'
#' @param links the tibble of all links on the data extraction page
#' @param control_links A control argument to decide to restrict the links to load
#'
#' @return A collated tibble of all the data on the data extraction page
#' @export
#'
#' @examples
dataExtraction <- function(links, control_links = TRUE){
        extract <- links %>% 
                restrictLinks(control_links) %>% 
                dplyr::mutate(extracted_data = purrr::map(link, customDataReader)) %>% 
                dplyr::mutate(extracted_data = purrr::map(extracted_data, alterNames, make.names))
        
        ## Check if the colnames are all the same
        colname_consistency <- extract %>% 
                dplyr::mutate(cols = purrr::map(extracted_data, colnames)) %>% 
                dplyr::pull(cols) %>% 
                {base::Reduce(allVectEqual, .)}
        if (!colname_consistency){
                warning("The colnames are not consistent, so the unnested dataframe is returned instead")
                return(extract)
        }
        
        # Unnesting and returning
        extract %>% 
                tidyr::unnest(cols = extracted_data)
}


#' Custom reader function for south glos payment data
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
customDataReader <- function(URL) {
        print(URL)
        temp <- tempfile()
        
        URL %>% 
                {`Encoding<-`(., "latin1")} %>% 
                {gsub("Â", "", .)} %>% 
                download.file(temp, method = "libcurl", mode = "wb")
        
        if(!is.na(readxl::excel_format(temp))){
                print("Excel Formatting :|")
                readxl::read_excel(temp) %>% 
                        # since we cannot assume a constant columnname for the excel files, we make the assumption 
                        # that one of the first and second column will be missing in the summary elements
                        dplyr::filter(!is.na(.[1]) & !is.na(.[2]))
        } else {
                
                raw_temp <- readLines(temp)
                
                # remove the cases with more than 2 missing values adjacent
                refined_temp <- raw_temp[!grepl(",,,", raw_temp)]
                print(paste0("number of rows removed:", sum(grepl(",,,", raw_temp))))
                # removing any extra commas at the end of the csv
                refined_temp2 <- gsub(",*$", ",", refined_temp)
                writeLines(refined_temp2, temp)
                readr::read_csv(
                        temp
                ) %>% 
                        dplyr::select(-dplyr::starts_with("X"))
                }
        
}

# Munging functions -------------------------------------------------------


#' Data refinement wrapper
#'
#' @param a_tibble 
#'
#' @return
#' @export
#'
#' @examples
refineData <- function(a_tibble){
        a_tibble %>% 
                dplyr::select(-link) %>% 
                dplyr::mutate_at(dplyr::vars(Payment.date), lubridate::as_date, format = "%d-%b-%Y", tz = "UTC")
}


#' Multiple vector equality checking
#'
#' @param vect_1 
#' @param vect_2 
#'
#' @return
#' @export
#'
#' @examples
allVectEqual <- function(vect_1, vect_2){
        all(vect_1 == vect_2)
}   

#' Link restriction wrapper
#'
#' @param tibble_of_links 
#' @param control 
#'
#' @return
#' @export
#'
#' @examples
restrictLinks <- function(tibble_of_links, control = T){
        output <- tibble_of_links %>% 
                dplyr::filter(grepl("councilpayments|documents", link)) 
        
        if (control) {
                head(output, 2)
        } else {
                output
        }
}



#' Colname modification framework
#'
#' @param df 
#' @param alter_func 
#'
#' @return
#' @export
#'
#' @examples
alterNames <- function(df, alter_func) {
         colnames(df) <- alter_func(colnames(df))
        df
}