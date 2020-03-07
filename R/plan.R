library(drake)
library(data.table)

source(here::here("R/extract_functions.R"))

data_load_plan <- drake_plan(
        
        # Extracts the links from the payments page, refreshing each month
        links = target(
                command = generateLinksFromPage(
                "https://www.southglos.gov.uk/business/tenders-and-contracts/how-we-spend-our-money/council-payments-over-500/"
                )
                , trigger = trigger(change = strftime(Sys.Date(), format = "%b"))
        )
        
        , refined_links = links %>% 
                restrictLinks(control = FALSE) 
        
        
)