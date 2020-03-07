library(drake)
library(data.table)
library(tools)


source(here::here("R/extract_functions.R"))
parsing_failures <- c( 
        # Phase 1
        URL_404_failure       = "http://www.southglos.gov.uk//documents/2-May-2018-Invoices-ALL-Paid-over-£500-for-web-publish.csv"    
        ,URL_parsing_failure_1 = "http://www.southglos.gov.uk//documents/December-2018-Invoices-All-Paid-Over-500-for-web-publishing.csv"
        ,URL_parsing_failure_2 = "https://www.southglos.gov.uk//documents/Payments-Over-500-July-2019.csv"
        
        
        #Phase 2
        , URL_parsing_failure_3 = "http://hosted.southglos.gov.uk/councilpayments/June17.csv"
        , URL_parsing_failure_4 = "http://hosted.southglos.gov.uk/councilpayments/Nov16.csv"
        , URL_parsing_failure_5 = "http://hosted.southglos.gov.uk/councilpayments/june16.csv"
        , URL_parsing_failure_6 = "http://hosted.southglos.gov.uk/councilpayments/march15.csv"
        , URL_parsing_failure_7 = "http://hosted.southglos.gov.uk/councilpayments/december12.csv"
        , URL_parsing_failure_8 = "http://hosted.southglos.gov.uk/councilpayments/october10.csv"
        , URL_parsing_failure_9 = "http://hosted.southglos.gov.uk/councilpayments/june10.csv"
        
        # Phase 3
        , URL_parsing_failure_10 = "http://hosted.southglos.gov.uk/councilpayments/november10.csv"
)

link_list <- readd(refined_links)$link
data_load_plan <- drake_plan(
        max_expand = 8
        # Extracts the links from the payments page, refreshing each month
        , links = target(
                command = generateLinksFromPage(
                "https://www.southglos.gov.uk/business/tenders-and-contracts/how-we-spend-our-money/council-payments-over-500/"
                )
                , trigger = trigger(change = strftime(Sys.Date(), format = "%b"))
        )
        
        , refined_links = links %>% 
                restrictLinks(control = FALSE) 
        
        , Reader_Output = target(
                customDataReader(URL = link_list)
                , transform = map(
                        link_list = !!link_list
                )
        )
        
        , Refined_Reader_Output = target(
                refineReaderOutput(Reader_Output)
                , transform = map(
                        Reader_Output 
                )
        )

)
