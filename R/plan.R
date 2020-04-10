
data_load_plan <- drake_plan(

        max_expand = Inf

        # Extracts the links from the payments page, refreshing each month
        , links = target(
                command = csr_ImpGenerateLinksFromPage(
                "https://www.southglos.gov.uk/business/tenders-and-contracts/how-we-spend-our-money/council-payments-over-500/"
                )
                , trigger = trigger(change = strftime(Sys.Date(), format = "%b"))
        )
        
        , refined_links = links %>%
                csr_PurRestrictLinks(control = FALSE)
        
        , Reader_Output = target(
                csr_ImpCustomDataReader(URL = link_list)
                , transform = map(
                        link_list = !!link_list
                )
        )

        , Refined_Reader_Output = target(
                csr_PurRefineReaderOutput(Reader_Output)
                , transform = map(
                        Reader_Output
                )
        )
        
        , Output_total = target(
                bind_rows(Refined_Reader_Output)
                , transform = combine(Refined_Reader_Output)
        )
        
        , Output_refined = Output_total %>% 
                csr_PurRefineOutputData()

# Unifying the creditor names ---------------------------------------------

        , credit_matching_list = Output_total %>% 
                pull(Creditor.name) %>% 
                unique() 
        
)

reporting_plan <- drake_plan(
        
        


# Data Trends over time ---------------------------------------------------


        
        date_level_spending_data = Output_refined  %>% 
                csr_PurAggregateCumulativeSpending()
        
        , dept_level_spending_data = Output_refined  %>% 
                csr_PurAggregateDepartments()
        
        
        , date_level_spending_plot = date_level_spending_data %>% 
                csr_PurPlotCumulativeSpend()
        
        , dept_level_spending_plot = dept_level_spending_data  %>% 
                csr_PurPlotDepartmentSpend()

# Who receives the most money ---------------------------------------------
        , creditor_level_spending_data = Output_refined  %>% 
                csr_PurAggregateCreditors()

        # Look into the names with the most money. Since they contain 40% of all payments
        , large_creditor_names = creditor_level_spending_data %>% 
                filter(rank <= 100) %>% 
                pull(Creditor.name) 
        , large_creditor_duplicate_lookup = match_list_generation(large_creditor_names, large_creditor_names)

        
        , creditor_name_lookup = csr_ImpGenerateCreditorLookup()
                
        , partially_rectified_creditor_level_spending_data = Output_refined %>% 
                csr_PurAggregateCreditors(reference_names = creditor_name_lookup)

        , creditor_plot = partially_rectified_creditor_level_spending_data  %>% 
                csr_PurPlotCreditors(limit = 20)
                


# Voluntary Composition over time -----------------------------------------

        , voluntary_data = Output_refined %>%
        csr_PurAggregateVoluntaryGrants

        , voluntary_plot = voluntary_data %>% 
        csr_PutPlotVoluntaryGrantSpend()
        

# Rendering Reports -------------------------------------------------------

        

        , spending_report = render(knitr_in(!!here("reports/spending_report.Rmd")))
        , implementation_report = render(knitr_in(!!here("reports/implementation_gap.Rmd")))
        )

full_plan <- bind_plans(
        data_load_plan
        , 
        reporting_plan
        )
