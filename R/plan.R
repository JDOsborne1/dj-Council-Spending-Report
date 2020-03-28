
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
        

# Unifying the creditor names ---------------------------------------------

        , credit_matching_list = Output_total %>% 
                pull(Creditor.name) %>% 
                unique() 
        
)

reporting_plan <- drake_plan(
        
        

# Looking at the introductory data ----------------------------------------

        
        date_level_spending_data = Output_total  %>% 
                mutate(Amount.Paid = coalesce(Net.amount, Gross.amount)) %>% 
                arrange(Payment.date) %>% 
                mutate(Total.Spend.so.far = cumsum(Amount.Paid)) 
        
        , dept_level_spending_data = Output_total  %>% 
                mutate(Amount.Paid = coalesce(Net.amount, Gross.amount)) %>% 
                filter(Payment.date > "2010-01-01") %>% 
                filter(Payment.date < "2021-01-01") %>% 
                group_by(Dept, year = lubridate::floor_date(Payment.date, unit = "years")) %>% 
                summarise(total.spend = sum(Amount.Paid, na.rm = T))
        
        
        , date_level_spending_plot = date_level_spending_data %>% 
                filter(Payment.date > "2010-01-01") %>% 
                filter(Payment.date < "2021-01-01") %>% 
                ggplot(aes(x = Payment.date, y = Total.Spend.so.far)) +
                geom_line(colour = '#CC2D2D',size = 2,linetype = 1,alpha = 0.67) + 
                scale_y_continuous(labels = function(x) scales::dollar(x, prefix = "£"))+
                theme_classic() +
                theme(
                        plot.subtitle = element_text(size = 13, hjust = 0.01, vjust = 1)
                        , plot.caption = element_text(vjust = 1)#
                        , axis.ticks = element_line(colour = "black")
                        , axis.title = element_text(face = "bold")
                        , axis.text = element_text(face = "bold")
                        , plot.title = element_text(face = "bold")
                ) + 
                labs(
                        title = "Total Daily Spend"
                        , x = "Payment Day"
                        , y = "Total Spend"
                        , subtitle = "The total council spend across all recipient categories"
                        , caption = "Source: South Gloucestershire Council"
                )
        
        , dept_level_spending_plot = dept_level_spending_data %>% 
                ggplot(aes(x = year, y = total.spend, colour = Dept)) +
                geom_line(aes(group = Dept)) +
                geom_point() +
                # scale_x_date(labels = date_formatter_base) +
                theme_classic() 
        

# Who receives the most money ---------------------------------------------
        , creditor_level_spending_data = Output_total  %>% 
                csr_PurAggregateCreditors()

        # Look into the names with the most money. Since they contain 40% of all payments
        , large_creditor_names = creditor_level_spending_data %>% 
                filter(rank <= 100) %>% 
                pull(Creditor.name) 
        , large_creditor_duplicate_lookup = match_list_generation(large_creditor_names, large_creditor_names)

        # While the algorithm can point out likely matches, it's not currently viable to use it to generate a lookup automatically
        , creditor_name_lookup = tribble(~standard.name, ~secondary.name
                                         , "Aspects &  Milestones Trust", "Milestones Trust"
                                         , "Aspects &  Milestones Trust", "Milestones - Unit 10"
                                         # Worth checking if there is a difference in the below.
                                         , "Bath & North East Somerset Council", "Bath & North East Somerset Dist Council"
                                         , "Brandon Trust", "The Brandon Trust"
                                         , "Brandon Trust", "The Brandon Trust  Olympus House"
                                         , "Eurotaxis Limited", "Eurotaxis Ltd" 
                                         , "Freeways Trust Limited", "Freeways Trust Ltd"
                                         , "Tarmac Trading Limited", "Lafarge Tarmac Trading Limited"
                                         # The two below arent identified as the same company on companies house, but they have identical directors so lumping together
                                         , "Tarmac Trading Limited", "Tarmac Aggregates Limited"
                                         , "Tarmac Trading Limited", "Lafarge Aggregates Limited"
                                         , "Medequip Assistive Technology Limited", "Medequip Assistive Technology Ltd"
                                         # The below are actually different legal entities, but almost certainly fill the same role
                                         , "Network Rail", "Network Rail Infrastructure Limited"
                                         )
                
        , partially_rectified_creditor_level_spending_data = Output_total %>% 
                csr_PurAggregateCreditors(reference_names = creditor_name_lookup)

        , creditor_plot = partially_rectified_creditor_level_spending_data  %>% 
                csr_PurPlotCreditors(limit = 20)
                

# Rendering Reports -------------------------------------------------------

        

        , spending_report = render(knitr_in(!!here("reports/spending_report.Rmd")))
        , implementation_report = render(knitr_in(!!here("reports/implementation_gap.Rmd")))
        )

full_plan <- bind_plans(
        data_load_plan
        , 
        reporting_plan
        )
