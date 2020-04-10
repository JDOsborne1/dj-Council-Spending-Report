
# Creditor Aggregation ----------------------------------------------------
csr_PurAggregateCreditors <- function(input_ds, reference_names = NULL){
        output <- input_ds %>% 
                mutate(Amount.Paid = coalesce(Net.amount, Gross.amount)) %>% 
                group_by(Creditor.name) %>% 
                summarise(
                        Number.of.payments = n()
                        , Total.payment = sum(Amount.Paid)
                        , Average.Payment = Total.payment/Number.of.payments
                        ) %>% 
                arrange(desc(Total.payment)) %>% 
                mutate(rank = row_number())
        
        if(!is.null(reference_names)){
               output <- output %>% 
                       left_join(reference_names, by = c("Creditor.name" = "secondary.name")) %>% 
                       mutate(standard.name = coalesce(standard.name, Creditor.name)) %>% 
                       group_by(standard.name) %>% 
                       summarise(
                               Number.of.payments = sum(Number.of.payments)
                               , Total.payment = sum(Total.payment)
                               , Average.Payment = Total.payment/Number.of.payments
                               ) %>% 
                       arrange(desc(Total.payment)) %>% 
                       mutate(rank = row_number())
        }
        output
}




# Voluntary Grant Aggregations --------------------------------------------

csr_PurAggregateVoluntaryGrants <- function(input_ds){
        input_ds  %>% 
                filter(Description == "Grants to Voluntary Bodies") %>%
                mutate(payment.month = lubridate::floor_date(Payment.date, unit = "month")) %>% 
                group_by(payment.month) %>% 
                summarise(total.spend = sum(Amount.Paid))
        }
