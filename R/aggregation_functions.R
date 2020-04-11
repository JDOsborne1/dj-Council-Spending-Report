
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



# Department Level Aggregations -------------------------------------------

csr_PurAggregateDepartments <- function(input_ds){
        input_ds %>% 
                left_join(csr_ImpGenerateDepartmentLookup(), by = "Dept") %>% 
                mutate_at(vars(Department.Desc), replace_na, "Unknown") %>% 
                group_by(Department.Desc, year = lubridate::floor_date(Payment.date, unit = "years")) %>% 
                summarise(total.spend = sum(Amount.Paid, na.rm = T)) %>% 
                ungroup()
}

# Date Level Aggregations -------------------------------------------------

csr_PurAggregateCumulativeSpending <- function(input_ds){
        input_ds %>% 
                # Removing one record where there is a genuine null value in the source data
                filter(!(Ref.no == "3532" & Payment.date == "2012-03-27")) %>% 
                arrange(Payment.date) %>% 
                mutate(Total.Spend.so.far = cumsum(Amount.Paid)) 
}



# Payment Spread Tracking -------------------------------------------------

csr_PurAggregatePaymentSpread <- function(input_ds){
        input_ds %>% 
                # Removing one record where there is a genuine null value in the source data
                filter(!(Ref.no == "3532" & Payment.date == "2012-03-27")) %>% 
                mutate(payment.month = lubridate::floor_date(Payment.date, unit = "quarter")) %>% 
                #  normalising many payments to the same creditor in one month
                group_by(payment.month, Creditor.name) %>% 
                summarise(total.payment = sum(Amount.Paid)) %>% 
                ungroup() %>% 
                # then aggregating to the mean value per month
                group_by(payment.month) %>% 
                summarise(mean.payment = mean(total.payment))
        
        
}

csr_PurInvestigatePaymentGradients <- function(input_ds){
        input_ds %>% 
                # Removing one record where there is a genuine null value in the source data
                filter(!(Ref.no == "3532" & Payment.date == "2012-03-27")) %>% 
                mutate(payment.month = lubridate::floor_date(Payment.date, unit = "quarter")) %>% 
                #  normalising many payments to the same creditor in one month
                group_by(payment.month, Description) %>% 
                summarise(total.payment = sum(Amount.Paid)) %>% 
                ungroup()  %>% 
                group_by(Description) %>% 
                summarise(
                        first.payment = mean(head(total.payment,4))
                        , last.payment = mean(tail(total.payment, 4))
                        , num.payments = n()
                ) %>% 
                mutate(
                        payment.gradient = (last.payment - first.payment)/num.payments/first.payment
                )
}
