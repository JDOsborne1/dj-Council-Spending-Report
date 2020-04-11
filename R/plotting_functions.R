
# Plotting Creditors ------------------------------------------------------
csr_PurPlotCreditors <- function(agg_ds, limit = 20){
        agg_ds %>% 
                filter(rank <= limit) %>% 
                mutate_at(vars(standard.name), as_factor) %>% 
                {
                        ggplot(., aes(x = standard.name, y = Total.payment)) +
                                geom_col() +
                                scale_x_discrete(limits = rev(levels(.$standard.name))) +
                                coord_flip()
                } +
                scale_fill_wsj() +
                scale_y_continuous(
                        labels = function(x) scales::number(x, prefix = `Encoding<-`("£", "UTF-") , big.mark = ",")
                        , expand = c(0,0,0.1, 0)
                ) +
                theme_wsj() +
                theme(
                        plot.title = element_text(size = rel(0.85))
                        , plot.title.position = "plot"
                        , plot.subtitle = element_text(size = rel(0.65), hjust = 0)
                ) +
                labs(
                        title = "Which Creditors Receive the Most?"
                        , subtitle = csr_UtilTitleWrapper("The total amount paid to the top 20 payees", width = 50)
                        , caption = "Data from 2010-04-01 to 2020-03-01"
                )
        
        }


# Plotting cumulative spend -----------------------------------------------

csr_PurPlotCumulativeSpend <- function(input_ds){
        input_ds %>% 
                ggplot(aes(x = Payment.date, y = Total.Spend.so.far)) +
                geom_line(colour = '#CC2D2D',size = 2,linetype = 1,alpha = 0.67) + 
                scale_y_continuous(
                        labels = function(x) scales::dollar(x, prefix = `Encoding<-`("£", "UTF-8"))
                        , expand = c(0, 0, 0.1, 0)
                        )+
                scale_colour_wsj() +
                theme_wsj() +
                theme(
                      plot.subtitle = element_text(size = rel(0.65))
                        , plot.title.position = "plot"
                ) +
                labs(
                        title = "Total Daily Spend"
                        , x = "Payment Day"
                        , y = "Total Spend"
                        , subtitle = csr_UtilTitleWrapper("The total council spend across all recipient categories", width = 50)
                        , caption = "Source: South Gloucestershire Council"
                )
}


# Plotting Monthly Spend --------------------------------------------------

csr_PurPlotMonthlySpend <- function(input_ds){
        NULL
}

# Plotting Departmental spend ---------------------------------------------

csr_PurPlotDepartmentSpend <- function(input_ds){
        input_ds %>% 
ggplot(aes(x = year, y = total.spend, colour = Department.Desc)) +
        geom_line(aes(group = Department.Desc)) +
        geom_point() +
        # scale_x_date(labels = date_formatter_base) +
        scale_y_continuous(
                labels = function(x) scales::number(x, prefix = `Encoding<-`("£", "UTF-8"), big.mark = ",")
        ) +
        # scale_color_wsj() +
        theme_wsj()  +
                theme(
                        plot.subtitle = element_text(size = rel(0.65))
                        , plot.title.position = "plot"
                ) +
        labs(
                title = "Department Spending"
                , subtitle = csr_UtilTitleWrapper("How much is spent by each department and how does this change over time?" , width = 50)
                , caption = "Decoded departments, various sources"
        )
}


# Plotting Voluntary Grants -----------------------------------------------

csr_PutPlotVoluntaryGrantSpend <- function(input_ds){
        input_ds %>% 
                ggplot(aes(x = payment.month, y = total.spend)) +
                geom_line(group = 1) +
                geom_smooth(method = "glm") +
                scale_y_continuous(
                        labels = function(x) scales::dollar(x, prefix = `Encoding<-`("£", "UTF-8"))
                )+
                theme_wsj()+
                theme(
                        plot.subtitle = element_text(size = rel(0.65))
                        , plot.title = element_text(size = rel(1))
                        , plot.title.position = "plot"
                ) +
                labs(
                        title = "Spending on 'Grants to Voluntary Bodies'"
                        , subtitle = csr_UtilTitleWrapper("How much is spent on voluntary grants each month?" , width = 50)
                )
}



# Plotting Spread of Spending ---------------------------------------------

csr_PurPlotSpread <- function(input_ds){
        input_ds %>% 
                ggplot(aes(x = payment.month, y = mean.payment)) +
                geom_point() +
                geom_smooth(method = "lm", formula = y~x) +
                theme_wsj()+
                theme(
                        plot.subtitle = element_text(size = rel(0.65))
                        , plot.title = element_text(size = rel(1))
                        , plot.title.position = "plot"
                ) +
                labs(
                        title = "Changes in the mean payment per month"
                        , subtitle = csr_UtilTitleWrapper("Does the concentration of payments change over time?" , width = 50)
                )
}

