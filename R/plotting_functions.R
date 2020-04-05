
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
                }
        }


# Plotting cumulative spend -----------------------------------------------

csr_PurPlotCumulativeSpend <- function(input_ds){
        input_ds %>% 
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
}



# Plotting Departmental spend ---------------------------------------------

csr_PurPlotDepartmentSpend <- function(input_ds){
        input_ds %>% 
ggplot(aes(x = year, y = total.spend, colour = Department.Desc)) +
        geom_line(aes(group = Department.Desc)) +
        geom_point() +
        # scale_x_date(labels = date_formatter_base) +
        scale_y_continuous(
                labels = function(x) scales::number(x, prefix = "£", big.mark = ",")
        ) +
        # scale_color_wsj() +
        theme_wsj() 
}
