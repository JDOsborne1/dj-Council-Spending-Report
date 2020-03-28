
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
