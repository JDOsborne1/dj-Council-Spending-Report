
# Title Wrapper -----------------------------------------------------------

# Source: https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-se:--
# --:t-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
csr_UtilTitleWrapper <- function(x, ...) 
{
        paste(strwrap(x, ...), collapse = "\n")
}