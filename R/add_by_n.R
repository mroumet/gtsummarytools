

#..............................................................----
# Function to add Nobs per group to a gtsummary table ----
#..............................................................----
#' bl_nobs_function
#'
#' This function can be used to calculate the number of Nobs (i.e. number of non-missing data).
#' It can be used to add the Nobs to a new column into a gtsummary table (see example). 
#'
#' @param data a dataset
#' @param variable a variable
#' @param by the grouping variable
#' @param ... 
#'
#'
#' @examples 
#'  table %>%
#'  add_stat( fns = everything() ~ add_by_n    )
#'  #(where table is a gtsummary table)
#'  
#' @import dplyr 
#' @noRd 



add_by_n <- function(data, variable, by, ...) {
  by_col <- NULL
  
  data |> 
    select (all_of ( c(variable, by)) ) |> 
    arrange (pick ( all_of( c(by, variable) ) ) ) |> 
    group_by (.data[[by]]) |> 
    summarise_all (~sum(!is.na(.))) %>%
    rlang::set_names( c("by", "variable")) %>%
    mutate(
      by_col = paste0 ("add_n_stat_", row_number()),
      variable = style_number (variable)
    ) %>%
    select (-by) %>%
    tidyr::pivot_wider(names_from = by_col, 
                       values_from = variable)
}
