# Function to create a bar plot
#' Generate a bar plot to visualize the distribution or comparison of categorical variables.
#' @param data The dataset containing the categorical variables to be visualized.
#' @param x The name of the categorical variable for the x-axis.
#' @param y (Optional) The name of the numeric variable for the y-axis if frequencies need to be plotted.
#' @return A bar plot representing the distribution or comparison of the specified categorical variables.
create_bar_plot <- function(data, x, y = NULL) {
  # Function implementation
