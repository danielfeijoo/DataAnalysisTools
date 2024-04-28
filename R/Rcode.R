#' DataVisTools: Simplifying Data Visualization in R
#'
#' This package provides functions to simplify data visualization tasks in R,
#' including creating common types of plots, customizing plot aesthetics,
#' and visualizing data distributions, trends, correlations, and relationships between variables.
#' @name DataVisTools
#' @docType package
#' @aliases DataVisTools
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes geom_histogram geom_point geom_line geom_bar labs theme
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 facet_wrap facet_grid
#' @importFrom ggplot2 theme_minimal theme_bw theme_classic theme_void theme_dark theme_light
#' @export

# Function to create a histogram
#' Generate a histogram to visualize the distribution of a numeric variable.
#' @param data The dataset containing the numeric variable to be visualized.
#' @param x The name of the numeric variable to be plotted.
#' @return A histogram plot displaying the distribution of the specified numeric variable.
create_histogram <- function(data, x) {
  ggplot(data, aes(x = {{x}})) +
    geom_histogram(fill = "skyblue", color = "black") +
    labs(x = x, y = "Frequency", title = "Histogram") +
    theme_minimal()
}

# Function to create a scatter plot
#' Create a scatter plot to visualize the relationship between two numeric variables.
#' @param data The dataset containing the numeric variables to be visualized.
#' @param x The name of the first numeric variable.
#' @param y The name of the second numeric variable.
#' @return A scatter plot showing the relationship between the specified numeric variables.
create_scatter_plot <- function(data, x, y) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "blue") +
    labs(x = x, y = y, title = "Scatter Plot") +
    theme_minimal()
}

# Function to create a line chart
#' Produce a line chart to visualize the trend of a numeric variable over time or another continuous variable.
#' @param data The dataset containing the variables to be plotted.
#' @param x The name of the continuous variable for the x-axis.
#' @param y The name of the numeric variable for the y-axis.
#' @return A line chart illustrating the trend of the specified numeric variable over the continuous variable.
create_line_chart <- function(data, x, y) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_line(color = "red") +
    labs(x = x, y = y, title = "Line Chart") +
    theme_minimal()
}

# Function to create a bar plot
#' Generate a bar plot to visualize the distribution or comparison of categorical variables.
#' @param data The dataset containing the categorical variables to be visualized.
#' @param x The name of the categorical variable for the x-axis.
#' @param y (Optional) The name of the numeric variable for the y-axis if frequencies need to be plotted.
#' @return A bar plot representing the distribution or comparison of the specified categorical variables.
create_bar_plot <- function(data, x, y = NULL) {
  if (is.null(y)) {
    ggplot(data, aes(x = {{x}})) +
      geom_bar(fill = "green", color = "black") +
      labs(x = x, y = "Count", title = "Bar Plot") +
      theme_minimal()
  } else {
    ggplot(data, aes(x = {{x}}, y = {{y}})) +
      geom_bar(stat = "identity", fill = "green", color = "black") +
      labs(x = x, y = y, title = "Bar Plot") +
      theme_minimal()
  }
}

# Function to customize plot aesthetics
#' Allow customization of plot aesthetics, labels, and annotations.
#' @param plot_object The plot object to be customized.
#' @param title (Optional) The title of the plot.
#' @param x_label (Optional) The label for the x-axis.
#' @param y_label (Optional) The label for the y-axis.
#' @param theme (Optional) The theme settings for the plot.
#' @return The customized plot object.
customize_plot <- function(plot_object, title = NULL, x_label = NULL, y_label = NULL, theme = NULL) {
  customized_plot <- plot_object +
    labs(title = title, x = x_label, y = y_label)
  
  if (!is.null(theme)) {
    customized_plot <- customized_plot + theme(theme)
  }
  
  return(customized_plot)
}

# Function to visualize distribution using a histogram
#' Visualize the distribution of a numeric variable using a histogram.
#' @param data The dataset containing the numeric variable to be visualized.
#' @param x The name of the numeric variable to be plotted.
#' @return A histogram plot displaying the distribution of the specified numeric variable.
visualize_distribution <- function(data, x) {
  create_histogram(data, x)
}

# Function to visualize relationship using a scatter plot
#' Visualize the relationship between two numeric variables using a scatter plot.
#' @param data The dataset containing the numeric variables to be visualized.
#' @param x The name of the first numeric variable.
#' @param y The name of the second numeric variable.
#' @return A scatter plot showing the relationship between the specified numeric variables.
visualize_relationship <- function(data, x, y) {
  create_scatter_plot(data, x, y)
}

# Function to visualize trend using a line chart
#' Visualize the trend of a numeric variable over time or another continuous variable using a line chart.
#' @param data The dataset containing the variables to be plotted.
#' @param x The name of the continuous variable for the x-axis.
#' @param y The name of the numeric variable for the y-axis.
#' @return A line chart illustrating the trend of the specified numeric variable over the continuous variable.
visualize_trend <- function(data, x, y) {
  create_line_chart(data, x, y)
}

# Function to visualize comparison using a bar plot
#' Visualize the distribution or comparison of categorical variables using a bar plot.
#' @param data The dataset containing the categorical variables to be visualized.
#' @param x The name of the categorical variable for the x-axis.
#' @param y (Optional) The name of the numeric variable for the y-axis if frequencies need to be plotted.
#' @return A bar plot representing the distribution or comparison of the specified categorical variables.
visualize_comparison <- function(data, x, y = NULL) {
  create_bar_plot(data, x, y)
}
