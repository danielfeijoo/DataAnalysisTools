# Create Markdown content
markdown_content <- "
# DataVisTools Package Documentation

## Introduction

The DataVisTools package is designed to simplify data visualization in R. It provides a set of functions for creating common types of plots, customizing plot aesthetics, and visualizing data distributions, trends, correlations, and relationships between variables. This document outlines the main functions developed as part of this package.

## Functions

### 1. `create_histogram`

#### Purpose:
This function generates a histogram to visualize the distribution of a numeric variable.

#### Inputs:
- `data`: The dataset containing the numeric variable to be visualized.
- `x`: The name of the numeric variable to be plotted.

#### Outputs:
- A histogram plot displaying the distribution of the specified numeric variable.

#### Example:
```R
# Load required libraries
library(DataVisTools)
library(ggplot2)

# Create example dataset
data <- data.frame(value = rnorm(100))

# Generate histogram
create_histogram(data, x = 'value')

### 2. `create_scatter_plot` 

  

#### Purpose: 

This function creates a scatter plot to visualize the relationship between two numeric variables. 

  

#### Inputs: 

- `data`: The dataset containing the numeric variables to be visualized. 

- `x`: The name of the first numeric variable. 

- `y`: The name of the second numeric variable. 

  

#### Outputs: 

- A scatter plot showing the relationship between the specified numeric variables. 

  

#### Example: 

```R 

# Load required libraries 

library(DataVisTools) 

library(ggplot2) 

  

# Create example dataset 

data <- data.frame(x = rnorm(100), y = rnorm(100)) 

  

# Generate scatter plot 

create_scatter_plot(data, x = 'x', y = 'y') 

``` 

  

### 3. `create_line_chart` 

  

#### Purpose: 

This function produces a line chart to visualize the trend of a numeric variable over time or another continuous variable. 

  

#### Inputs: 

- `data`: The dataset containing the variables to be plotted. 

- `x`: The name of the continuous variable for the x-axis. 

- `y`: The name of the numeric variable for the y-axis. 

  

#### Outputs: 

- A line chart illustrating the trend of the specified numeric variable over the continuous variable. 

  

#### Example: 

```R 

# Load required libraries 

library(DataVisTools) 

library(ggplot2) 

  

# Create example dataset 

data <- data.frame(time = seq(1, 100), value = rnorm(100)) 

  

# Generate line chart 

create_line_chart(data, x = 'time', y = 'value') 

``` 
### 5. `customize_plot` 

  

#### Purpose: 

This function allows customization of plot aesthetics, labels, and annotations. 

  

#### Inputs: 

- `plot_object`: The plot object to be customized. 

- `title`: (Optional) The title of the plot. 

- `x_label`: (Optional) The label for the x-axis. 

- `y_label`: (Optional) The label for the y-axis. 

- `theme`: (Optional) The theme settings for the plot. 

  

#### Outputs: 

- The customized plot object. 

  

#### Example: 

```R 

# Load required libraries 

library(DataVisTools) 

library(ggplot2) 

# Create example dataset 

data <- data.frame(x = rnorm(100), y = rnorm(100)) 

# Generate scatter plot 

plot <- create_scatter_plot(data, x = 'x', y = 'y') 

# Customize plot 

customized_plot <- customize_plot(plot, title = 'Scatter Plot', x_label = 'X-axis', y_label = 'Y-axis') 

``` 

