#'Calories Burned in Each Workout Day Boxplot
#'
#' The function, calories_burned_boxpt, takes an x-variable that is categorical and
#' numeric y-variable of a dataset to create a boxplot in ggplot.
#'
#' data <- The desired data set being used
#' x <- The workout day that is being examined.
#' y <- The amount of calories burned
#'
#' The ggplot() command maps the x-variable and y-varialbe from the data set desired
#' to use for the plot.
#' The geom_boxplot command maps the color of each level in the x-variable to be
#' specified through scale_color_manual and for the inside of each box-whisker to be
#' be colored to be gray70 throught scale_fill_manual command.
#'
#' A x-axis, y-axis, and title for the graph are added generically where whatever
#' x-variable and y-variable is desired will be printed out on the graph.
#'
#' @import ggplot2
#'
#' @return
#' calories_burned_boxpt will return an aesthetic colored boxplot of the distribution
#' of the calories burned in each workout day.
#'
#'@example calories_burned_boxpt(workouts, "Workout_Day", "Calories_Burned")
#'@export



calories_burned_boxpt <- function(data,x,y){
  ggplot(data,
         aes(data[[x]], data[[y]])) +
    geom_boxplot(aes(color = data[[x]], fill = data[[x]])) +
         xlab(gsub("_", " ", x)) +
         ylab(gsub("_", " ", y)) +
         ggtitle(paste(gsub("_"," ", y), "vs", gsub("_", " ", x))) +

      scale_color_manual(values=c("black", "magenta3", "dodgerblue2",
                               "seagreen4", "firebrick1", "darkorange2")) +
      scale_fill_manual(values = c("gray76","gray76","gray76","gray76","gray76",
                                   "gray76"))
}

