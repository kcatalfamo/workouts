#' Average Amount of Weight (lbs) Used in Exercises Each Week
#'
#' This function, avg_lbs_day, generates a ggplot where the average amount of weight in
#' pounds used in different exercises are displayed over a certain number of weeks for
#' different workout days.
#'
#' data <- The desired data set being used
#' b <- The workout day that is being examined.
#'
#'
#' The ggplot() command first subsets the desired data set only for the workout day that
#' is being called into the function where the x-variable is the week number and
#' y-variable is the average weight in pounds.
#'
#' The x-variable, week #, is subtracted by the number 36 because it is ideal to look
#' at the week in which the workout started, week 1, rather than the 37th week of the
#' year.
#' @import ggplot2
#'
#' @return
#' avg_lbs_day will return an aesthetic plot of the workout day being examined.
#'
#' @example avg_lbs_day(workouts, "Leg")
#' @export


avg_lbs_day <- function(data, b)
{
  ggplot(subset(data, Workout_Day == b),
         aes(x = Week - 36, y = value)) +
    xlab("Week") + ylab(" Avg Weight (lbs)") +
    ggtitle("Average Weights (lbs) used for each exercise in Workout Day") +
    geom_line(aes(col = variable))
}
