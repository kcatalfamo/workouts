#' Body Fat Percentage
#'
#' This function, bodyfat, takes all the measurements needed to determine the body fat
#' percentage of a woman and calculate both the lean body mass and body fat percentage of
#' that said person.
#'
#'
#' important
#' This function will only calculate the lean body mass and body fat percentage with
#' measurements for a women.
#'
#' weight <- The weight in pounds of that person.
#' wrist <- The circumference in inches of the person's wrist, at the widest spot,
#'  divided by the correct constant for women.
#' waist <- The circumference in inches of the person's waist, at the naval,
#'  multiplied by the correct constant for women.
#' hip <- The circumference in inches of a person's hips, at their fullest point,
#'  multiplied by the correct constant for women.
#' forearm <- The circumference in inches of a person's forearms, at the widest point,
#'  multiplied by the correct constant for women.
#' LBM <- The caculation to determine the lean body mass of that person with their
#'  weight and wrist, waist, hip, and forearm circumferences being considered.
#' fat_percent <- The caculation of the body fat percentage of that person with their
#'  previously calculated lean body mass and weight being considered.
#'
#'
#'
#' @return
#' This function will return the lean body mass and body fat percentage of someone who
#' is a woman.
#'
#' @example bodyfat(145, 6.0, 24, 38, 9.5)
#' @export



bodyfat <- function(weight, wrist, waist, hip, forearm){
  wrist <- wrist/3.14
  waist <- waist * 0.157
  hip <- hip * 0.249
  forearm <- forearm * 0.434

  # lean body mass
  LBM <- (
    ((
      ((0.732 * weight) + 8.987) + wrist) - waist) - hip) + forearm

  # body_fat_percent
  fat_percent <- ((weight - LBM) * 100) / weight

  ## want I want to see
  LBM <- round(LBM, digits=2) ; fat_percent <- round(fat_percent, digits = 2)
  c(paste("LBM: ",LBM),
    paste("body fat %: ",fat_percent))
}

