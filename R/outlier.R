#' Exploratory Data Analysis (EDA) - Outlier Treatment
#'
#' This function takes a integer/numerical variable as an input and gives a list with 2 dataframes, one is for the outlier values and one is after removing outliers from that variable.
#'
#' @param x Data Frame that you want to work with
#' @param response Dependent(Target) Variable
#' @param seed Seed Value, default = 42
#' @param splitRatio Ratio to split the dataframe into Train and Test
#' @param minbucket Minimum number of observations that should be present in each leaf/terminal node
#' @param cp Cost Complexity Paramter Value
#' @param prune Pruning Tree on the basis of minimum Cross Validation Error (xerror), default = FALSE
#' @param plot Plot the model Tree and CP Table
#' @author Gupta, Deepak <deepak@analyticsacedemy.ml>
#' @return A List with 2 dataframes, one is for the outlier values and one is after removing outliers from that variable.
#' @export
#' @examples
#' create.eval.cart(sleep, group, plot = FALSE)


outlier <- function(x) {


  IQR <- stats::IQR(x)

  LL <- stats::quantile(x, 0.25) - 1.5 * IQR
  UL <- stats::quantile(x, 0.75) + 1.5 * IQR

  out <- x[which(x < LL | x > UL)]

  return(IQR)


}
