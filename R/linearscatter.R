#' Create a simple plot with a linear regression line
#'
#' @param df Data table
#' @param y Dependent variable name
#' @param x Independent variable name
#' @param xlb x-axis label
#' @param ylb y-axis label
#' @return A plot with independent and dependent variables labelled with a linear regression line.
#' @export
#' @examples
#' spruce.df <- read.csv("SPRUCE.csv")
#' linearscatter(spruce.df, "Height", "BHDiameter", "Breast Height Diameter (cm)", "Height (m)")
linearscatter <- function(df, y, x, xlb, ylb){
  # Convert strings to formula
  formula <- as.formula(paste(y, "~", x))

  # Fit linear model
  dataf.lm <- lm(formula, data = df)

  # Plot
  with(df, {
    plot(get(x), get(y), xlab = xlb, ylab = ylb, pch = 21, bg = "Blue", cex = 1.2)
    abline(dataf.lm)
  })
}

