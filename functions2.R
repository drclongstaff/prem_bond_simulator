# Functions 2
# Read once at startup, not on every input changes

#function to generate number and value of prizes each month
Earnest <- function(stake, pool, vals, probs) {
  mywins <- sample(pool, stake, replace = TRUE)
  monthPrizes <- sum(mywins)
  monthWinnings <- sample(vals, monthPrizes, replace = TRUE, prob = probs)
  monthTotal <- sum(monthWinnings)
  yearlyReturn <- round((12 * monthTotal / stake) * 100, 2)
  
  # Collapse winnings into a readable string
  winningsStr <- if (length(monthWinnings) == 0) "0" else paste(sort(monthWinnings), collapse = ", ")
  results <- c(monthPrizes, monthTotal, yearlyReturn, winningsStr)
}

# function to get monthly winnings
earnMonth <- function(stake, pool, vals, probs) {
  mywins <- sample(pool, stake, replace = TRUE)
  monthPrizes <- sum(mywins)
  monthWinnings <- sample(vals, monthPrizes, replace = TRUE, prob = probs)
  monthWin <- sum(monthWinnings)
}

# function to get yearly winnings
earnYear <- function(stake, pool, vals, probs) {
  Resmnth <- sapply(1:12, function(x) earnMonth(stake, pool, vals, probs))
  Resyrtotal <- sum(Resmnth)
}