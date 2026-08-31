# test tidyplots
# Read once at startup, not on every input change

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

# Long term simulations
earnMonth <- function(stake, pool, vals, probs) {
  mywins <- sample(pool, stake, replace = TRUE)
  monthPrizes <- sum(mywins)
  monthWinnings <- sample(vals, monthPrizes, replace = TRUE, prob = probs)
  monthWin <- sum(monthWinnings)
}

earnYear <- function(stake, pool, vals, probs) {
  Resmnth <- sapply(1:12, function(x) earnMonth(stake, pool, vals, probs))
  Resyrtotal <- sum(Resmnth)
}

#ResLife <- reactive({
  
#})

allMonths <- c("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
# set up the pool for prize draws = 1 in 23000 zeros
prizePool <- rep(0, 21000) #from September 2026 goes from 22000 to 21000
x <- round(runif(1) * 21000)
prizePool[x] <- 1

prizeVal <- c(1e6, 100000, 50000, 25000, 10000, 5000, 1000, 500, 100, 50, 25)
# Prize numbers updated for September 2026 from MSE website
prizeNos <- c(2, 95, 192, 382, 954, 1909, 19892, 59676, 2366135, 2366135, 1717659)
allPrizes <- sum(prizeNos)
prizeProb <- prizeNos / allPrizes

bondStake <- 10000
simYrs <- 10
ResLife <- sapply(1:simYrs, function(x) earnYear(bondStake, prizePool, prizeVal, prizeProb))
#ResLife <- data.frame("wins"=sort(ResLife, decreasing = TRUE))

library(ggplot2)
p <- ggplot() +
  aes(ResLife) +
  geom_histogram(binwidth = 0.075, fill = "steelblue", col = "lightblue") +
  scale_x_continuous(trans = "log10") + 
  xlab("£ Value of prizes") +
  ylab("Count")+
  theme_minimal()
p

my_style <- function(x) {
  x |>
    #adjust_colors(colors_discrete_candy) |>
    #add_data_points(shape = 23, size = 3, fill = "lightblue1", color="steelblue") |>
    #add_mean_dot(color = "red") |> 
    adjust_font(family = "verdana", face = "bold") |>
    adjust_x_axis(transform = "log10") |> #, title = "Prize value") |> 
    adjust_x_axis_title("Value of wins per yer £", face = "bold") |>
    adjust_y_axis_title("Count", face = "bold") |>
    adjust_size(width = 100, height = 125) |> 
    theme_tidyplot(fontsize = 16)
}

# Set global options
tidyplots_options(my_style = my_style)

library(tidyplots)
bondStake <- 50000
simYrs <- 100
ResLife <- sapply(1:simYrs, function(x) earnYear(bondStake, prizePool, prizeVal, prizeProb))

ResLife2 <- data.frame("wins"=sort(ResLife, decreasing = TRUE))
ResLife2 |>
  tidyplot(x = wins) |>
  add_histogram(binwidth = 0.075) |> 
  add_reference_lines(
    x = mean(ResLife2$wins),
    linetype = "dotdash", 
    color="red",
    linewidth = 1) |> 
  add_reference_lines(
    x=median(ResLife2$wins),
    color="white",
    linewidth = 1)
  #adjust_x_axis(transform = "log10") |> #, title = "Prize value") |> 
  #adjust_y_axis_title ("Count")
  #add_mean_line(mean(ResLife$wins))