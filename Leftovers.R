#Leftover Value Box Row

## Row {height="5%"}

```{r}
#| content: valuebox
#| title: "Prize probablilities"
#| color: pink
#| icon: calculator
p("High to low prizes")
```

```{r}
#| content: valuebox
#| title: "Monthly prizes"
#| color: green
#| icon: calendar-day
p("Enter £ of bonds held")
```

```{r}
#| content: valuebox
#| title: "Long term simulations"
#| color: blue
#| icon: hourglass-split
p("Enter years for simulation")
```
#2 Monthly draws
#write a function to generate the monthly winnings

EarnestTab <- function(stake, pool, vals, probs){
  mywins <- sample(pool,stake, replace = TRUE)
  monthPrizes <- sum(mywins)
  monthWinnings <- sample(vals, monthPrizes, replace = TRUE,  prob = probs)
  monthTotal <- sum(monthWinnings)
  yearlyReturn <- (12*monthTotal/stake)*100
  
  # Collapse winnings into a readable string
  winningsStr <- if (length(monthWinnings) == 0) "0" else paste(sort(monthWinnings), collapse = ", ")
  results <- c(monthPrizes, monthTotal, yearlyReturn, winningsStr)
  
}

#arguments for the function

yearResTab <- data.frame(t(sapply(allMonths, function(x) EarnestTab(bondStake, prizePool, prizeVal, prizeProb))))
colnames(yearResTab)[c(1,2,3,4)] <- c("Number_prizes", "Prize_value", "Annual_yield", "Prizes")


Earnest <- function(stake, pool, vals, probs){
  mywins <- sample(pool,stake, replace = TRUE)
  monthPrizes <- sum(mywins)
  monthWinnings <- sample(vals, monthPrizes, replace = TRUE,  prob = probs)
  monthTotal <- sum(monthWinnings)
  
  
  yearRes <- data.frame(t(sapply(allMonths, function(x) Earnest(bondStake, prizePool, prizeVal, prizeProb))))
  colnames(yearRes)[c(1,2,3,4)] <- c("Number_prizes", "Prize_value", "Annual_yield", "Prizes")
  
  yearRes
  
}

YearRes_single <- data.frame(t(sapply(allMonths, function(x) Earnest(bondStake, prizePool, prizeVal, prizeProb))))
