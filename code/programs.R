#Day 3
get_symbols <- function(){
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(wheel, size = 3, replace = TRUE,prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}
symbols = get_symbols()
print(symbols)
price <- 0
same <- all(symbols == symbols[1])
bars <-symbols %in% c("B", "BB", "BBB")

if (same) {
    payouts <- c("DD"=100, "7" =80, "BBB"=40, "BB"=25, "B"=10, "C"=10, "0"= 0)
    price <- unname(payouts[symbols[1]])
} else if (all(bars)) {
   price <- 5
} else{
    cherries <- sum(symbols=="C")
    price <- c(0,2,5)[cherries+1]
}
diamonds <- sum (symbols == "DD")
price * 2 ^ diamonds
print(price)