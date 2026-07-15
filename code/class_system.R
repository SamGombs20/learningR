#Day 4: S3 class system for R:-attributes(stucture defines an object with it's attributes at a go), methods, generic functions
get_symbols <- function(){
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(wheel, size = 3, replace = TRUE,prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

score <- function(sym){
    price <- 0
    same <- all(sym == sym[1])
    bars <-sym %in% c("B", "BB", "BBB")

    if (same) {
        payouts <- c("DD"=100, "7" =80, "BBB"=40, "BB"=25, "B"=10, "C"=10, "0"= 0)
        price <- unname(payouts[sym[1]])
    } else if (all(bars)) {
    price <- 5
    } else{
        cherries <- sum(sym=="C")
        price <- c(0,2,5)[cherries+1]
    }
    diamonds <- sum (sym == "DD")
    price * 2 ^ diamonds
}
play <- function() {
    symbols <- get_symbols()
    structure(score(symbols), symbols=symbols)
}
slot_display <- function(price){
    #Extract symbols
    symbols <- attr(price, "symbols")

    #Collapse symbols into a single string
    symbols <- paste(symbols, collapse=" ")

    #combine symbol with price
    string <- paste(symbols, price, sep = "\n$")

    cat(string, sep="\n")
}
slot_display(play())

plays_till_broke <-function(start_with){
    cash <- start_with
    n <- 0
    while (cash > 0){
        cash <- cash - 1 + play()
        n <- n +1 
    }
    print(n)
}
plays_till_broke(100)

play_till_broke2 <- function(start_with){
    cash <- start_with
    n <- 0
    repeat{
        cash <- cash - 1 + play()
        n <- n +1
        if (cash <= 0){
            break
        }
    }
    print(n)
}
play_till_broke2(100)
