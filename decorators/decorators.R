rm(list=ls())

# przyklad 1 - dekorator podajacy czas wykonania funkcji

# funkcja ze slabej jakosci petla, aby wykonanie jej nie bylo efektywne 
f <- function(K = 1) {
  r <- c()
  for (k in 1:K) {
    r <- c(r, rnorm(1)) 
  }
}

decorator <- function(f) {
  result <- function(...){
    start <- Sys.time()
    res <- f(...)
    print(paste("Time elapsed:", Sys.time() - start))
  }
  result()
}

`%time_info%` <- function(decorator, f) {
  decorator(f)
}

decorator %time_info%
  f(10)

# dla wiekszych wartosci argumentu czas wykonywania wzrasta
decorator %time_info%
  f(10 ^ 4)


# przyklad 2 - dekorator zapewniajacy wywolanie funkcji z dynamicznym scopingiem

# funkcja przyjmujaca argument y oraz wykorzystujaca zmienna x, ktora nie jest argumentem funkcji 
# ani nie jest zdefiniowana lokalnie
f <- function(y) {
  print(paste(x, " -- ", y))
}

`%dynamic_call%` <- function(decorator, f) {
  x <<- get("x", parent.frame())
  y <- match.call(expand.dots = TRUE)$f$y
  res <- function(...) {
    do.call("f", args = list(y))
  }
  res()
}

# wywolanie w ramach srodowiska globalnego
x <- "global"
f(y = "call from global (static)")

# wywolanie w ramach srodowiska roboczego innej funkcji
g <- function() {
  x <- "local"
  f(y = "call from local (static)")
}

g()

# srodowiskiem otoczeniowym funkcji f jest srodowisko globalne, wiec wywolanie funkcji zawsze 
# bedzie prowadzilo do znalezienia globalnej wartosci zmiennej x


# wywolanie w ramach srodowiska globalnego
x <- "global"

decorator %dynamic_call%
  f(y = "call from global (dynamic)")

# wywolanie w ramach srodowiska roboczego innej funkcji
g <- function() {
  x <- "local"
  decorator %dynamic_call%
    f(y = "call from local (dynamic)")
}

g()

# dzieki dynamicznemu scopingowi zmienna x jest pobierana ze srodowiska wywolania 