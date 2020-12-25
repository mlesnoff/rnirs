.bisquare <- function(x, a = 1)
    c(ifelse(abs(x) < a, (1 - (x / a)^2)^2, 0))

.cauchy <- function(x, a = 1)
    c(ifelse(abs(x) < a, 1 / (1 + (x / a)^2), 0))

.epan <- function(x, a = 1)
    c(ifelse(abs(x) < a, 1 - (x / a)^2, 0))

.fair <- function(x, a = 1)
    c(ifelse(abs(x) < a, 1 / (1 + abs(x / a))^2, 0))

.gauss <- function(x, a = 1)
    c(ifelse(abs(x) < a, exp(-(x / a)^2), 0))

.huber <- function(x, a = 1.345)
    c(ifelse(abs(x) < a, 1, a / abs(x)))

.invexp <- function(x, a = 1)
    c(ifelse(abs(x) < a, 1 / exp(abs(x / a)), 0))

.talworth <- function(x, a = 1)
    c(ifelse(abs(x) < a, 1, 0))

.trian <- function(x, a = 1)
    c(ifelse(abs(x) < a, 1 - abs(x / a), 0))

.tricube <- function(x, a = 1)
    c(ifelse(abs(x) < a, (1 - abs(x / a)^3)^3, 0))


