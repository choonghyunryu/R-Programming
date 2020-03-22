my.mean <-
function(x) {
total <- sum(x)

size <- function(x) {
len <- length(x)
return (len)
}

return (total/size(x))
}

