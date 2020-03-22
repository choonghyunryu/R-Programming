center <-
function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)

  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

