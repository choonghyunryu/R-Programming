pkgname <- "Testpkg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('Testpkg')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("center")
### * center

flush(stderr()); flush(stdout())

### Name: center
### Title: The mean, median calculations
### Aliases: center
### Keywords: ~kwd1 ~kwd2

### ** Examples

set.seed(1)
x <- rcauchy(10)
center(x, "t")		# trimmed mean
center(x, "med")	# median



cleanEx()
nameEx("my.mean")
### * my.mean

flush(stderr()); flush(stdout())

### Name: my.mean
### Title: Calculate the arithmetic mean
### Aliases: my.mean
### Keywords: ~kwd1 ~kwd2

### ** Examples

my.mean(1:10)



cleanEx()
nameEx("mydata")
### * mydata

flush(stderr()); flush(stdout())

### Name: mydata
### Title: normal random numbers
### Aliases: mydata
### Keywords: datasets

### ** Examples

data(mydata)
mean(mydata)
plot(mydata, type="o")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
