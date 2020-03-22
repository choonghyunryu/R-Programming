###################################################
# A. 조건문
###################################################

###################################################
# 1. if
###################################################
item <- 0 ; opt <- 0
x <- 10; y <- pi
if (item>=0 && opt==1)  x <- log10(x)	# (1) 기본 if문
x

if (!opt) {								# (2) 블록 사용
	x <- x * pi
	y <- y / pi
	cat("condition is TRUE, block running!!!\n")
}
x;y

base <- -3
if (TRUE) {								# (3) 중첩 if 문
	tmp=10
	if (base<0) stop("Base must be positive")
	log(tmp, base)
}

if (c(T,F)) 
  cat("Condition length is two logical vector.\n")	# (4) 조건 벡터 길이 2
  
if (NA) cat("Condition is NA.\n")		# (5) NA

###################################################
# 2. if~else
###################################################
x <-10; y<-3
if (x==0) 0 
else exp(y*log(x))				# (1) if~else
if (x==0) 0 else exp(y*log(x)) 	# (2) if~else 수정

x<-1; y<-2; z<-3
if (x>=y) {						# (3) 중첩 if~else
  if (x>=z) cat("MAX number(x) is", x, "\n")
  else cat("MAX number(z) is", z, "\n")
} else {						# (4) 회피 구문
    if (y>=z) cat("MAX number(y) is", y, "\n")
	else cat("MAX number(z) is", z, "\n")
}

###################################################
# 3. ifelse 함수
###################################################
(x <- 2:-2)
ifelse(x>=0, x, -x)					# (1) 절대값 구하기
x>=0								# (2) test 벡터
ifelse(x>=0, LETTERS, letters)		# (3) test 길이가 짧음
log(x)								# (4) 경고 발생
log(ifelse(x>=0, x, NA))			# (5) 경고 미발생
ifelse(x>=0, log(x), NA)			# (6) 경고 발생

x <- c(2,7,3)
y <- c(5,4)
z <- 1:6
ifelse(x>y, x, y)					# (7) Warning
ifelse(x>z, x, z)					# (8) Recycle

ifelse(NA, x, y)   					# (9) NA
ifelse(TRUE, x, y) 					# (10) x[1]
ifelse(0, x, y)						# (11) y[1]

ifelse2 <-							# (12) 함수 정의
function (test, yes, no) 
{
	if (is.na(test)) return("test is NA!!!")
	
    if (test) return(yes)
    else return(no)
}

ifelse2(NA, x, y)   				# (13) NA
ifelse2(4, x, y) 					# (14) x
ifelse2("FALSE", x, y)				# (15) y

set.seed(1)
(x <- rnorm(5))
round(ifelse(x>1.5, x, 
	ifelse(x<0, -1, 0)))			# (16) 중첩 ifelse

###################################################
# 4. switch 함수
###################################################
opt <- 3
switch(opt, 10, 20, 30, 40, 50)		# (1) 정수 EXPR
opt <- TRUE
switch(opt, 10, 20, 30, 40, 50)		# (2) 논리값 EXPR
opt <- 10
switch(opt, 10, 20, 30, 40, 50)		# (3) 목록 개수 초과

x <- rnorm(1000, mean=0, sd=2)

set.seed(1)
measure <- function(x, type) {		# (4) 문자열 EXPR
	switch(type,					
        mean = mean(x),
        median = median(x),
        trimmed = mean(x, trim = .1),
		sd = sd(x))
}

measure(x, "mean")					# (5) 평균
measure(x, "MEAN")					# (6) 매치 안됨	
measure(x, "sd")					# (7) 표준편차
measure(x, "abc")					# (8) 매치 안됨

measure2 <- function(x, type) {		# (9) 대소문자 미 구별1
	cat(type, ":",
	switch(type,
		MEAN =,	Mean =,
        mean = mean(x),
		MEDIAN =, Median =,
        median = median(x),
		TRIMMED =, Trimmed =,
        trimmed = mean(x, trim = .1),
		SD =, Sd =,
		sd = sd(x),
		"Invalid type!!!"		
		), "\n")	
}

measure2(x, "mean")					# (10) 소문자
measure2(x, "MEAN")					# (11) 대문자
measure2(x, "abc")					# (12) 매치 안됨

measure3 <- function(x, type) {		# (13) 대소문자 미 구별2
	cat(type, ":",
	switch(tolower(type),
        mean = mean(x),
        median = median(x),
        trimmed = mean(x, trim = .1),
		sd = sd(x),
		"Invalid type!!!"		
		), "\n")	
}

measure3(x, "mean")					# (14) 소문자
measure3(x, "Mean")					# (15) 대문자


###################################################
# B. 반복문
###################################################
###################################################
# 1. repeat 문
###################################################
repeat cat("무한루프, ESC로 탈출!!\n")	# (1) 무한루프

tot <- 0
i <- 1
repeat  {								# (2) 탈출 조건
	tot <- tot + i
	if (i >= 100)  break				
	i <- i + 1
}
tot

###################################################
# 2. while 문
###################################################
(x <- 5);(y <- 10)
while (x==y) { 			# (1) 표현식 미 수행
	y <- y - 1 
	x <- x + 1
}
x; y

tot <- 0
i <- 1
while (i<=100) {		# (2) 정상 수행
	tot <- tot + i			
	i <- i + 1
}
tot

tot <- 0
i <- 1
while (TRUE) {			# (3) while 문과 유사
	tot <- tot + i
	if (i >= 100)  break				
	i <- i + 1
}
tot

###################################################
# 3. for 문
###################################################
tot <- 0
for (i in 1:100) {				# (1) 1부터 100
	tot <- tot + i 
}
tot

str <- ""
for (i in LETTERS) { 			# (2) A부터 Z
	str <- paste(str, i, sep="") 
}
str

x <- c(3,5,7,11,13)
for (i in 1:length(x)) { 	# (3) 벡터
	cat(x[i])
	if (length(x)==i) cat("\n")
}

month <- factor(month.abb, levels=month.abb)
for (i in month) { 				# (4) 범주형자료
	cat(i, ":", is(i)[1], "\n")
}

###################################################
# C. 루프 탈출문
###################################################

###################################################
# 1. break
###################################################
repeat {
	cat ("Only one excute.\n")
	break						# (1) repeat문 탈출
}

for (i in 2:9) {
	for (j in 1:9) {
		cat(i, "*", j, "=", i*j, "\n")
		if (j==3) break			# (2) 안쪽 for문 탈출
	}
	cat("\n")
	if (i==4) break				# (3) 바깥 for문 탈출
}

###################################################
# 2. next
###################################################
for(n in 1:5) {
	cat(n,": ")
	cat(sum(1:n),"\n")
}

for(n in 1:5) {
	cat(n,": ")
	next						# (1) next 
	cat(sum(1:n),"\n")
}

for(n in 1:5) {
   if (n == 3) next				# (2) for문에서의 next 
   cat(n,":", sum(1:n),"\n")
}

n=0
repeat {
   n = n+1
   if (n > 5) break	
   if (n == 3) next				# (3) repeat문에서의 next
   cat(n,":", sum(1:n),"\n")
}

n=0
while (n<5) {
   n = n+1
   if (n == 3) next				# (4) while문에서의 next
   cat(n,":", sum(1:n),"\n")
}

x=1000

	 repeat { 
	   if ( x <=10e-10) break 	
        x <- x / 10
	   if (x == 0.01) next
	   cat("aaaaa ", x)
	 }


