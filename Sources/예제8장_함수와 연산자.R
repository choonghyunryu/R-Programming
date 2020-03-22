###################################################
# A. 함수
###################################################

###################################################
# 1. 함수의 정의
###################################################
# (1) 인수를 갖는 함수
my.mean1 <- function(data) { 
	n <- length(data)
	sum <- 0
	
	for (i in 1:n) sum <- sum + x[i]
	
	return (sum/n)
}

int.x <- 1:10
mean(int.x)
my.mean1(int.x)
my.mean1(1:10)

# (2) 인수를 갖지 않는 함수
dumy.arg <- function()
{
	cat("Hello Wolrd!!!\n")
	cat("Welcome R System ", date(), "\n")
}

dumy.arg()

# (3) 값을 반환하는 함수
my.mean2 <- function(x, na.rm=FALSE) {
	if (na.rm) x <- x[!is.na(x)]	# NA 제거

	mean.val <- sum(x)/length(x)

	return (mean.val)
}

x <- c(1:5, NA, NA, 10)
my.mean2(x)
my.mean2(x, na.rm=TRUE)
my.mean2(na.rm=TRUE)

# (4) 값을 반환하지 않는 함수
my.mean3 <- function(data) { 
	cat("Data = ", data, "\n")
	cat("Arithmetic Mean = ", mean(data), "\n")
}

my.mean3(1:10)

my.func <-				# (1) 함수 생성 확인
function (data, na.rm=FALSE) 
{
        if (!is.numeric(data))
                stop("Data is not numeric vecter")

        mean.val <- my.mean2(data, na.rm)

        return(mean.val)
}


###################################################
# 2. 함수의 인수 전달 방법
###################################################
swap <- function(x, y) # 스왑함수
{	
	temp <- x
	x <- y
	y <- temp
}

a <- 10
b <- 20
 
swap(a, b)
a
b

x <- c(3,5,7,11,NA,17,19)
y <- c(3,5,7,11,13,17,19)
sd						# (1) 표준편차 함수
sd(y)					# (2) 순서에 의한 전달
sd(x, T)				# (3) 순서에 의한 전달
sd(na.rm=T, x=x)		# (4) 이름에 의한 전달
sd(na.rm=T, y=y)		# (5) 오류

###################################################
# 3. 함수의 기본 구성 요소
###################################################
x <- c(3,5,7,11,13,17,NA,23,27)
y <- 5

# (1) 심볼 인수 함수
func.symbol <- function(x, y)		 
{
	return(x+y)
}

func.symbol(x, y)

# (2) 디폴트 인수 함수
func.dflt <- function(data, trim=0, na.rm=F) 
{
	mean(data, trim=trim, na.rm=na.rm)
}

func.dflt(x)
func.dflt(x, 0.2, T)
func.dflt(trim=0.3, data=x, na.rm=T)

# (3) ... 인수 함수
func.dots <- function(data, ...) 
{
	trim <- 0
	na.rm <- F

	dots <- list(...)

	for (arg in names(dots)) {
		if (arg=="trim")
			trim <- as.numeric(dots[arg])
		if (arg=="na.rm")
			na.rm <- as.logical(dots[arg])
	}

	mean(data, trim=trim, na.rm=na.rm)
}

func.dots(x)
func.dots(x, na.rm=T)
func.dots(x, trim=0.1)
func.dots(x, trim=0.1, na.rm=T)


# (4) ... 인수 함수: 다른 함수로 인수 패스
func.dots1 <- function(data, ...) 
{
	mean(data, ...)
}

func.dots1(x)
func.dots1(x, na.rm=T)
func.dots1(x, trim=0.1)
func.dots1(x, trim=0.1, na.rm=T)

# (5) 인수 패스된 다른 함수
mean

# (6) 인수 조회
args(func.symbol)
args(func.dflt)
args(func.dot)

# (7) 함수 몸체 조회
body(func.symbol)
body(func.dflt)
body(func.dots)

# (8) 함수 environment 조회
environment(func.symbol)
environment(plot)
environment(ls)


###################################################
# 4. 인수 사용 여부 파악
###################################################
x <- c(3,5,7,11,13,17,19,23,27,29)

# (1) 인수의 개수 구하기
get.args.no <- function(x, ...) 
{
	nargs()
}

get.args.no(x, trim=0.1, na.rm=T)

# (2) 인수 사용 여부
get.arg.yn <- function(a, ...) 
{
	cat("a 인수 사용여부 :", hasArg(a), "\n")
	cat("b 인수 사용여부 :", hasArg(b), "\n")
}

get.arg.yn(x)		# (2-1) 위치에 의한 전달
get.arg.yn(1, 2)	# (2-2) 위치에 의한 전달
get.arg.yn(b=2)		# (2-3) 이름에 의한 전달
get.arg.yn(c=2)		# (2-4) 이름에 의한 전달(매치안됨)
get.arg.yn(b=2, a=x)	# (2-5) 이름에 의한 전달



# (3) 인수 누락 여부
get.arg.missing <- function(x, n) 
{
	if (missing(n))
		n <- length(x)
	sum(x)/n
}

get.arg.missing(x)	# (3-1) x만 사용
get.arg.missing(x, 5)	# (3-2) x, n 사용



###################################################
# 5. 매치 함수
###################################################
# 가. 인수의 매치
match.arg(arg=c("med", "tri"),	# (1) 인수의 매치
          choices=c("mean", "median", "trimmed"),
          several.ok = TRUE)

match.arg(c("median","trimmed"),# (2) 인수의 매치
          c("mean", "med", "tri"),
          several.ok = TRUE)

match.arg(c("med", "tri"),	# (3) 에러 발생
          c("mean", "median", "trimmed"),
          several.ok = FALSE)

match.arg(c("t"),		# (4) 1개만 매치
          c("mean", "median", "trimmed"),
          several.ok = FALSE)

match.arg(c("m"),		# (5) 매치의 경합
          c("mean", "median", "trimmed"))

# (6) 중심값 통계량 구하기
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)

  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

set.seed(1)
x <- rcauchy(10)
center(x, "t")  		# (7) 극단 제거 평균
center(x, "med") 		# (8) 중위수
center(x, "m")			# (9) 에러 발생


# 나. 함수의 매치
f <- function(x, FUN)		# (1) match.fun 사용
{
	FUN <- match.fun(FUN)
	FUN(x)
}

x <- c(3,5,7,11,13,17)
f(x, mean)			# (2) 평균 구하기
f(x, median)			# (3) 중위수 구하기
f(x, foo)			# (4) foo 함수 없음
mean <- 1:2			# (5) 평균 구하기
f(x, mean)			# (6) 평균 구하기
match.fun(mean, descend=FALSE)  # (7) descend 인수 

# 다. 호출의 매치

get.call <- function(x, ...) 	# (1) match.call 사용
{
	mat <- match.call()
	print(mat)

	mean(x, ...)
}

set.seed(1)
rnd <- rnorm(100)

get.call(rnd, trim=0.1, na.rm=T)# (2) 순서에 의한 매치
get.call(trim=0.1, x=rnd)	# (3) 이름에 의한 매치


# Function Calls
A <- c(3,5,7,11,13,17)
call("mean", A)        		# (1) mean(1:10)
call("mean", quote(A)) 		# (2) mean(A)
f <- "mean"
call(f, quote(A))       	# (3) mean(A)

eval(call("mean", A))		# (4) 산술평균
eval(call("mean", quote(A)))	# (5) 산술평균
eval(call(f, quote(A)))		# (6) 산술평균

do.call("mean", list(A))     	# (7) mean(1:10)
A[4] <- NA
do.call("mean", list(quote(A)))	# (8) mean(A)
do.call("mean", list(quote(A), na.rm=T)) # (9) mean(A)

###################################################
# B. 함수의 제어
###################################################

###################################################
# 1. 결과의 반환
###################################################
###################################################
# return 함수
###################################################

a <- seq(1, 10, 2)

# (1) 객체 반환 함수
value.return1 <- function(x)
{
	tot <- sum(x)
	n <- length(x)
	result <- list(length=n, total=tot, average=mean(x)

	return(result)
}

value.return1(a)
x <- value.return1(a)	# (2) 결과의 할당
x * 10

# (3) 표현식의 결과 반환 함수
value.return2 <- function(x)
{
	return(sum(x)/length(x))
}

value.return2(a)

a[2] <- NA
a

# (4) 강제 종료의 응용
value.return3 <- function(x)
{
	if (any(is.na(a))) return 

	return(sum(x)/length(x))
}

value.return3(a)

# (5) NULL의 반환
value.return4 <- function(x)
{
	return()
}

value.return4(a)


###################################################
# invisible 함수
###################################################

f1 <- function(x) x
f2 <- function(x) invisible(x+3)
f1(1)		# (1) 명령행 라인 출력
f2(1)		# (2) 명령행 라인 출력 안됨
a <- f2(1)	# (3) 결과 할당 가능
a

f3 <- function(x) 
{
	invisible(x+3)

	cat("End of Function\n")
}

a <- f3(1)	# (4) 강제 종료 안됨
a		# (5) NULL 반환



###################################################
# 2. 예외처리 함수
###################################################
###################################################
# stop 함수
###################################################

# (1) stop 함수 사용
get.complex <- function (x, call.=TRUE) 
{
	if (!is.complex(x))
		stop("x value is not complex.", call.=call.)

	re <- Re(x)
      	im <- Im(x)

	return(list(real=re, imaginary=im))
}

get.complex(pi)		# (2) 에러 발생
get.complex(TRUE)	# (3) 에러 발생
get.complex(23+4i)	# (4) 정상 수행
get.complex(pi, FALSE)	# (5) 에러 발생 부분 미 출력
geterrmessage()		# (6) 이전 에러 메시지
ls(123)
geterrmessage()		# (7) ls(123) 에러 메시지


###################################################
# stopifnot 함수
###################################################
stopifnot(1==1, 1 < 2, TRUE) 	# (1) 에러 미발생
x <- c(NA, 3, 4)
stopifnot(1==1, is.na(x))	# (2) 두번째 표현식이 모두 TRUE 아님
stopifnot(1==1, all(is.na(x))) 	# (3) 두번째 표현식이 TRUE아님
stopifnot(1==1, any(is.na(x))) 	# (4) 모두 TRUE

# (5) stopifnot 함수의 응용
get.complex1 <- function (x) 
{
	stopifnot(is.complex(x), any(!is.na(x)))

	re <- Re(x)
      	im <- Im(x)

	return(list(real=re, imaginary=im))
}

get.complex1(pi)	# (6) 에러 발생
get.complex1(TRUE)	# (7) 에러 발생
get.complex1(23+4i)	# (8) 정상 수행



###################################################
# warning 함수
###################################################
get.complex2 <- function (x, immediate.=FALSE, call.=TRUE) 
{
	if (!is.complex(x))
		warning("x value is not complex.", immediate.=immediate., call.=call.)

	re <- Re(x)
      	im <- Im(x)

	return(list(real=re, imaginary=im))
}

get.complex2(pi)		# (2) 경고 발생
get.complex2(TRUE)		# (3) 경고 발생
get.complex2(23+4i)		# (4) 정상 수행
get.complex2(pi, call.=FALSE)	# (5) 경고 발생 부분 미 출력
get.complex2(pi, immediate.=T)	# (6) 경고 발생 즉시 출력


###################################################
# tryCatch 함수
###################################################
x <- NA
y <- NA

# (1) tryCatch 사용 예 1
tryCatch({
  x <- log(123);
  y <- log("a");
}, error = function(ex) {
  print(ex);
})

print(x)
print(y)

x <- NA
y <- NA

# (2) tryCatch 사용 예 2
tryCatch({
  x <- log(123);
  y <- log("a");
}, error = function(ex) {
  print(ex);
}, finally = print("tryCatch statment is done")
)

print(x)
print(y)



###################################################
# try 함수
###################################################
try(log("a"))			# (1) try 함수
try(log("a"), silent=TRUE)	# (2) silent=TRUE
print(.Last.value)		# (3) 마지막 값

# (4) 표본평균 구하는 함수
sample.mean <- function(x)
{
    if (x > 0) {
        set.seed(1)
        smpl <- sample(x, replace=T)
    }
    else
      stop("negative numeric")

    mean(smpl)
}

x <- c(3,7,-1,4)

lapply(x, function(x) sample.mean(x))	# (5) 에러 발생
lapply(x, function(x) try(sample.mean(x), TRUE)) # (6) 예외처리




###################################################
# 3. 이벤트 인식 함수
###################################################
###################################################
# on.exit 함수
###################################################

# (1) on.exit 함수의 사용
my.func1 <-
function (n=100)
{
	on.exit(cat("running on.exit argument\n"))

	set.seed(1)
	rnd <- rnorm(n)

	cat("Sampe size : ", n, "\n")

	return(mean(rnd))
}
my.func1(50)


# (2) on.exit 함수의 사용 (오류 발생의 경우)
my.func2 <-
function (n=100)
{
	on.exit(cat("running on.exit argument\n"))

	set.seed(1)
	rnd <- rnorm(n)

	cat("Sampe size : ", n, "\n")

	stop("Function stop!!!!")

	return(mean(rnd))
}
my.func2(50)


# (3) on.exit 함수의 사용 (두번 호출)
my.func3 <-
function (n=100)
{
	on.exit(cat("running on.exit argument\n"))

	set.seed(1)
	rnd <- rnorm(n)

	cat("Sampe size : ", n, "\n")

	on.exit(cat("add on.exit argument\n"))

	return(mean(rnd))
}
my.func3(50)

# (4) on.exit 함수의 사용 (두번 호출, add=T)
my.func4 <-
function (n=100)
{
	on.exit(cat("running on.exit argument\n"))

	set.seed(1)
	rnd <- rnorm(n)

	cat("Sampe size : ", n, "\n")

	on.exit(cat("add on.exit argument\n"), add=T)

	return(mean(rnd))
}
my.func4(50)


# (5) on.exit 함수의 응용
get.pi <-
function (flag=TRUE)
{
	if (flag) 
		on.exit(options(digits=7))

	options(digits=3)

	print(pi)
}
get.pi()
pi
get.pi(FALSE)
pi
options(digits=7)

###################################################
# Sys.sleep 함수
###################################################
testit <- function(x)
{
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 
}
testit(3.7)
testit(1)


###################################################
# .First 함수와 .Last 함수
###################################################
.First <- function()
{
	cat("\n   Welcome to my R wolrd!\n\n")
}


.First <- function()
{
	library(lattice)
}


.Last <- function()
{
	winDialog("ok", "bye bye R session")
	Sys.sleep(3)
}

###################################################
# C. 함수의 Scoping Rule
###################################################
###################################################
# 1. 변수의 사용 범위
###################################################
###################################################
# (1) scope
###################################################
x <- 1			# (1) 전역 변수 (global variable)
y <- 5			# (2) 전역 변수 (global variable)

scope <- function(x) {
	y <- 2*x	# (3) 지역변수의 생성

	print(x)		# (4) 매개변수 (formal parameter)
	print(y)		# (5) 지역변수 (local variable)
	print(z)		# (6) 자유변수 (free variable)
}

scope(3)		# (7) scope 함수 실행
y			# (8) y의 값 출력

z <- 10		# (9) 전역변수 z 생성
scope(x)		# (10) scope 함수 재 실행

###################################################
# (2) <<- 연산자
###################################################
x <- 1
y <- 2
z <- 3
f <- 10

scope1 <- function(x) {
	y <<- x+10	# (1) <<- 연산자 이용
	y*2 ->> z	# (2) ->> 연산자 이용

	print(x)		# (3) 매개변수 출력
	print(y)		# (4) 지역변수 출력
	print(z)		# (5) 지역변수 출력
	print(f)		# (6) 자유변수 출력
}

scope1(x)		# (7) scope1 실행
x;y;z;f			# (8) 전역변수 출력

###################################################
# (3) 함수 안에서의 ls 함수
###################################################
rm(x,y,z)
x <- y <- z <- 10	# (1) x,y,z 생성
x;y;z 
scope2 <- function() {	
	ls()		# (2) 지역 객체 조회
}
scope2()

scope3 <- function() {
	x <- 20	# (3) 지역 객체 생성
	ls()		# (4) 지역 객체 조회
}
scope3()

scope4 <- function() {
	ls(pos=1)	# (5) 워크스페이스 객체 조회
}
scope4()

###################################################
# (4) 자유변수
###################################################
euro <- 1:10		# (1) euro의 생성

scope5 <- function(x) {
	y <- euro[1:3]*x	# (2) 자유변수 euro의 사용
	y
}
scope5(2)		# (3) scope5 실행

rm(euro)		# (4) 워크스페이스의 euro 삭제
scope5(2)		# (5) scope5 실행

scope6 <- function(x) {
	y <- datasets::euro[1:3]*x		# (6) datasets 패키지 euro의 사용
	y
}

euro <- 1:10		# (7) euro 생성
scope6(2)		# (8) scope6 실행

###################################################
# 2. 함수 안에서 함수 생성
###################################################
###################################################
# (1) 지역 함수의 생성
###################################################
my.mean <- function(x) {
	total <- sum(x)

	size <- function(x) {		# (1) 함수 안에서 함수의 생성
		len <- length(x)
		return (len)
	}

	return (total/size(x))		# (2) 생성한 함수의 호출
}

my.mean(1:10)
ls(size)				# (3) size 함수의 조회

###################################################
# (2) <<- 연산자의 이용
###################################################
size <- function(x) {			# (1) size 함수의 생성
	len <- length(x)
	return (len)
}

my.mean1 <- function(x) {
	total <- sum(x)

	size <<- function(x) {	# (2) 지역 함수의 생성
		length(x)
	}

	return (total/size(x))		# (3) 생성한 함수의 호출
}

my.mean1(1:10)
size					# (4) size 함수 원형

###################################################
# (3) S-PLUS와의 차이점
###################################################
cube <- function(n) {
	sq <- function() n*n		# (1) sq 함수의 정의
	n*sq()				# (2) sq 함수의 호출
}

cube(2)				# (3) cube 함수의 실행

###################################################
# 3. 재귀호출 함수
###################################################

cum.sum <- function (n) {
	tot <- 0

	if (n==0)  return (tot)			# (1) 재귀 호출 탈출 조건
	else tot <- n + cum.sum(n-1)		# (2) cum.sum 함수 호출

	return (tot)
}

cum.sum(0)					# (3) 0부터 5까지의 합
cum.sum(5)					# (4) 0부터 5까지의 합
cum.sum(10)					# (5) 0부터 10까지의 합


###################################################
# B. 연산자
###################################################

###################################################
# 1. 산술연산자
###################################################
5 + 3			# (1) 덧셈
5 - 3			# (2) 뺄셈
5 * 3			# (3) 곱셈
5 / 3			# (4) 나눗셈
5 ^ 3			# (5) 누승 5의 3제곱		
5 ** 3			# (6) 누승 5의 3제곱
5  %/% 3		# (7) 몫
5 %% 3		# (8) 나머지
a <- matrix(1:4, ncol=2)
a
b <- matrix(c(2,1,2,2), ncol=2)
b
a * b			# (9) 행렬의 곱셈
a %*% b		# (10) 행렬의 곱
b %*% a		# (11) 행렬의 곱
x <- 1:3
y <- c(2,1,4)
x;y
x %o% y		# (12) 벡터의 외적
y %o% x		# (13) 벡터의 외적

c(1,3,4) + 3		# (14) Recycle Rule
c(1,3,4) + c(2,4)	# (15) Warning 발생

###################################################
# 2. 비교연산자
###################################################
x <- c(1,3,2,4)
y <- c(1,3,5,2)
z <- c(2,4,5)
x != y			# (1) x, y가 같지 않음
x < y			# (2) x가 y보다 작음
x <= y			# (3) x가 y보다 작거나 같음
x == y			# (4) x, y가 같음
x > y			# (5) x가 y보다 큼
x >= y			# (6) x가 y보다 크거나 같음
x  < z			# (7) Recycle Rule

###################################################
# 3. 논리연산자
###################################################
x <- c(T, F, T, F)
y <- c(F, T, T, F)

!x			# (1) 논리 부정
x | y			# (2) 논리 합
x || y			# (3) 논리 합
x & y			# (4) 논리 곱
x && y			# (5) 논리 곱
xor (x, y)		# (6) 논리 XOR

all(x | y)		# (7) 벡터의 모든 원소가 TRUE일때 TRUE 
any(x | y)		# (8) 벡터의 하나의 원소라도 TRUE이면 TRUE 

!c(1, 3+4i, 0)		# (9) 복소수 벡터의 논리 부정
c(0,1,3) || c(1,2,0)	# (10) 수치 벡터의 논리 합
c(0,1,3) & c(1,2,0)	# (11) 수치 벡터의 논리 곱
xor(c(0,1,3), c(1,2,0))# (12) 수치 벡터의 논리 XOR

 
###################################################
# 4. 기타연산자
###################################################
# 할당 연산자
days <- 365		# (1) name <- value
x <- y <- 3		# (2) name1 <- name2 <- value
x;y
x = y = 5		# (3) name1 = name2 = value
x;y
x <- 10 -> y		# (4) name1 <- value -> name2
x;y
# (5) function define ; name <- function
my.fun <- function(x) print(x)	
my.fun
x_pi			# (6) x에 파이 값을 할당하는 S-PLUS 할당문

# 괄호 연산자
3 + 4 * 3
(3 + 4) * 3
2 + 3i * 4 - 2i
(2 + 3i) * (4 - 2i)
1:3 * 2
1:(3 * 2)

# Sequence 연산자
4:6
6:-2
seq(from=4, to=6, by=1)

# Braces({ and })연산자
if (x>5) 
     { 
       x<-10
       y<-20
     }

# Component Selection 연산자 ($) 
x <- list(objects=3, score=c(kor=95, eng=98, mat=89))
x
x$objects
mean(x$score)

# 원소 참조 연산자 ([, [[)
(vec <- c(1, 3, 7, 4, 2))
vec[4]			# (1) 4번째 원소
vec[vec>3]		# (2) 3보다 큰 원소

(mat <- matrix(1:8, byrow=T, ncol=4))
mat[2,3]		# (3) 2행 3열 원소
mat[mat>3]		# (4) 3보다 큰 원소

(arry <- array(1:18, dim=c(2,3,3)))
arry[2,1,3]		# (5) 3번째 행렬의 2행 1열 원소 
arry[arry>15]		# (6) 15보다 큰 원소

(lst <- list(name="ryu", member=5, ages=c(41,38,11,7)))
lst[1]			# (7) 1번째 성분
lst[3]			# (8) 3번째 성분
lst[[3]]		# (9) 3번째 성분
lst[3][2]		# (10) 3번째 성분 2번째 원소
lst[[3]][2]		# (11) 3번째 성분 2번째 원소
lst[3][1]		# (12) 3번째 성분
lst[3]; is(lst[3])	# (13) [] 연산자의 결과
lst[[3]];

# Slot 추출 연산자
# (1) S4 클래스 정의
setClass("circle", 
          representation
	  (radius="numeric",
	   circumference="numeric",
	   area="numeric",
	   volume="numeric"))

# (2) S4 객체 생성 함수
"circle" <- function(r)
{
	value <- new("circle")
	value@radius <- r
	value@circumference <- 2*pi*r
	value@area <- pi*r^2
	value@volume <- (4/3)*pi*r^3
	value
}

myCircle <- circle(3)	# (3) S4 객체 생성
myCircle		# (4) myCircle
myCircle@radius		# (5) 반지름 슬롯
myCircle@circumference	# (6) 원주 슬롯
myCircle@area		# (7) 원의 면적 슬롯
myCircle@volume		# (8) 구의 체적 슬롯


# Unary Minus연산자
x <- -4			# (1) 음수
x
y <- 1:5
y[-c(1,4)]		# (2) 벡터에서 해당 인덱스 원소 제외

# Model Formula 연산자
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
anova(lm.D9 <- lm(weight ~ group))	# (1) Formula 연산

# 도움말 연산자
? matrix
? "%/%"
help(matrix)
help("%/%")

# 연결 연산자
34+6; r <- 4; pi*2*r

###################################################
# 5. 사용자 정의 연산자
###################################################
# (1) 상위 n개 원소 추출는 연산자 정의
"%aT%" <- function(x, y)
{
	return(sort(x)[1:y])
}

# (2) 하위 n개 원소 추출는 연산자 정의
"%B%" <- function(x, y)
{
	return(sort(x, decreasing=T)[1:y])
}

x <- c(1,3,2,4,6,5,7,8,9,12,4,10)
x %T% 5			# (3) 상위 5개의 원소 추출 연산
x %B% 3			# (4) 하위 3개의 원소 추출 연산
"%B%"(x, 3)		# (5) 함수 호출방법으로 구하기
"+"(2, 3)		# (6) 연산자의 함수호출 방법