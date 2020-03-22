###################################################
# 1. 객체지향 프로그램
###################################################

##########
# S3
##########
set.seed(1)
(x <- sample(1:3, 10, replace=T))		# (1) 수치벡터

class(x)								# (2) 수치벡터 클래스
summary(x)								# (3) 집계함수

y <- as.factor(x)						# (4) 범주형 자료
class(y)								# (5) 범주형자료 클래스
summary(y)								# (6) 집계함수

apropos("^summary\\.")					# (7) summary 메소드
summary									# (8) summary 함수의 몸체

# (9) summary generic 함수 호출할 수 있는 함수들  
methods(summary)						
methods(class="aov")					# (10) aov 클래스의 method 함수


test <- function(x) UseMethod("test")	# (11) generic 함수 생성
test.matrix <- function(x) {			# (12) method 함수 생성
    cat("This is matrix class\n")
	cat("Rows number :", nrow(x), "\n")
	cat("Cols number :", ncol(x), "\n")
    x
}

test.myclass <- function(x) {			# (13) method 함수 생성
    cat("This is myclass class\n")
    x
}

test.default <- function(x) {			# (14) method 함수 생성
    cat("This Default\n")
    x
}

(mat <- matrix(1:6, ncol=3))			# (15) 행렬 생성
test(mat)								# (16) generic 함수 호출(행렬)

obj <- mat
class(obj) <- c("myclass")				# (17) 클래스 속성 생성
test(obj)								# (18) generic 함수 호출(myclass)
inherits(obj, "myclass") 				# (19) 상속 여부 검증
is(obj)									# (20) obj 객체
is.matrix(obj)							# (21) 행렬임


test(1:4)								# (22) generic 함수 호출(integer)

obj <- unclass(obj)						# (23) 클래스 속성 제거
inherits(obj, "myclass") 				# (24) 상속 여부 검증
inherits(obj, "matrix") 				# (25) 상속 여부 검증
is(obj)									# (26) obj 객체
test(obj)								# (27) generic 함수 호출(행렬)

test.matrix 							# (28) 함수 내용 조회
getS3method("test", "matrix")			# (29) 함수 내용 조회

exists("predict.ppr") 					# (30) 숨김 함수
head(getS3method("predict", "ppr"))		# (31) 함수 내용 조회

##########
# S4
##########

setClass("coordinate",					# (1) coordinate 클래스 생성
    representation(x="numeric", y="numeric"))
getClass("coordinate")					# (2) getClass

origin <- new("coordinate", x=0, y=0)	# (3) coordinate 클래스 객체 생성
origin
origin@x; origin@y 						# (4) Slots 추출
origin@x <- 3							# (5) Slots 변경
origin

new("coordinate")						# (6) 객체 생성
setClass("coordinate",					# (7) slot 초기화 생성
    representation(x="numeric", y="numeric"),
	prototype=list(x=0, y=0))
new("coordinate")						# (8) 객체 생성
	
getSlots("coordinate")					# (9) slot 종류 조회
slotNames("coordinate")					# (10) slot 이름 조회

validCoord <- function(object)			# (11) 유효성 검사 함수
{
    if (!length(object@x)==1 || !length(object@y)==1)
        return("mismatch in length of slots!")
    return(TRUE)
}	

setClass("coordinate",					# (12) 유효성 검사 하기
    representation(x="numeric", y="numeric"),
	prototype=list(x=0, y=0),
	validity=validCoord)

new("coordinate", x='a', y=0)			# (13) 에러
new("coordinate", x=1:2, y=3:4)			# (14) 에러
new("coordinate", x=3, y=2)				# (15) 정상

origin@y <- 1:3							
origin									# (16) 변경된 객체

validObject(origin)						# (17) 유효성 검사
	
setClass("coordinate.1",				# (18) coordinate 클래스 생성
    representation(x="numeric", y="numeric"))	
setValidity("coordinate.1", validCoord)	# (19) setValidity 함수 설정
new("coordinate.1", x=1:2, y=3:4)		# (20) 에러

setClass("circle",						# (21) coordinate 상속 받기
    representation(radius="numeric"),
	prototype=list(radius=1),
	contains="coordinate")
new("circle")							# (22) 단위 원
new("circle", x=3, y=2, radius=3)		# (23) (3,2) 반지름 3인 원
	
getClass("circle")						# (24) circle 클래스
extends("circle", "coordinate")			# (25) 상속 여부

origin <- new("coordinate", x=5, y=10)
as(origin, "circle")					# (26) 강제 클래스 변환
extends("numeric", "integer")			# (27) 상속 여부
as(c(0.2, 0.3, pi), "integer")			# (28) 강제 클래스 변환 


extends("coordinate", "numeric")		# (29) 상속 여부
as(origin, "numeric")					# (30) 강제 클래스 변환
setAs("coordinate", "numeric", 			# (31) 변환 설정
  function(from) c(from@x,from@y))
as(origin, "numeric")					# (32) 강제 클래스 변환


setClass("rectangular",					# (33) 직사각형 클래스
    representation(x.len="numeric", y.len="numeric"),
	prototype=list(x.len=1, y.len=1),
	contains="coordinate")

setGeneric("circumference", 			# (34) generic 함수 생성
  function(x) standardGeneric("circumference"),
  useAsDefault=function(x) 
      cat("Not support class : ", class(x), "\n"))
	  
setMethod ("circumference", "circle", 	# (35) method 함수 생성
function(x) 
{
  cat("circle circumference : ", 2*pi*x@radius, "\n") 
})

setMethod ("circumference", "rectangular", # (36) method 함수 생성
function(x) 
{
  cat("rectangular circumference : ", 2*x@x.len+2*x@y.len, "\n") 
})

crl <- new("circle", x=3, y=2, radius=3)	# (37) circle 객체
rct <- new("rectangular", 					# (38) rectangular 객체
  x=3, y=2, x.len=3, y.len=4)

circumference(crl)						# (39) 원의 둘레
circumference(rct)						# (40) 직사각형 둘레
circumference(1:5)						# (41) integer

setGeneric("area", 						# (42) generic 함수 생성
  function(x) standardGeneric("area"),
  useAsDefault=function(x) 
      cat("Not support class : ", class(x), "\n"))
  
setMethod ("area", "circle",			# (43) method 함수 생성
function (x) {
  cat("circle area : ", pi*x@radius^2, "\n") 
})

setMethod ("area", "rectangular",		# (44) method 함수 생성
function (x) {
  cat("rectangular area : ", x@x.len*x@y.len, "\n") 
})

area(crl)								# (45) 원의 면적
area(rct)								# (46) 직사각형 면적

setMethod ("plot", "circle",			# (47) method 함수 생성
function (x, xlab="", ylab="", axes=T, asp=1, col="blue", ...) {
  par(pty='s')
  
  x.lim <- c(x@x-x@x*3, x@x+x@x*3)
  y.lim <- c(x@y-x@y*3, x@y+x@y*3)
  
  angle <- (0:(10*360))/(10*360)*2*pi
  x.vec <- x@radius*cos(angle)+x@x
  y.vec <- x@radius*sin(angle)+x@y
  
  plot(x.lim, y.lim, type='n', axes=axes, xlab=xlab, ylab=ylab,
    asp=asp, ...)
  polygon(x.vec, y.vec, col=col, border=col, ...)
})

setMethod ("plot", "rectangular",		# (48) method 함수 생성
function (x, xlab ="", ylab ="", axes=T, asp=1, col="blue", ...) {
  par(pty='s')

  xx <- x@x
  yy <- x@y
  x.len <- x@x.len
  y.len <- x@y.len

  x.lim <- c(xx-x.len/4, xx+x.len)
  y.lim <- c(yy-y.len*2, yy+y.len)
  
  x.vec <- c(xx, xx+x.len, xx+x.len, xx)
  y.vec <- c(yy, yy, yy-y.len, yy-y.len)
  
  plot(x.lim, y.lim, type='n', axes=axes, xlab=xlab, ylab=ylab,
        asp=asp, ...)
  polygon(x.vec, y.vec, col=col, ...)
})

# (49) plot 함수를 이용한 원과 직사각형 그림들
par(mfrow=c(2,2))
plot(crl)						
plot(rct, col="gold")
plot(new("circle", x=2, y=4, radius=5), lwd=2)
plot(new("rectangular", 	
  x=0, y=2, x.len=7, y.len=2), col="yellow")
par(mfrow=c(1,1))

body(selectMethod)==body(getMethod)		# (50) 같은 함수
selectMethod("circumference", "circle")	# (51) method 함수 조회
body(existsMethod)==body(hasMethod)		# (52) 같은 함수
existsMethod("area", "rectangular")		# (53) method 함수 검사

removeMethod("area", "rectangular")		# (54) method 함수 삭제
existsMethod("area", "rectangular")		# (55) method 함수 검사

removeClass("circle")					# (56) 클래스 삭제
getClass("circle")						# (57) 클래스 조회





###################################################
# 2. 프로그램 디버깅
###################################################

#############
# traceback
#############
my.power <- function(x, y) 
{
    exp(y * func.1(x))
}

func.1 <- function(x) 
{
    tmp <- log(x)
	
    if (tmp > 709) Inf else tmp  
}

my.power(5, 3)						# (1) 5^3
my.power(-2, 2)						# (2) 에러 발생

traceback()							# (3) 역 추적

my.power <- function(x, y) 			# (4) 함수 수정
{
    flag <- ifelse(y%%2, -1, 1)
	
	exp(y * func.1(abs(x))) * flag
}

my.power(-2, 2)						# (5) 정상 수행
my.power(-2, 3)						# (6) 정상 수행

my.power(1:5, 2)					# (7) 경고 발생

func.1 <- function(x) 				# (8) 함수 수정
{
    tmp <- log(x)
	
    if (any(tmp > 709)) Inf else tmp  
}

my.power(1:5, 2)					# (9) 경고 미발생


############
# debug
############

my.sd <- function(x)				# (1) 표준편차 함수
{
    mu <- mean(x)
    sumsq <- sum(x - mu)^2
    n <- length(x)
    var <- sumsq / (n-1)
    sqrt(var)
}

sd(1:10)							# (2) R의 표준편차
my.sd(1:10)							# (3) 사용자 정의 함수

debug(my.sd)						# (4) debug mode 시작
isdebugged(my.sd)					# (5) debug mode?

my.sd(1:10)							# (6) debug 시작

undebug(my.sd)						# (7) debug mode 해제

my.sd <- function(x)				# (8) 수정된 함수
{
    mu <- mean(x)
    sumsq <- sum((x - mu)^2)		# (9) 수정한 곳
    n <- length(x)
    var <- sumsq / (n-1)
    sqrt(var)
}
my.sd(1:10)							# (10) 수정함수 실행

debugonce(my.sd)					# (11) 한번만
isdebugged(my.sd)					# (12) debug mode?

my.sd(rnorm(100))					# (13) debug 시작
my.sd(rnorm(100))					# (14) debug 시작 안됨

############
# browser
############

my.sd.1 <- function(x)				# (1) 사용자 정의 함수
{
    mu <- mean(x)
    sumsq <- sum((x - mu)^2)		
	browser()						# (2) browser 함수
    n <- length(x)
    sqrt(sumsq / (n-1))
}
my.sd.1(1:10)						# (3) 함수 실행

############
# trace
############

my.power <- function(x, y)			# (1) 테스트 함수
{
    tmp <- y * log(x)
    exp(tmp)
}

as.list(body(my.power))				# (2) at 값 위치 

# (3) trace with browser
trace("my.power", quote(if(any(is.nan(tmp))) { browser() }), at=3, print=T)

my.power							# (4) 함수 조회
body(my.power)						# (5) 함수 몸체

my.power(3, 2)						# (6) 함수 호출
my.power(-1:2, 2)					# (7) 함수 호출
untrace("my.power")					# (8) untrace
body(my.power)						# (9) 함수 몸체


############
# recover
############
my.power <- function(x, y) 
{
    exp(y * func.1(x))
}

func.1 <- function(x) 
{
    tmp <- log(x)
	
    if (tmp > 709) Inf else tmp  
}

# (1) trace with recover
trace("func.1", quote(if(any(is.nan(tmp))) { recover() }), at=3, print=T)

my.power(3, 2)						# (2) 함수 호출
my.power(-1, 2)						# (3) 함수 호출
untrace("my.power")					# (4) untrace

options(error=recover)				# (5) 모든 에러 발생 시
rnorm(1,10,2,3)						# (6) 에러 발생
options(error=NULL)					# (7) 해제
rnorm(1,10,2,3)						# (8) 에러 발생


###################################################
# 3. 프로그램 최적화
###################################################

########################
# 벡터화(Vectorization)
########################

x <- runif(1000000)					# (1) 백만개의 일양 난수

system.time({						# (2) 반복문으로 개별 계산
    res <- numeric(1000000)
    for (i in 1:1000000)
        res[i] <- log(x[i])
    res
})
system.time(res <- log(x)) 			# (3)벡터화로 한번에 계산

set.seed(1)
x <- runif(10000000)				# (4) 천만개의 일양 난수

system.time({						# (5) 반복문 처리
    c <- 0
    for (i in 1:10000000) 
        c <- c + x[i]/2
    print(c)
})

system.time({						# (6) 반복문, 마지막에 한번 나눔
    c <- 0
    for (i in 1: 10000000) 
        c <- c + x[i]
    print(c/2)
})

system.time(print(sum(x/2)))		# (7) 벡터화

system.time(print(sum(x)/2))		# (8) 벡터화, 마지막 한번 나눔


########################
# 저장 공간 확보
########################
system.time({				# (1) 벡터:저장공간 미확보	
    x <- NULL
    for (i in 1:10000) x <- c(x, runif(1))
})


system.time({				# (2) 벡터:정장공간 확보
    x <- numeric(10000)
    for (i in 1:10000) x[i] <- runif(1)
})

# 10000행 100열
system.time({				# (3) 행렬:저장공간 미확보	
    x <- NULL
    for (i in 1:10000) x <- rbind(x, 1:100)
    x
})

system.time({				# (4) 행렬:저장공간 미확보	
    x <- NULL
    for (i in 1:10000) x <- cbind(x, 1:100)
    t(x)
})


system.time({				# (5) 행렬:정장공간 확보
    x <- matrix(0, 10000, 100)
    for (i in 1:10000) x[i,] <- 1:100
    res
})


########################
# apply 함수군 활용
########################

x <- matrix(rnorm(1000000), 10000, 100)
m <- numeric(100)

system.time({				# (1) 반복문
    n <- ncol(x)

    for (i in 1:n) m[i] <- mean(x[,i])
})


system.time(m <- apply(x, 2, mean))	# (2) apply

system.time(m <- rowMeans(x))		# (3) rowMeans


########################
# 기타의 방법
########################

# 조건문 지양
x <- 1:1000000
y <- sample(c(T, F), 1000000, replace=T) 
s <- 0
system.time({				# (1) 조건문 사용
    for (i in 1:1000000)
        if (y[i])
            s <- s + x[i] else 
        s <- s - x[i]
})

system.time(s <- sum((2*y-1)*x)) 	# (2) 조건문 미사용

# 빈번히 사용하는 값은 변수로 인용
system.time(for (i in 1:1000000) sample(1:100))	# (3) 값으로
x <- 1:100
system.time(for (i in 1:1000000) sample(x)) 	# (4) 변수로

# 벡터 초기화 방법
x <- 1:5
system.time(for(i in 1:10^6) y <- numeric(5))	# (5) numeric
system.time(for(i in 1:10^6) y <- 0*x)		# (6) 0으로
system.time(for(i in 1:10^6) y <- x-x)		# (7) 0으로

# 행렬 초기화 방법
x <- matrix(1:16, 4, 4)
system.time(for(i in 1:10^4) y <- matrix(0,4,4)) # (8) matrix
system.time(for(i in 1:10^4) y <- 0*x)		# (9) 0으로
system.time(for(i in 1:10^4) y <- x-x)		# (10) 0으로

###################################################
# 4. 프로그램에 유용한 함수
###################################################

#####################
# 시스템 명령어 호출
#####################
# (1) c:/windows 디렉토리의 파일 개수
shell("ls c:/windows | wc -l")		
# (2) c:/windows 디렉토리의 win으로 시작하는 파일
shell("ls c:/windows | grep '^win'")
# (3) MS-Windows의 계산기 프로그램 실행
system("C:/WINDOWS/system32/calc.exe", wait=FALSE)


#####################
# 메모리 관리
#####################
rm(list=ls(all.names=T))	# (1) 모든 객체 삭제
gc()						# (2) Garbage Collection
obj <- 1:100000000			# (3) 대용량 벡터
x <- y <- z <- 1:1000		# (4) 작은 용량	
object.size(obj)			# (5) 객체 사이즈	
object.size(obj)/(1024*1024)# (6) MB 단위
# (7) 객체 사이즈 조사(wrokspace)
z <- sapply(ls(), function(x)
    object.size(get(x, envir=.GlobalEnv)))
as.matrix(rev(sort(z))) 	# (8) 보기 좋게	
gc()						# (9) Garbage Collection
rm(obj)						# (10) obj 삭제
Sys.sleep(10)				# (11) 10초 후
gc()						# (12) Garbage Collection


rm(list=ls(all.names=T))	# (13) 모든 객체 삭제
gc()						# (14) Garbage Collection
memory.size()				# (15) 사용량
memory.size(max=T)			# (16) 최대 사용량 (OS)
obj <- 1:100000000			# (17) 벡터 생성
object.size(obj)/(1024*1024)# (18) MB 단위
memory.size()				# (19) 사용량
memory.size(max=T)			# (20) 최대 사용량 (OS)
							# (21) obj의 용량을 더함
object.size(obj)/(1024*1024)+12.46
obj.1 <- 1:10000000			# (22) 추가 벡터 생성
memory.size()				# (23) 사용량
memory.size(max=T)			# (24) 최대 사용량 (OS)

memory.limit()				# (25) 최대 사용 한계치
memory.limit(size=500)		# (26) 최대 사용 한계치 변경
memory.limit(memory.limit()+10)	# (27) 최대 사용 한계치 변경

# Windows에서 R의 바로가기 수정
## "C:\Program Files\R\R-2.11.1\bin\Rgui.exe" --min-mem-size=500M --max-mem-size=1600M
memory.size()				# (28) 사용량
memory.size(max=T)			# (29) 최대 사용량 (OS)
memory.limit()				# (30) 최대 사용 한계치
gc()						# (31) gc 함수


