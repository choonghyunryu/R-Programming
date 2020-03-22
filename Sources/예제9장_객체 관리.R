###################################################
# A. 패키지
###################################################
###################################################
# 1. 패키지 조회 및 로드
###################################################

library()		# (1) 설치된 패키지 조회

(.packages())		# (2) 로드된 패키지 조회
library(MASS)		# (3) MASS 패키지 로드
(.packages())		# (4) 로드된 패키지 조회
detach("package:MASS")	# (5) MASS 패키지 언로드
(.packages())		# (6) 로드된 패키지 조회

(rslt <- library(MASS))	# (7) 로드된 패키지 목록 반환
(rslt <- require(MASS)) # (8) 로드 성공여부 반환
(rslt <- library(MASS, logical.return=T)) # (9) 로드 성공여부 반환
(rslt <- library(mass)) # (10) 에러 메시지
(rslt <- require(mass)) # (11) 로드성공 여부; 경고 메시지

library(MASS, character.only=T)		# (12) 문자 인수값만 인정
require("MASS", character.only=T)	# (13) 문자 인수값	
detach("package:MASS")			# (14) MASS 패키지 언로드
require("MASS", quietly=T)		# (15) 로드되는 정보 미출력

library(lib.loc=.Library)	# (16) 기본 라이브러리의 패키지 리스트
.Library					# (17) 기본 라이브러리 경로
library(help=lattice)		# (18) lattice 패키지 정보
library(lattice, pos=5)		# (19) 5번째 위치에 로드
(.packages())				# (20) 로드된 패키지 조회

getOption("defaultPackages") # (21)기본 패키지 설정 조회

###################################################
# 2. Search Path
###################################################

search()					# (1) search 함수
library(lattice)			# (2) lattice 패키지 로드
search()					# (3) search path
searchpaths()				# (4) searchpaths 함수

names(USArrests)			# (5) USArrests의 변수 이름
USArrests$Murder[1:5]		# (6) USArrests$Murder
attach(what=USArrests)		# (7) data.frame attach
search()					# (8) search 목록
Murder[1:5]					# (9) Murder 변수
ls("USArrests")				# (10) 목록 조회

lst <- list(x1=1:5, y1=letters[1:3]) # (11) list
attach(lst, pos=3)			# (12) list attch, pos 인수
search()					# (13) search 목록
y1							# (14) lst의 y1 성분

env <- new.env()			# (15) environment 생성
env$var1 <- 1:3				# (16) 객체 생성
env$var2 <- rnorm(3)		# (17) 객체 생성
attach(env)					# (18) list attch, pos 인수
search()					# (19) search 목록
var2						# (20) env의 var2

attach(env, warn.conflicts=T) # (21) warn.conflicts=T
attach(env, warn.conflicts=F) # (22) warn.conflicts=F
search()					# (23) search 목록
	   
detach(name=package:lattice)# (24) package detach
loadedNamespaces()			# (25) loadedNamespaces
detach("USArrests")	  		# (26) character string
search()					# (27) search 목록
detach(pos=2)				# (28) pos 인수
search()					# (29) search 목록
detach(env, character.only=T) # (30) character.only=T	   
detach(env, character.only=F) #	(31) character.only=F
search() 	  				# (32) search 목록
library(lattice)			# (33) lattice load
detach(package:lattice, unload=T) # (34) unload=T
loadedNamespaces()			# (35) loadedNamespaces

detach(pos=1)				# (36) workspace detach 불가
detach(package:base)		# (37) package:base detach 불가

# (38) USArrests 이용 방법 1 ($ 연산자 사용)
list(max.muder=max(USArrests$Murder), 
     min.mass=min(USArrests$Assault))

# (39) USArrests 이용 방법 2 (attach 함수 사용)
attach(USArrests)
list(max.muder=max(Murder), 
     min.mass=min(Assault))
detach(USArrests)
	 
# (40) USArrests 이용 방법 3 (with 함수 사용)
with(USArrests,
  list(max.muder=max(Murder), 
       min.mass=min(Assault))
)

# (41) 데이터 프레임 생성
(student <- data.frame(
  name=c("Kim","Lee","Park","Choi"),
  kor=c(89,97,78,90),
  eng=c(99,78,90,92),
  mat=c(90,90,87,89),
  gen=c("F","M","M","F")
))

# (42) 데이터 프레임 수정
(student.score <- within(student, {
  tot <- kor + eng + mat 
  avg <- round(tot/3,1)
  rnk <- order(tot, decreasing=T)
  rm(gen)}
))


###################################################
# 3. Namespace
###################################################
t <- function(x)			# (1) 사용자 정의함수 t
{
	dims <- dim(x)
	cat("row:", dims[1], " col:", dims[2], "\n")
}
(A <- matrix(c(1,2,0,-1,-1,3), ncol=3))		# (2) 행렬 A 정의
(B <- matrix(c(1,1,1,0,2,4,3,0,7), ncol=3))	# (3) 행렬 B 정의

all(t(A%*%B) == t(B)%*%t(A))				# (4) (AB)´=B´A´ 검증
all(base::t(A%*%B) == base::t(B)%*%base::t(A))	# (5) namespace 이용

loadedNamespaces()							# (6) 로드된 namespace
MASS::lda 									# (7) MASS 패키지의 lad 함수
MASS::abbey									# (8) MASS 패키지의 abbey 벡터
library(MASS)								# (9) MASS 패키지 로드
loadedNamespaces()							# (10) 로드된 namespace
abbey										# (11) abbey 참조

coef.default								# (12) stats 패키지의 internal 객체
stats:::coef.default						# (13) ::: 연산자 사용

library(lattice)							# (14) lattice 패키지 로드
(.packages())								# (15) 로드된 패키지 조회 
search()									# (16) search path
searchpaths()								# (17) searchpaths 함수
loadedNamespaces()							# (18) 로드된 namespace

rgl::bg3d									# (19) rgl 패키지의 bg3d 함수
search()									# (20) search path
loadedNamespaces()							# (21) 로드된 namespace

getAnywhere("t")							# (22) t 함수 검색
getAnywhere("pi")							# (23) pi
getAnywhere("edit")							# (24) edit 함수 
getAnywhere("AaaaA")						# (25) 존재하지 않는 객체 



###################################################
# 4. 패키지 설치
###################################################

pkgs <- available.packages()				# (1) 사용 가능 패키지 조회
pkgs[1,]									# (2) 첫 패키지 조회
dim(pkgs)									# (3) 차원
colnames(pkgs)								# (4) 변수 이름
xtabs(~Priority, pkgs)						# (5) Priority 변 현황
pkgs[!is.na(pkgs[,"Priority"]), 			# (6) Priority 있는 패키지
  c("Priority")]

install.packages(c("Rserve", "RODBC"))		# (1) 패키지 설치
old.packages()								# (2) 변경된 패키지 조회
update.packages()							# (3) 패키지 업데이트
download.packages("rgl", destdir="c:/")		# (4) 패키지 다운로드
install.packages("c:/rgl_0.91.zip", 
  lib=.libPaths()[1L], repos = NULL)		# (5) 로컬 패키지 파일 설치
.libPaths()									# (6) .libPaths
utils:::menuInstallLocal					# (7) 메뉴 설치의 함수 내용


###################################################
# 5. 패키지 제거
###################################################

remove.packages("rgl", lib=.libPaths()[1])	# (1) rgl 패키지 제거
library(rgl)								# (2) rgl 패키지 로드


###################################################
# 6. 패키지 생성
###################################################

# http://www.murdoch-sutherland.com/Rtools 사이트에서 Rtools를 설치

# (1) center 함수 생성 (9장에서 생성)
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)

  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

# (2) my.mean 함수 생성 (9장에서 생성)
my.mean <- function(x) {
	total <- sum(x)

	size <- function(x) {		
		len <- length(x)
		return (len)
	}

	return (total/size(x))		
}

mydata <- sample(10)	# (3) 수치 벡터 생성

# (4) 패키지 골격 생성
package.skeleton(list=c("center","my.mean","mydata"), name="Testpkg")

getwd()									# (5) 현재의 작업 공간
list.files(getwd(), pattern="^T")		# (6) 파일 검색  
list.files("Testpkg", full.names=T,
  pattern=".", recursive=T)				# (7) 파일 검색

library(Testpkg)  						# (8) 패키지 로드
center(rnorm(100), "mean")				# (9) center 함수
data(mydata)							# (10) 데이터 세트 로드
mydata									# (11) mydata
? center								# (12) center 도움말 조회


###################################################
# B. 객체
###################################################
###################################################
# 1. 객체 조회
###################################################
rm(list=ls(all.names=T))			# (1) workspace의 모든 객체 삭제
w <- x <- y <- z<- 1				# (2) 객체 생성
.hid <- .x <- .y1 <- 2				# (3) 숨김 객체 생성

ls()								# (4) 객체 조회
ls(name=".GlobalEnv")				# (5) 객체 조회
length(ls(name="package:datasets"))	# (6) dataset 패키지 객체 개수
head(ls("package:graphics"))		# (7) graphics 패키지 일부 조회			

search()							# (8) search 리스트
head(ls(pos=3))						# (9) pos 인수
head(ls(envir=as.environment(3)))	# (10) envir 인수
objects(name=".GlobalEnv", pos=3)	# (11) 인수의 상충

objects(all.names=TRUE)				# (12) 모든 객체 조회(숨김객체 포함)

objects("package:graphics", pattern="^plot") # (13) 패턴 검색
objects(all.names=T, pattern="^\\.") 	# (14) 숨김 객체 검색

body(ls)==body(objects)				# (15) 두 함수의 비교

myfunc1 <- function() 				# (16) 함수 안에서의 사용 1
{
    ls()
}		
myfunc1()

myfunc2 <- function() 				# (17) 함수 안에서의 사용 2
{
    yy <- 1; ls()
} 
myfunc2()               

myfunc3 <- function() 				# (18) 함수 안에서의 사용 3
{
    yy <- 1; ls(name=".GlobalEnv")
}
myfunc3()         

# (19) 여러 mode의 객체 만들기 a~h, mat
a <- c(T,F,T,T)
b <- 1:4
c <- c(pi, 1.23, 3.2)
d <- 23+4i
e <- "R programming"
f <- list(x=12, y=1:3)
g <- function() cat("User defined function\n")
h <- expression(1+1)
mat <- matrix(1:12, ncol=4)

ls.str()							# (20) ls.str
ls.str(mode=c"function")			# (21) 함수만 조회
ls.str(pos=9, mode="double")		# (22) 실수 조회
ls.str(pos=6, pattern="stat", mode="integer")	# (23) 패턴 조회

browseEnv(excludepatt="^.$")		# (24) 이름이 한 글자가 아닌 객체
browseEnv(envir=as.environment(6), 	# (25) 이름이 stat로 시작하는 데이터
  pattern="^stat", main="Package:datasets 중 ^stat")

###################################################
# 2. 객체 검색
###################################################

ls.temp <- 1:10						# (1) 수치 벡터 생성
apropos(what="LS\\.")				# (2) 객체 이름 패턴 검색
apropos("LS\\.", ignore.case=F)		# (3) 대소문자 구별
apropos("LS\\.", mode="function")	# (4) 함수만 검색
search()							# (5) search 목록
apropos("LS\\.", where=T)			# (6) search 목록 위치 출력
apropos("^.$")						# (7) 이름이 한 문자로 된 객체
apropos("^.{30,}$")					# (8) 30자 이상의 이름을 갖는 객체

find(what="ls\\.")					# (9) 패턴 검색
find("ls.temp")						# (10) 전체 이름 검색
find("ls\\.", simple.word=F)		# (11) simple.word=F
find("LS\\.", simple.word=F)		# (12) 대소문자 구별
paste <- 1:10						# (13) paste 벡터 생성
find("paste", numeric=T) 			# (14) search 목록 위치 출력
find("paste", numeric=T, mode="function") # (15) 함수만 검색 
rm(paste)							# (16) paste 벡터 삭제

exists("myfunc3")					# (17) myfunc3 객체 존재여부
exists("iris", where=6)				# (18) where 인수
exists("iris3", envir=as.environment(6)) # (19) envir 인수
exists("myfunc3", mode="integer")  	# (20) mode 인수

baseenv()							# (21) R의 기본 environment
e1 <- new.env(parent=baseenv()) 	# (22) enclosure package:base.
e2 <- new.env(parent=e1)			# (23) e1 상속
assign("a", 3, envir=e1)			# (24) e1에 a 생성
ls(envir=e1)						# (25) e1의 객체 조회
ls(e2)								# (26) e2의 객체 조회
exists("a", envir=e2)   			# (27) e2에 a가 존재하는가? 
exists("a", envir=e2, inherits=F)	# (28) 상속하지 않고 검증
exists("search", envir=e2)   		# (29) 상속받은 결과

# (30) exists 함수의 응용 예
ls(pattern="som")
if(!exists("some.fun", mode="function"))
  some.fun <- function(x) { cat("some.fun(x)\n"); x }
ls(pattern="som")


###################################################
# 3. 객체 생성
###################################################

a <- 3:1								# (1) 할당 연산자로 생성
assign(x="obj", rnorm(5))				# (2) assign 함수로 생성
assign("a", value=1:3)					# (3) assign 함수로 수정
a
assign("a[1]", 2)						# (4) a[1]을 수정?
a[1] == 2          						# (5) FALSE
ls(pattern="^a")						# (6) a로 시작하는 객체
get("a[1]") == 2   						# (7) TRUE

env <- new.env(parent=baseenv())		# (8) new environment
assign("obj", pi, envir=env)			# (9) envir 인수
obj										# (10) .GlobalEnv의 obj
get("obj", env)							# (11) env의 obj
assign("ls", 10, envir=env, inherits=T)	# (12) inherits 인수
assign("ls", 10, envir=env)				# (13) envir 인수
 
for(i in 1:3) { 						# (14) assign 함수 응용
 nam <- paste("obj", i, sep=".")
 assign(nam, i:1)
}
ls(pattern="^obj\\..$")					# (15) obj.?

# (16) 함수 안에서 전역 할당(Global assignment)
myf <- function(x) {
 innerf <- function(x) assign("Global.res", x^2, envir = .GlobalEnv)
 innerf(x+1)
}
myf(3)									# (17) 함수 실행
Global.res 								# (18) 16

get(x="+")								# (19) 덧셈 연산자
base::"%o%"								# (20) 외적 구하기
search()								# (21) search 목록
get("df", pos=2, mode="function")		# (22) pos, mode 인수
get("obj", envir=env, mode="double")	# (23) invir 인수
get("get", envir=env, inherits=T)		# (24) inherits=T
get("get", envir=env, inherits=F)		# (25) inherits=F


###################################################
# 4. 객체 삭제
###################################################

rm(list=ls(all.names=T))			# (1) workspace의 모든 객체 삭제
w <- x <- y <- z <- 1				# (2) 객체 생성
.hid <- .x <- .y1 <- 2				# (3) 숨김 객체 생성

e1 <- new.env(parent=baseenv()) 	# (4) enclosure package:base.
e2 <- new.env(parent=e1)			# (5) e1 상속
assign("a", 3, envir=e1)			# (6) e1에 a 생성
assign(".a", 3, envir=e1)			# (7) e1에 .a 생성
assign("x", 3, envir=e2)			# (8) e2에 x 생성

ls(all.names=T)						# (9) workspace의 모든 객체
ls(e1, all.names=T)					# (10) e1의 모든 객체
ls(e2)						        # (11) e2의 객체

remove(x, 'y')						# (12) workspace의 x, y 삭제
rm(list=ls(pattern="^\\..$", all.name=T))	# (13) list 인수
rm("w", pos=1)						# (14) pos 인수
rm("ts", pos=2)						# (15) locked
rm(".a", envir=e1)					# (16) envir 인수
ls(e1, all.names=T)					# (17) e1의 모든 객체
rm("a", envir=e2, inherits=F)		# (18) inherits=F
rm("a", envir=e2, inherits=T)		# (19) inherits=T
ls(e1, all.names=T)					# (20) e1의 모든 객체

ls(all.names=T)						# (21) workspace의 모든 객체 삭제
rm(e1)								# (22) environment 삭제
ls(all.names=T)						# (23) workspace의 모든 객체 삭제

body(rm) == body(rm)				# (24) remove, rm 비교


###################################################
# 5. 객체의 저장 및 로드
###################################################
rm(list=ls(all.name=T))

a <- b <- c <- d <- 1:10
.a <- .b <- xyz <- pi

save(a, "b", file="ab.Rdata")					# (1) 객체 a, b 저장
save(list=ls(pattern="^\\.", all.name=T),
     file="c:/.ad.Rdata", ascii=T)				# (2) 객체 .a, .b 저장
list.files("C:/Documents and Settings/Administrator/My Documents",
            pattern="^a")						# (3) ad.Rdata 파일 조회
list.files("c:/", all.files=T, pattern="^\\.") 	# (4) .ad.Rdata 파일 조회
	   
save.image()									# (5) 일반적인 사용
list.files(".", all.files=T, pattern="^\\.R") 	# (6) .Rdata 파일 조회
save.image(file=".RData", ascii=F, safe=T)		# (7) 인수들의 기본 값
list.files(".", all.files=T, pattern="^\\.R") 	# (8) .Rdata 파일 조회
	 
q()												# (9) R 세션 종료
q("yes")										# (10) workspace 저장 후 종료
body(q) == body(quit)							# (11) 동일한 함수

rm(list=ls(all.name=T))						
ls()											# (12) 로드 전 객체 목록
load(file="ab.Rdata")							# (13) ab.Rdata 파일 로드
ls()											# (14) 로드 후 객체 목록
env <- new.env()								# (15) environment 생성
load(file="c:/.ad.Rdata", envir=env)			# (16) environment에 로드
ls(envir=env, all.name=T)						# (17) environment 객체 조회

attach(what="ab.Rdata")							# (18) attach 함수의 이용
search()										# (19) search path
ls(pos=2)										# (20) 목록 조회
get("a", pos=2)									# (21) 객체 액세스




