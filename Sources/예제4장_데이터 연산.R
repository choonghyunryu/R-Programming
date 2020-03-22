###################################################
# A.벡터의 연산
###################################################

###################################################
# 4.1.1 통계량 계산하기 
###################################################

# 1. 최대값, 최소값
x <- c(3, 1, 4, 5, 9, 2, 6, 3, 7, 6)
y <- c(2, NA, 4, 5, 9, NA, 6, 3, 7, 1)
length(x)              # (1) 자료의 개수를 구함
length(y)              # (2) 자료의 개수를 구함(결측치 포함)
min(x)                 # (3) 최소값
max(y)                 # (4) 최대값
max(y, na.rm=TRUE)     # (5) 최대값 (결측치 제거)
range(x)               # (6) 범위함수
c(min(x), max(x))      # (7) 범위함수의 구현
range(y)               # (8) 범위함수(결측치 포함)
range(y, na.rm=TRUE)   # (9) 범위함수(결측치 제거)
diff(range(x))         # (10) 통계학에서의 범위
pmax(x, 5)             # (11) 병렬 최대값(Recycle Rule)
pmax(x, c(5,6))        # (12) 병렬 최대값(Recycle Rule)
pmin(x, y)             # (13) 병렬 최소값(결측치 포함)
pmin(x, y, na.rm=TRUE) # (14) 병렬 최소값(결측치 제거)


# 2. 벡터의 합과 곱
sum(x)                 # (1) 벡터의 합
sum(y)                 # (2) 벡터의 합(결측치 포함)
sum(y[!is.na(y)])      # (3) 결측치 제거한 벡터의 합
sum(y, na.rm=TRUE)     # (4) 벡터의 합(결측치 제거) 
prod(x)                # (5) 벡터의 곱
prod(y)                # (6) 벡터의 곱(결측치 포함)
prod(y, na.rm=TRUE)    # (7) 벡터의 곱(결측치 제거)


# 3. 중심값 및 분산
median(x)              # (1) 중위수
median(y, na.rm=TRUE)  # (2) 중위수 (결측치 제거)
x.1 <- c(x,30)        
mean(x.1)              # (3) 산술평균
sum(x.1)/length(x.1)   # (4) 공식을 이용한 산술평균
mean(x.1, trim=0.1)    # (5) 산술평균(극단 제거)
mean(y, na.rm=TRUE)    # (6) 산술평균(결측치 제거)
sum(y, na.rm=TRUE)/sum(!is.na(y)) # (7) 공식을 이용한 방법
var(x)                 # (8) 분산 
sum((x-mean(x))**2)/(length(x)-1) # (9) 공식이용 방법
var(y, na.rm=TRUE)     # (10) 분산(결측치 제거)
sd(x)                  # (11) 표준편차
sd(y, na.rm=TRUE)      # (12) 표준편차(결측치 제거)
sd(x)==sqrt(var(x))    # (13) 분산 제곱근과 비교 
y.1 <- c(na.omit(y),2,4) # (14) 분산 제곱근과 비교(결측치제거) 
y.1
cor(x, y.1)            # (15) 상관계수


# 4. 순서통계량 
fivenum(x)                        # (1) 5개 순서통계량
fivenum(y, na.rm=TRUE)            # (2) 5개 순서통계량(결측치 제거)
quantile(x)                       # (3) 분위수
quantile(x, probs=c(0, 0.5, 0.9)) # (4) 특정 분위수
quantile(y, na.rm=TRUE)           # (5) 분위수(결측치 제거)
IQR(x)                            # (6) 사분위 범위
IQR(y, na.rm=TRUE)                # (7) 사분위 범위(결측치제거)
quantile(x,3/4)-quantile(x,1/4)   # (8) quantile을 응용한 IQR 
diff(fivenum(x)[c(2,4)])          # (9) fivenum을 응용한 IQR 
IQR                               # (10) IQR의 함수 정의
mad(x)                            # (11) 중위 절대편차
mad(y, na.rm=TRUE)                # (12) 중위 절대편차(결측치제거)
median(abs(x - median(x)))        # (13) MAD의 정의 1
median(abs(x - median(x)))*1.4826 # (14) MAD의 정의 2


# 5. 통계량 요약
summary(x)
summary(y)

# 6. 도수분포
set.seed(1) 
x <- rpois(100, lambda=5)       # (1) λ=5인 100개의 포아송난수 
x 
table(x)						# (2) x의 도수분포 



###################################################
# 4.1.2 정렬 및 순서와 순위 구하기 
###################################################

###################################################
# 정렬
###################################################
x <- c(3, 1, 4, 5, 9, 2, 6, 3, 7, 6)
y <- c(2, NA, 4, 5, 9, NA, 6, 3, 7, 1)
sort(x)                         # (1) 오름차순 정렬
sort(x, decreasing = TRUE)      # (2) 내림차순 정렬
sort(y)                         # (3) 오름차순 정렬
sort(y, na.last = NA)           # (4) 결측치 제외 정렬
sort(y, na.last = TRUE)         # (5) 결측치 후순 정렬
sort(y, na.last = FALSE)        # (6) 결측치 선순 정렬

is.unsorted(x)                  # (7) 정렬 여부 검증
is.unsorted(sort(x))            # (8) 정렬 여부 검증

vec.char <- c("sangwon", "sangmin", "brother") 
vec.logical <- c(T, F, T, T, F)         
vec.complex <- c(3+4i, 2+8i, 2-5i)
sort(vec.char)                  # (9) 문자형 벡터의 정렬
sort(vec.logical)               # (10) 논리형 벡터의 정렬
sort(vec.complex)               # (11) 복소수형 벡터의 정렬
sort(x, index.return = TRUE)    # (12) index.return 인수 사용

# Sort 알고리즘별 속도 측정
N <- 2000
Sim <- 7
rep <- 10000
c1 <- c2 <- c3 <- numeric(Sim)
for(is in 1:Sim){
  x <- as.integer(abs(rnorm(N) * 1000))
  c1[is] <- system.time(for(i in 1:rep) sort(x, method = "shell"))[1]      # Shell Sort
  c2[is] <- system.time(for(i in 1:rep) sort(x, method = "quick"))[1]      # Quick Sort
  c3[is] <- system.time(for(i in 1:rep) sort.list(x, method = "radix"))[1] # Radix Sort
}
rbind(ShellSort = c1, QuickSort = c2, RadixSort = c3)
summary({sq = c1 / c2; sq[is.finite(sq)]})  # (1) Shell/Quick
summary({sd = c1 / c3; sd[is.finite(sd)]})  # (2) Shell/Radix


###################################################
# 순서
###################################################
x <- c(3, 1, 4, 5, 9, 2, 6, 3, 7, 6)
y <- c(2, NA, 4, 5, 9, NA, 6, 3, 7, 1)
order(x)                        # (1) 오름차순 순서
order(x, decreasing = TRUE)     # (2) 내림차순 순서
order(y)                        # (3) 오름차순 순서
order(y, na.last = NA)          # (4) 결측치 제외 순서
order(y, na.last = TRUE)        # (5) 결측치를 마지막에 위치
order(y, na.last = FALSE)       # (6) 결측치를 처음에 위치
sort(x, index.return = TRUE)$ix # (7) sort 함수의 응용


###################################################
# 순위
###################################################
x <- c(3, 1, 4, 5, 9, 2, 6, 3, 7, 6)
y <- c(2, NA, 4, 5, 9, NA, 6, 3, 7, 1)
rank(x)                         # (1) 순위
rank(y)                         # (2) 순위
rank(y, na.last = NA)           # (3) 결측치 제외 순위
rank(y, na.last = TRUE)         # (4) 결측치를 마지막에 위치
rank(y, na.last = FALSE)        # (5) 결측치를 처음에 위치  

rank(x, ties.method= "average") # (6) 동점일 경우 평균 순위 취함
rank(x, ties.method= "first")   # (7) 동점일 경우 위치 순으로 취함
rank(x, ties.method= "random")  # (8) 동점일 경우 랜덤하게 순위 취함
rank(x, ties.method= "random")  # (9) 동점일 경우 랜덤하게 순위 취함
rank(x, ties.method= "max")     # (10) 동점일 경우 큰 순위 취함
rank(x, ties.method= "min")     # (11) 동점일 경우 작은 순위 취함



###################################################
# 4.1.3 집합의 연산 
###################################################

###################################################
# 두 벡터의 생성
###################################################
set.seed(1)               # random seed를 1로 지정 
x <- sort(sample(1:10, 5)) # 벡터 x의 생성
x
set.seed(2)               # random seed를 1로 지정
y <- sort(sample(1:10, 5)) # 벡터 y의 생성
y

# 1. 합집합 (x∪y) 
union(x, y)                     
unique(c(x, y))


# 2. 교집합 (x∩y) 
intersect(x, y)                        # (1) intersect 함수의 이용
sort(unique(c(x,y)))[table(c(x,y))==2] # (2) c, table, unique, sort함수의 응용
match(x, y, nomatch=0)                 # (3) match 함수의 사용법 1
match(y, x, nomatch=0)                 # (4) match 함수의 사용법 2
unique(x[match(y, x, nomatch=0)])      # (5) match, unique 함수를 이용한 방법 1
unique(y[match(x, y, nomatch=0)])      # (6) match, unique 함수를 이용한 방법 2
intersect                              # (7) intersect 함수의 내용 
is.element(x, y)                       # (8) is.element 함수의 사용법 1
is.element(y, x)                       # (9) is.element 함수의 사용법 2
unique(x[is.element(x, y)])            # (10) is.element, unique 함수 응용(x∩y)
unique(y[is.element(y, x)])            # (11) is.element, unique 함수 응용(y∩x)


# 3. 차집합 (x-y) 
setdiff(x, y)        # (1) setdiff 함수의 이용(x-y)
setdiff(y,x)         # (2) setdiff 함수의 이용(y-x)
union(x,y)           # (3) x와 y의 합집합
table(c(x,y))==1     # (4) x와 y 전체집합에서 도수 1인 원소 체크
union(x,y)[table(c(x,y))==1] # (5) x와 y 전체집합에서 도수 1인 원소 추출
intersect(union(x,y)[table(c(x,y))==1],x) # (6) intersect 함수 응용(x-y)
intersect(union(x,y)[table(c(x,y))==1],y) # (7) intersect 함수 응용(y-x)
x[!is.element(x,y)]  # (8) is.element 함수의 응용(x-y)
y[!is.element(y,x)]  # (9) is.element 함수의 응용(y-x)
setdiff              # (10) setdiff 함수의 내용


# 4. 원소의 집합포함관계 (a∈x, a∈y) 
a <- 3
is.element(a, x)        # (1) is.element 함수 이용 (a∈x)
is.element(a, y)        # (2) is.element 함수 이용 (a∈y)
 
as.logical(sum(x==a))   # (3) as.logical, sum 함수 응용(a∈x)
as.logical(sum(y==a))   # (4) as.logical, sum 함수 응용(a∈y)

!is.na(match(a,x))      # (5) match, all 함수 응용(a∈x)
!is.na(match(a,y))      # (6) match, all 함수 응용(a∈y)
 
match(a, x, 0) > 0      # (7) match 함수 응용(a∈x)
match(a, y, 0) > 0      # (8) match 함수 응용(a∈y)
 
is.element              # (9) is.element 함수의 내용

a %in% x                # (10) %in% 연산자 이용 (a∈x)
a %in% y                # (11) %in% 연산자 이용 (a∈y)


# 5. 집합의 집합포함관계 (z⊂x, z⊂y) 
z <- c(4, 5)                        # (1) 집합 z 생성
z
as.logical(prod(is.element(z, x)))  # (2) is.element, prod 함수 응용(z⊂x)
as.logical(prod(is.element(z, y)))  # (3) is.element, prod 함수 응용(z⊂y)
as.logical(prod(is.element(x, z)))  # (4) is.element, prod 함수 응용(x⊂z)
 
as.logical(prod(intersect(x, z)==z))# (5) intersect, prod 함수 응용(z⊂x)
as.logical(prod(intersect(y, z)==z))# (6) intersect, prod 함수 응용(z⊂y)
as.logical(prod(intersect(x, z)==x))# (7) 원소의 개수가 배가 아니면 경고가 발생

all(match(z,x,0)>0)                 # (8) match, all 함수 응용(z⊂x)
all(match(z,y,0)>0)                 # (9) match, all 함수 응용(z⊂y)
all(match(x,z,0)>0)                 # (10) match, all 함수 응용(x⊂z)

all(is.element(z,x))                # (11) match, all 함수 응용(z⊂x)
all(is.element(z,y))                # (12) match, all 함수 응용(z⊂y)
all(is.element(x,z))                # (13) match, all 함수 응용(x⊂z)


# 6. 집합의 상등(x=y) 
setequal(x, y)                      # (1) setequal 함수의 이용 (x=y)
setequal(x, x)                      # (2) setequal 함수의 이용 (x=x)
setequal                            # (3) setequal 함수의 내용
all(sort(x)==sort(y))               # (4) sort와 all을 이용한 방법 (x=y)
all(sort(x)==sort(x))               # (5) sort와 all을 이용한 방법 (x=x)
all(sort(x)==sort(z))               # (6) sort와 all을 이용한 방법 (x=z)
options(warn=-1)                    # (7) warning 메시지 비활성
all(sort(x)==sort(z))               # (8) warning 메시지가 발생하지 않음
options(warn=0)                     # (9) warning 메시지 활성
ifelse(length(x) != length(z), FALSE, all(sort(x)==sort(z))) # (10) 개선된 방법


###################################################
# 4.1.4 기타 유용한 벡터의 연산
###################################################

###################################################
# 1. 통계 분포함수
###################################################

set.seed(1)										# (1) 난수의 seed 값 설정
rnorm(5, mean=3, sd=2)							# (2) 정규난수

mu <- 3											# (3) 평균
sigma <- 2										# (4) 표준편차
(x <- -4:4)										# (5) x	
1/sqrt(2*pi)/sigma*exp((x-mu)^2/(-2*sigma^2))	# (6) pdf의 표현식
dnorm(x, mean=mu, sd=sigma)						# (7) d + norm

#	P(Z<-1.64 ) + P(Z>1.64)
pnorm(-1.64, 0, 1) 	+							# (8) P(Z<-1.64)
(1- pnorm(1.64, 0, 1))							# (9) P(Z>1.64)

#	P(Z>1)
1- pnorm(1, 0, 1) 								# (10) P(Z>1)

#	P(Z<1.5)
pnorm(1.5, 0, 1) 								# (11) P(Z<1.5)

#	P(-2<Z<2)
pnorm(2, 0, 1) 	-								# (12) P(Z<2)
pnorm(-2, 0, 1) 								# (13) P(Z<-2)

qnorm(c(0.05,0.95), 0, 1)						# (14) P(|Z|<a) = 0.9
qnorm(c(0.025, 0.975), 0, 1)					# (15) P(|Z|<b) = 0.95
qnorm(c(0.005, 0.995), 0, 1)					# (16) P(|Z|<c) = 0.99

###################################################
# 통계 분포함수 : 예제 그림 그리기 - pnorm
###################################################
mu <- 0
sigma <- 1

x <- (-50:50)/10 * sigma + mu
y <- dnorm(x, mu, sigma)

par(mfrow=c(2,2))

# (1) P(Z<-1.64) + P(Z>1.64)
plot(x, y, type="l", ylim=c(0, 0.5), main="1. P(Z<-1.64) + P(Z>1.64)")
for (i in seq(-5,-1.64,length=1000))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")
for (i in seq(1.64,5,length=1000))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")
  
# (2) P(Z>1)
plot(x, y, type="l", ylim=c(0, 0.5), main="2. P(Z>1)")
for (i in seq(1,5,length=1000))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")

# (3) P(Z<1.5)
plot(x, y, type="l", ylim=c(0, 0.5), main="3. P(Z<1.5)")
for (i in seq(-5,1.5,length=1000))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")

# (4) P(-2<Z<2)
plot(x, y, type="l", ylim=c(0, 0.5), main="4. P(-2<Z<2)")
for (i in seq(-2,2,length=1000))
  lines(c(i,i), c(0,dnorm(i, mu, sigma)), col="blue")

par(mfrow=c(1,1))  
  

###################################################
# 통계 분포함수 : 예제 그림 그리기 - qnorm
###################################################
mu <- 0
sigma <- 1

x <- (-50:50)/10 * sigma + mu
y <- dnorm(x, mu, sigma)

par(mfrow=c(2,2))

# (1) P(|Z|<a) = 0.9  
plot(x, y, type="l", ylim=c(0, 0.5), main="1. P(|Z|<a) = 0.9")

abline(v=-1.64, col = "blue", lty=3, lwd=2)
abline(v=1.64, col = "blue", lty=3, lwd=2)

arrows(-5, 0.21, -1.64, 0.21, col="red", code=1, length=0.1, lwd=1.8)
arrows(-1.64, 0.2, 1.64, 0.2, col="red", code=3, length=0.1, lwd=2)
arrows(1.64, 0.21, 5, 0.21, col="red", code=2, length=0.1, lwd=1.8)

text(-3.5, 0.23, "5%", cex=0.8)
text(0, 0.23, "90%", cex=1)
text(3.5, 0.23, "5%", cex=0.8)

# (2) P(|Z|<b) = 0.95  
plot(x, y, type="l", ylim=c(0, 0.5), main="2. P(|Z|<b) = 0.95")

abline(v=-1.96, col = "blue", lty=3, lwd=2)
abline(v=1.96, col = "blue", lty=3, lwd=2)

arrows(-5, 0.21, -1.96, 0.21, col="red", code=1, length=0.1, lwd=1.8)
arrows(-1.96, 0.2, 1.96, 0.2, col="red", code=3, length=0.1, lwd=2)
arrows(1.96, 0.21, 5, 0.21, col="red", code=2, length=0.1, lwd=1.8)

text(-3.5, 0.23, "2.5%", cex=0.8)
text(0, 0.23, "95%", cex=1)
text(3.5, 0.23, "2.5%", cex=0.8)

# (3) P(|Z|<c) = 0.99  
plot(x, y, type="l", ylim=c(0, 0.5), main="3. P(|Z|<c) = 0.99")

abline(v=-2.58, col = "blue", lty=3, lwd=2)
abline(v=2.58, col = "blue", lty=3, lwd=2)

arrows(-5, 0.21, -2.58, 0.21, col="red", code=1, length=0.1, lwd=1.8)
arrows(-2.58, 0.2, 2.58, 0.2, col="red", code=3, length=0.1, lwd=2)
arrows(2.58, 0.21, 5, 0.21, col="red", code=2, length=0.1, lwd=1.8)

text(-3.5, 0.23, "0.5%", cex=0.8)
text(0, 0.23, "99%", cex=1)
text(3.5, 0.23, "0.5%", cex=0.8)

par(mfrow=c(1,1))  

  
###################################################
# 2. 표본추출
###################################################

vec.1 <- c(3, 5, 7, 11, 13, 17, 19) 	# (1) 수치벡터
vec.2 <- letters 						# (2) 수치벡터

sample(x=vec.1, size=3, replace=F)		# (3) 비복원 추출
sample(vec.1, 3, replace=T)				# (4) 복원 추출
sample(vec.1, size=10, replace=F)		# (5) size>length(x),비복원
sample(vec.1, size=10, replace=T)		# (6) size>length(x), 복원
sample(vec.2, 7)						# (7) 문자벡터에서 추출
sample(vec.2, 7, replace=T)				# (8) 복원 추출

sample(7)								# (9) x=1:x, size=x
sample(7, 5)							# (10) x=1:x, size=5
sample(7, 5, replace=T)					# (11) 복원 추출
sample(vec.1)							# (12) size=length(x)
sample(letters[1:5])					# (13) size=length(x)

set.seed(1)								# (14) 랜덤 seed
sample(7)								# (15) set.seed 적용
sample(7)								# (16) set.seed 비적용
set.seed(1)								# (17) 랜덤 seed
sample(7)								# (18) set.seed 적용

ls(pattern="^\\.R", all.name=T)			# (19) 목록 조회
length(.Random.seed)					# (20) 벡터의 길이
set.seed(1)								# (21) 랜덤 seed 1 지정
.Random.seed[1:5]						# (22) .Random.seed
set.seed(8)								# (23) 랜덤 seed 8 지정
.Random.seed[1:5]						# (24) .Random.seed
set.seed(1)								# (25) 랜덤 seed 1 지정
.Random.seed[1:5]						# (26) .Random.seed


###################################################
# 3. any, all
###################################################

x <- c(F, T, F)
y <- c(T, T, T)
z <- c(F, F, F)

any(x); any(y); any(z)					# (1) any
all(x); all(y); all(z)					# (2) all

z[3] <- NA								# (3) z에 NA 추가
any(z)									# (4) na.rm=F
any(z, na.rm=T)							# (5) na.rm=T

x <- c(1, 3, -2)
if (any(x < 0)) 						# (6) 응용 예시
  cat("x contains negative values\n") else log(x)


###################################################
# 4. 값의 매치
###################################################

# match
1:5 %in% c(1,3,5,9)						# (1) 논리값 
match(1:5, c(1,3,5,9))					# (2) 위치값
match(1:5, c(1,3,5,9), nomatch=0)		# (3) 미매치값은 0

sstr <- c("c","ab","B","bba","c","@")
alphabet <- c(letters, LETTERS)
sstr %in% alphabet						# (4) 논리값
sstr[sstr %in% alphabet]				# (5) 알파벳 1글자 찾기

(idx <- match(sstr, alphabet, nomatch=0) > 0)	# (6) %in%과 동일
sstr[idx]								# (7) (5)와 동일

# charmatch
tabs <- c("mean", "median", "mode")
charmatch("", "")                      	# (8) 단일 매치
charmatch("m",   tabs) 					# (9) 경합 발생
charmatch("med", tabs) 					# (10) median
charmatch("a",   tabs) 					# (11) no match
charmatch("a",   tabs, nomatch=9) 		# (12) nomatch=9

# pmatch
tabs <- c("mean", "median", "mode")
pmatch("", "")                      	# (13) 단일 매치
pmatch("m",   tabs) 					# (14) 경합 발생
pmatch("med", tabs) 					# (15) median
pmatch("a",   tabs) 					# (16) no match
pmatch("a",   tabs, nomatch=0) 			# (17) no match

x <- c("", "ab", "ab")
tabs <- c("abc", "ab")

pmatch(x, tabs, duplicates.ok=F)		# (18) duplicates.ok=F
pmatch(x, tabs, dup=T)					# (19) duplicates.ok=T

charmatch(x,   tabs) 					# (20) charmatch


###################################################
# B.행렬의 연산
###################################################

###################################################
# 행렬의 기본 연산
###################################################

# 1. 행렬의 전치
a <- matrix(1:12, nrow=4)       	# (1) 행렬 a 생성
a
t(a)		                  		# (2) t 함수를 이용한 전치
N.row <- NROW(a)                   	# (3) 행렬 a의 행의 수   
N.row             
N.col <- NCOL(a)                   	# (4) 행렬 a의 열의 수                
N.col
ta <- matrix(0, nrow=N.col, ncol=N.row) # (5) 전치행렬을 위한 행렬 생성
ta
for (i in 1:N.col)               	# 
   for (j in 1:N.row)            	# 
      ta[i,j] <- a[j,i]           	# (6) 행과 열의 원소를 바꿈
ta                     		  		# (7) 생성된 전치행렬

	
# 2. 행렬의 덧셈
(zero <- matrix(rep(0,12), nrow=4))	# (1) 영행렬 생성
3+zero                            	# (2) 스칼라와 행렬의 덧셈
zero+3                            	# (3) 행렬 덧셈의 교환법칙
1:3+zero                          	# (4) 벡터와 행렬의 덧셈 1
1:4+zero                          	# (5) 벡터와 행렬의 덧셈 2 
set.seed(1)
(b <- matrix(sample(12), nrow=4))
a+b                               	# (6) 행렬과 행렬으 덧셈
1:3                               	# (7) 열벡터
t(1:3)                            	# (8) 행벡터(열벡터의 전치)
t(t(1:3))                         	# (9) 열벡터(전치의 전치) 

# 3. 행렬의 뺄셈
zero-3                            	# (1) 행렬과 스칼라의 뺄셈
1:4-zero                          	# (2) 벡터와 행렬의 뺄셈 
a-b                               	# (3) 행렬과 행렬의 뺄셈 1
b-a                               	# (4) 행렬과 행렬의 뺄셈 2
  
# 4. 행렬의 곱셈 (벡터의 내적 외적)
a <- 1:3				  			# (1) 원소 개수가 3인 벡터
b <- 3:5                            # (2) 원소 개수가 3인 벡터
c <- 3:6                            # (3) 원소 개수가 4인 벡터
d <- 3:8                            # (4) 원소 개수가 6인 벡터
### R의 데이터 구조로서의 벡터의 곱셈
a*b                               	# (5) 원소 개수가 동일한 두 벡터의 곱
a*c                               	# (6) 원소 개수가 다른 두 벡터의 곱
a*d                               	# (7) 원소 개수가 배수인 두 벡터의 곱
### 수학적인 두 벡터의 곱셈
sum(a*b)                          	# (8) 두 벡터의 내적
ab <- a%*%b                       	# (9) 두 벡터의 내적
ab                                
is.matrix(ab)                     	# (11) 1 by 1 행렬
drop(a%*%b)                       	# (12) 스칼라로 변환 
a%*%d                             	# (13) 성립하지 않는 두 벡터의 내적
a%o%b                             	# (14) 두 벡터의 외적 1
a%*%t(b)                          	# (15) 두 벡터의 외적 2
outer(a,b)                        	# (16) 두 벡터의 외적 3


# 5. 행렬의 곱셈 (벡터와 행렬의 곱셈)
a <- 1:3				  			# (1) 원소 개수가 3인 벡터
A <- matrix(1:12, nrow=3)         	# (2) 3 by 4 행렬
A
B <- matrix(1:12, nrow=4)          	# (3) 4 by 3 행렬
B
a%*%A                             	# (4) 벡터와 행렬의 곱셈 1
A%*%a                             	# (5) 벡터와 행렬의 곱셈 2
B%*%a                             	# (6) 행렬과 벡터의 곱셈


# 6. 행렬의 곱셈 (행렬 행렬의 곱셈)
A <- matrix(1:6, nrow=2)          	# (1) 2 by 3 행렬
A
B <- matrix(1:6, nrow=3)          	# (2) 3 by 2 행렬
B
A%*%B                             	# (3) 행렬과 행렬의 곱셈 1
B%*%A                             	# (4) 행렬과 행렬의 곱셈 2
A%*%A                             	# (5) 행렬과 행렬의 곱셈 2
t(A)%*%A                          	# (6) 행렬 A의 외적 1
crossprod(A)                      	# (7) 행렬 A의 외적 2

# 8. 행렬식 구하기
A2 <- matrix(1:4, ncol=2)          	# (1) 2차 정방행렬
A2
A3 <- cbind(1,1:3,c(2,0,1))       	# (2) 3차 정방행렬
A3
det(A2)                            	# (3) 2차 행렬식
z <- determinant(A3)              	# (4) 3차 행렬식의 log
z
c(z$sign * exp(z$modulus))         	# (5) 일반적인 행렬식 계산
determinant(A3, logarithm = FALSE) 	# (6) 3차 행렬식
prod(unlist(determinant(A3, F)))   	# (7) 3차 행렬식
det(t(A3))                         	# (8) 전치행렬의 행렬식

# 8. 역행렬 구하기
A <- matrix(1:9, ncol=3)          	# (1) 3차 정방행렬
A
A.rev <- solve(A)               	# (2) A의 역행렬 구하기
det(A)                            	# (3) A의 행렬식
B <- matrix(c(1,3,5,7), ncol=2)    	# (4) 2차 정방행렬
B
B.rev <- solve(B)                 	# (5) B 역행렬 구하기
B.rev       
det(B)                            	# (6) B의 행렬식
B %*% B.rev                       	# (7) 역행렬 검증하기1
round(B %*% B.rev, 3)             	# (8) 역행렬 검증하기2
diag(3)                           	# (9) 3차 단위행렬
solve(diag(3))                    	# (10) 단위행렬의 역행렬


###################################################
# 행렬의 분해
###################################################

# 1. Norm과 Trace
x <- c(1,2,4,2)               		# (1) 벡터 x
x1 <- c(0.5,0.5,0.5,0.5)        	# (2) 벡터 x1
sqrt(t(x)%*%x)                    	# (3) 벡터 x의 Norm 
sqrt(sum(x1^2))                   	# (4) 벡터 x1의 Norm

A <- matrix(c(1,2,-3,-4), ncol=2) 	# (5) 행렬 A
A
install.packages("Matrix")        	# (6) Matrix 라이브러리 설치
library(Matrix)                   	# (7) Matrix 라이브러리 로드
norm(A, "1")                      	# (8) 행렬 A의 Norm 1
norm(A, "I")                      	# (9) 행렬 A의 Norm 2
norm(A, "F")                      	# (10) 행렬 A의 Norm 3
norm(A, "M")                      	# (11) 행렬 A의 Norm 4

max(apply(A,2,function(x) sum(abs(x)))) # (12) "1"의 계산
max(apply(A,1,function(x) sum(abs(x)))) # (13) "I"의 계산
sqrt(sum(diag(t(A)%*%A)))               # (14) "F"의 계산1
sqrt(sum(A^2))                          # (15) "F"의 계산2
max(abs(A))                             # (16) "M"의 계산
diag(A)                           	# (17) 행렬A의 대각원소
sum(diag(A))                      	# (18) 행렬A의 Trace


# 2. 고유치, 고유벡터
A <- matrix(c(1,9,4,1), ncol=2)    	# (1) 행렬 A
A
eA <- eigen(A)                   	# (2) 고유치, 고유벡터
eA
A %*% eA$vectors[,1]             	# (3) Au
eA$values[1] * eA$vectors[,1]      	# (4) λu

t(eA$vectors[,1]) %*% eA$vectors[,1]  # (5) 첫째 고유벡터의 노옴
t(eA$vectors[,2]) %*% eA$vectors[,2]  # (6) 둘째 고유벡터의 노옴
sum(diag(A))                          # (7) A의 고유화(Trace)
sum(eA$values)                        # (8) 고유치의 합
det(A)                                # (9) A의 행렬식
prod(eA$values)                       # (10) 고유치의 곱


# 3. LU분해
library(Matrix)
A <- Matrix(c(1,1,1,1,2,4,1,3,9), ncol=3, byrow=T)	# (1) 3차 정방행렬
A
A.PLU <- lu(A)                                   	# (2) P*A = L*U 분해
plu <- expand(A.PLU)                               	# (3) L,U,P              
plu 
plu$L %*% plu$U                                    	# (4) P*L*U 행렬
A2 <- with(plu, P %*% L %*% U)
A2
stopifnot(all.equal(as(A, "matrix"),               	# (5) A와 P*L*U 비교
                    as(A2, "matrix")))
stopifnot(all.equal(plu$P %*% A, plu$L %*% plu$U)) 	# (6) P*A와 L*U 비교  


# 4. QR분해
A <- matrix(c(1,1,1,1,2,4,1,3,9), ncol=3, byrow=T)	# (1) 3차 정방행렬
A
A.Q <- qr(A)                                      	# (2) QR 분해
A.QR
(Q <- qr.Q(A.QR))                                 	# (3) Q
(R <- qr.R(A.QR))                                 	# (4) R 
Q%*%R                                              	# (5) Q*R 
qr.X(A.QR)                                         	# (6) 원래의 행렬
Q%*%t(Q)                                           	# (7) 직교행렬검증
round(Q%*%t(Q),15)                                 	# (8) 반올림오차 보정


# 5. svd분해
X <- matrix(c(1,1,1,1,2,4,1,3,9), ncol=3, byrow=T) 	# (1) 3차 정방행렬
(s <- svd(X))                                      	# (2) SVD 분해
D <- diag(s$d)                                     	# (3) 대각행렬만들기
s$u %*% D %*% t(s$v)                               	# (4) X = U D V'
t(s$u) %*% X %*% s$v                               	# (5) D = U' X V
round(s$u%*%t(s$u),15)                             	# (6) 직교행렬검증 
round(s$v%*%t(s$v),15)                             	# (7) 직교행렬검증

# 6. 콜레스키 분해
(M <- matrix(c(5,1,1,3),2,2))                		# (1) 2차 정방행렬
(M.CHOL <- chol(M))                              	# (2) Choleski 분해
t(M.CHOL) %*% M.CHOL                               	# (3) R'R
crossprod(M.CHOL)                                  	# (4) R의 외적



###################################################
# 행렬연산의 응용
###################################################

# 1. 연립장정식의 해1
A <- matrix(c(3,1,2,-4,-1,5,3,1,-1), ncol=3)    	# (1) 계수행렬
A
b <- c(8,4,3)                                      	# (2) 상수벡터
b
solve(A) %*% b                                     	# (3) 연립방정식해 1
(x <- solve(A,b))                               	# (4) 연립방정식해 2
A%*%x                                              	# (5) 연립방정식의 검증

# 2. 연립장정식의 해2
A <- matrix(c(2,0,0,4,1,0,2,4,2),ncol=3)         	# (1) 삼상각행렬
A
b <- c(2,1,4)
b
solve(A,b)                                         	# (2) 연립방정식해 1
backsolve(A,b)                                     	# (3) 연립방정식해 2
rev(forwardsolve(t(A),rev(b)))                     	# (4) forwardsolve

# 3. 중회귀분석
# 데이터
stackloss                                          	# (1) Stack Loss Plant Data
stack.x                                            	# (2) stackloss의 독립변수 
stack.loss                                         	# (3) stackloss의 종속변수 

# 회귀계수
X <- cbind(1,stack.x)                           	# (1) 종속변수 행렬 X
X                                                  	# (2) 행렬 X
solve(t(X) %*% X) %*% t(X) %*% stack.loss          	# (3) 회귀계수의 계산
lm(stack.loss ~ stack.x)                           	# (4) lm을 이용한 회귀계수 추정

# 추정 회귀값과 잔차
beta <- solve(t(X) %*% X) %*% t(X) %*% stack.loss  	# (1) 회귀 계수
X %*% beta                                         	# (2) 추정 회귀값
res <- stack.loss - X %*% beta                   	# (3) 잔차의 계산 
res                                                	# (4) 잔차
sum(res)                                           	# (5) 잔차의 합
round(sum(res),10)                                 	# (6) 잔차의 합2



#############################################################
### C. apply 함수 군 활용
#############################################################

#############################################################
### apply 함수
#############################################################

mat.1 <- matrix(1:12, ncol=4)	# 3*4 행렬을 만든다.
mat.1
mat.2 <- mat.1			
mat.2[2,3] <- NA           		# 결측치(NA)가 있는 행렬을 만든다.

# apply의 일반적인 사용 예
apply(mat.1, 1, sum)					# (1) 행렬의 각 행의 합을 구함
apply(mat.1, 2, sum)					# (2) 행렬의 각 열의 합을 구함
apply(mat.1, 1, min)					# (3) 행렬의 각 행에서 최소값을 구함
apply(mat.1, 2, max)					# (4) 행렬의 각 열에서 최대값을 구함
apply(mat.1, 2, prod)					# (5) 행렬의 각 열의 원소를 곱합
apply(mat.1, 2, mean)					# (6) 행렬의 각 열의 평균을 구함
apply(mat.1, 1, range)   				# (7) 행렬의 각 행에 range 함수를 적합함(최소값, 최대값을 구함)
apply(mat.1, 2, range)     				# (8) 행렬의 각 열에 range 함수를 적합함(최소값, 최대값을 구함)
apply(mat.1, 1, quantile)  				# (9) 행렬의 각 행에 분위수를 구함

# 적용함수에서 사용할 인수를 지정하는 예
apply(mat.2, 1, sum)					# (10) 결측치가 있는 행렬의 각 행의 합을 구함
apply(mat.2, 1, sum, na.rm=TRUE)		# (11) 결측치가 있는 행렬의 각 행의 합을 구함 (sum 함수의 인수 지정)
apply(mat.1, 1, sort, decreasing=TRUE) 	# (12) 행렬의 각 행을 내림차순으로 정렬함

# FUN 인수값으로 연산자를 사용한 예 
apply(mat.1, 1, "!")            		# (13) 단항연산자의 사용
apply(mat.1, 1, "/", 2)					# (14) 행렬의 각 행의 원소를 2로 나눔 (다항연산자)

vec.1=1:4
vec.2=1:3
apply(mat.1, 1, "%*%", vec.1)   		# (15) 행렬의 행에 대해서 vec.1 벡터로 행렬의 곱을 구함
apply(mat.1, 2, "%*%", vec.2)   		# (16) 행렬의 열에 대해서 vec.2 벡터로 행렬의 곱을 구함

# 사용자 정의함수를 사용한 예
apply(mat.1, 1, function(x) sum(ifelse(x%%5,0,1))) # (17) 행렬의 각 행에서 5의 배수의 개수를 계산
apply(mat.1, 2, function(x) sum(ifelse(x%%5,0,1))) # (18) 행렬의 각 열에서 5의 배수의 개수를 계산

# 배열에 대해서 apply 함수를 사용한 예
ary.1 <- array(1:12, dim=c(2,2,3)) 		# 2*2*3 배열을 만든다.
ary.1
apply(ary.1, 1, sum)					# (19) 배열의 각 행(첫번째 차원)의 합을 구한다.
apply(ary.1, 2, sum)	        		# (20) 배열의 각 열(두번째 차원)의 합을 구한다.
apply(ary.1, 3, sum)            		# (21) 배열의 각 행렬(세번째 차원)의 합을 구한다.
apply(ary.1, c(1,2), sum)       		# (22) 배열의 각 행과 열별로 원소의 합을 구한다.


#############################################################
### lapply 함수
#############################################################
lst <- list(x=1:10, y=c(T,F,T,T,F), z=c(3,5,7,11,13,17))
lst
lapply(lst, mean)						# (1) 각 성분에 mean 함수 적용
lapply(lst, max)						# (2) 각 성분에 max 함수 적용
lapply(lst, quantile, probs = 1:3/4)	# (3) quantile 함수에 probs 인수 적용


#############################################################
### sapply 함수
#############################################################
lst <- list(x=1:10, y=c(T,F,T,T,F), z=c(3,5,7,11,13,17))
lst
sapply(lst, mean)						# (1) 각 성분에 mean 함수 적용
sapply(lst, max, simplify=F)			# (2) simplify=FALSE
sapply(lst, quantile, probs = 1:3/4)	# (3) quantile 함수에 probs 인수 적용
sapply("char", nchar)					# (4) USE.NAMES=T, X is character
sapply("char", nchar, simplify=F)		# (5) simplify=F
sapply("char", nchar, USE.NAMES=F)		# (6) USE.NAMES=F


#############################################################
### sapply 함수
#############################################################
lst <- list(x=1:10, y=c(T,F,T,T,F), z=c(3,5,7,11,13,17))
lst
vapply(lst, is.logical, FUN.VALUE=T)	# (1) logical
vapply(lst, is.logical, FUN.VALUE=0)	# (2) integer
vapply(lst, is.logical, FUN.VALUE=0i)	# (3) complex
vapply(lst, quantile, probs = 1:3/4, 
  FUN.VALUE=c("1Q"=0, "2Q"=0, "3Q"=0))	# (4) names 지정
vapply(lst, quantile, probs = 1:3/4, 
  FUN.VALUE=c("1Q"=0i, "2Q"=0, "3Q"=0))	# (5) 복소수
vapply(lst, quantile, probs = 1:3/4, USE.NAMES=F,
  FUN.VALUE=c("1Q"=0i, "2Q"=0, "3Q"=0))	# (6) USE.NAMES=F

  
#############################################################
### tapply 함수
#############################################################
head(warpbreaks)						# (1) 직조기 인장강도 자료
dim(warpbreaks)							# (2) 데이터 개수
levels(warpbreaks$wool)					# (3) wool 종류의 수준
levels(warpbreaks$tension)				# (4) tension 종류의 수준
# (5) tension별 wool별 끊어진 개수합 : 분할표
tapply(warpbreaks$breaks, warpbreaks[,-1], sum) 
# (6) tension별 끊어진 개수의 최대값
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], max)
# (7) 리스트 형태로 출력 : 분할표로 출력
tapply(warpbreaks$breaks, warpbreaks[, -1], sum, simplify=F)
# (8) 리스트 형태로 출력 : 리스트로 출력
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], max, simplify=F)


#############################################################
### mapply 함수
#############################################################
mapply(rep, letters[1:3], 3:5)									# (1) SIMPLIFY=T
mapply(rep, letters[1:3], 3:5, SIMPLIFY=F)						# (2) SIMPLIFY=F
mapply(rep, letters[1:3], 3:5, USE.NAMES=F)						# (3) USE.NAMES=F
concat <- function(x, times) paste(rep(x, times), collapse="") 	# (4) 함수정의
mapply(concat , letters[1:3], 3:5)								# (5) SIMPLIFY=T
mapply(concat , times=3:5, x=letters[1:3], SIMPLIFY=F)			# (6) SIMPLIFY=F
mapply(rep, letters[1:3], each=3, SIMPLIFY=F)					# (7) Recycle Rule

my.mean <- function (n, ...) 
{
	set.seed(1)
	mean(rnorm(n), ...)
}

# (8) 정규난수 평균 구하기
mapply(my.mean, 
  c(n10=10,n100=100,n1000=1000,n10000=10000,n100000=100000))

# (9) MoreArgs 인수 사용
mapply(my.mean, 
  c(n10=10,n100=100,n1000=1000,n10000=10000,n100000=100000),
  MoreArgs=list(trim=0.1, na.rm=F))

mapply("*", c("1*6"=1, "2*7"=2, "3*8"=3), 6:8)					# (10) 연산자의 적용


#############################################################
### replicate 함수
#############################################################
my.sample <- function(n, size, seed)
{
	set.seed(seed)
	sample(n, size)
}

replicate(3, my.sample(1:10, size=3, 1))						# (1) 행렬 반환
replicate(3, mean(my.sample(1:10, size=3, 1)))					# (2) 벡터 반환
replicate(3, my.sample(1:10, size=3, 2), simplify=F) 			# (3) simplify=F
body(replicate)													# (4) sapply 사용


#############################################################
### sweep 함수
#############################################################
(mat <- matrix(1:12, ncol=4))	# (1) 행렬 생성
r <- 1:3
c <- 1:4
sweep(mat, 1, r)				# (2) 행별로 - 연산
sweep(mat, 2, c, "*")			# (3) 열별로 * 연산
sweep(mat, 1, c)				# (4) 경고 발생
sweep(mat, 1, c, check.margin=F)# (5) 경고 미 발생	

dim(Titanic)					# (6) 타이타닉 생존자료의 차원
apply(Titanic, c(1,4), sum)		# (7) 선실등급별 생존현황
apply(Titanic, c(2,4), sum)		# (8) 성별 생존현황
apply(Titanic, c(3,4), sum)		# (9) 연령별 생존현황

# (10) 연령별 생존구분 그룹별 선실등급, 성별 분포(백분율) 
round(sweep(Titanic, c(3,4), apply(Titanic, c(3,4), sum), "/")*100)
round(prop.table(Titanic, c(3,4))*100) # (11) prop.table 함수 이용


#############################################################
### aggregate 함수
#############################################################
dim(USArrests)					# (1) USArrests 차원
names(USArrests)				# (2) USArrests 변수이름
state.region[1:7]				# (3) 50개 주의 지역분류
row.names(USArrests)[1:7]		# (4) 50개 주의 이름
aggregate(USArrests, list(Region = state.region), mean) # (5) 지역별 평균
aggregate(USArrests, list(Region = state.region), mean, trim=0.1) # (6) 적용함수의 선택 인수 사용 
# (7) by 인수의 확장 
aggregate(USArrests,
          list(Region = state.region,
               Urbanization = USArrests[,"UrbanPop"] >= 70),
          mean)


#############################################################
### by 함수
#############################################################
attach(warpbreaks)							# (1) attach
by(warpbreaks[, 1:2], tension, summary)		# (2) tension별 summary
# (3) wool별, tension별 평균
(tmp <- by(warpbreaks[, 1], 
    list(wool = wool, tension = tension), mean))

is(tmp)										# (4) by 객체
attributes(tmp)								# (5) by 객체 속성
as.table(tmp)								# (6) table로 변환
(tmp1 <- as.data.frame(as.table(tmp)))		# (7) 데이터프레임 변환
names(tmp1)[3] <- "mean"					# (8) Freq를 mean으로
tmp1

# (9) 사용자 정의 함수
get.info <- function(x) 
{
    data.frame(n=length(x),
    mean=mean(x),
    sd=sd(x))
}

# (10) wool별, tension별 집계
(tmp2 <- by(warpbreaks[, 1], 
    list(wool = wool, tension = tension), get.info))

do.call(rbind, tmp2)						# (11) 데이터 프레임으로
# (12) 완전한 데이터 프레임
cbind(expand.grid(dimnames(tmp2)), do.call(rbind, tmp2))

# (13) tension별 lm fit
(fit <- by(warpbreaks, tension, 
    function(x) lm(breaks ~ wool, data = x)))

sapply(fit, coef)							# (14) 회귀계수 추출

detach(warpbreaks)							# (15) detach


#############################################################
### 합/평균 집계 함수 
#############################################################
mat <- matrix(1:12, ncol=4, 
  dimnames=list(c("R.1","R.2","R3"),
                c("C.1","C.2","C.3","C.4")))
mat[2,3] <- NA
mat

colSums (mat)					# (1) 열의 합
rowSums (mat)					# (2) 행의 합
colMeans(mat)					# (3) 열의 평균
rowMeans(mat)					# (4) 행의 평균

colSums (mat, na.rm=T)			# (5) 열의 합, na.rm
rowSums (mat, na.rm=T)			# (6) 행의 합, na.rm
colMeans(mat, na.rm=T)			# (7) 열의 평균, na.rm
rowMeans(mat, na.rm=T)			# (8) 행의 평균, na.rm

UCBAdmissions  					# (9) UC Berkeley 입학생 자료
dim(UCBAdmissions)				# (10) 배열의 차원

rowSums(UCBAdmissions)			# (11) 합격 여부별 
rowSums(UCBAdmissions, dims = 2)# (12) 합격 여부별 성별 

colSums(UCBAdmissions)			# (13) 단과대학별 성별
colSums(UCBAdmissions, dims = 2)# (14) 단과대학별



###################################################
# B.집계표 함수군 활용 	
###################################################

###################################################
# 4.4.1 table 함수
###################################################

# 데이터 프레임 객체
with(warpbreaks, table(wool))			# (1) 1차원 테이블
with(warpbreaks, table(wool, tension))	# (2) 분할표
# (3) Temp 분위수 구간별 월별 집계 
with(airquality, table(cut(Temp, quantile(Temp)), Month))
# 범주형 자료
table(state.division, state.region)		# (4) 분할표

a <- letters[1:3]
b <- sample(a)
table(a, sample(a), deparse.level=0) 	# (5) deparse.level=0
table(a, b)                    			# (6) 모두 심볼 이름
table(a, sample(a))                    	# (7) a만 심볼 이름
table(a, b=sample(a))                  	# (8) 이름 지정
table(a, sample(a), deparse.level=2) 	# (9) deparse.level=2
table(a, b, dnn=c("letter","sample")) 	# (10) dnn

set.seed(1)
(df.1 <- data.frame(
    a=sample(c(NA, NaN, 1, 2),10, replace=T),
    b=rep(c("X","Y"),5)))

with(df.1, table(a))					# (11) default
with(df.1, table(a, exclude=NA))		# (12) exclude=NA
with(df.1, table(a, exclude=NULL))		# (13) exclude=NULL

with(df.1, table(a, b, useNA="no"))		# (14) useNA="no"
with(df.1, table(a, b, useNA="ifany"))	# (15) useNA="ifany"
with(df.1, table(a, b, useNA="always"))	# (16) useNA="always"


###################################################
# 4.4.2 ftable 함수
###################################################

ftable(Titanic, row.vars=1:3)						# (1) row.vars
ftable(Titanic, row.vars=1:2, col.vars="Survived")	# (2) character
ftable(Titanic, row.vars=2:1, col.vars=4)			# (3) 집계순서바뀜

names(mtcars)
(x <- ftable(mtcars[c("cyl", "vs", "am", "gear")]))	# (4) 4차원
ftable(x, row.vars=c(2, 4), col.vars=1)				# (5) ftabe 객체 사용

ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear, # (6) dnn 인수
       row.vars = c(2, 4),
       dnn=c("Cylinders", "V/S", "Transmission", "Gears"))
	   
  
###################################################
# 4.4.3 margin.table 함수
###################################################

(m <- matrix(1:4, 2))
margin.table(m)				# (1) 전체 집계
margin.table(m, 1)			# (2) 행 방향 집계
margin.table(m, margin=2)	# (3) 열 방향 집계	
apply(m, 1, sum)			# (4) apply 함수-행집계
colSums(m)					# (5) colSums-열집계

(ary <- array(1:12, c(2, 2, 3)))
margin.table(ary, 1)		# (6) 배열 1차원 집계
margin.table(ary, 2)		# (7) 배열 2차원 집계
margin.table(ary, 3)		# (8) 배열 3차원 집계


###################################################
# 4.4.4 prop.table 함수
###################################################
  
(m <- matrix(1:4, 2))
prop.table(m)				# (1) 전체 집계
prop.table(m, 1)			# (2) 행 방향 집계
prop.table(m, margin=2)		# (3) 열 방향 집계	
# sweep(x, margin, margin.table(x, margin), "/") 
sweep(m, 1, margin.table(m, 1), "/") # (4) 행 방향 집계
sweep(m, 2, margin.table(m, 2), "/") # (5) 열 방향 집계	

(ary <- array(1:8, c(2, 2, 2)))
prop.table(ary, 1)		# (6) 배열 1차원 집계
prop.table(ary, 2)		# (7) 배열 2차원 집계
prop.table(ary, 3)		# (8) 배열 3차원 집계
  
  
###################################################
# 4.4.5 xtabs 함수
###################################################  

dimnames(UCBAdmissions)
xtabs( ~ Gender + Admit, data=UCBAdmissions)				# (1) 자료의 개수
(tab <- xtabs(Freq ~ Gender + Admit, data=UCBAdmissions))	# (2) 성별 합격여부별 돗수
summary(tab)												# (3) 카이스퀘어 검정
summary(xtabs(Freq ~ .,UCBAdmissions))						# (4) 카이스퀘어 검정

xtabs(Freq~ Sex+Class, data=Titanic)						# (5) 성별 선실등급별
xtabs(Freq~ Class, data=Titanic, subset=Sex=="Male")		# (6) subset 인수

(df.1 <- data.frame(
  Sex=c("Female","Male","Male","Male","Female","Male"),
  Class=c("2nd","1st","1st",NA,"2nd","2nd"),
  Count=c(12,32,42,34,23,11)))
xtabs(Count ~ Class, data=df.1, subset=Sex=="Female")		# (7) drop.unused.levels=F
xtabs(Count ~ Class, data=df.1, subset=Sex=="Female",		# (8) drop.unused.levels=T
  drop.unused.levels=T)
  
xtabs(Count ~ Sex+Class, data=df.1, na.action=na.fail)		# (9) na.action
xtabs(Count ~ Sex+Class, data=df.1, sparse=T)				# (10) sparse


	  