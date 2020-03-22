###################################################
# A. 데이터의 삭제 및 변경과 추가
###################################################

###################################################
# 1. 벡터의 조작
###################################################
vec.name <- c("1st","2nd","3rd","4th","5th")
vec.1 <- 1:5
names(vec.1) <- vec.name
vec.1

vec.1[3:5]      							# (1) 세번째,네번째,다섯번째 원소 추출(첫째, 둘째 원소 삭제)
vec.1[-c(1,2)]  							# (2) 첫번째, 두번째 원소의 삭제
vec.1[c(-1,-2)] 							# (3) 첫번째, 두번째 원소의 삭제
vec.1[c("1st","2nd")] 						# (4) 벡터의 이름을 이용한 원소의 추출
vec.1[!names(vec.1) %in% c("1st","2nd")] 	# (5) 벡터의 이름을 이용한 원소의 삭제(조건식 이용)
vec.1[3] <- 3.3 								# (6) 벡터의 특정 원소 값 변경 (index 이용)
vec.1

vec.2 <- c(vec.1, c(6,7)) 					# (7) 벡터에 원소 추가하기 (append)
names(vec.2)<- c(vec.name, "6th", "7th")
vec.2

vec.3 <- c(vec.2[1:5], NA, vec.2[6:7]) 		# (8) 벡터에 원소 추가하기 (insert)
names(vec.3) <- c(vec.name, "6th", "7th", "8th")
vec.3

vec.3[vec.3>4]       						# (9) 논리연산을 이용한 원소 추출 1
vec.3[!is.na(vec.3)] 						# (10) 논리연산을 이용한 원소 추출 2
vec.3[vec.3>4 & !is.na(vec.3)] 				# (11) 논리연산을 이용한 원소 추출

vec.3[is.na(vec.3)] = 0.0 					# (12) 논리 연산을 이용한 원소의 변경 1
vec.3

vec.3[vec.3>=4] <- vec.3[vec.3>=4] %% 4 		# (13) 논리 연산을 이용한 원소의 변경 2
vec.3

vec.4 <- replace(vec.3, c(4,6), 5) 			# (14) replace 함수를 이용한 원소 변경
vec.4
append(vec.4, 3:1)                			# (15) append 함수를 이용한 원소의 추가
append(vec.4, 3:1, after=2)       			# (16) append 함수를 이용한 원소의 삽입

vec.5  <- c(1, 3, 5, 7, NA, 9)				
idx <- which(is.na(vec.5)==TRUE)			# (17) which 함수로 NA index 수집
replace(vec.5, idx, 0)						# (18) replace 함수로 변경
******************
###################################################
# 2. 행렬의 조작
###################################################
row.name <- c("Row.1","Row.2","Row.3")
col.name <- c("Col.1","Col.2","Col.3","Col.4")
mat.1 <- matrix(1:12, ncol=4, byrow=TRUE, dimnames=list(row.name, col.name))
mat.1

mat.1[2,3]									# (1) 2행 3열 원소 추출
mat.1[2,]									# (2) 2행 원소 추출
mat.1[,2:3]									# (3) 3,4열 원소 추출
mat.1[,-3]									# (4) 3열을 제외한 원소 추출

mat.1[c("Row.2","Row.3"),]					# (5) 행의 이름을 이용한 원소 추출

mat.1[,c(T,F,T,F)] 							# (6) 논리값을 이용한 열의 추출
mat.1[,as.logical(1:ncol(mat.1)%%2)] 		# (7) 논리식을 이용한 열의 추출

mat.2 <- mat.1
mat.2[,3] <- 5 								# (8) 열의 값을 바꿈
mat.2
mat.2[!mat.2%%3==0] <- 0 					# (9) 논리식을 이용해서 열의 값을 바꿈 
mat.2

cbind(mat.1,0) 								# (10) 행렬에 열 벡터를 추가함 (append)
rbind(mat.1[1:2, ], 0, mat.1[3, ]) 			# (11) 행렬에 열 벡터를 추가함 (insert)


###################################################
# 3. 배열의 조작
###################################################
row.name <- c("Row.1","Row.2")
col.name <- c("Col.1","Col.2","Col.3")
mat.name <- c("Mat.1","Mat.2")
ary.1 <- array(1:12, dim=c(2,3,2),  
  dimnames=list(row.name, col.name, mat.name))
ary.1

ary.1[,,2] 										# (1) index를 이용한 추출 
ary.1[,-3,]       								# (2) index를 이용한 삭제
ary.1[,,"Mat.1"]  								# (3) 이름을 이용한 추출
ary.1[,,c(T, F)]  								# (4) 논리 값을 이용한 추출
dim(ary.1)
ary.2 <- ary.1[as.logical(1:dim(ary.1)[1]%%2),,]  # (5) 논리 값을 이용한 추출
ary.2

array(ary.2, dim=c(1,3,2), 
  dimnames=list(row.name[-2], col.name, mat.name))# (6) 차원 구조의 변경

ary.1[,c(1,3),] <- NA 							# (7) 1,3열 값을 NA로 바꿈
ary.1
ary.1[is.na(ary.1)] <- 0 						# (8) NA를 0으로 바꿈
ary.1

(x <- array(1:(2^3), c(2,2,2)))					# (9) 3차원 배열 생성
(y <- aperm(x, perm=c(2,1,3)))  				# (10) 차원의 교환

mat.1 <- matrix(1:4, ncol=2)					# (11) 행렬 생성
t(mat.1) == aperm(mat.1, c(2, 1))				# (12) 전치행렬



###################################################
# 4. 리스트의 조작
###################################################

lst <- as.list(NA)  								# (1) 리스트의 초기화 생성
lst

lst[1] <- 1:3 										# (2) 리스트의 제1성분(벡터)
lst
lst[[1]] <- 1:3 									# (3) 리스트의 제1성분(벡터)
lst

lst[[3]] <- matrix(1:4, ncol=2)						# (4) 리스트의 제3성분(행렬)
lst

lst[[2]] <- "Second component" 						# (5) 리스트의 제2성분(문자열)
lst

names(lst) <- c("numeric", "character", "matrix") 	# (6) 성분이름 지정
lst
names(lst) <- c("numeric", "character", "") 		# (7) 성분이름 지우기
lst
names(lst)[[3]] <- "matrix" 						# (8) 성분이름 채우기

lst[[4]] <- iris[1:3,]								# (9) 데이터 프레임 추가

substr(lst[2],1,6)									# (10) 문자벡터 추출 ([)
substr(lst[[2]],1,6)								# (11) 문자벡터 추출 ([[)

sum(lst$numeric)									# (12) 수치벡터 추출(성분명)
sum(lst[1])											# (13) 수치벡터 추출([)
sum(lst[[1]])										# (14) 수치벡터 추출([[)

lst$matrix[1,]										# (15) 행렬의 1행
lst[[3]][,2]										# (16) 행렬의 2열
lst[[3]][2,2]										# (17) 행렬의 2행2열 

lst[[4]]$Sepal.Width								# (18) 데이터 프레임 변수
lst[[4]][1,]										# (19) 데이터 프레임 1행
lst[[4]][,c(1,5)]									# (20) 데이터 프레임 변수들

lst[[1]][3] <- 10									# (21) 리스트 성분값 수정
lst$numeric											# (22) 수정 결과



###################################################
# 5. 데이터 프레임의 조작
###################################################

(dfram <- iris[1:3,])	
dfram$Petal.Length			# (1) 변수 액세스 by $
dfram[["Petal.Length"]]		# (2) 변수 액세스 by [[name
dfram[1,]					# (3) 1행
dfram[2,2]					# (4) 2행 2번째 번수 
dfram[2,"Petal.Length"]		# (5) index와 이름으로

a <- letters[1:5]
b <- c(NA, 3, 7, 2, 4)
c <- c(100,200,150,120,NA)
d <- c(3, 5, 6, 8, 2)
(df.1 <- data.frame(a, b, c, d))

df.1[df.1$d==3,]					# (6) 변수 d가 3인 행 추출
df.1[df.1["d"]==3,]					# (7) 변수 d가 3인 행 추출
df.1[!is.na(df.1$c),]				# (8) 변수 c가 NA 아닌 행 추출
df.1[!is.na(df.1$c),c("a","d")]		# (8) 변수 c가 NA 아닌 행의 c,d 추출

df.1[df.1$c==200,]					# (9) 변수 c가 200인 행 추출
df.1$c==200							# (10) 조건식의 결과
subset(df.1, df.1["c"]==200)		# (11) subset 함수 이용

tmp <- df.1							# (12) 임시 데이터프레임
is.factor(df.1$a)					# (13) a 변수는 factor

df.1[1,] <- c("c", 5, 130, 7)		# (14) 1행 변경
is(df.1$d)							# (15) 문자로 강제 형변환

df.1 <- tmp							# (16) 원래 데이터 프레임으로

df.1[1,] <- list("c", 5, 130, 7) 	# (17) 1행 변경 (리스트 이용)
is(df.1$d)							# (18) 형변환 안됨
levels(df.1$a)						# (19) a 범주형자료의 레벨
df.1[1,] <- list("f", 5, 130, 7)	# (20) 오류가 발생
df.1								# (21) 변경된 df.1

df.1 <- tmp							# (22) 원래 데이터 프레임으로

# 범주형을 문자형으로 변경는 방법
df.1$a <- as.character(df.1$a)		# (23) 범주형에서 문자형으로
df.1[1,] <- list("f", 5, 130, 7)	# (24) 1행 변경

df.1 <- tmp							# (25) 원래 데이터 프레임으로

# 레벨을 추가하는 방법
levels(df.1$a) <- c(levels(df.1$a),"f") # (26) 레벨 추가
df.1[1,] <- list("f", 5, 130, 7)	# (27) 1행 변경
tmp <- df.1							# (28) 임시 데이터프레임

# 데이터 프레임 재 생성
a <- I(letters[1:5])				# (29) I 함수 사용
(df.1 <- data.frame(a, b, c, d))	# (30) 데이터 프레임
is(df.1$a)							# (31) 변수 a의 형
df.1[1,] <- list("f", 5, 130, 7)	# (32) 1행 변경

# 행을 추가하는 방법들
df.1[6,] <- list("z", 6, 230, 6)	# (33) 6행 추가
df.1 <- tmp							# (34) 원래 데이터 프레임으로
rbind(df.1, list("z", 6, 230, 6))	# (35) rbind 함수
df.1
df.1 <- tmp							# (36) 원래 데이터 프레임으로

# 변수를 추가하는 방법들
vec.e <- c(10, 20, 15, 10, 5)
df.1["e"] <- vec.e					# (37) 변수 e 추가 - 1
df.1
df.1 <- tmp							# (38) 원래 데이터 프레임으로

transform(df.1, e=vec.e)			# (39) transform - 2
transform(df.1, c=c/10+d)			# (40) transform 함수 사용 예
df.1 <- tmp							# (41) 원래 데이터 프레임으로

df.1 <- cbind(df.1, vec.e)			# (42) cbind 함수 - 3
names(df.1) <- c("a","b","c","d","e") # (43) 변수이름 조정
df.1
df.1 <- tmp							# (44) 원래 데이터 프레임으로

e <- data.frame(e=vec.e)			# (45) 데이터 프레임
cbind(df.1, e)						# (46) cbind 함수 - 4

data.frame(df.1, e)					# (47) data.frame 함수 - 5

# 제품정보 데이터 프레임
(products <- data.frame(
  product.id=I(c('A1', 'A2', 'B1', 'C1')),
  product.name=c('Prod AB','Prod A+','Prod B','Prod C1'),
  unit.price=c(120, 210, 150, 100)))

# 판매정보 데이터 프레임
(sales <- data.frame(
  product.id=I(c('B1', 'A2', 'C1', 'A1')),
  revenue=c(21200, 19100, 12000, 195000),
  profit=c(4500, 2300, 2000, 1300)))

merge(products, sales)  					# (1) 단순 병합
intersect(names(products), names(sales))	# (2) 공통 이름
merge(sales, products)  					# (3) 병합 순서 변경
names(sales) <- c("id","revenue","profit")	# (4) 변수 이름 변경
merge(products, sales)  					# (5) 카테시안 곱 발생
merge(products, sales, 
      by.x="product.id", by.y="id") 		# (6) Key 이름 지정

# 새로운 데이터 프레임 생성
(products.1 <- products[products$product.id!="B1",])
sales.1 <- sales[sales$id!="A2",]
names(sales.1) <- c("product.id","revenue","profit")
sales.1

merge(products.1, sales.1)  				# (7) 교집합
merge(products.1, sales.1, all=T)  			# (8) 합집합
merge(products.1, sales.1, all.x=T)  		# (9) x기준 Outer Join
merge(products.1, sales.1, all.y=T)  		# (10) y 기준 Outer Join


###################################################
# B. 데이터 객체의 전환
###################################################

###################################################
# 1. 행렬의 전환
###################################################

(mat <- matrix(1:8, ncol=4))				# (1) 행렬의 생성
dim(mat)									# (2) 행렬의 차원
tmp <- mat								
dim(mat) <- c(2, 2, 2)						# (3) 차원의 확장
mat											# (4) 2 by 2 by 2 배열

mat <- tmp
ary.a <- mat %o% rep(1,2)  					# (5) 행렬에 길이 2의 제3 차원을 부가
dim(ary.a)									# (6) 3차원 
# ary.a[i,j,k] (k=1,2) 의 값은 mat[i,j]
ary.a[,,1]             						# (7) mat[i,j]
ary.a[,,2]									# (8) mat[i,j]

set.seed(1)									# (9) random seed
(mat <- matrix(sample(12), ncol=4))			# (10) 수치 행렬
data.frame(mat)								# (11) 데이터 프레임
(df.1 <- data.frame(mat,
   row.names=c("no.1","no.2","no.3")))		# (12) 행의 이름 지정
names(df.1) <- c('a','b','c','d')			# (13) 변수의 이름 지정
df.1

mat <- matrix(1:6, c(2,3), 					# (14) 행렬 생성 
  dimnames=list(c("R1","R2"),c("C1","C2","C3")))
mat											# (15) 행이름 열이름 있음
data.frame(mat)								# (16) 이름을 계승함

mat <- tmp									# (17) (1)의 행렬로
as.vector(mat)								# (18) 벡터로 전환
dim(mat) <- c(8)							# (19) 차원 축소(1차원)
mat											# (20) 벡터임

###################################################
# 2. 배열의 전환
###################################################

(ary.1 <- array(1:8, c(2,2,2)))				# (1) 배열 생성
tmp <- ary.1
is(ary.1)									# (2) 배열 
is.matrix(ary.1)							# (3) 행렬이 아님
dim(ary.1) <- c(2, 4)						# (4) 차원의 축소(2차원)
ary.1										# (5) 행렬
is(ary.1)									# (6) 행렬임
is.array(ary.1)								# (7) 배열임	
ary.1 <- tmp								# (8) 원래 배열로
matrix(ary.1, ncol=4)						# (9) matrix 함수 이용

as.vector(ary.1)							# (10) 벡터로 전환
dim(ary.1) <- c(8)							# (11) 차원 축소(1차원)
ary.1										# (12) 벡터임

a <- array(1:24, dim=c(2,3,4))				# (13) 배열 생성
dimnames(a)[[3]] <- paste("n", 1:4, sep="")	# (14) 3차원의 이름 지정
a
# (15) 행 기준으로 3차원의 이름 추출
dim3 <- rep(dimnames(a)[[3]], each=dim(a)[1]) 
dim3
dim2 <- apply(a, 2, c)						# (16) 열차원으로 데이터 묶음
dimnames(dim2) <- list(NULL, c("C.1", "C.2", "C.3")) # (17) 열 이름 지정						   
dim2										# (18) 행렬
data.frame(dim3, dim2)						# (19) 데이터 프레임

###################################################
# 3. 리스트의 전환
###################################################

(lst <- list(1,2,3,4))						# (1) 리스트 객체
dim(lst) <- c(2,2)							# (2) 2차원 (행렬?)
lst
is.matrix(lst); is.list(lst)				# (3) 행렬/리스트 검증
lst[1,2] 									# (4) 1행 2열
is.list(lst[1,2])							# (5) 결과는 리스트
lst[[1,2]] 									# (6) 리스트 아님
lst[,2] 									# (7) 2열, 리스트
lst[[,2]]									# (8) 에러 발생

mat <- matrix(lst, ncol=2) 					# (9) matrix 함수 이용
mat[,2]										# (10) 역시나 리스트
is.matrix(mat); is.list(mat)				# (11) 행렬/리스트 검증
mat <- matrix(unlist(lst), ncol=2) 			# (12) unlist 함수 활용
mat[,2]										# (13) 리스트 아님
is.matrix(mat); is.list(mat)				# (14) 행렬/리스트 검증

set.seed(1)
lst <- list(a=1:3, b=rnorm(3))				# (15) 리스트 객체 생성
do.call("rbind", lst) 						# (16) 2행 3열 행렬
do.call("cbind", lst)						# (17) 3행 2열 행렬

t(sapply(lst, "+"))							# (18) 2행 3열 행렬
sapply(lst, "+")							# (19) 3행 2열 행렬


###################################################
# 4. 데이터 프레임의 전환
###################################################

a <- c('1st', '2nd', '3rd', '4th')
b <- c(1, 3, 2, 4)
c <- c(T, F, T, T)
(df.1 <- data.frame(a, b, c))				# (1) 데이터 프레임

as.matrix(df.1)								# (2) 행렬로 전환
as.list(df.1)								# (3) 리스트로 전환
array(df.1, dim=c(2,2,3))					# (4) 배열로 전환-1
array(as.character(df.1), dim=c(2,2,3))		# (5) 배열로 전환-2
array(as.matrix(df.1), dim=c(2,2,3))		# (6) 배열로 전환-3	

