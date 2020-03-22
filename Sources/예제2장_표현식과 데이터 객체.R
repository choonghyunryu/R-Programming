#########################
### 1. 기본형
#########################
mode(3 + 4)         		# (1) 수치의 연산 식 
mode(pi)                	# (2) 파이를 나타내는 내장 상수 
mode(3 < 4)              	# (3) 논리 연산 식 
mode(T)                  	# (4) 논리 참을 의미하는 내장 상수 
mode(FALSE)               	# (5) 논리 거짓을 의미하는 내장 상수 
mode(True)                 	# (6) 논리 참의 상수가 아님 
mode(f)       				# (7) 논리 거짓의 상수가 아님 
mode("Hi, sangmin")        	# (8) 큰따옴표로 만든 문자열 
mode('Hi, sangwon')       	# (9) 작은따옴표로 만든 문자열 
mode(1+4i)              	# (10) 복소수 
storage.mode(pi)           	# (11) 파이가 저장되는 형태 
storage.mode(1:5)          	# (12) 정수가 저장되는 형태 

as.raw(65)        			# (1) ASCII 코드 65에 해당하는 raw 생성 
rawToChar(41)        		# (2) 오류 : 함수의 인수가 numeric임 
rawToChar(as.raw(65))  		# (3) ASCII 코드 65에 해당하는 raw 문자출력
charToRaw("Aa")      		# (4) 문자 A, a에 해당하는 raw (16진수) 
as.numeric(charToRaw("Aa")) # (5) 문자 A,a에 ASCII (10진수) 

## 형 변환
char.x <- "123"         			# (1) 문자형 객체 생성 
mode(char.x) 
mode(char.x) <- "numeric"  			# (2) 수치형으로 형 변환 
char.x 
mode(char.x) 
double.x <- pi 
storage.mode(double.x) 
storage.mode(double.x) <- "integer"	# (3) double을 integer로
double.x 
raw.x <- as.raw(41) 
mode(raw.x) <- "character"   		# (4) raw를 문자형으로 변환 
raw.x 
mode(raw.x) <- "numeric"    		# (5) raw를 수치형으로 변환 
raw.x 
   
   
char.x <- "123" 
mode(char.x) 
char.x <- as.numeric(char.x) 		# (1) 수치형으로 변환 
char.x 
mode(char.x)  
pi 
as.integer(pi)             			# (2) 정수로 변환 
as.double(TRUE)             		# (3) 논리형이 수치형으로 변환 
as.logical(0)              			# (4) 수치형이 논리형으로 변환 


#########################
### 2. 벡터
#########################
   
c(3.14, 5+2i, 10)    # 1. 수치형, 복소수형 = 복소수형  
c(365, 12, "ex")     # 2. 수치형, 문자형 = 문자형 
c(123, 8710, T, F)   # 3. 수치형, 논리형 = 수치형 
c(T, F, 3-1i)        # 4. 논리형, 복소수형 = 복소수형 
c(T, F, "abc")       # 5. 논리형, 문자형 = 문자형 
c(2+5i, "hello", 4i) # 6. 복소형, 문자형 = 문자형          
c(as.raw(12), 1, 2)  # 7. raw, 수치형 = 수치형          
c(as.raw(12), T, F)  # 8. raw, 논리형 = 논리형        
c(as.raw(12), "one") # 9. raw, 문자형 = 문자형     
c(as.raw(12), 3+i)   # 10. raw, 복소수형 = 에러발생
c(as.raw(12), 3i)    # 11. raw, 복소수형 = 복소수형    
c(3.14, 5)           # 12. 실수(double), 정수(integer) = 실수(double)

2:6 
2:-2 
1:3
"a":"z"				# 문자형의 경우 에러 발생
1:3+3i				# 연산의 우선순위로 1:3 이후에 +3i를 계산
1:(3+3i)			# 3+3i에서 3i가 0이 되어 1:3을 계산
1+2i:(3+3i) 		# 2i가 0이 되어 0:3이 되고 앞의 1+를 연산
(1+2i):(3+3i)		# 허수부가 모두 0이 되어 1:3을 계산
2i:(3+3i)			# 허수부가 모두 0이 되어 0:3을 계산 

T:3
-3:F
F:T

1:5+4 
1:5*4 
-1:5/4 
-2:5*4-1 
3+5i:7+6i			# Warning 발생 (3 + 0:7 + 6와 동일)
(3+5i):(7+6i)		# Warning 발생  (3:4와 동일)
3:10+6i 			# Warning이 발생하지 않았다. 

seq(-pi, pi, 0.8) 
seq(1, 3, .5) 
seq(0,20, length=5)
seq(3,1)			# seq(from=3, to=1)
seq(5)				# seq(to=5)
seq(pi)				# seq(to=pi)
x <- c(1,3,5,7,9)
seq(along=x)		# 1:length(x)

rep(T, 7) 
rep(c(1, 2, 3), 2) 
rep(3:5, 2) 
rep(seq(1, 2, .5), 2) 
rep(seq(1, 2, .5), each=2) 
# 두개 문자를 각각 2번 반복하여 길이 4로
rep(c('A','a'), each = 2, len = 4)
# 각각 두번 반복하여 7문자로, Recycle Rule
rep(c('A','a'), each = 2, len = 7)
# 길이 12, 각각 2개씩 3번 반복
rep(c('A','a'), each = 2, times = 3)  

scan() 
scan("d:\\ex.txt")
scan("d:\ex.txt")
scan("d:/ex.txt")


#########################
### 3. 행렬
#########################

matrix(1:12, ncol=4, byrow=T)       	# (1) 3 by 4 수치행렬
mat <- matrix(LETTERS[1:12], nrow=4, dimnames=list(
  c("Row.1", "Row.2", "Row.3", "Row.4"),
  c("Col.1", "Col.2", "Col.3")))		# (2) 4 by 3 문자행렬 
mat
attributes(mat)							# (3) 행렬 mat의 속성
dim(mat)								# (4) 행렬 mat의 차원
dimnames(mat)							# (5) 행렬 mat의 행/열 이름
length(mat)								# (6) 행렬 mat의 길이
mode(mat)                           	# (7) 행렬 mat의 기본형
matrix(1:3)								# (8) nrow, ncol 미지정

# rbind 함수
rbind(1:6,rep(c(1,2),3))				# (1) rbind 함수
rbind(1:4, 4:1, 1:3)					# (2) Recycle Rule 미적용
rbind(1:4, 4:1, 1:2)					# (3) Recycle Rule 적용

# cbind 함수
cbind("COL.1"=1:3, "COL.2"=rep(2,3), "COL.3"=c(4,5,6))

# dim 함수
mat <- 1:6								# (1) 길이 6인 벡터
dim(mat) <- c(2,3)						# (2) 벡터를 2행 3열 행렬로 변경
mat 									# (3) 2행 3열 행렬
dim(mat) <- NULL						# (4) 차원을 없애서 다시 벡터로 변경
mat										# (5) 길이 6인 벡터

# array 함수
mat <- array(1:12, dim=c(3,4))			# (1) array 함수를 이용한 3×4 행렬
mat
is.matrix(mat)							# (2) 행렬인지 아닌지 검증
array(1:2, dim=c(3,4))					# (3) Recycling Rule


#########################
### 4. 배열
#########################
ary <- array(1:24, dim=c(2,3,4), 
  dimnames = list(c("ROW.1","ROW.2"),c("COL.1","COL.2","COL.3"),
   c("REP.1","REP.2","REP.3","REP.4")))	# (1) 3차원 배열
ary
mode(ary)								# (2) 기본형
length(ary)								# (3) 길이
dim(ary)								# (4) 차원
dimnames(ary)							# (5) 차원의 이름


#########################
### 5. 리스트
#########################
lst <- list(5:10, c("A","B","C"), 
  matrix(c(T,T,F,T), ncol=2))			# (1) 성분명 없는 리스트
lst
mode(lst)								# (2) 리스트의 형
length(lst)								# (3) 리스트의 성분 개수
names(lst)								# (4) 리스트의 성분 이름 
my.family <- list(city="SEOUL",
   parent=list(father="RYU", father.age=40, 
       mother="JUN", mother.age=37),
   child=list(son.cnt=2, son.name=c("sangmin","sangwon"),
   son.age=c(10,10))) 					# (5) 성분명 있는 리스트
my.family
mode(my.family)							# (6) 리스트의 형
length(my.family)						# (7) 리스트의 성분 개수
names(my.family)						# (8) 리스트의 성분 이름


#########################
### 6. 범주형 자료
#########################

answer.vec <- c("yes","no","yes")		# (1) 문자 벡터
answer.fct <- factor(answer.vec)   		# (2) factor 생성 (기본)
answer.fct
length(answer.fct)						# (3) factor 길이
mode(answer.fct)						# (4) factor 형
levels(answer.fct)						# (5) factor의 수준
names(answer.fct)						# (6) 각 원소의 이름
person.vec <- c("M","F","M","F","m") 	# (7) 문자 벡터	
names(person.vec) <- c("1st","2nd","3rd","4th","5th") # (8) 벡터 원소에 이름 부여
person.fct <- factor(person.vec, levels=c("M","F"), 
   labels=c("Male","Female"))			# (9) factor 생성
person.fct
length(person.fct)						# (10) factor 길이
mode(person.fct)						# (11) factor 형
levels(person.fct)              		# (12) factor의 수준
names(person.fct)						# (13) 각 원소의 이름

volt <- c("high","low","high","middle","middle")
voltage <- ordered(volt, 
  levels=c("low","middle","high"))		# (1) ordered factor
voltage
length(voltage)							# (2) factor 길이
mode(voltage)							# (3) factor 형
levels(voltage)              			# (4) factor의 수준
names(voltage)							# (13) 각 원소의 이름


#########################
### 7. 데이터 프레임
#########################

height <- c(168,182,175,177,172)		
weight <- c(62,69,65,69,58)
sex <- c("female","male","male","male","female")
married <- c(T,F,F,T,T)
# (1)데이터 프레임 생성
my.df <- data.frame(height,weight,sex,married,	
	row.names=c("kim","park","choi","lee","han"))
my.df	

length(my.df)							# (2) 변수의 개수			
mode(my.df)								# (3) 데이터프레임 형
names(my.df)							# (4) 변수 명
row.names(my.df)						# (5) 행 이름
class(my.df)							# (6) class
dim(my.df)								# (7) 차원

my.df$weight							# (8) weight 변수
my.df$sex								# (9) sex 변수
is.numeric(my.df$weight)				# (10) 수치 변수
is.factor(my.df$sex)					# (11) 범주형 자료

# (12)데이터 프레임 생성 : stringsAsFactors=F
my.df1 <- data.frame(height,weight,sex,married,	
    stringsAsFactors=F,
	row.names=c("kim","park","choi","lee","han"))
my.df1$sex								# (13) sex 변수
is.character(my.df1$sex)				# (14) 범주형 자료
	

#########################
### 8. 시계열
#########################

set.seed(1)
(x <- round(runif(12,1,100)))				# (1) 수치 벡터
(my.ts <- ts(x, start=2007, frequency=4))	# (2) 분기
ts(x, start=2010, frequency=12)				# (3) 월
ts(x, start=2000, frequency=1)				# (4) 년
ts(x, end=2010, frequency=1)				# (5) end 인수
print(ts(x, end=2010, frequency=1), calendar=T) # (6) ts 출력

length(my.ts)								# (7) 데이터 개수
mode(my.ts)									# (8) mode
tsp(my.ts)									# (9) tsp
start(my.ts);end(my.ts);frequency(my.ts)	# (10) 개별 함수	

(mat <- matrix(1:24, ncol=2))				# (11) 수치 행렬
(my.mts <- ts(mat, start=2007, frequency=4))# (12) mts 
dim(my.mts)									# (13) 차원
length(my.mts)								# (14) 데이터 개수
dimnames(my.mts)							# (15) 차원이름
dimnames(my.mts) <- list(NULL, c("Revenue","Profit")) # (16) 차원이름 변경
my.mts										# (17) mts
is.ts(my.mts); is.mts(my.mts)				# (18) 객체 검증

   
   
   
   
   
   
	
	
	
