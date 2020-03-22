###################################################
# A. 데이터의 입력
###################################################

###################################################
# 1. 콘솔에서의 데이터 입력하기
###################################################
scan()                 								# (1)데이터의 입력을 종료 시 엔터(Enter)키 입력
prime.num <- scan(n=5)  							# (2) 인수의 개수 지정 - 3,5,7,11,13 입력
prime.num
scan(what="")          								# (3) 입력할 데이터 유형 지정 (character)
scan(what="", sep="|") 								# (4) 입력 데이터의 분리자 지정
scan(what=list(name="", age=0)) 					# (5) 리스트 객체의 입력
mat <- matrix(scan(), ncol=3, nrow=2, byrow=T) 		# (6) 행렬 객체의 입력 
mat
df <- data.frame(x=scan(what=""), y=scan(what=0))	# (7) 데이터프레임 객체 입력
df


###################################################
# 2. 파일에서의 데이터 입력하기
###################################################
## scan 함수의 이용
var.name <- scan(file='c:/R_data/input_scan.txt', 
  nline=1, what="") 									# (1) 파일의 헤더 읽기
var.name
week.list=scan(file='c:/R_data/input_scan.txt', 
  skip=1, what=list(week="", score=0, flag=logical())) 	# (2) 데이터부 읽기
week.list												# (3) 리스트 객체임
week.data <- data.frame(cbind(week.list[[1]],
week.list[[2]],week.list[[3]]))							# (4) 데이터프레임으로 변환
week.data
week.data$X1											# (5) 모든 변수의 값이 범주형 값으로 생성됨
week.data$X2 <- as.numeric(week.data$X2) 				# (6) 수치형으로 변경
week.data$X3 <- as.numeric(week.data$X3) 				# (7) 논리형으로 변경
names(week.data)										# (8) 데이터 프레임의 변수명
names(week.data) <- var.name 							# (9) 변수명을 변경
week.data

## read.table 함수의 이용
read.table(file='c:/R_data/input_table.txt') 			# (1) 기본적인 사용법
read.table(file='c:/R_data/input_table.txt', header=T) 	# (2) 헤더의 사용
read.table(file='c:/R_data/input_table.txt', header=T,
  row.names=1)											# (3) 행 이름 선택
tab=read.table(file='c:/R_data/input_table.txt', header=T,
  row.names=paste(1:7, 'th' ,sep=''))                 	# (4) 행 이름 지정
tab
is.factor(tab$week)										# (5) week 변수 데이터 유형				
is.numeric(tab$score)									# (6) score 변수 데이터 유형
is.logical(tab$flag)									# (7) flag 변수 데이터 유형
read.table(file='c:/R_data/input_table.txt', header=T,
  colClasses=c('character','numeric','character'))		# (8) 변수의 데이터 유형 지정
tab=read.table(file='c:/R_data/input_table.txt', header=T,
  stringsAsFactors=F) 									# (9) 문자열의 범주형 전환 방지
tab
is.character(tab$week)									# (10) week 변수 데이터 유형

## read.fwf 함수의 이용
read.fwf(file='c:/R_data/input_fwf.txt', header=T,
  widths=c(3,2,1)) 										# (1) 기본적인 사용법
read.fwf(file='c:/R_data/input_fwf.txt', skip=2,
  widths=c(3,2,1),col.names=c('week','score','flag')) 	# (2) 2행 무시
read.fwf(file='c:/R_data/input_fwf.txt', header=T,
  widths=c(3,2,1),col.names=c('week','score','flag')) 	# (3) 열의 이름 지정


## RODBC를 이용하여 엑셀 읽기
install.packages('RODBC')								# (1) RODBC 패키지의 설치
library(RODBC)											# (2) RODBC 패키지의 이용
channel <- odbcConnectExcel('c:/R_data/input_excel.xls')# (3) 엑셀파일 연결하기
sqlTables(channel)										# (4) 워크시트 조회하기
tab.name <- sqlTables(channel)[1,"TABLE_NAME"]			# (5) 첫째 워크시트 이름
tab.name
tab <- sqlFetch(channel, tab.name)						# (6) 첫째 워크시트 읽기
tab
close(channel)											# (7) ODBC 닫기(엑셀파일 닫기)
is(tab$week)											# (8) week의 데이터 유형
is(tab$score)											# (9) score의 데이터 유형
is(tab$flag)											# (10) flag의 데이터 유형

###################################################
# 3. DBMS에서의 데이터 입력하기
###################################################
## RODBC를 이용하여 엑셀 읽기
library(RODBC)											# (1) RODBC 패키지의 이용
channel <- odbcConnect(dsn='myodbc', 
  uid='bdboy', pwd='******', case='mysql')				# (2) DBMS 연결
usa <- sqlFetch(channel, 'USArrests')					# (3) 테이블 읽기
dim(usa)												# (4) 데이터프레임 차원
head(usa)					        					# (5) 앞부분 읽기
sql <- ''												# (6) sql문 생성
sql <- paste(sql, 'select *')							# (6) sql문 생성
sql <- paste(sql, 'from USArrests')  					# (6) sql문 생성	
sql <- paste(sql, 'where murder >= 15')					# (6) sql문 생성
sql <- paste(sql, 'order by murder desc')				# (6) sql문 생성
sql														# (7) 생성된 sql
usa.sql <- sqlQuery(channel, sql)						# (8) sql 실행
usa.sql
close(channel)											# (9) ODBC 닫기


###################################################
# 1. 콘솔에서의 데이터 입력하기
###################################################
scan()                 									# (1)데이터의 입력을 종료 시 엔터(Enter)키 입력
prime.num <- scan(n=5)  								# (2) 인수의 개수 지정 - 3,5,7,11,13 입력
prime.num
scan(what="")          									# (3) 입력할 데이터 유형 지정 (character)
scan(what="", sep="|") 									# (4) 입력 데이터의 분리자 지정
scan(what=list(name="", age=0)) 						# (5) 리스트 객체의 입력
mat <- matrix(scan(), ncol=3, nrow=2, byrow=T) 			# (6) 행렬 객체의 입력 
mat
df <- data.frame(x=scan(what=""), y=scan(what=0))		# (7) 데이터프레임 객체 입력
df


###################################################
# B. 데이터의 출력
###################################################

###################################################
# 1. 콘솔로 데이터 출력
###################################################
vec.num <- 1:5											# (1) 수치벡터 생성
vec.logical <- c(T,F,T,T)								# (2) 논리벡터 생성
mat <- matrix(1:4, ncol=2, byrow=T)						# (3) 행렬 생성
fct <- factor(c('Low','Mid','High'), ordered=T,
   levels=c('Low','Mid','High'))						# (4) 범주형자료 생성
lst <- list(x=1:5, y=pi)								# (5) 리스트 생성
df <- USArrests[1:3,]									# (6) 데이터프레임 생성

cat()                 									# (7) 아무런 변화 없음
cat(vec.num)                							# (8) 벡터 출력
cat(vec.num, '\n')										# (9) 개행문자 포함
cat(vec.num, vec.logical)								# (10) 두 벡터의 출력
cat(vec.num, '\n', vec.logical, '\n')					# (11) 개행문자 포함
cat('Five integers : ', vec.num, '\n\t', 'Done!!!\n')	# (12) 데코레이션 출력
cat(vec.num, sep="->", "\n") 							# (13) sep 인수 사용
options("width")										# (14) 콘솔의 width 값
cat(vec.num, sep="->", fill=T) 							# (15) fill 인수 사용 1
cat(1:15, sep="->", fill=15) 							# (16) fill 인수 사용 2
mat														# (17) 행렬
cat(mat, "\n")											# (18) 행렬의 출력
fct														# (19) 범주형자료
cat(fct, "\n")											# (20) 범주형 출력1
cat(as.character(fct), "\n")							# (21) 범주형 출력2
lst														# (22) 리스트
cat(lst, "\n")											# (23) 리스트 출력1
cat('factor x is ', lst$x, 
    '\nfactor y is ', lst$y, "\n")						# (24) 리스트 출력2
df														# (25) 데이터프레임
cat(df, "\n")											# (26) 데이터프레임 출력1
cat("\tMurder   =>", paste(row.names(df), formatC(df[,1], width=4), 
           sep=":", collapse="\t"),"\n",
    "\tAssault  =>", paste(row.names(df), formatC(df[,2], width=4), 
           sep=":", collapse="\t"),"\n",
    "\tUrbanPop =>", paste(row.names(df), formatC(df[,3], width=4), 
           sep=":", collapse="\t"),"\n",
    "\tRape     =>", paste(row.names(df), formatC(df[,4], width=4), 
           sep=":", collapse="\t"),"\n")				# (27) 데이터프레임 출력2


print(vec.num)      									# (1) 수치벡터 출력
print(mat)												# (2) 행렬 출력
pi														# (3) Phi
getOption("digits")										# (4) 기본 자리수
print(pi, digits = 16)									# (5) Phi 출력
LETTERS[1:10]											# (6) 문자벡터
print(LETTERS[1:10], quote = FALSE)						# (7) 따옴표 제거
vec.na <- c(1,3,NA,7,9)									# (8) NA 포함 벡터
print(vec.na)											# (9) 벡터 출력
print(vec.na, print.gap=3)								# (10) 열 간 공백 지정
print(vec.na, na.print="결측치")						# (11) 결측치 문자 지정
print(vec.na, max = 2)									# (12) 최대출력 개수 지정
x=matrix(c("1","100","2","10000"))						# (13) 문자 행렬 
print(x)												# (14) 문자행렬 출력
print(x, right=T)										# (15) 우측 정렬 해제



###################################################
# 2. 파일로 데이터 출력
###################################################
# 벡터의 파일 출력
cat(vec.num, file='c:/R_data/output_vector.txt')		# (1) 벡터의 파일로 출력
cat(vec.num, '\n', append=T,
	file='c:/R_data/output_vector.txt')					# (2) 개행문자 포함
cat(vec.num, vec.logical, append=T,
	file='c:/R_data/output_vector.txt')					# (3) 두 벡터의 출력
cat(vec.num, '\n', vec.logical, '\n', append=T,
	file='c:/R_data/output_vector.txt')					# (4) 개행문자 포함
cat('Five integers : ', vec.num, '\n\t', 'Done!!!\n', 
	append=TRUE, file='c:/R_data/output_vector.txt')	# (5) 데코레이션 출력
cat(vec.num, sep="->", "\n", append=T,
	file='c:/R_data/output_vector.txt')					# (6) sep 인수 사용
cat(vec.num, sep="->", fill=T, append=T,
	file='c:/R_data/output_vector.txt')					# (7) fill 인수 사용 1

# 행렬의 파일 출력
dim(mat) <- c(2,2)
cat(mat, file='c:/R_data/output_matrix.txt')			# (8) 행렬의 출력

# 범주형의 파일 출력
cat(fct, file='c:/R_data/output_factor.txt')			# (9) 범주형 출력1
cat("\n", as.character(fct), append=T,
	file='c:/R_data/output_factor.txt')					# (10) 범주형 출력2

# 리스트의 파일 출력
cat(lst, file='c:/R_data/output_list.txt')				# (11) 리스트 출력1

cat('\nfactor x is ', lst$x, 
	'\nfactor y is ', lst$y, "\n", append=T,
	file='c:/R_data/output_list.txt')					# (12) 리스트 출력2

# 데이터프레임의 파일 출력
cat(df, "\n", file='c:/R_data/output_df.txt')			# (13) 데이터프레임 출력1
cat("\tMurder   =>", paste(row.names(df), formatC(df[,1], width=4), 
           sep=":", collapse="\t"),"\n",
    "\tAssault  =>", paste(row.names(df), formatC(df[,2], width=4), 
           sep=":", collapse="\t"),"\n",
    "\tUrbanPop =>", paste(row.names(df), formatC(df[,3], width=4), 
           sep=":", collapse="\t"),"\n",
    "\tRape     =>", paste(row.names(df), formatC(df[,4], width=4), 
           sep=":", collapse="\t"),"\n",
	file='c:/R_data/output_df.txt')						# (14) 데이터프레임 출력2

##########################################
# write 함수를 이용한 파일로의 출력
##########################################
x <- matrix(1:12,ncol=4, byrow=T)						# (1) 3행 4열 행렬 생성		
x
write('--- x ---')										# (2) write 함수
write(x, append=T)										# (3) append 인수 사용
write('\n--- t(x) ---', append=T)
t(x)
write(t(x), append=T)									# (4) 행렬 x의 전치
getwd()													# (5) 작업경로 알아보기
write													# (6) write 함수원형
is.character(x)											# (7) x가 문자형인가
write('\n--- t(x), ncolumns=3 ---', append=T, 			# (8) file 인수 사용
  file=paste(getwd(), "data", sep='/'))
write(t(x), append=T, ncolumns=3)						# (9) x의 전치, ncolumns 인수 사용
write('\n--- x, ncolumns=3 ---', append=T)
write(x, append=T, ncolumns=3)							# (10) x, ncolumns 인수 사용

##########################################
# write.table 함수를 이용한 파일로의 출력
##########################################
USArrests[1:5,]											# (1) 데이터프레임
write.table(USArrests[1:5,])							# (2) 데이터프레임 콘솔 출력
write.table(USArrests[1:5,], 
	file='c:/R_data/USArrests.txt')						# (3) 데이터프레임 파일 출력
write.table(USArrests[1:5,], row.names=F) 				# (4) 행 이름 생략
write.table(USArrests[1:5,], col.names=F) 				# (5) 열 이름 생략
write.table(USArrests[1:5,], quote=F) 					# (6) 따옴표 생략
write.table(USArrests[6:10,], append=T, col.names=F,
	file='c:/R_data/USArrests.txt')						# (7) 파일에 추가 출력

##########################################
# write.csv 함수를 이용한 CSV파일로의 출력
##########################################
write.csv(USArrests[1:5,])								# (1) 데이터프레임 콘솔 출력
write.csv(USArrests[1:5,], 
	file='c:/R_data/USArrests.csv')						# (2) CSV 파일 출력
write.csv(USArrests[1:5,], row.names=F) 				# (3) 행 이름 생략
write.csv(USArrests[1:5,], col.names=F) 				# (4) 열 이름 생략
write.csv(USArrests[1:5,], quote=F) 					# (5) 따옴표 생략
write.csv(USArrests[6:10,], append=T, col.names=F,
	file='c:/R_data/USArrests.csv')						# (6) 파일에 추가 출력


##########################################
# sink 함수를 이용한 파일로의 출력
##########################################
sink('c:/R_data/output_sink.txt')						# (1) sink 함수 on
print(vec.num)      										
pi				
sink()													# (2) sink 함수 off

sink('c:/R_data/output_sink.txt', append=T)				# (3) 파일에 추가
cat('\n------- append=T----------\n')
print(vec.num)      									
pi							
sink()													# (4) sink 함수 off

sink('c:/R_data/output_sink.txt', append=T, 
	type='output', split=T)								# (5) 콘솔과 파일에 추가
cat('\n------ type=output--------\n')
print(vec.num)      										
pi	
log("a")												# (6) 에러 발생						
sink()													# (7) sink 함수 off

zz <- file("c:/R_data/output_message.txt", open="wt")	# (8) 쓰기 파일 생성
sink(zz)												# (9) sink 함수 on
sink(zz, type="message")								# (10) sink 함수(message) on
print(vec.num)      										
pi
log("a")												# (11) 에러 발생	
sink(type="message")									# (12) sink 함수(message) off
log("a")												# (13) 에러 발생	
pi
sink()													# (14) sink 함수 off


##########################################
# 엑셀 파일로의 출력
##########################################
# 폴더가 읽기 전용이면 안된다.
library(RODBC)											# (1) RODBC 패키지의 이용
channel=odbcConnectExcel('c:/R_data/output_excel.xls', 
	readOnly=FALSE)										# (2) 엑셀파일 생성하기
dat=USArrests[1:5,]										# (3) 데이터 생성
sqlSave(channel, dat, tablename="USArrests_1")			# (4) 엑셀파일 저장
sqlSave(channel, dat, tablename="USArrests_2",
	rownames=F, colnames=F)								# (5) 엑셀파일 저장
close(channel)											# (6) 엑셀파일 닫기
channel=odbcConnectExcel('c:/R_data/output_excel.xls', 
	readOnly=FALSE)										# (7) 엑셀파일 생성하기
sqlSave(channel, dat, tablename="USArrests_1", 
	verbose=T)											# (8) 엑셀파일 저장
sqlSave(channel, USArrests[6:7,], tablename="USArrests_2", 
	append=T, rownames=F, colnames=F)					# (9) 엑셀파일 저장
close(channel)											# (10) 엑셀파일 닫기
channel <- odbcConnectExcel('c:/R_data/output_excel.xls', 
	readOnly=FALSE)										# (11) 엑셀파일 생성하기	
sqlUpdate(channel, USArrests[1:5,], verbose=T,
	tablename="USArrests_1")							# (12) 엑셀파일 수정
dat <- dat[5,]
dat[1,4] <- 100
dat														# (13) 수정된 데이터프레임
sqlUpdate(channel, dat, tablename="USArrests_1")		# (14) 엑셀파일 수정
close(channel)											# (15) 엑셀파일 닫기


##########################################
# dput/dget 함수를 이용한 백업 및 복원
##########################################
dput(sd)												# (1) dput-기본
dput(sd, "c:/R_data/sd.R")								# (2) dput-백업
my.sd <- dget("c:/R_data/sd.R")							# (3) dget-복원	
my.sd													# (4) 함수 내용	
my.sd(1:10)												# (5) 함수 실행
my.sd(1:10)==sd(1:10)									# (6) 결과 비교
set.seed(1)						
dput(rnorm(10), "c:/R_data/rnorm.R")					# (7) dput-백업
dget("c:/R_data/rnorm.R")								# (8) dget-복원	

##########################################
# dump/source 함수를 이용한 백업 및 복원
##########################################
x <- 1:10												# (1) 벡터
y <- matrix(1:12, ncol=4, byrow=T)						# (2) 행렬	
z <- USArrests[1:5,]									# (3) 데이터프레임
dump(ls(pattern = '^[xyz]'), "c:/R_data/xyz.R")			# (4) dump로 백업하기
rm(list=ls(pattern = '^[xyz]'))							# (5) 객체의 삭제
ls(pattern = '^[xyz]')									# (6) 객체 조회
source("c:/R_data/xyz.R")								# (7) source로 불러오기
ls(pattern = '^[xyz]')									# (8) 객체 조회
x
y
z
source("c:/R_data/xyz.R", echo=T)						# (9) 반향처리
source("c:/R_data/xyz.R", echo=T, continue.echo='')		# (10) 연결문자 지정


##########################################
# save/load 함수를 이용한 백업 및 복원
##########################################
rm(list=ls())											# (1) 전 객체 삭제
ls()													# (2) 객체 조회
x <- 1:10												# (3) 벡터
y <- matrix(1:12, ncol=4, byrow=T)						# (4) 행렬	
z <- USArrests[1:5,]									# (5) 데이터프레임
save(x,y, file="c:/R_data/xy.Rdata")					# (6) 객체 저장
save.image()											# (7) 모든 객체 저장
rm(list=ls())											# (8) 전 객체 삭제
load("c:/R_data/xy.Rdata")								# (9) 백업 불러오기
ls()													# (10) 객체 조회
load(".RData")											# (11) 전 백업 불러오기
ls()													# (12) 객체 조회
save(x,y, file="c:/R_data/xy.txt", ascii=T)				# (13) 객체 저장-텍스트



ls(pat='iris')											# (1) iris 객체명 조회
# (2) datasets 패키지의 iris 객체의 행이름 변경
rownames(iris)=paste(rownames(iris), "th", sep="")      					
ls(pat='iris')											# (3) iris 객체명조회
iris[1:3,]												# (4) iris 객체 조회
library(RODBC)											# (5) RODBC 패키지의 이용
channel<-odbcConnect(dsn='myodbc', 
	uid='bdboy', pwd='sm0113', case='mysql')			# (6) DBMS 연결
sqlTables(channel)[
  grep('iris',sqlTables(channel)$TABLE_NAME),] 			# (7) 테이블 이름 조회
sqlSave(channel, iris, addPK=T)							# (8) 데이터프레임 저장1
sqlSave(channel, iris, table='iristab', rownames=F)		# (9) 데이터프레임 저장2
sqlTables(channel)[
  grep('iris',sqlTables(channel)$TABLE_NAME),] 			# (10) 테이블 이름 조회
head(sqlFetch(channel, 'iris'))							# (11) 테이블 읽기1
sqlColumns(channel, 'iris')[,
  c("COLUMN_NAME","TYPE_NAME","COLUMN_SIZE",
    "ORDINAL_POSITION")]								# (12) 테이블의 컬럼 조회
sqlQuery(channel, "DESCRIBE iris")						# (13) 테이블의 컬럼 조회
query <- paste("select * from iristab",
            "where sepallength > 7",
            "and sepalwidth < 3",
            "order by sepallength")						# (14) 쿼리 생성
sqlQuery(channel, query)								# (15) 테이블 읽기2
sqlColumns(channel, 'iristab')[,
  c("COLUMN_NAME","TYPE_NAME","COLUMN_SIZE",
    "ORDINAL_POSITION")]								# (16) 테이블의 컬럼 조회
sqlCopy(channel, query, 'iris1', rownames = FALSE)		# (17) 테이블 복사
sqlCopyTable(channel, 'iris1', 'iris2',
  destchannel=channel)									# (18) 테이블 복사
sqlTables(channel)[
  grep('iris',sqlTables(channel)$TABLE_NAME),] 			# (19) 테이블 이름 조회
sqlClear(channel, 'iristab')							# (20) 테이블 Truncate
sqlFetch(channel, 'iristab')							# (21) 테이블 조회
sqlDrop(channel, 'iristab')								# (22) 테이블 삭제
sqlTables(channel)[
  grep('iris',sqlTables(channel)$TABLE_NAME),] 			# (23) 테이블 이름 조회
channel.excel <- odbcConnectExcel('c:/R_data/dbms2excel.xls', 
	readOnly=FALSE)										# (24) 테이블 이름 조회
sqlSave(channel, iris, table='iristab', rownames=F)		# (25) 재 저장
sqlCopy(channel, query, 'iris2', , rownames=F,
  destchannel=channel.excel)							# (26) 데이터프레임 저장3
close(channel.excel)									# (27) ODBC 연결 끊기
close(channel)											# (28) ODBC 연결 끊기



