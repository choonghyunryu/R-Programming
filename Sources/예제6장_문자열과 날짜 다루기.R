###################################################
# A. 문자열 조작 및 연산
###################################################

###################################################
# 1. 콘솔에서의 문자열 길이 확인
###################################################
x <- c("abc","가나다","1234[]","R program\n","\"R\"")	# (1) 문자열 벡터
x
nchar(x)						# (2) 문자열 길이(글자수)
nchar(x, type="chars")			# (3) 문자열 길이(글자수)
nchar(x, type="bytes")			# (4) 문자열 길이(바이트)
nchar(x, type="width")			# (5) 문자열 길이(넓이)
###################################################
# 2. 그래픽 디바이스에서의 문자열 길이 확인
###################################################
plot.new()						# (6) 그래프 디바이스 열기
strwidth(x)						# (7) 글자의 출력 폭
strheight(x)					# (8) 글자의 출력 높이

###################################################
# 3. 출력을 위한 문자열 벡터의 인코딩
###################################################
x <- c("1","123","12345")				# (1) 문자열 벡터
encodeString(x, width=NA) 				# (2) 좌측정렬	
encodeString(x, width=NA, justify="c")	# (3) 중앙정렬
encodeString(x, width=NA, justify="r")	# (4) 우측정렬
encodeString(x, width=NA, justify="n")	# (5) 정렬없음
encodeString(x, width=7)				# (6) 폭 지정
encodeString(x, width=NA, quote="'", justify="r") 	# (7) 따옴표 지정
paste(encodeString(x, width=7, justify="r"),
	colapse="\n", sep="")				# (8) 응용 예
cat("",paste(encodeString(x, width=7, justify="r"),
	colapse="\n", sep=""))				# (9) 응용 예

###################################################
# 4. 문자열 일부 자르기
###################################################
(str <- c(paste(letters[1:10],collapse=""),
       paste(LETTERS[1:7],collapse=""),
	   "가나다라마바사아"))# (1) 문자열 벡터
substr(str, 3, 5) 						# (2) substr, same length	
substr(str, 1:2, 4:7) 					# (3) substr, same length	
substring(str, 3, 5) 					# (4) substring, same length
substring(str, 1:2, 4:7) 				# (5) substring, longest length
x <- str
substr(str, 2, 4) <- "123"				# (6) substr, same index
str
str <- x
substr(str, 2, 3) <- "123"				# (7) substr, short index
str
str <- x
substr(str, c(1,4), c(2,5,6)) <- "XY"	# (8) substr, recycle
str
str <- x
substr(str, 6) <- "++"					# (9) substr, one args
str
str <- x
substring(str, 6) <- "++"				# (10) substring, one args
str

###################################################
# 4. 문자 변경하기
###################################################
str <- "I am Tom. I am a Student."
str1 <- "나는 탐입니다. 나는 학생입니다."
chartr("Ia","Xq", str)					# (1) 문자 변경
chartr("a-dS-UT","W-Z1-30", str)		# (2) 문자 변경-구간 지정	
chartr("나니","너오", str1)				# (3) 문자 변경-한글
chartr("나니","XZ", str1)				# (4) 문자 변경-한글/영문
chartr("가-다","라-바", str1)			# (5) 문자 변경-한글 구간지정
toupper(str)							# (6) 대문자로 변경
tolower(str)							# (7) 소문자로 변경
casefold(str)							# (8) 소문자로 변경
casefold(str, upper=FALSE)				# (9) 소문자로 변경
casefold(str, upper=TRUE)				# (10) 대문자로 변경


###################################################
# 5. 문자 벡터 연결하기
###################################################
str <- c("one","two","three")				# (1) 문자 벡터
num <- 1:3									# (2) 수자 벡터
bool <- c(T,F,T)							# (3) 논리 벡터	
paste("A", 1:5, sep = "")					# (4) sep 인수
paste(str, num)								# (5) 옵션 미사용
paste(str, num, sep=" is ")					# (6) sep 인수
paste(str, num, sep="-", collapse=":")		# (7) collapse 인수
paste(paste("Row", 1:3, sep = ""), 
  paste("Col", 1:2, sep = ""))				# (8) paste 두개 사용
expand.grid(paste("Row", 1:3, sep = ""), 
  paste("Col", 1:2, sep = ""))				# (9) 경우의 수
paste("Col", 1:2, bool, sep = " ", collapse="<->")	# (10) 세 객체 묶기
paste("올해는 ", format(Sys.time(), "%Y"), 
  "년이다.", sep="")						# (11) 응용
paste(str, collapse=" ")  					# (12) 응용
toString(str)								# (13) toString 함수
toString(str, width=7)						# (14) width 인수 사용
getS3method("toString", "default")			# (15) 함수의 내용
toString(1:5)								# (16) 수치벡터
toString(c(T,F,T,T))						# (17) 논리벡터

###################################################
# 7. 문자열 나누기
###################################################
args(strsplit)
str <- c("one two three  four")				# (1) 문자열 정의
(rslt <- strsplit(str, ' '))				# (2) 공백문자로 나누기
is(rslt)									# (3) 객체 유형
length(rslt)							
length(rslt[[1]])							# (4) 문자열 개수
(rslt1 <- strsplit(str, ' +'))				# (5) 정규표현식 이용
unlist(rslt1)								# (6) 벡터 만들기
strsplit(str, '')							# (7) 문자로 분리
str <- c("I am Tom.", "I am a student.",
  "You are Jane.", "You are a student, too.") # (8) 문자 벡터
(rslt2 <- strsplit(str, '[., ]'))			# (9) 단어 추출
(rslt2 <- sapply(rslt2, function(x) x[x!=""]))# (10) 공백 제거
sapply(rslt2, length)						# (11) 단어 개수
unlist(rslt2)								# (12) 벡터 만들기
sum(sapply(rslt2, length))					# (13) 벡터의 길이


###################################################
# 8. 패턴 검색 및 문자열 변경
###################################################
str <- c("I am Tom.", "I am a student.",
  "You are Jane.", "You are a student, too.") 	# (1) 문자 벡터
grep("",str)									# (2) 패턴 검색
grep("^I",str)									# (3) 정규 표현식 검색
str[grep("m.$|,",str)]							# (4) 실제 값 출력
grep("\\<a\\>", str, value=T)					# (5) value 옵션
grep("you", str, value=T, ignore.case=T)		# (6) 대소문자 무시
grep("\\<a\\>", str, value=T, invert=T)			# (7) 검색 결과의 반대
hi <- c("안녕!", "안녕?", "안녕.")				# (8) 문자 벡터
grep('\\?', hi)									# (9) 문자(?) 검색
grep('?', hi, fixed=T)							# (10) fixed 옵션

grepl("",str)									# (1) 패턴 검색
grepl("^I",str)									# (2) 정규 표현식 검색
str[grepl("m.$",str)]							# (3) 실제 값 출력
grepl("you", str, ignore.case=T)				# (4) 대소문자 무시
grepl("?", hi, fixed=T)							# (5) fixed 옵션

sub("[a-e\\.]", "X", str)						# (1) 패턴 변경
gsub("[a-e\\.]", "X", str, ignore.case=T)		# (2) 패턴 변경-전역
sub("[^a-z[:blank:]]", "X", str)				# (3) 기 정의 문자 클래스
gsub("[^a-z[:blank:]]", "X", str)				# (4) 기 정의 문자 클래스
gsub("a\\b", "X", str)							# (5) 단어 경계 문자 클래스
gsub("\\ba\\b", "X", str)						# (6) 단어 경계 문자 클래스
gsub("\\sa\\s", "X", str)						# (7) 단어 공백 문자 클래스
gsub("\\Ba\\B", "X", str)						# (8) 단어 안쪽 문자 클래스
sub("t", "X", str, ignore.case=T)				# (9) ignore.case 인수
str1 = 'one.two.three.four'						# (10) 문자 벡터
gsub('\\.',' ', str1)							# (11) 문자(.) 바꾸기
gsub('.',' ', str1, fixed=TRUE)					# (12) fixed 인수

str2 <- c("지붕위에 콩깍지는 F9 깐 콩깍지 A4", 
         "책상위의 콩깍지는 안깐 콩깍지",
		 "콩깍지 B5") 							# (1) 문자 벡터
regexpr("콩깍지", str2)							# (2) regexpr 함수
gregexpr("콩깍지", str2)						# (3) gregexpr 함수
gregexpr("콩깍지", str2, useBytes=T)			# (4) useBytes 인수
(expr <- gregexpr("[A-Z][0-9]", str2))			# (5) gregexpr 함수
sapply(expr, attr, "match.length")				# (6) match length
getString <- function(str, rgxpr)
 substring(str, rgxpr, 
   rgxpr + attr(rgxpr,"match.length")-1)		# (7) 사용자 함수 정의
mapply(getString, str2, expr)					# (8) 매치문자 추출

glob2rx("abc.*")								# (1) "abc.txt"
glob2rx("a?b.*")								# (2) "azb.doc"
glob2rx("a?b.*", trim.tail=FALSE)				# (3) "acb.docx"
glob2rx("*.doc")								# (4) "t123.doc"
glob2rx("*.doc", trim.head=TRUE) 				# (5) "c.doc"
glob2rx("*.t*")									# (6) "x.ttt12"
glob2rx("*.t??")								# (7) "xyz.txt"
glob2rx("*[*")									# (8) "abc[12345"


########################
# 응용의 예제
########################
str3 <- 'Now is the time.      '
sub(' +$', '', str3)  							# (1) 뒷쪽 스페이스 제거
sub('[[:space:]]+$', '', str3) 					# (2) 뒷쪽 스페이스 제거

str4 <- c("21,345,231원","9,987,123원","19,098,472원")
as.numeric(gsub('[원,]', '', str4))				# (3) 문자열 벡터 수치벡터화

str5 <- c("1234","865","(238)")
gsub('\\(([0-9.]+)\\)', '-\\1', str5)			# (4) 음수 기호 변경

str6 <- "a test of capitalizing"				
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", 
  str6, perl=TRUE) 								# (5) 알파벳 캡 씌우기
gsub("\\b(\\w)",    "\\U\\1",       
  str6, perl=TRUE) 								# (6) 알파벳 캡 씌우기


###################################################
# B. 날짜 조작 및 연산
###################################################

###################################################
# 1. 현재 날짜와 시간
###################################################
date()					# (1) 시스템 날짜 및 시각
Sys.timezone()			# (2) TZ 조회
Sys.Date()				# (3) 현재 날짜
Sys.time()				# (4) 현재 시각
format(Sys.time(), "%Y년 %m월 %d일 %X")  # (5) 현재 날짜 및 시각

###################################################
# 2. 날짜 데이터 만들기
###################################################
as.Date("2010-01-03")					# (1) 날짜 만들기
as.Date("2010-1-03")					# (2) 날짜 만들기
as.Date("2010-2-29")					# (3) 유효하지 않은 날짜
as.Date("3/1/2010", format="%d/%m/%Y")	# (4) 포맷 지정
as.Date("3jan2010", format="%d%b%Y")	# (5) 포맷 지정
(today <- as.Date("30-5월-2010", 
  format="%d-%B-%Y"))					# (6) 포맷 지정
format(today, format="%A %d-%B-%Y")		# (7) 포맷 조회
format(today, format="%a %d-%b-%y")		# (8) 포맷 조회
lct <- Sys.getlocale("LC_TIME")			# (9) 로케일 저장
Sys.setlocale("LC_TIME", "C")			# (10) 로케일 설정
as.Date("3JAN2010", format="%d%b%Y")	# (11) 포맷 지정
as.Date("January 3,10", format="%B %d,%y")	# (12) 포맷 지정
format(today, format="%A %d-%B-%Y")		# (13) 포맷 조회
Sys.setlocale("LC_TIME", lct)			# (14) 로케일 원복
as.Date(365, origin="2000-01-01")		# (15) origin 인수
as.Date(366, origin="2000-01-01")		# (16) origin 인수
as.Date(-10, origin="2000-01-01")		# (17) 기준일 이전
as.numeric(today)						# (18) 날짜를 숫자로 변경
(today.num <- 
  as.Date(14759, origin="1970-01-01"))	# (19) 숫자를 날짜로 변경
class(today.num) <- 'Date'				# (20) Date 클래스로 변경
today.num								# (21) 변경된 결과	
seq(as.Date("2010-5-31"), 
  by="days", length=5)					# (22) 날짜 벡터 만들기 1
seq(as.Date("2010-5-31"), 
  by="weeks", length=6)					# (23) 날짜 벡터 만들기 2
seq(as.Date("2000-1-1"), 
  to=as.Date("2000-3-1"), by="3 weeks")	# (24) 날짜 벡터 만들기 3

###################################################
# 3. 날짜 데이터 연산
###################################################
sdate <- as.Date("1970-05-17")			# (1) 날짜 만들기
edate <- as.Date("2010-05-30")			# (2) 날짜 만들기
edate - sdate; is(edate - sdate)		# (3) 날짜 차이
as.numeric(edate - sdate)				# (4) 숫자로 변환
sdate - edate							# (5) 음수로 반환
Sys.time() - 3600						# (6) 한시간 전 시간
Sys.Date() + 7							# (7) 일주일 후
edate == sdate							# (8) 논리식 1
edate >= sdate							# (9) 논리식 2
weekdays(sdate)							# (10) 요일 구하기 1
weekdays(sdate, abbreviate=T)			# (11) 요일 구하기 2
getS3method("weekdays", "Date")			# (12) 함수 정의 부
months(sdate)							# (13) 월 구하기 1
months(sdate, abbreviate=T)				# (14) 월 구하기 2
getS3method("months", "Date")			# (15) 함수 정의 부
quarters(sdate)							# (16) 분기 구하기
julian(sdate)							# (17) 줄리안 데이 1
julian(sdate, as.Date("2000-05-30"))	# (18) 줄리안 데이 2
(z <- Sys.time())						# (19) 현재 시간
(z <- as.numeric(z))					# (20) 초 단위 변경	
floor(z/(60*60*24))  					# (21) 1970-01-01이후 날짜
(dts=c(sdate, edate,
  as.Date("2002-11-14")))				# (22) 날짜 벡터
mean(dts)								# (23) 평균
max(dts)								# (24) 최대값
range(dts)								# (25) range	
table(weekdays(dts))					# (26) 요일 도수
table(quarters(dts))					# (27) 분기 도수
as.factor(quarters(dts))				# (28) factor 변환
difftime(edate, sdate, units="weeks")	# (29) 주 차이
difftime(sdate, edate, units="days")	# (30) 일 차이(음수)
difftime(edate, sdate, units="hours")	# (31) 시간 차이
difftime(edate, sdate, units="mins")	# (32) 분 차이
difftime(edate, sdate, units="secs")	# (33) 초 차이
difftime(edate, sdate, units="auto")	# (34) 일 차이

###################################################
# 4. chron 패키지
###################################################
library(chron)							# (1) chron 패키지
dts <- dates(c("02/25/2010", "02/28/2010",
    "03/09/2010", "03/28/2010"))		# (2) 날짜 데이터
dts	
is(dts)									# (3) dts 객체 종류
tms <- times(c("22:04:10", "19:01:23",
	"15:11:05", "13:36:24"))			# (4) 시간 데이터
tms
is(tms)									# (5) tms 객체 종류
chron(dates=dts, times=tms)				# (6) 연대 데이터 생성
chron(dates=dts, times=tms,
  format=c("year-m-d","h:m:s"))			# (7) format 이용 1 
chron(dates=dts, times=tms,
  format=c("y-month-d","h:m:s"))		# (8) format 이용 2
chron(dates=dts, times=tms,
  format=c("y-mon-d","h:m:s"))			# (9) format 이용 3
chron(dates=dts, times=tms,
  format=c("month day year","h:m:s"))	# (10) format 이용 4
  
###################################################
# 5. POSIX 클래스
###################################################
(z <- Sys.time())             # (1) 현재 시간(POSIXct 클래스)
unclass(z)                    # (2) 정수
floor(unclass(z)/(60*60*24))  # (3) 1970-01-01 이후의 일자
(z <- as.POSIXlt(Sys.time())) # (4) 현재 시간(POSIXlt 클래스)
unlist(unclass(z))            # (5) 리스트의 성분별 값

z <- c(1275292046, 1275292046) # (6) 정수 벡터
as.POSIXct(z, origin="1970-01-01")        		# (7) 로컬 시간 
as.POSIXct(z, origin="1970-01-01", tz="GMT")	# (8) UTC 시간
as.POSIXct(z, origin=ISOdatetime(1970,1,1,0,0,0))# (9) 로컬 시간
ISOdatetime(1970,1,1,0,0,0) + z            		# (10) 로컬 시간
dts <- z
class(dts)										# (11) 원래 클래스
class(dts) <- c("POSIXt","POSIXct")				# (12) 클래스 변경
dts												# (13) 로컬 시간
structure(z, class=c("POSIXt","POSIXct"))		# (14) 로컬 시간

as.POSIXlt(dts, tz="GMT") 						# (15) 현재 시간(POSIXct 클래스)
as.POSIXlt(dts, tz="KST")    					# (16) 오류
as.POSIXlt(dts, tz="Asia/Seoul")  				# (17) 로컬 시간
unclass(as.POSIXlt(dts, tz="Asia/Seoul")) 		# (18) 리스트의 성분별 값

ISOdatetime(1970,5,17,06,02,10, tz="GMT")		# (19) ISOdatetime 함수
ISOdate(1970,5,17)								# (20) ISOdate 함수
ISOdate(1970,5,17, tz="Asia/Seoul")				# (21) tz 인수
ISOdate											# (22) ISOdate 함수 내용

dates <- c("02/27/10", "12/27/10", "01/15/10")
times <- c("03:03:20", "22:29:56", "11:03:30")
x <- paste(dates, times)
(dts <- strptime(x, "%m/%d/%y %H:%M:%S"))		# (23) strptime 함수 1
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "German")
strptime("03/mai/00 22:29:30",
	"%d/%b/%y %H:%M:%S") 						# (24) strptime 함수 2
strftime(dts, "%Y년 %m월(%b) %d일, %X (%Z)")	# (25) strftime 출력
strftime(dts, "%Y년 %m월(%B) %d일 %A, %X %j %U")# (26) strftime 출력
Sys.setlocale("LC_TIME", lct)
strftime(dts, "%Y년 %m월 %d일 %a,%x")			# (27) strftime 출력
strftime(Sys.time(), "%Y %B %d %A, %X")			# (28) strftime 출력
strftime(Sys.time(), "%c")						# (29) strftime 출력



