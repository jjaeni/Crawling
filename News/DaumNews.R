library(tidyverse)
library(rvest)
library(tm)
library(qdapRegex)
library(KoNLP)
library(wordcloud)
library(wordcloud2)


# url 만들기 - 다음 뉴스 닭가슴살 출시
base_url <- 'https://search.daum.net/search?nil_suggest=btn&w=news&DA=PGD&q=%EB%8B%AD%EA%B0%80%EC%8A%B4%EC%82%B4+%EC%B6%9C%EC%8B%9C&p='

urls <- NULL
for ( x in 1:9) {
  urls <- c(urls, paste(base_url, x, sep = ''))
}
urls


# HTML 불러오기
html <- read_html(urls[1])
html1 <- html_nodes(html, 'ul.list_news > li > div > span > a')
html_attr(html1, 'href')


# 9페이지까지 뉴스기사 url 크롤링
news_links <- NULL
for (url in urls) {
  html <- read_html(url)
  news_links <- c(news_links, html %>%
                    html_nodes('ul.list_news > li > div > span > a') %>%
                    html_attr('href'))
}
news_links


# 기사 세부 내용 크롤링
Contents <- NULL
Title <- NULL

for (link in news_links) {
  html <- read_html(link)
  # 본문
  Contents <- c(Contents, html %>% html_nodes('div.news_view') %>% html_text())
  
  # 제목
  Title <- c(Title, html %>% html_nodes('h3.tit_view') %>% html_text())
}

Contents <- as.data.frame(Contents)
Title <- as.data.frame(Title)
news <- cbind(Title, Contents)
View(news)


# 추출된 46개의 기사 중 랜덤으로 3개만 추출
set.seed(123)
sample(length(news_links), 5, replace = T) # 결과 14 15 42
news_links[c(14, 15, 31, 3, 42)]


# 단어 사전 만들기
my_dic <- c('닭가슴살', '오일', '기자', '재배포', '금지')

buildDictionary(ext_dic = 'woorialsam',
                user_dic = data.frame(my_dic, 'ncn'))


# 14번 기사 명사 추출 및 전처리
(con14 <- news$Contents[14] %>%
    extractNoun())

{
  grep('파이낸셜', con14)
  con14[c(1, 2, 96, 97, 102)] <- ''
  con14[101] <- '금지'
  con14
  
  grep('[A-z]', con14); grep('[A-z]', con14, value = T)
  con14[c(67, 95)] <- ''
  con14
  
  grep('로', con14); grep('로', con14, value = T)
  con14[59] <- '중불'; con14[84] <- '코로나'; con14[87] <- '활동량'
  con14
  
  con14[82] <- '장기화'; con14[83] <- ''
  con14
  
  grep('수', con14); grep('수', con14, value = T)
  con14[c(56, 79)] <- ''
  con14
  
  grep('은', con14); grep('은', con14, value = T)
  con14[c(3, 77)] <- gsub('은', '', con14[c(3, 77)])
  con14[32] <- ''
  con14
  
  grep('한', con14); grep('한', con14, value = T)
  con14[72] <- ''
  con14
  
  con14 <- gsub('[[:digit:]]', '', con14)
  con14
  
  con14[30] <- '오일'; con14[31] <- ''
  con14
  
  grep('트러플', con14); grep('트러플', con14, value = T)
  con14[103] <- '탄두리'; con14[c(8, 27)] <- '트러플'
  con14
  
  grep('을', con14); grep('을', con14, value = T)
  con14[c(6, 24)] <- gsub('을', '', con14[c(6, 24)])
  con14
  
  grep('과', con14); grep('과', con14, value = T)
  con14[23] <- '식감'
  con14
  
  grep('줄', con14)
  con14[7] <- ''
  con14
  
  grep('시', con14); grep('시', con14, value = T)
  con14[c(54, 58)] <- ''
  con14
  
  con14 <- Filter(function(x) {x != ''}, con14) # 공백 모두 삭제
  con14
}



# 3번 기사 명사 추출 및 전처리
(con3 <- news$Contents[3] %>%
    extractNoun())

{
  grep('[A-z]', con3); grep('[A-z]', con3, value = T)
  con3[grep('[A-z]', con3)] <- ''
  con3
  
  con3[188] <- '금지'; con3[c(186, 189)] <- ''
  con3
  
  grep('이', con3); grep('이', con3, value = T)
  con3[36] <- '스파이시'
  con3[c(51, 112, 183)] <- ''
  con3
  
  con3[175:176] <- ''
  con3
  
  grep('수', con3); grep('수', con3, value = T)
  con3[c(116, 174)] <- ''
  con3
  
  con3[166:167] <- ''
  con3
  
  con3[154:155] <- ''
  con3
  
  grep('소시지', con3); grep('소시지', con3, value = T)
  con3[c(19, 152)] <- '소시지'
  con3
  
  con3[148] <- '기획전'; con3[149] <- ''
  con3
  
  con3 <- gsub('[[:digit:]]', '', con3)
  con3
  
  con3[147] <- ''
  con3
  
  grep('은', con3); grep('은', con3, value = T)
  con3[c(5, 65, 136)] <- '아임닭'
  con3
  
  con3[135] <- ''
  grep('식', con3); grep('식', con3, value = T)
  con3[133] <- '간식'
  con3
  
  grep('시', con3); grep('시', con3, value = T)
  con3[c(37, 132)] <- ''
  con3
  
  grep('비롯', con3)
  con3[c(129, 130)] <- ''
  con3
  
  grep('한', con3); grep('한', con3, value = T)
  con3[grep('한', con3)] <- ''
  con3
  
  grep('형', con3); grep('형', con3, value = T)
  con3[c(12, 90, 110)] <- ''
  con3
  
  grep('소', con3); grep('소', con3, value = T)
  con3[c(7, 108, 139)] <- '소시지'
  con3
  
  grep('용', con3); grep('용', con3, value = T)
  con3[103] <- ''
  con3
  
  grep('당', con3)
  con3[93] <- '팩'
  con3
  
  grep('과', con3)
  con3[82] <- '식감'
  con3
  
  con3[79] <- '장점'; con3[80] <- ''
  con3
  
  grep('두', con3)
  con3[75] <- ''
  con3
  
  grep('억', con3)
  con3[71] <- ''
  con3
  
  grep('전', con3); grep('전', con3, value = T)
  con3[66] <- ''
  con3
  
  grep('만', con3)
  con3[54] <- ''
  con3
  
  grep('적', con3); grep('적', con3, value = T)
  con3[31] <- ''
  con3
  
  con3[15] <- ''
  grep('닭가슴살', con3); grep('닭가슴살', con3, value = T)
  con3[17] <- '닭가슴살'
  con3
  
  con3[2:4] <- ''
  con3
  
  con3 <- Filter(function(x) {x != ''}, con3) # 공백 모두 삭제
  con3
}



# 42번 기사 명사 추출 및 전처리
(con42 <- news$Contents[42] %>%
    extractNoun())

{
  grep('[A-z]', con42); grep('[A-z]', con42, value = T)
  con42[c(96, 99, 155, 156, 157, 158)] <- ''
  con42[95] <- '봉지'
  con42
  
  con42[162] <- '금지'; con42[163] <- ''
  con42
  
  grep('이충진', con42)
  con42[153:154] <- ''
  con42
  
  grep('고', con42); grep('고', con42, value = T)
  con42[151] <- '노력'
  con42
  
  grep('수', con42); grep('수', con42, value = T)
  con42[grep('수', con42)] <- ''
  con42
  
  con42[145:146] <- ''
  con42
  
  grep('꼬꼬칩', con42); grep('꼬꼬칩', con42, value = T)
  con42[grep('꼬꼬칩', con42)] <- '꼬꼬칩'
  con42
  
  con42 <- gsub('[[:digit:]]', '', con42)
  con42
  
  grep('도', con42)
  con42[164] <- '안주'; con42[138] <- '간식'
  con42
  
  grep('들', con42); grep('들', con42, value = T)
  con42[c(107, 112, 137)] <- ''; con42[115] <- '홈술족'
  con42
  
  grep('최근', con42)
  con42[131] <- '최근'
  con42
  
  con42[125] <- ''
  grep('사조', con42); grep('사조', con42, value = T)
  grep('대', con42); grep('대', con42, value = T)
  con42[c(3, 31, 54, 122, 126)] <- '사조대림'
  con42[c(4, 32, 55, 123, 127)] <- ''
  con42
  
  grep('한', con42); grep('한', con42, value = T)
  con42[grep('한', con42)] <- ''
  con42
  
  con42[113] <- '영양식'
  con42[c(114, 116)] <- ''
  con42
  
  grep('과', con42); grep('과', con42, value = T)
  con42[c(41, 94)] <- '식감'
  con42[69] <- '오리지널'
  con42
  
  con42[86] <- '김스틱'
  con42
  
  con42[75:76] <- ''
  con42
  
  grep('을', con42)
  con42[66] <- '식감'
  con42
  
  grep('\\.', con42); grep('\\.', con42, value = T)
  con42[grep('\\.', con42)] <- ''
  con42
  
  grep('있', con42)
  con42[49] <- ''
  con42
  
  con42[45:46] <- ''
  con42
  
  grep('담백', con42); grep('담백', con42, value = T)
  con42[grep('담백', con42)] <- ''
  con42
  
  grep("해", con42)
  con42[38] <- ''
  con42
  
  grep('어포', con42); grep('어포', con42, value = T)
  con42[c(25, 36)] <- '어포'
  con42
  
  con42[33] <- '국내산'; con42[34] <- ''
  con42
  
  grep('식', con42); grep('식', con42, value = T)
  con42[c(11, 30)] <- '간식'
  con42
  
  con42[c(1:2, 6, 12)] <- ''
  con42
  
  con42 <- Filter(function(x) {x != ''}, con42) # 공백 모두 삭제
  con42
}



# 전체 빈도수 단어구름 형태 만들기
wordcount42 <- table(unlist(con42)) # 빈도수 확인하기

re_word42 <- as.data.frame(wordcount42, stringsAsFactors = F)
re_word42 <- rename(re_word42, word = Var1, freq = Freq)
re_word42 <- filter(re_word42, nchar(word) >= 2) # 2글자 이상의 단어만 추출
re_word42 <- re_word42 %>%
  arrange(desc(freq))
View(re_word42)

wordcloud2(re_word42) # 단어구름 만들기



# 3개의 랜덤 추출 기사를 하나의 리스트에 묶어오기
news_t <- list(con3, con14, con42)
str(news_t)
names(news_t) <- c('news3', 'news14', 'news42')



# 전체 기사 빈도수 단어 구름 만들기
wordcount_t <- table(unlist(news_t)) # 빈도수 확인하기

re_word_t <- as.data.frame(wordcount_t, stringsAsFactors = F)
re_word_t <- rename(re_word_t, word = Var1, freq = Freq)
re_word_t <- filter(re_word_t, nchar(word) >= 2) # 2글자 이상의 단어만 추츨
re_word_t <- re_word_t %>%
  arrange(desc(freq))
View(re_word_t)

wordcloud2(re_word_t) # 단어구름 만들기



# 말뭉치 만들어 TermDocumentMatrix 만들기
news_cor <- Corpus(VectorSource(news_t))
news_Dtm <- TermDocumentMatrix(news_cor, control = list(minWordLength = 1))
View(as.matrix(news_Dtm))
Encoding(news_Dtm$dimnames$Terms) = 'CP949'



# 클러스터링 분석 후 Dendrogram 만들기
(d <- dist(t(news_Dtm)))
(fit <- hclust(d=d))
plot(fit)
rect.hclust(fit, k = 2)
