
#create tables and figures

library(dplyr)
library(readr)
library(ggplot2)

#1915-1946-----------------------------------------------------------------------------
meta <- read_csv('./jstor/2015/meta.csv')
auths <- read_csv('./jstor/2015/authors.csv')

b1947 <- merge(filter(meta,year<1947),auths,by='doi',x.all=TRUE) %>% arrange(author,year,journal,doi)
early <- c('A. B. WOLFE', 'ABRAM J. JAFFE','ALBERTO ARCA PARRO','ALEXANDER M. CARR-SAUNDERS',
           'ALEXANDER STEVENSON','ALFRED J. LOTKA','BERNARD D. KARPINOS','C. HORACE HAMILTON',
           'CHARLES CLOSE','CHRISTOPHER TIETZE','CLYDE V. KISER','CONRAD TAEUBER','CORRADO GINI',
           'DAVID V. GLASS','DOROTHY S. THOMAS','DOROTHY SWAINE THOMAS','DUDLEY KIRK','E. P. HUTCHINSON',
           'EDGAR SYDENSTRICKER','EDWARD P. HUTCHINSON', 'ELLSWORTH HUNTINGTON','ENID CHARLES',
           'ERNEST W. BURGESS','F. STUART CHAPIN','FRANK G. BOUDREAU','FRANK H. HANKINS','FRANK LORIMER',
           'FRANK W. NOTESTEIN','FREDERICK OSBORN','G. PITT-RIVERS','HALBERT L. DUNN','HAROLD F. DORN',
           'HENRY PRATT FAIRCHILD','HENRY S. SHRYOCK JR.','HOPE TISDALE','HOPE TISDALE ELDRIDGE',
           'IRENE B. TAEUBER','JACOB S. SIEGEL','JOHN D. DURAND','JOSEPH A. HILL','JOSEPH J. SPENGLER',
           'KINGSLEY DAVIS','LEON E. TRUESDELL','LEONARD S. COTTRELL JR.','LOUIS I. DUBLIN',
           'LOUISE K. KISER','LOWELL J. REED','MARGARET JARMAN HAGOOD','MORTIMER SPIEGELMAN',
           'NORMAN E. HIMES','O. E. BAKER','PASCAL K. WHELPTON','PAUL C. GLICK','PAUL K. HATT',
           'PHILIP M. HAUSER','RADHAKAMAL MUKERJEE','RAYMOND PEARL','REGINE K. STIX','REUBEN HILL',
           'ROBERT E. CHADDOCK','ROBERT R. KUCZYNSKI','RUPERT B. VANCE','S. CHANDRASEKHAR',
           'SAMUEL A. STOUFFER','SIDNEY E. GOLDSTEIN','T. J. JR. WOOFTER','T. J. WOOFTER JR.',
           'WALTER F. WILLCOX','WARREN S. THOMPSON','WILBERT E. MOORE','WILLIAM FIELDING OGBURN')
ed <- filter(b1947,author %in% early)
oth <- filter(b1947,!(author %in% early))

docdoi <- read_csv('./jstor/2015/topic-docs-200.csv')

#articles by demographers before 1947
ear <- as.data.frame(unique(ed$doi))
names(ear) <- c('doi')
ear <- mutate(ear,doi=gsub('/','_',doi))
ear <- merge(ear,docdoi,by='doi',all.x=TRUE)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(ear[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  b <- rbind(b,a)
}
b <- arrange(b,-size)
demog <- rename(b,demog=size)

#articles by non-demographers before 1947
other <- as.data.frame(unique(oth$doi))
names(other) <- c('doi')
other <- mutate(other,doi=gsub('/','_',doi))
other <- merge(other,docdoi,by='doi',all.x=TRUE)
c <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(other[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  c <- rbind(c,a)
}
c <- arrange(c,-size)
nondemog <- rename(c,nondemog=size)

#top 20 words in each topic
topics <- read_csv('./jstor/2015/topic-words-200.csv')
tops <- data.frame(topic=numeric(),words=character())
for(i in 1:200) {
  wds <- topics[,c(i,201)]
  names(wds) <- c('prob','word')
  wds <- arrange(wds,-prob)
  text <- ''
  for(j in 1:20) {
    text <- paste(text,wds$word[j],sep=' ')
  }
  words <- as.data.frame(text)
  names(words) <- c('words')
  words$topic <- i
  tops <- rbind(tops,words)
}

#merge words onto percentages
all <- merge(merge(demog,nondemog,by='topic'),tops,by='topic')
all <- arrange(all,-demog)
write_csv(all,'./jstor/2015/pre_1947_topics.csv')

#Tables 2 and 3
t2 <- filter(all,topic %in% c(160,56,164,51,26,120,189,143,121,129,47))
t3 <- filter(all, topic %in% c(146,185,131,192))


#1947-1984------------------------------------------------
articles <- read_csv('./jstor/2015/articles.csv')
articles <- mutate(articles,doi=gsub('/','_',doi))

keep <- select(filter(articles,field %in% c('demography','sociology') & year >= 1947 & year <= 1984),field,year,doi,journal)

#demography journals
djournals <- c()
demography <- filter(keep,field=='demography')$journal
for (i in 1:length(demography)) {
  if (!(demography[i] %in% djournals)) {
    djournals <- c(djournals,demography[i])
  }
}

#sociology journals
sjournals <- c()
sociology <- filter(keep,field=='sociology')$journal
for (i in 1:length(sociology)) {
  if (!(sociology[i] %in% sjournals)) {
    sjournals <- c(sjournals,sociology[i])
  }
}

keep <- filter(keep,(field==demography | journal %in% c("INTERNATIONAL JOURNAL OF SOCIOLOGY",
                  "ANNALS OF THE AMERICAN ACADEMY OF POLITICAL AND SOCIAL SCIENCE","SOCIAL FORCES",                                                 
                  "AMERICAN JOURNAL OF SOCIOLOGY","AMERICAN SOCIOLOGICAL REVIEW",                             
                  "THE AMERICAN CATHOLIC SOCIOLOGICAL REVIEW","THE SOCIOLOGICAL QUARTERLY","ACTA SOCIOLOGICA",                                              
                  "AMERICAN JOURNAL OF ECONOMICS AND SOCIOLOGY","SOCIAL PROBLEMS","INTERNATIONAL JOURNAL OF SOCIOLOGY OF THE FAMILY",              
                  "SOCIOLOGICAL FOCUS","SOCIOLOGICAL ANALYSIS","MID-AMERICAN REVIEW OF SOCIOLOGY",                       
                  "INTERNATIONAL REVIEW OF SOCIOLOGY","ANNUAL REVIEW OF SOCIOLOGY","THE BRITISH JOURNAL OF SOCIOLOGY",                              
                  "INTERNATIONAL REVIEW OF MODERN SOCIOLOGY","THE CANADIAN JOURNAL OF SOCIOLOGY","SOCIOLOGY OF EDUCATION",                                        
                  "SOCIAL RESEARCH","SOCIOMETRY","TEACHING SOCIOLOGY","THE PACIFIC SOCIOLOGICAL REVIEW",                          
                  "THE MIDWEST SOCIOLOGIST","SOCIOLOGICAL METHODOLOGY","THE AMERICAN SOCIOLOGIST",                                      
                  "HISTORICAL SOCIAL RESEARCH","BERKELEY JOURNAL OF SOCIOLOGY","KANSAS JOURNAL OF SOCIOLOGY",                                   
                  "THEORY AND SOCIETY","SYMBOLIC INTERACTION","SOCIAL ANALYSIS","CONTEMPORARY SOCIOLOGY",                                        
                  "SOCIOLOGICAL PERSPECTIVES","SOCIOLOGICAL THEORY")))

keep <- merge(keep,docdoi,by='doi',all.x=TRUE)
keep$art <- 1
#demography
demog <- select(filter(keep,field=='demography'),-doi,-field,-doc,-journal)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(demog[,(i+1)])
  c <- as.data.frame(demog[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts)*100)
  b <- rbind(b,a)
}
demog <- mutate(b,field='demography')
#sociology
soc <- select(filter(keep,field=='sociology'),-doi,-field,-doc)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(soc[,(i+1)])
  c <- as.data.frame(soc[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts)*100)
  #a <- a %>% group_by(year,topic) %>% summarise(size=sum(size))
  b <- rbind(b,a)
}
#sociology <- rename(b,sociology=size)
sociology <- mutate(b,field='sociology')

topics <- read_csv('topic-words-200.csv')
tops <- data.frame(topic=numeric(),words=character())
for(i in 1:200) {
  wds <- topics[,c(i,201)]
  names(wds) <- c('prob','word')
  wds <- arrange(wds,-prob)
  text <- ''
  for(j in 1:20) {
    text <- paste(text,wds$word[j],sep=' ')
  }
  words <- as.data.frame(text)
  names(words) <- c('words')
  words$topic <- i
  tops <- rbind(tops,words)
}

all <- rbind(demog,sociology)

#articles
arts <- articles %>% mutate(articles=1) %>% filter(field %in% c('sociology','demography')) group_by(year,field) %>% 
  summarise(size=sum(articles)) %>% filter(year >=1947 & year <= 1984)
png('./fig2.png',height=500,width=1000)
ggplot(data=arts,aes(x=year,y=size,color=field)) + geom_line() + theme_bw() + 
  labs(x='Year',y='Number of Articles',title='Figure 2: Population-Related Articles by Journal Field, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()
png('./fig3.png',height=500,width=1000)
ggplot(data=s,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + 
  labs(x='Year',y='Percent of Field',title='Figure 3: Topics More Prevalent in Demography, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()
#sociology
socio <- c(24,30,124)
soc <- all %>% filter(topic %in% socio)
soc <- soc %>% mutate(top=ifelse(topic==24,'items table sample scale behavior scores subjects significant',
                                 ifelse(topic==30,'areas city metropolitan segregation cities area residential central',
                                        ifelse(topic==124,'mobility occupational status occupation occupations prestige american',''))))
png('./fig4.png',height=250,width=1000)
ggplot(data=soc,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + 
  labs(x='Year',y='Percent of Field',title='Figure 4: Topics More Prevalent in Sociology, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()   


write_csv(all,'1947_1984_years.csv')







getTopics <- function(ntopics) {
  topics <- read_csv(paste('topic-words-',toString(ntopics),'.csv',sep=''))
  for(i in 1:ntopics) {
    choose <- c(i,ntopics+1)
    topic <- topics[,choose]
    names(topic) <- c('size','text')
    topicsort <- arrange(topic,desc(size))
    top <- slice(topicsort,1:100)
    top$topic <- i
    write_csv(top,paste('/users/emily/dropbox/website/ps/topics/t',ntopics,'/topic',i,'_',ntopics,'_words.csv',sep=''))
  }
}

getDocs <- function(ntopics) {
  docs <- read_csv(paste('topic-docs-',toString(ntopics),'.csv',sep=''))
  docdoi <- mutate(docs,doi=gsub('.txt','',gsub('//users//emily//dropbox//jstor//2015//clean/','',doc)))
  alldocs <- merge(docdoi,articles,by='doi',all.x = TRUE)
  for(i in 1:ntopics) {
    choose <- c(i+1,ntopics+5,ntopics+6)
    topic <- alldocs[,choose]
    names(topic) <- c('size','year','field')
    group <- group_by(topic,field,year)
    arts <- summarise(group,articles = sum(size))
    allfields <- as.data.frame(years)
    names(allfields) <- c('year')
    for(j in 1:length(fields)) {
      field <- filter(arts,field==fields[j])
      field <- field[,c(2,3)]
      names(field) <- c('year',fields[j])
      allfields <- merge(allfields,field,by='year',all.x=TRUE)
    }
    allfields[is.na(allfields)] <- 0
    allfields <- mutate(allfields, size=history + economics + geography + anthropology + policy + education +
                          sociology + areastud + eugenics + demography + famplan + health)
    allfields$topic <- i
    write_csv(allfields,paste('/users/emily/dropbox/website/ps/topics/t',ntopics,'/topic',toString(i),'_',toString(ntopics),'_graph.csv',sep=''))
  }
}

articles <- read_csv('/users/emily/dropbox/jstor/2015/articles.csv')
articles <- mutate(articles,doi=gsub('/','_',doi))
articles <- filter(articles,!(field %in% c('science','statistics')))
articles$nfield <- ifelse(articles$field %in% c('libsci','psychology'),'education',
                          ifelse(articles$field %in% c('polisci'),'policy',articles$field))
fields <- unique(articles$nfield)
#years <- 1915:2004
years <- 1947:1984

getTopics(50)
getDocs(50)

getTopics(75)
getDocs(75)

getTopics(100)
getDocs(100)

getTopics(125)
getDocs(125)

getTopics(150)
getDocs(150)

getTopics(175)
getDocs(175)

getTopics(200)
getDocs(200)

#Early demographyers------------------------------------------------------------------------------
setwd('/users/emily/dropbox/jstor/2015/metadata')

meta <- read_csv('meta.csv')
auths <- read_csv('authors.csv')

b1947 <- arrange(merge(filter(meta,year < 1947),auths,by='doi',x.all=TRUE),year,journal,doi)
early <- c('PASCAL K. WHELPTON','WARREN S. THOMPSON','FRANK W. NOTESTEIN','KINGSLEY DAVIS','IRENE B. TAEUBER',
           'ROBERT R. KUCZYNSKI','RAYMOND PEARL','CLYDE V. KISER','WILLIAM FIELDING OGBURN','HENRY PRATT FAIRCHILD',
           'LOUIS I. DUBLIN','T. J. WOOFTER JR.','ALFRED J. LOTKA','JOSEPH J. SPENGLER','WALTER F. WILLCOX',
           'FREDERICK OSBORN','FRANK H. HANKINS','FRANK LORIMER','DOROTHY SWAINE THOMAS','LOWELL J. REED',
           'HENRY S. SHRYOCK JR.','SAMUEL A. STOUFFER','ROBERT E. CHADDOCK','HAROLD F. DORN','LEON E. TRUESDELL',
           'HALBERT L. DUNN','CONRAD TAEUBER','DAVID V. GLASS','MARGARET JARMAN HAGOOD','RUPERT B. VANCE',
           'W. PARKER MAULDIN','DOROTHY S. THOMAS','CHRISTOPHER TIETZE','HOPE TISDALE','HOPE TISDALE ELDRIDGE',
           'DUDLEY KIRK','WILBERT E. MOORE','JOHN D. DURAND','PHILIP M. HAUSER','LOUISE K. KISER','RONALD FREEDMAN',
           'JOHN HAJNAL','DONALD J. BOGUE','GEORGE J. STOLNITZ','NORMAN B. RYDER','GEORGE F. MAIR','NATHAN KEYFITZ',
           'AMOS H. HAWLEY','ALEXANDER M. CARR-SAUNDERS','CALVIN L. BEALE','OTIS DUDLEY DUNCAN','CHARLES F. WESTOFF',
           'JOHN F. KANTNER','BEVERLY DAVIS','BEVERLY DUNCAN','WILLIAM BRASS','JACOB S. SIEGEL','SIDNEY GOLDSTEIN',
           'ROBERT G. POTTER JR.','REUBEN HILL','J. MAYONE STYCOS','ANSLEY J. COALE','BERNARD D. KARPINOS',
           'C. HORACE HAMILTON','CALVERT L. DEDRICK','CHARLES CLOSE','CORRADO GINI','EDGAR SYDENSTRICKER',
           'ENID CHARLES','ERNEST JURKAT','ERNEST W. BURGESS','F. STUART CHAPIN','G. PITT-RIVERS',
           'LEONARD S. COTTRELL JR.','NORMAN E. HIMES','PAUL C. GLICK','RADHAKAMAL MUKERJEE','REGINE K. STIX',
           'S. CHANDRASEKHAR','T. J. JR. WOOFTER','T. LYNN SMITH')
ed <- filter(b1947,author %in% early)
oth <- filter(b1947,!(author %in% early))

setwd('/users/emily/dropbox/jstor/2015/tmod')
docs <- read_csv('topic-docs-200.csv')
docdoi <- mutate(docs,doi=gsub('.txt','',gsub('//users//emily//dropbox//jstor//2015//clean/','',doc)))
#articles by demographers before 1947
ear <- as.data.frame(unique(ed$doi))
names(ear) <- c('doi')
ear <- mutate(ear,doi=gsub('/','_',doi))
ear <- merge(ear,docdoi,by='doi',all.x=TRUE)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(ear[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  b <- rbind(b,a)
}
b <- arrange(b,-size)
demog <- rename(b,demog=size)
#articles by non-demographers before 1947
other <- as.data.frame(unique(oth$doi))
names(other) <- c('doi')
other <- mutate(other,doi=gsub('/','_',doi))
other <- merge(other,docdoi,by='doi',all.x=TRUE)
c <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(other[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  c <- rbind(c,a)
}
c <- arrange(c,-size)
nondemog <- rename(c,nondemog=size)

topics <- read_csv('topic-words-200.csv')
tops <- data.frame(topic=numeric(),words=character())
for(i in 1:200) {
  wds <- topics[,c(i,201)]
  names(wds) <- c('prob','word')
  wds <- arrange(wds,-prob)
  text <- ''
  for(j in 1:20) {
    text <- paste(text,wds$word[j],sep=' ')
  }
  words <- as.data.frame(text)
  names(words) <- c('words')
  words$topic <- i
  tops <- rbind(tops,words)
}

all <- merge(merge(demog,nondemog,by='topic'),tops,by='topic')
all <- arrange(all,-demog)
write_csv(all,'1947_topics_new.csv')

#Demography, Sociology, and Family Planning, 1947-1984------------------------------------------------
articles <- read_csv('/users/emily/dropbox/jstor/2015/articles.csv')
articles <- mutate(articles,doi=gsub('/','_',doi))
docs <- read_csv('topic-docs-200.csv')
docdoi <- mutate(docs,doi=gsub('.txt','',gsub('//users//emily//dropbox//jstor//2015//clean/','',doc)))
keep <- select(filter(articles,field %in% c('demography','sociology','famplan') & year >= 1947 & year <= 1984),field,year,doi)
keep <- merge(keep,docdoi,by='doi',all.x=TRUE)
keep$art <- 1
#demography
demog <- select(filter(keep,field=='demography'),-doi,-field,-doc)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(demog[,(i+1)])
  c <- as.data.frame(demog[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts)*100)
  #a <- a %>% group_by(year,topic) %>% summarise(size=sum(size))
  b <- rbind(b,a)
}
#demog <- rename(b,demog=size)
demog <- mutate(b,field='demography')
#sociology
soc <- select(filter(keep,field=='sociology'),-doi,-field,-doc)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(soc[,(i+1)])
  c <- as.data.frame(soc[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts)*100)
  #a <- a %>% group_by(year,topic) %>% summarise(size=sum(size))
  b <- rbind(b,a)
}
#sociology <- rename(b,sociology=size)
sociology <- mutate(b,field='sociology')
#family planning
fp <- select(filter(keep,field=='famplan'),-doi,-field,-doc)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(fp[,(i+1)])
  c <- as.data.frame(fp[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  #a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts))
  a <- a %>% group_by(year,topic) %>% summarise(size=sum(size))
  b <- rbind(b,a)
}
#fp <- rename(b,fp=size)
fp <- mutate(b,field='famplanning')

topics <- read_csv('topic-words-200.csv')
tops <- data.frame(topic=numeric(),words=character())
for(i in 1:200) {
  wds <- topics[,c(i,201)]
  names(wds) <- c('prob','word')
  wds <- arrange(wds,-prob)
  text <- ''
  for(j in 1:20) {
    text <- paste(text,wds$word[j],sep=' ')
  }
  words <- as.data.frame(text)
  names(words) <- c('words')
  words$topic <- i
  tops <- rbind(tops,words)
}

all <- rbind(demog,sociology)

#all <- merge(merge(merge(demog,sociology,by=c('year','topic')),fp,by=c('year','topic')),tops,by=c('topic'))

#methodological
method <- c(160,56,164,51,26,120)
m <- all %>% filter(topic %in% method)
m <- m %>% mutate(top=ifelse(topic==160,'census information statistics persons bureau statistical censuses',
                             ifelse(topic==56,'survey sample surveys respondents interview questions response',
                                    ifelse(topic==164,'rate rates increase year total birth period million',
                                           ifelse(topic==51,'age mortality table estimates ages life method populations',
                                                  ifelse(topic==26,'rates rate year period figure total average higher',
                                                         ifelse(topic==120,'life mortality age expectancy death ages survival tables','')))))))
png('fig2.png',height=500,width=1000)
ggplot(data=m,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + 
  labs(x='Year',y='Percent of Field',title='Figure 2: Methodological Topics, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()
#sbustantive
sub <- c(47,129,93,38,168,87)
s <- all %>% filter(topic %in% sub)
s <- s %>% mutate(top=ifelse(topic==47,'contraceptive method contraception methods contraceptives pill',
                             ifelse(topic==129,'birth births parity interval months order intervals live',
                                    ifelse(topic==93,'size preferences preference children intentions desired ideal sons',
                                           ifelse(topic==38,'family planning program programs studies bangladesh contraceptive',
                                                  ifelse(topic==168,'iud acceptors rates acceptance months continuation program users',
                                                         ifelse(topic==87,'reproductive infertility menstrual reproduction cycle human days cycles','')))))))
png('fig3.png',height=500,width=1000)
ggplot(data=s,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + 
  labs(x='Year',y='Percent of Field',title='Figure 3: Topics More Prevalent in Demography, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()
#sociology
socio <- c(24,30,124)
soc <- all %>% filter(topic %in% socio)
soc <- soc %>% mutate(top=ifelse(topic==24,'items table sample scale behavior scores subjects significant',
                                 ifelse(topic==30,'areas city metropolitan segregation cities area residential central',
                                        ifelse(topic==124,'mobility occupational status occupation occupations prestige american',''))))
png('fig4.png',height=250,width=1000)
ggplot(data=soc,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + 
  labs(x='Year',y='Percent of Field',title='Figure 4: Topics More Prevalent in Sociology, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()   
#articles
arts <- articles %>% mutate(articles=1) %>% filter(field %in% c('sociology','demography')) group_by(year,field) %>% 
  summarise(size=sum(articles)) %>% filter(year >=1947 & year <= 1984)
png('fig2.png',height=500,width=1000)
ggplot(data=arts,aes(x=year,y=size,color=field)) + geom_line() + theme_bw() + 
  labs(x='Year',y='Number of Articles',title='Figure 2: Population-Related Articles by Journal Field, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()

write_csv(all,'1947_1984_years.csv')





#PIndex, Pstudies, Demog, 1947-1972------------------------------------------------------
setwd('/users/emily/dropbox/jstor/2015/tmod')
articles <- read_csv('/users/emily/dropbox/jstor/2015/articles.csv')
articles <- mutate(articles,doi=gsub('/','_',doi))
docs <- read_csv('topic-docs-200.csv')
docdoi <- mutate(docs,doi=gsub('.txt','',gsub('//users//emily//dropbox//jstor//2015//clean/','',doc)))
midc <- select(filter(articles,journal %in% c('MILBANK MEMORIAL FUND QUARTERLY','POPULATION INDEX','POPULATION STUDIES','DEMOGRAPHY') & year >= 1947 & year <= 1972),doi)
midc <- merge(midc,docdoi,by='doi',all.x=TRUE)
soc <- select(filter(articles,field=='sociology' & year >= 1947 & year <= 1972),doi)
soc <- merge(soc,docdoi,by='doi',all.x=TRUE)
fp <- select(filter(articles,field=='famplan' & year >= 1947 & year <= 1972),doi)
fp <- merge(fp,docdoi,by='doi',all.x=TRUE)
middemog <- data.frame(topic=numeric(),size=numeric())
midsoc <- data.frame(topic=numeric(),size=numeric())
midfp <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(midc[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  middemog <- rbind(middemog,a)
}
names(middemog) <- c('topic','demog')
for(i in 1:200) {
  a <- as.data.frame(soc[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  midsoc <- rbind(midsoc,a)
}
names(midsoc) <- c('topic','soc')
for(i in 1:200) {
  a <- as.data.frame(fp[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  midfp <- rbind(midfp,a)
}
names(midfp) <- c('topic','fp')

midcen <- arrange(merge(merge(merge(middemog,midsoc,by='topic'),midfp,by='topic'),tops,by='topic'),-soc)
write_csv(midcen,'midcentury.csv')

#Demography journals----------------------------------------------------------------------------------------------
topics <- read_csv('topic-words-200.csv')
for(i in 1:200) {
  choose <- c(i,201)
  topic <- topics[,choose]
  names(topic) <- c('size','text')
  topicsort <- arrange(topic,desc(size))
  top <- slice(topicsort,1:100)
  top$topic <- i
  write_csv(top,paste('/users/emily/dropbox/website/ps/topics/demography/topic',i,'_200_words.csv',sep=''))
}

articles <- read_csv('/users/emily/dropbox/jstor/2015/articles.csv')
articles <- mutate(articles,doi=gsub('/','_',doi))
demog <- filter(articles,field=='demography')
demog <- mutate(demog,journal=ifelse(journal=='DEMOGRAPHY', 'demog',
                                     ifelse(journal=='POPULATION STUDIES','pstud',
                                            ifelse(journal=='POPULATION INDEX','pindex',
                                                   ifelse(journal=='POPULATION AND DEVELOPMENT REVIEW','pdevel',
                                                          ifelse(journal=='POPULATION AND ENVIRONMENT','poen',
                                                                 ifelse(journal %in% c('IN DEFENSE OF THE ALIEN','INTERNATIONAL MIGRATION DIGEST','INTERNATIONAL MIGRATION REVIEW'),'migration','other')))))))
demog$narts <- 1
dcorpus <- summarise(group_by(demog,year,journal),articles=sum(narts))
years <- unique(dcorpus$year)
journals <- unique(dcorpus$journal)
corpus <- as.data.frame(years)
names(corpus) <- c('year')
for(i in 1:length(journals)) {
  j <- select(filter(dcorpus,journal==journals[i]),year,articles)
  names(j) <- c('year',journals[i])
  corpus <- merge(corpus,j,by='year',all.x = TRUE)
}
corpus[is.na(corpus)] <- 0
write_csv(corpus,'/users/emily/dropbox/website/ps/topics/demography/corpus.csv')

docs <- read_csv('topic-docs-200.csv')
docdoi <- mutate(docs,doi=gsub('.txt','',gsub('//users//emily//dropbox//jstor//2015//clean/','',doc)))
demog <- merge(select(demog,doi,year,journal),docdoi,by='doi',all.x = TRUE)
for(i in 1:200) {
  demography <- as.data.frame(years)
  names(demography) <- c('year')
  tops <- select(demog,year,journal,i+3)
  names(tops) <- c('year','journal','topic')
  for(j in 1:length(journals)) {
    top <- filter(tops,journal==journals[j])
    top <- summarise(group_by(top,year),articles=sum(topic))
    names(top) <- c('year',journals[j])
    demography <- merge(demography,top,by='year',all.x=TRUE)
  }
  demography[is.na(demography)] <- 0
  demography <- mutate(demography,size=(demog + pstud + pindex + pdevel + poen + migration + other),topic=i)
  write_csv(demography,paste('/users/emily/dropbox/website/ps/topics/demography/topic',toString(i),'_200_graph.csv',sep = ''))
}


#Notestein and Davis----------------------------------------------------------------------------------------------
setwd('/users/emily/dropbox/jstor/2015/metadata')

meta <- read_csv('meta.csv')
auths <- read_csv('authors.csv')

notestein <- mutate(merge(filter(auths,author=='FRANK W. NOTESTEIN'),filter(meta,year >= 1947),by='doi'),doi=gsub('/','_',doi))
davis <- mutate(merge(filter(auths,author=='KINGSLEY DAVIS'), filter(meta,year >= 1947),by='doi'),doi=gsub('/','_',doi))

notestein <- merge(select(notestein,doi),docdoi,by='doi',all.x=TRUE)
davis <- merge(select(davis,doi),docdoi,by='doi',all.x=TRUE)

fn <- data.frame(topic=numeric(),size=numeric())
kd <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(notestein[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  fn <- rbind(fn,a)
}
names(fn) <- c('topic','notestein')
for(i in 1:200) {
  a <- as.data.frame(davis[,i+1])
  names(a) <- c('size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,topic),size=sum(size)/sum(arts)*100)
  kd <- rbind(kd,a)
}
names(kd) <- c('topic','davis')

setwd('/users/emily/dropbox/jstor/2015/tmod')
topics <- read_csv('topic-words-200.csv')
tops <- data.frame(topic=numeric(),words=character())
for(i in 1:200) {
  wds <- topics[,c(i,201)]
  names(wds) <- c('prob','word')
  wds <- arrange(wds,-prob)
  text <- ''
  for(j in 1:20) {
    text <- paste(text,wds$word[j],sep=' ')
  }
  words <- as.data.frame(text)
  names(words) <- c('words')
  words$topic <- i
  tops <- rbind(tops,words)
}

nd <- merge(merge(fn,kd,by='topic'),tops,by='topic')

#Articles on specific topics---------------------------------------------------------------------------------
docs <- read_csv('topic-docs-200.csv')
docdoi <- mutate(docs,doi=gsub('.txt','',gsub('//users//emily//dropbox//jstor//2015//clean/','',doc)))
articles <- read_csv('/users/emily/dropbox/jstor/2015/articles.csv')
artdoi <- mutate(articles,doi=gsub('/','_',doi))
names <- 't1'
for(i in 2:200) {
  names <- c(names,paste('t',i,sep = ''))
}
names <- c(names,'doc','doi')
names(docdoi) <- names
labels <- 'doi'
for(i in 1:5) {
  labels <- c(labels,paste('t',i,sep=''),paste('p',i,sep = ''))
}
docbytop <- data.frame(doi=character(),t1=numeric(),p1=numeric(),t2=numeric(),p2=numeric(),t3=numeric(),p3=numeric(),t4=numeric(),p4=numeric(),t5=numeric(),p5=numeric())
for(i in 1:length(docdoi$doi)) {
  doc <- filter(docdoi,doi==docdoi$doi[i])
  alltops <- data.frame(pct=numeric(),topic=numeric())
  for(j in 1:200) {
    top <- as.data.frame(doc[,j])
    names(top) <- c('pct')
    top$topic <- j
    alltops <- rbind(alltops,top)
  }
  alltops <- arrange(alltops,-pct)
  toptops <- data.frame(docdoi$doi[i],alltops$topic[1],alltops$pct[1],alltops$topic[2],alltops$pct[2],
                        alltops$topic[3],alltops$pct[3],alltops$topic[4],alltops$pct[4],alltops$topic[5],alltops$pct[5])
  names(toptops) <- labels
  docbytop <- rbind(docbytop,toptops)
}
write_csv(docbytop,'docbytopic.csv')
alldocs <- merge(artdoi,docbytop,by='doi')