
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
write_csv(all,'./pre_1947_topics.csv')

#Tables 2 and 3
t2 <- filter(all,topic %in% c(160,56,164,51,26,120,189,143,121,129,47))
t3 <- filter(all, topic %in% c(146,185,131,192))


#1947-1984------------------------------------------------
articles <- read_csv('./jstor/2015/articles.csv')
articles <- mutate(articles,doi=gsub('/','_',doi))

keep <- select(filter(articles,field %in% c('demography','sociology') & year >= 1947 & year <= 1984),field,year,doi,journal)
keepd <- filter(keep,field=='demography')
keeps <- filter(keep,(journal %in% c("INTERNATIONAL JOURNAL OF SOCIOLOGY",
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
keep <- rbind(keepd,keeps)

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

#topics by field
keep <- merge(keep,docdoi,by='doi',all.x=TRUE)
keep$art <- 1
#demography
demog <- select(filter(keep,field=='demography'),-doi,-field,-doc,-journal)
b <- data.frame(year=integer(),topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(demog[,(i+1)])
  c <- as.data.frame(demog[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts))
  b <- rbind(as.data.frame(b),as.data.frame(a))
}
demog <- mutate(b,field='demography')
#sociology
soc <- select(filter(keep,field=='sociology'),-doi,-field,-doc,-journal)
b <- data.frame(year=integer(),topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(soc[,(i+1)])
  c <- as.data.frame(soc[,1])
  a <- cbind(c,a)
  names(a) <- c('year','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,topic),size=sum(size)/sum(arts))
  b <- rbind(as.data.frame(b),as.data.frame(a))
}
sociology <- mutate(b,field='sociology')

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

all <- rbind(demog,sociology)

#articles
arts <- select(keep,field,year,art) %>% group_by(year,field) %>%summarise(articles=sum(art))

#figure 2
png('./fig2.png',height=500,width=1000)
ggplot(data=arts,aes(x=year,y=articles,color=field)) + geom_line() + theme_bw() + 
  labs(x='Year',y='Number of Articles',title='Figure 2: Population-Related Articles by Journal Field, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()

#figure 3
dtops <- c(47,129,93,38,168,87) #topics more prevalent in demography
demog <- all %>% filter(topic %in% dtops) #keep only topics more prevalent in demography
#add topic lables
demog <- demog %>% mutate(top=ifelse(topic==47,'contraceptive method contraception methods contraceptives pill',
                                     ifelse(topic==129,'birth births parity interval months order intervals live',
                                            ifelse(topic==93,'size preferences preference children intentions desired ideal sons',
                                                   ifelse(topic==38,'family planning program programs studies bangladesh contraceptive',
                                                          ifelse(topic==168,'iud acceptors rates acceptance months continuation program users',
                                                                 ifelse(topic==87,'reproductive infertility menstrual reproduction cycle human days cycles','')))))))
png('./fig3.png',height=500,width=1000)
ggplot(data=demog,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(x='Year',y='Percent of Field',title='Figure 3: Topics More Prevalent in Demography, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()
#sociology
stops <- c(24,30,124) #topics more prevalent in sociology
soc <- all %>% filter(topic %in% stops) #keep only topics more prevalent in sociology
#add topic labels
soc <- soc %>% mutate(top=ifelse(topic==24,'items table sample scale behavior scores subjects significant',
                                 ifelse(topic==30,'areas city metropolitan segregation cities area residential central',
                                        ifelse(topic==124,'mobility occupational status occupation occupations prestige american',''))))
png('./fig4.png',height=250,width=1000)
ggplot(data=soc,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top) + theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(x='Year',y='Percent of Field',title='Figure 4: Topics More Prevalent in Sociology, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()   

#export data
write_csv(all,'./1947_1984_demog_soc.csv')
