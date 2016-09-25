
#create tables and figures

library(dplyr)
library(readr)
library(ggplot2)

#read in topic modeling data
articles <- mutate(read_csv('./jstor/2015/articles.csv'),doi=gsub('/','_',doi))
docdoi <- read_csv('./jstor/2015/topic-docs-200.csv')
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
  words <- as.data.frame(text,stringsAsFactors=FALSE)
  names(words) <- c('words')
  words$topic <- i
  tops <- rbind(tops,words)
}

#Table 1
migration <- docdoi %>% filter(doi=='10.2307_2060344') #select example article
#reshape from wide to long
mig <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(migration[,i])
  names(a) <- c('size')
  a$topic <- i
  mig <- rbind(mig,a)
}
#select most prevalent topics
mig <- inner_join(mig,tops) %>% mutate(words=ifelse(topic %in% c(3,106,195,84),words,'all other topics')) 
mig <- mig %>% group_by(words) %>% summarise(proportion=sum(size))
t1 <- mig %>% filter(words=='all other topics')
t1 <- rbind(arrange(filter(mig,words!='all other topics'),-proportion),t1)
t1 <- t1 %>% select(proportion,topic=words)

#Figure 1
t3 <- docdoi %>% select(size=`topic 3`,doi) %>% mutate(topic=3) #older vocabulary of racial inequality
t71 <- docdoi %>% select(size=`topic 71`,doi) %>% mutate(topic=71) #newer vocabulary of racial inequalit
#combine data frames, add article metadata and topic words
race <- left_join(articles,left_join(rbind(t3,t71),tops)) %>% group_by(year,words) %>% summarise(size=sum(size)) %>% mutate(topic=factor(words))
race$topic <- factor(race$topic,levels=rev(levels(race$topic)))
png('./fig1.png',height=500,width=1000)
ggplot(data=race,aes(x=year,y=size,color=topic, order=-as.numeric(topic))) + geom_line() + theme_bw() +
  labs(x='Year',y='Number of Articles',title='Figure 1: Vocabularies of Racial Inequality',color='Topic:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom") +
  guides(color=guide_legend(nrow=2)) + scale_y_continuous(limits=c(0,10),breaks=c(0,2,4,6,8,10)) +
  scale_x_continuous(limits=c(1915,1984),breaks=c(1915,1935,1955,1975))
dev.off()

#1915-1946-----------------------------------------------------------------------------
#Read in additional article metadata
meta <- read_csv('./jstor/2015/meta.csv')
auths <- read_csv('./jstor/2015/authors.csv')
#identify articles published before 1947
b1947 <- merge(filter(meta,year<1947),auths,by='doi',x.all=TRUE) %>% arrange(author,year,journal,doi)
#identify demographers before 1947
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
ed <- filter(b1947,author %in% early) #articles by demographers
oth <- filter(b1947,!(author %in% early)) #articles by non-demographers

#articles by demographers before 1947
ear <- as.data.frame(unique(ed$doi))
names(ear) <- c('doi')
ear <- mutate(ear,doi=gsub('/','_',doi),field='demography')
ear <- merge(ear,select(articles,doi,year),by='doi',all.x=TRUE)
ear <- merge(ear,docdoi,by='doi',all.x=TRUE)

#articles by non-demographers before 1947
other <- as.data.frame(unique(oth$doi))
names(other) <- c('doi')
other <- mutate(other,doi=gsub('/','_',doi),field='other social sciences')
other <- merge(other,select(articles,doi,year),by='doi',all.x=TRUE)
other <- merge(other,docdoi,by='doi',all.x=TRUE)

#all articles before 1947
arts <- rbind(ear,other) %>% mutate(articles=1) %>% group_by(year,field) %>% summarize(articles=sum(articles))
png('./fig2.png',height=500,width=1000)
ggplot(data=arts,aes(x=year,y=articles,color=field)) + geom_line() + theme_bw() + 
  labs(x='Year',y='Number of Articles',title='Figure 2: Population-Related Articles by Author Field, 1915-1946',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom") 
dev.off()

#export pre-1947 data
#demography: reshape data from wide to long and sum over period
a <- data.frame(topic=numeric(),demog=numeric())
for(i in 1:200) {
  b <- as.data.frame(ear[,(i+3)])
  names(b) <- c('size')
  c <- b %>% mutate(arts=1,topic=i)
  a <- rbind(as.data.frame(a),as.data.frame(c))
}
a1 <- a %>% group_by(topic) %>% summarise(demog=sum(size)/sum(arts))
#non-demography: reshape data from wide to long and sum over period
a <- data.frame(topic=numeric(),other=numeric())
for(i in 1:200) {
  b <- as.data.frame(other[,(i+3)])
  names(b) <- c('size')
  c <- b %>% mutate(arts=1,topic=i)
  a <- rbind(as.data.frame(a),as.data.frame(c))
}
a2 <- a %>% group_by(topic) %>% summarise(other=sum(size)/sum(arts))
#join tables, add topic words, and export
d1947 <- inner_join(inner_join(a1,a2),tops)
write_csv(d1947,'./pre_1947_topics.csv')

#articles before 1947 by field
#reshape data from wide to long and sum by year
b <- data.frame(year=integer(),field=character(),topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(ear[,(i+3)])
  c <- ear %>% select(year,field)
  a <- cbind(c,a)
  names(a) <- c('year','field','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,field,topic),size=sum(size)/sum(arts))
  b <- rbind(as.data.frame(b),as.data.frame(a))
}
for(i in 1:200) {
  a <- as.data.frame(other[,(i+3)])
  c <- other %>% select(year,field)
  a <- cbind(c,a)
  names(a) <- c('year','field','size')
  a$arts <- 1
  a$topic <- i
  a <- summarise(group_by(a,year,field,topic),size=sum(size)/sum(arts))
  b <- rbind(as.data.frame(b),as.data.frame(a))
}

#three-year moving average
c <- data.frame(field=character(),topic=numeric(),size=numeric(),year=numeric())
for(i in 1916:1945) {
  d <- b %>% filter(year >= i-1 & year <= i+1) %>% group_by(field,topic) %>% summarise(size=mean(size)) %>% mutate(year=i)
  c <- rbind(as.data.frame(c),as.data.frame(d))
}

#Topics more prevalent in demography
demog <- c %>% filter(topic %in% c(164,51,189,121,129,47)) 
#create factor variable for topic words to control graphing order
demog <- demog %>% mutate(top=ifelse(topic==164,1,ifelse(topic==51,2,
                              ifelse(topic==189,3,ifelse(topic==121,4,
                              ifelse(topic==129,5,6))))))
demog$top <- factor(demog$top,labels=c('rate rates increase year total birth period million average table',
                                        'age mortality table estimates ages life method populations estimated',
                                        'demographic projections future size growth projection projected populations',
                                        'fertility demographic decline levels transition low control economic',
                                        'birth births parity interval months order intervals live conception born',
                                        'contraceptive method contraception methods contraceptives pill pregnancy'))
png('./fig3.png',height=1000,width=750)
ggplot(data=demog,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top,nrow=3,scales="free_y") + theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(x='Year',y='Percent of Field (3-year moving average)',title='Figure 3: Topics More Prevalent in Demography, 1915-1946',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()

#Topics more prevalent in other fields
other <- c %>% filter(topic %in% c(146,185,131,192,88,111)) 
#add topic words
other <- other%>% mutate(top=ifelse(topic==146,'marriage kinship kin sons son daughters father property residence',
                              ifelse(topic==185,'society cultural traditional culture system societies values life modern',
                              ifelse(topic==131,'girls woman young man girl boys love mother good boy',
                              ifelse(topic==192,'school education schools primary educational secondary girls teachers',
                              ifelse(topic==88,'immigration illegal aliens legal citizens citizenship alien status',
                              'japan trade japanese foreign economic exports countries investment'))))))
png('./fig4.png',height=1000,width=750)
ggplot(data=other,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top,nrow=3,scales="free_y") + theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(x='Year',y='Percent of Field (3-year moving average)',title='Figure 4: Topics More Prevalent in Other Social Sciences, 1915-1946',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()

#1947-1984------------------------------------------------
#identify articles published between 1947 and 1984
keep <- select(filter(articles,field %in% c('demography','sociology') & year >= 1947 & year <= 1984),field,year,doi,journal)
keepd <- filter(keep,field=='demography')
#select sociology journals
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

#check demography journals
djournals <- c()
demography <- filter(keep,field=='demography')$journal
for (i in 1:length(demography)) {
  if (!(demography[i] %in% djournals)) {
    djournals <- c(djournals,demography[i])
  }
}

#check sociology journals
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
#reshape data from wide to long
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
#reshape data from wide to long
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
#combine fields
all <- rbind(demog,sociology)

#figure 5
arts <- select(keep,field,year,art) %>% group_by(year,field) %>%summarise(articles=sum(art))
png('./fig5.png',height=500,width=1000)
ggplot(data=arts,aes(x=year,y=articles,color=field)) + geom_line() + theme_bw() + 
  labs(x='Year',y='Number of Articles',title='Figure 5: Population-Related Articles by Journal Field, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()

#figure 6
dtops <- c(47,129,93,38,168,87) #topics more prevalent in demography
demog <- all %>% filter(topic %in% dtops) #keep only topics more prevalent in demography
#add topic lables
demog <- demog %>% mutate(top=ifelse(topic==47,'contraceptive method contraception methods contraceptives pill',
                              ifelse(topic==129,'birth births parity interval months order intervals live',
                              ifelse(topic==93,'size preferences preference children intentions desired ideal sons',
                              ifelse(topic==38,'family planning program programs studies bangladesh contraceptive',
                              ifelse(topic==168,'iud acceptors rates acceptance months continuation program users',
                              ifelse(topic==87,'reproductive infertility menstrual reproduction cycle human days cycles','')))))))
png('./fig6.png',height=1000,width=750)
ggplot(data=demog,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top,nrow=3,scales="free_y") + theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(x='Year',y='Percent of Field',title='Figure 6: Topics More Prevalent in Demography, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()

#figure 7
stops <- c(24,30,124,185,135,43) #topics more prevalent in sociology
soc <- all %>% filter(topic %in% stops) #keep only topics more prevalent in sociology
#create factor variable to control graphing order
soc <- soc %>% mutate(top=ifelse(topic==24,1,ifelse(topic==30,2,
                          ifelse(topic==124,3,ifelse(topic==185,4,
                          ifelse(topic==135,5,6))))))
#add factor labels
soc$top=factor(soc$top, labels=c('items table sample scale behavior scores subjects significant',
                                'areas city metropolitan segregation cities area residential central',
                                'mobility occupational status occupation occupations prestige american',
                                'society cultural traditional culture system societies values life',
                                'marriage family adjustment marital marriage counseling living life',
                                'religious religion catholic church catholics attendance religiosity'))
png('./fig7.png',height=1000,width=750)
ggplot(data=soc,aes(x=year,y=size,color=field)) + geom_line() + 
  facet_wrap(~ top,nrow=3,scales="free_y") + theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(x='Year',y='Percent of Field',title='Figure 7: Topics More Prevalent in Sociology, 1947-1984',color='Field:') + 
  scale_color_manual(values=c("black", "gray50")) + theme(legend.position="bottom")
dev.off()   

#export data
#demography: reshape data from wide to long and sum over period
demog <- select(filter(keep,field=='demography'),-doi,-field,-doc,-journal)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(demog[,(i+1)])
  names(a) <- c('size')
  a <- a %>% mutate(arts=1,topic=i)
  b <- rbind(as.data.frame(b),as.data.frame(a))
}
b1 <- b %>% group_by(topic) %>% summarise(demography=sum(size)/sum(arts))
#sociology: reshape data from wide to long and sum over period
soc <- select(filter(keep,field=='sociology'),-doi,-field,-doc,-journal)
b <- data.frame(topic=numeric(),size=numeric())
for(i in 1:200) {
  a <- as.data.frame(soc[,(i+1)])
  names(a) <- c('size')
  a <- a %>% mutate(arts=1,topic=i)
  b <- rbind(as.data.frame(b),as.data.frame(a))
}
b2 <- b %>% group_by(topic) %>% summarise(sociology=sum(size)/sum(arts))
#join tables, add topic words, and export
d1984 <- inner_join(inner_join(b1,b2),tops)
write_csv(d1984,'./1947_1984_demog_soc.csv')
