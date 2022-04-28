library(EValue)
library(tidyverse)
library(ggrepel)

setwd('/Users/andy/Desktop/Lilly/Evalue')



d = read_csv('comp_cohort1_1.csv')
d$cohort = ifelse(d$cohort1 == 'Anti-IL-17A',1,0); table(d$cohort)
resp = 'pasi_prim_i'
trmt = 'cohort'
cnts = c('AGE','MNAPSI_SCORE')
cats = c('PSAYN','PRNONBIO','CURTRT2','CURTRT4')


rareV = F
suffix = ifelse(rareV,'rare','nonrare')

# d = read_csv('maincohort.csv')
# d$cohort = ifelse(d$cohort == 'Dexa',1,0); table(d$cohort)
# resp = 'death'
# trmt = 'cohort'
# cnts = c(NULL)
# cats = c('ageGroup2','nIAIDOnIndex')




cats = paste0('factor(',cats,')')

RRX = RRY = NULL
for(x in c(cnts,cats)){
  #print(x)
  #x = cats[1]
  fmX = paste0(trmt,' ~ ', x)
  m = glm(as.formula(fmX), family='binomial', data=d)
  ORX = exp(coef(m))[-1]
  RRX = c(RRX,toRR(OR(ORX, rare = rareV),rare = rareV))
  
  fmY = paste0(resp,'~',x)
  m = glm(as.formula(fmY), family='binomial', data=d)
  ORY = exp(coef(m))[-1]
  RRY = c(RRY,toRR(OR(ORY, rare = rareV),rare = rareV))
}

names(RRX) = names(RRY) = str_replace_all(names(RRX),pattern='factor\\(',replacement='')
names(RRX) = names(RRY) = str_replace_all(names(RRX),'\\)','_')

RRX = ifelse(RRX<1,1/RRX,RRX)
RRY = ifelse(RRY<1,1/RRY,RRY)

v = names(RRX)

xy = tibble(
  v = v,
  #v_desc = v,
  v_desc = c('Age in Years',
             'Baseline mNAPSI Total score',
             'Presence of Psoriatic Arthritis',
             'Prior treatment with biologics',
             'Current Topical Agent',
             'Current Non-biologic Systemic Therapy'),
  x = RRX,
  y = RRY
)

# m = data.frame(
#   point = c(1.075639,1.360877),
#   lower = c(1.043552,1.256738),
#   upper = c(1.1077,NA)
# )
m = as.data.frame(evalues.OR(1.933,1.556,2.387,rare=rareV))
#write.csv(x=m,file=paste0('m_',suffix,'.csv'))

xmax = 10


RR = m[1,1]
e_curve = tibble(
  x = seq(RR, xmax, 0.0001),
  y = RR * (RR - 1)/(x - RR) + RR
) %>% filter(y<xmax,x<xmax)
#e_curve = e_curve %>% filter(y<=10)

RR = m[1,2]
e_curve_low = tibble(
  x = seq(RR, xmax, 0.0001),
  y = RR * (RR - 1)/(x - RR) + RR
) %>% filter(y<xmax,x<xmax)

RR1 = m[1,1]
RR2 = m[1,2]
x1 = seq(RR1, xmax, 0.0001)
x2 = seq(RR2, xmax, 0.0001)
y1 = RR1 * (RR1 - 1)/(x1 - RR1) + RR1
y2 = RR2 * (RR2 - 1)/(x2 - RR2) + RR2
y21 = RR1 * (RR1 - 1)/(x2 - RR1) + RR1
y2 = ifelse(y2>y21,y21,y2)

shade = tibble(
  x = c(x2),
  y = c(y2)
)

m = round(m,2)


ggplot() + 
  geom_point(data=xy,aes(x,y)) + 
   geom_text_repel(data=xy,aes(x,y,label=v_desc),size=3,
                   box.padding = 3,
                   label.padding = 3,
                   max.iter	= 1000,
                   max.time = 5,
                   max.overlaps = getOption("ggrepel.max.overlaps", default = 30))+
  #geom_polygon(data=shade,aes(x=x,y=y),fill='pink',alpha=0.3) + 
  
  
  #geom_text(data=xy,aes(x,y,label=v_desc),size=1.9,hjust=-0.05,vjust=0.3)+
  
  geom_line(data=e_curve,aes(x,y),color='red',alpha=0.4,size=1) + 
  geom_point(data=m,aes(x=m[2,1],y=m[2,1]),color='red') +
  geom_text(data=m,aes(x=m[2,1],y=m[2,1],label=paste0('E-value (from FMA OR): (',m[2,1],',',m[2,1],')')),size=3.5,vjust=-1,fontface='bold',color='red')+
  
  geom_line(data=e_curve_low,aes(x,y),color='blue',alpha=0.4,size=1) + 
  geom_point(data=m,aes(x=m[2,2],y=m[2,2]),color='blue') +
  geom_text(data=m,aes(x=m[2,2],y=m[2,2],label=paste0('E-value (lower CI from FMA OR): (',m[2,2],',',m[2,2],')')),size=3.5,vjust=1.5,fontface='bold',color='blue')+
  
  xlab('RR(Treatment~Variable)') + 
  ylab('RR(Outcome~Variable)') +
  
  #scale_x_continuous(breaks=seq(0.75,2,0.5),limits = c(0.75,2)) +
  #scale_y_continuous(breaks=seq(0,4,0.5),limits = c(0,4)) + 
  
  scale_x_continuous(breaks=seq(0,3,0.5),limits = c(0,3)) +
  scale_y_continuous(breaks=seq(0,3,0.5),limits = c(0,3)) + 
  
  theme_bw()

ggsave(paste0('evalue_',suffix,'.pdf'),width=12,height=7)



