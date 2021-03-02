library(tidyverse)
library(jsonlite)
library(lubridate)
library(zicolors)

inputlink <- "https://raw.githubusercontent.com/zidatalab/covid19dashboard/master/data/tabledata/impfsim_lieferungen.json" 

df <- fromJSON(inputlink) %>% tibble()  %>% filter(Verteilungsszenario=="Linearer Anstieg der Produktion in Q2") %>% 
  select(-Verteilungsszenario) %>% filter(Bundesland=="Gesamt")

plotdata <- df %>% group_by(hersteller) %>% arrange(hersteller,kw) %>% mutate(Datum=as_date("2020-01-01")+weeks(kw),
                                                                   Dosen=cumsum(dosen_kw)) %>% select(hersteller,Datum,Dosen)

theplot <- ggplot(plotdata) + aes(x=Datum,y=Dosen/1e6,fill=hersteller) + 
  geom_hline(yintercept = 83)+ geom_hline(yintercept = 83*2) + geom_area(position  ="stack",alpha=.9) + scale_x_date()+ scale_fill_zi("main4colors") + labs(y="Mio. Dosen",x="",fill="")+theme_minimal() +
  theme(legend.position = "bottom")

ggsave(plot=theplot,filename = "Liefermenge.png", width=120,height=120,units = "mm")