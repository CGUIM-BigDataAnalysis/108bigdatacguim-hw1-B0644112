#install.packages("magrittr")
hh
library(readr)
wage104_url<-read_csv("http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv")
wage107_url<-read_csv("C:/Users/m0470/OneDrive/桌面/107wages.csv")
library(dplyr)

#將104,107年的職業名稱統一
wage104_url$大職業別<-gsub("、|_",",",wage104_url$大職業別)
wage107_url$大職業別<-gsub("、|_",",",wage107_url$大職業別)
wage104_url$大職業別<-gsub("部門","",wage107_url$大職業別)

#Join 104和107年度表格
wage_table<-full_join(wage104_url,wage107_url,by="大職業別")

#將104,107年大學-薪資轉成numeric
wage_table$`大學-薪資.x`<-gsub("—|…"," ",wage_table$`大學-薪資.x`)
wage_table$`大學-薪資.x`<-as.numeric(wage_table$`大學-薪資.x`)

wage_table$`大學-薪資.y`<-gsub("—|…"," ",wage_table$`大學-薪資.y`)
wage_table$`大學-薪資.y`<-as.numeric(wage_table$`大學-薪資.y`)

#將104,107年大學-女/男的資料轉乘numeric
wage_table$`大學-女/男.x`<-gsub("—|…"," ",wage_table$`大學-女/男.x`)
wage_table$`大學-女/男.x`<-as.numeric(wage_table$`大學-女/男.x`)

wage_table$`大學-女/男.y`<-gsub("—|…"," ",wage_table$`大學-女/男.y`)
wage_table$`大學-女/男.y`<-as.numeric(wage_table$`大學-女/男.y`)

#將104,107年研究所-薪資轉呈numeric
wage_table$`研究所及以上-薪資`<-gsub("—|…"," ",wage_table$`研究所及以上-薪資`)
wage_table$`研究所及以上-薪資`<-as.numeric(wage_table$`研究所及以上-薪資`)

wage_table$`研究所-薪資`<-gsub("—|…"," ",wage_table$`研究所-薪資`)
wage_table$`研究所-薪資`<-as.numeric(wage_table$`研究所-薪資`)

#1
#計算107,104薪資提高比率並新增到欄位'大學薪資成長'
wage_table$大學薪資成長<-wage_table$`大學-薪資.y`/wage_table$`大學-薪資.x`

#107年度薪資較104年度薪資高的職業取前十名

wage_107over104<-wage_table %>%
                select(大職業別,大學薪資成長) %>%
                filter(大學薪資成長>1) %>%
                arrange(desc(大學薪資成長))%>%
                head(10)

#取提高超過5%的的職業
wage_add_over5<-wage_table %>%
  select(大職業別,大學薪資成長) %>%
  filter(大學薪資成長>1.05) 

#取出主要的職業類別種類
library(magrittr)
wage_add_over5$job_type<-
  strsplit(wage_add_over5$大職業別,"-")%>%
  sapply("[",1)
table(job_type)


#2
#wage_table_gender<-wage_table[,c(1,2,12,15,25)]

#取104男生薪資比女生薪資多的職業前十名

manwage_104<-wage_table %>%
             select(年度.x,大職業別,`大學-女/男.x`)%>%
             filter(`大學-女/男.x`<100)%>%
             arrange(`大學-女/男.x`)%>%
             head(10)

#取107男生薪資比女生薪資多的職業前十名

manwage_107<-wage_table %>%
             select(年度.y,大職業別,`大學-女/男.y`)%>%
             filter(`大學-女/男.y`<100)%>%
             arrange(`大學-女/男.y`)%>%
             head(10)

#取104年女生金資比男生多的職業前十名
womanwage_104<-wage_table %>%
               select(年度.x,大職業別,`大學-女/男.x`)%>%
               filter(`大學-女/男.x`>100)%>%
               arrange(desc(`大學-女/男.x`))%>%
               head(10)

#取107年女生金資比男生多的職業前十名
womanwage_107<-wage_table %>%
               select(年度.y,大職業別,`大學-女/男.y`)%>%
               filter(`大學-女/男.y`>100)%>%
               arrange(desc(`大學-女/男.y`))%>%
               head(10)

#3
#計算研究所薪資 / 大學研究所薪資成長到新增欄位'大學研究所薪資成長'
wage_table$大學研究所薪資成長<-wage_table$`研究所-薪資`/wage_table$`大學-薪資.y`

wage_compare<-wage_table %>%
              select(年度.y,大職業別,`大學-薪資.y`,`研究所-薪資`,大學研究所薪資成長)%>%
              arrange(desc(大學研究所薪資成長))%>%
              head(10)
              

wage_table$`研究所-薪資`
#薪資差異比例前十名名的資料
head(wage_table[order(wage_table$大學研究所薪資成長,decreasing = T),],10)  

#4
#職業別篩選
favorite_job<-wage_table[c(65,66,86,87,93),c(1:2,11,13,15,24,26)]

#研究所薪資與大學薪資差
favorite_job$wage_gap104<-favorite_job$`研究所及以上-薪資`-favorite_job$`大學-薪資.x`
favorite_job$wage_gap107<-favorite_job$`研究所-薪資`-favorite_job$`大學-薪資.y`


