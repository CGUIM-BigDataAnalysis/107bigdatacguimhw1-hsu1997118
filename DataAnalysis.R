library(dplyr)
library(readr)
library(knitr)


X103educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/103年各教育程度薪資分.csv")
X104educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/104年各教育程度薪資分.csv")
X105educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/105年各教育程度薪資分.csv")
X106educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/106年各教育程度薪資分.csv")

colnames(X103educate_salary) <- c("Year","Occupation","Regular_Salary","Regular_Salary_Gender",
                                  "Middle_School_Salary","Middle_School_Salary_Gender","Senior_High_Salary",
                                  "Senior_High_Salary_Gender","Junior_College_Salary","Junior_College_Salary_Gender",
                                  "College_Salary","College_Salary_Gender","Master_Salary","Master_Salary_Gender")
colnames(X106educate_salary) <- c("Year","Occupation","Regular_Salary","Regular_Salary_Gender",
                                "Middle_School_Salary","Middle_School_Salary_Gender","Senior_High_Salary",
                                 "Senior_High_Salary_Gender","Junior_College_Salary","Junior_College_Salary_Gender",
                                 "College_Salary","College_Salary_Gender","Master_Salary","Master_Salary_Gender")
colnames(X104educate_salary) <- c("Year","Occupation","Regular_Salary","Regular_Salary_Gender",
                                  "Middle_School_Salary","Middle_School_Salary_Gender","Senior_High_Salary",
                                  "Senior_High_Salary_Gender","Junior_College_Salary","Junior_College_Salary_Gender",
                                  "College_Salary","College_Salary_Gender","Master_Salary","Master_Salary_Gender")
colnames(X105educate_salary) <- c("Year","Occupation","Regular_Salary","Regular_Salary_Gender",
                                  "Middle_School_Salary","Middle_School_Salary_Gender","Senior_High_Salary",
                                  "Senior_High_Salary_Gender","Junior_College_Salary","Junior_College_Salary_Gender",
                                  "College_Salary","College_Salary_Gender","Master_Salary","Master_Salary_Gender")

#1. 
X103educate_salary$College_Salary <- as.numeric(X103educate_salary$College_Salary)
X106educate_salary$College_Salary <- as.numeric(X106educate_salary$College_Salary)

CollegeSalary103_106<- data.frame(Occupation=X103educate_salary$Occupation,
                                  CollegeSalary103=X103educate_salary$College_Salary,
                                  CollegeSalary106=X106educate_salary$College_Salary,
                                  stringsAsFactors=FALSE)
CollegeSalary103_106$Devide106By103<- CollegeSalary103_106$CollegeSalary106/CollegeSalary103_106$CollegeSalary103

compare103106<- CollegeSalary103_106 %>% 
  filter(Devide106By103>=1) %>% 
  select(Occupation,CollegeSalary103,CollegeSalary106,Devide106By103) %>% 
  arrange(desc(Devide106By103)) %>%
  head(10)

over5<- compare103106 %>% 
  filter(Devide106By103>1.05) %>% 
  select(Occupation,CollegeSalary103,CollegeSalary106,Devide106By103) %>% 
  arrange(desc(Devide106By103))

#產生一個變數名稱為occupation_cat的array
occupation_cat<-c()
for(n in 1:nrow(over5)){
  occupation_cat<-c(occupation_cat,strsplit(over5$Occupation,"-")[[n]][1])
}
SUM<-data.frame(Occupation=occupation_cat,stringsAsFactors = F)
SUM%>% group_by(Occupation) %>% summarise(Count=n()) 
#職業總數
table(occupation_cat)

#2.
X103educate_salary$College_Salary_Gender<-as.numeric(X103educate_salary$College_Salary_Gender)
X104educate_salary$College_Salary_Gender<-as.numeric(X104educate_salary$College_Salary_Gender)
X105educate_salary$College_Salary_Gender<-as.numeric(X105educate_salary$College_Salary_Gender)
X106educate_salary$College_Salary_Gender<-as.numeric(X106educate_salary$College_Salary_Gender)

SexRatio<- data.frame(Occupation=X106educate_salary$Occupation,
                      College103SexRatio=X103educate_salary$College_Salary_Gender,
                      College104SexRatio=X104educate_salary$College_Salary_Gender,
                      College105SexRatio=X105educate_salary$College_Salary_Gender,
                      College106SexRatio=X106educate_salary$College_Salary_Gender, 
                      stringsAsFactors = FALSE)

SexRatio$Average<-(SexRatio$College103SexRatio+SexRatio$College104SexRatio+
                     SexRatio$College105SexRatio+SexRatio$College106SexRatio)/4

SexRatio %>% filter(Average >100) %>%
select(Occupation,Average,College103SexRatio,College104SexRatio,College105SexRatio,College106SexRatio) %>% arrange(desc(Average))


SexRatio %>% filter(Average<100) %>%
       select(Occupation,Average,College103SexRatio,College104SexRatio,College105SexRatio,College106SexRatio) %>% arrange(Average)

#3.
New106educate_salary<-X106educate_salary
New106educate_salary$MasterDevideCollege<- New106educate_salary$Master_Salary/New106educate_salary$College_Salary
View(arrange(New106educate_salary,desc(MasterDevideCollege)) %>% head(10))

#4.
##有興趣的職業1.「專業_科學及技術服務業-專業人員」
filter(X106educate_salary,Occupation=="專業_科學及技術服務業-專業人員") %>% select(Occupation,College_Salary,Master_Salary)
##有興趣的職業2.「資訊及通訊傳播業-專業人員」
filter(X106educate_salary,Occupation=="資訊及通訊傳播業-專業人員") %>% select(Occupation,College_Salary,Master_Salary)
##有興趣的職業3.「金融及保險業-專業人員」
filter(X106educate_salary,Occupation=="金融及保險業-專業人員") %>% select(Occupation,College_Salary,Master_Salary)

##有興趣的職業1. 「專業_科學及技術服務業-專業人員」
filter(X106educate_salary,Occupation=="專業_科學及技術服務業-專業人員") %>% 
      select(Occupation,College_Salary,Master_Salary) %>% 
      mutate(`Master-Bachelor`=as.numeric(Master_Salary)-as.numeric(College_Salary))
##有興趣的職業2. 「資訊及通訊傳播業-專業人員」
filter(X106educate_salary,Occupation=="資訊及通訊傳播業-專業人員") %>% 
      select(Occupation,College_Salary,Master_Salary) %>%
      mutate(`Master-Bachelor`=as.numeric(Master_Salary)-as.numeric(College_Salary))
##有興趣的職業3. 「金融及保險業-專業人員」
filter(X106educate_salary,Occupation=="金融及保險業-專業人員") %>% 
       select(Occupation,College_Salary,Master_Salary) %>%
       mutate(`Master-Bachelor`=as.numeric(Master_Salary)-as.numeric(College_Salary))
