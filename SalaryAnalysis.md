107-2 大數據分析方法 作業一
================
許芮萍/B0544113

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和106年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
#利用readr()套件讀取csv檔
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(knitr)

#讀取103年度各教育程度別初任人員的薪資資料
X103educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/103年各教育程度薪資分.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#讀取104年度各教育程度別初任人員的薪資資料
X104educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/104年各教育程度薪資分.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#讀取105年度各教育程度別初任人員的薪資資料
X105educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/105年各教育程度薪資分.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#讀取106年度各教育程度別初任人員的薪資資料
X106educate_salary <- read_csv("~/Downloads/A17000000J-020066-Qod/106年各教育程度薪資分.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
##將103~106的資料表的欄位改成英文。 （以下為欄位中英對照）
#Year=年度、ccupation=大職業別、Regular_Salary=經常性薪資-薪資、Regular_Salary_Gender=經常性薪資-女/男
#Middle_School_Salary=國中及以下-薪資、Middle_School_Salary_Gender=國中及以下-女/男、Senior_High_Salary=高中或高職-薪資
#Senior_High_Salary_Gender=高中或高職-女/男、Junior_College_Salary=專科-薪資、Junior_College_Salary_Gender=專科-女/男
#College_Salary=大學-薪資、College_Salary_Gender=大學-女/男、Master_Salary=研究所及以上-薪資、Master_Salary_Gender=研究所及以上-女/男

colnames(X103educate_salary) <- c("Year","Occupation","Regular_Salary","Regular_Salary_Gender",
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
colnames(X106educate_salary) <- c("Year","Occupation","Regular_Salary","Regular_Salary_Gender",
                                "Middle_School_Salary","Middle_School_Salary_Gender","Senior_High_Salary",
                                 "Senior_High_Salary_Gender","Junior_College_Salary","Junior_College_Salary_Gender",
                                 "College_Salary","College_Salary_Gender","Master_Salary","Master_Salary_Gender")
```

-   結合103年度和106年度的資料，其中包含「職業類別、103年度大學薪資、106年度大學薪資」欄位
    -   將一個資料表中的字串放到新的資料表中時，character會被轉成factor。所以要用**stringAsFactors=FALSE**才能確保資料型態為**character**
    -   新增一名稱為CollegeSalary103\_106的資料表，比較103年度和106年度的大學薪資
    -   欄位Occupation為「職業類別」、欄位CollegeSalary103為「103年度大學薪資」、欄位CollegeSalary106為「106年度大學薪資」

``` r
CollegeSalary103_106<- data.frame(Occupation=X103educate_salary$Occupation,
                                  CollegeSalary103=X103educate_salary$College_Salary,
                                  CollegeSalary106=X106educate_salary$College_Salary,
                                  stringsAsFactors=FALSE)
```

計算出106年除以103年的薪資比例
------------------------------

-   說明：
    -   在CollegeSalary103\_106中新增一個名稱為Devide106By103（103年和106年薪資比例）的欄位
    -   將106年度的大學薪資(CollegeSalary106) 除 103年度的大學薪資(CollegeSalary103)，可得出薪資比例。
    -   利用**head()**列出**尚未排序**的前10項資料
-   執行結果：
    -   Devide106By103欄位紀錄各項職業在103年和106年的薪資比例

``` r
CollegeSalary103_106$Devide106By103<- CollegeSalary103_106$CollegeSalary106/CollegeSalary103_106$CollegeSalary103
kable(head(CollegeSalary103_106,10))
```

| Occupation                                    |  CollegeSalary103|  CollegeSalary106|  Devide106By103|
|:----------------------------------------------|-----------------:|-----------------:|---------------:|
| 工業及服務業部門                              |             27193|             28446|        1.046078|
| 工業及服務業部門-專業人員                     |             30449|             32108|        1.054485|
| 工業及服務業部門-技術員及助理專業人員         |             27383|             28647|        1.046160|
| 工業及服務業部門-事務支援人員                 |             25664|             26781|        1.043524|
| 工業及服務業部門-服務及銷售工作人員           |             25745|             26644|        1.034919|
| 工業及服務業部門-技藝、機械設備操作及組裝人員 |             25704|             26842|        1.044273|
| 工業及服務業部門-基層技術工及勞力工           |                NA|                NA|              NA|
| 工業部門                                      |             27151|             28263|        1.040956|
| 工業部門-專業人員                             |             30215|             31775|        1.051630|
| 工業部門-技術員及助理專業人員                 |             27098|             28126|        1.037936|

### 106年度薪資較103年度薪資高的職業有哪些?

-   說明＆步驟：
    -   Step1: 先用**filter()**篩選出所有符合條件**觀察值**
        -   因為要找106年薪資大於103薪資年，就要找出106薪資/103薪資**大於1**的職業
    -   Step2: 再用**select()**挑選出需要的**欄位**
    -   Step3: 最後用**arrange()**將106/103（Devide106By103欄位）的結果進行排序，**desc()**為由大到小的排序
    -   Step4: 利用**head()**列出薪資差距最大的前10項職業
-   執行結果：
    -   可看出Divide106By103這個欄位的值都大於1。 分析出來的結果都是CollegeSalary106大於CollegeSalary103
    -   得出的結果（Divide106By103）越大，代表106年度和103年度的薪資差越大

``` r
compare103106<- CollegeSalary103_106 %>% 
                filter(Devide106By103>=1) %>% 
                select(Devide106By103,Occupation,CollegeSalary103,CollegeSalary106) %>% 
                arrange(desc(Devide106By103)) 
kable(head(compare103106,10))
```

|  Devide106By103| Occupation                                |  CollegeSalary103|  CollegeSalary106|
|---------------:|:------------------------------------------|-----------------:|-----------------:|
|        1.131278| 其他服務業-技術員及助理專業人員           |             24688|             27929|
|        1.129498| 住宿及餐飲業-服務及銷售工作人員           |             22564|             25486|
|        1.129402| 用水供應及污染整治業-技術員及助理專業人員 |             27944|             31560|
|        1.113654| 專業、科學及技術服務業-專業人員           |             29977|             33384|
|        1.109735| 其他服務業-技藝、機械設備操作及組裝人員   |             24222|             26880|
|        1.109005| 營造業-服務及銷售工作人員                 |             27164|             30125|
|        1.103448| 其他服務業-專業人員                       |             29000|             32000|
|        1.103263| 資訊及通訊傳播業-專業人員                 |             28839|             31817|
|        1.097758| 不動產業-專業人員                         |             30637|             33632|
|        1.095684| 教育服務業-事務支援人員                   |             22334|             24471|

### 提高超過5%的的職業有哪些?

-   說明＆步驟：
    -   Step1: 已經由上述compare103106表中得到106年度薪資大於103度薪資的職業。 再用此表分析出薪資提高超過5%的職業又有哪些。
    -   Step2: 利用**filter()**篩選出所有符合條件**觀察值**，再用**select()**挑選出需要的**欄位**
    -   Step3: 利用arrange()將結果進行排序，desc()為由大到小的排序
    -   Step4: 利用head()列出前10項資料
-   執行結果：
    -   可看出Divide106By103這個欄位的值都**大於1.05**
    -   得出的值（Divide106By103欄位）越大，代表106年度和103年度的薪資差越大
    -   可看到差距前10大的職業別和薪資差距

``` r
over5<- compare103106 %>% 
        filter(Devide106By103>1.05) %>% 
        select(Occupation,CollegeSalary103,CollegeSalary106,Devide106By103) %>% 
        arrange(desc(Devide106By103))
kable(head(over5,10))
```

| Occupation                                |  CollegeSalary103|  CollegeSalary106|  Devide106By103|
|:------------------------------------------|-----------------:|-----------------:|---------------:|
| 其他服務業-技術員及助理專業人員           |             24688|             27929|        1.131278|
| 住宿及餐飲業-服務及銷售工作人員           |             22564|             25486|        1.129498|
| 用水供應及污染整治業-技術員及助理專業人員 |             27944|             31560|        1.129402|
| 專業、科學及技術服務業-專業人員           |             29977|             33384|        1.113654|
| 其他服務業-技藝、機械設備操作及組裝人員   |             24222|             26880|        1.109735|
| 營造業-服務及銷售工作人員                 |             27164|             30125|        1.109005|
| 其他服務業-專業人員                       |             29000|             32000|        1.103448|
| 資訊及通訊傳播業-專業人員                 |             28839|             31817|        1.103263|
| 不動產業-專業人員                         |             30637|             33632|        1.097758|
| 教育服務業-事務支援人員                   |             22334|             24471|        1.095684|

### 主要的職業種別是哪些種類呢?

-   說明＆步驟:
    -   Step1: 建立一個變數名稱為occupation\_cat的陣列
    -   Step2: 利用for()迴圈讀取所有薪資提高超過5%的資料表（over5）
    -   Step3: 利用strsplit(欲切割之內容,"分割條件")，取出職業類別中"-"之前的內容
        -   List（列表）取值要用到**雙中括號\[\[ \]\]**
    -   Step4: 產生一個新的資料表SUM，存放occupation\_cat的所有資料
    -   Step5: 利用group\_by把所有職業組成群組
    -   Step6: 利用summarise(n())算出所有職業別出現的次數

``` r
occupation_cat<-c()
for(n in 1:nrow(over5)){
  occupation_cat<-c(occupation_cat,strsplit(over5$Occupation,"-")[[n]][1])
}
SUM<-data.frame(Occupation=occupation_cat,stringsAsFactors = F)
```

-   執行結果：
    -   可看出主要的職業種類別包含：不動產業、電力及燃氣供應業、服務業部門、工業部門、工業及服務業部門、教育服務業、 金融及保險業、礦業及土石採取業、其他服務業、醫療保健服務業、藝術＿娛樂及休閒服務業、營造業、用水供應及污染整治業、 運輸及倉儲業、支援服務業、製造業、住宿及餐飲業、專業＿科學及技術服務業、資訊及通訊傳播業
    -   在這些職業類別中「用水供應及污染整治業」的數量又為最多

``` r
kable(SUM%>% group_by(Occupation) %>% summarise(Count=n()) %>% arrange(desc(Count)))
```

| Occupation             |  Count|
|:-----------------------|------:|
| 用水供應及污染整治業   |      6|
| 服務業部門             |      5|
| 教育服務業             |      5|
| 其他服務業             |      5|
| 專業、科學及技術服務業 |      5|
| 資訊及通訊傳播業       |      5|
| 運輸及倉儲業           |      4|
| 住宿及餐飲業           |      4|
| 藝術、娛樂及休閒服務業 |      3|
| 營造業                 |      3|
| 支援服務業             |      3|
| 電力及燃氣供應業       |      2|
| 醫療保健服務業         |      2|
| 不動產業               |      1|
| 工業部門               |      1|
| 工業及服務業部門       |      1|
| 金融及保險業           |      1|
| 礦業及土石採取業       |      1|
| 製造業                 |      1|

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

-   程式碼說明&步驟：
    -   男女薪資比例看法：
        -   女薪資/男薪資\*100 &gt; 100 則是女性薪資**&gt;**男性薪資
        -   女薪資/男薪資\*100 &lt; 100 則是女性薪資**&lt;**男性薪資
    -   Step1: 利用rbind建立一個103~106年的薪資對照表，名稱為bind103\_106
    -   Step2: 再建立一個名稱為SexRatio的資料表，欄位包含Year（各年）、Occupation（職業）、SexRatio（薪資性別比）
    -   Step3: 將一個資料表中的字串放到新的資料表中時，character會被轉成factor。所以要用**stringAsFactors=FALSE**才能確保資料型態為character

``` r
bind103_106<- rbind(X103educate_salary,X104educate_salary,
                    X105educate_salary,X106educate_salary)
SexRatio<-data.frame(Year=bind103_106$Year,Occupation=bind103_106$Occupation,
                         SexRatio=bind103_106$College_Salary_Gender,
                         stringsAsFactors = FALSE)
```

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

-   程式碼說明＆步驟：
    -   Step1: 用filter()篩選出103~106年度男女薪資比例。要篩選SexRatio小於100的資料
    -   Step2: 再用select()選出要顯示的欄位
    -   Step3: 再用用arrange()由**小到大**排序，**比例數值越小男性薪資越高於女性**
    -   Step4: 最後用head()顯示出差距越大的前10項職業
-   執行結果：
    -   可觀察到「礦業及土石採取業-技藝\_機械設備操作及組裝人員」、「電力及燃氣供應業-技藝\_機械設備操作及組裝人員」...等較需**「體力、 勞動力」**的職業，男性薪資比女性薪資來的高最多

``` r
 kable(SexRatio %>%
       filter(SexRatio<100) %>% 
       select(Year,Occupation,SexRatio) %>% 
       arrange(SexRatio) %>% 
       head(10))
```

|  Year| Occupation                                    |  SexRatio|
|-----:|:----------------------------------------------|---------:|
|  2014| 礦業及土石採取業-技藝、機械設備操作及組裝人員 |     84.97|
|  2014| 教育服務業-技藝、機械設備操作及組裝人員       |     88.49|
|  2014| 其他服務業-技術員及助理專業人員               |     89.36|
|  2016| 不動產業-技藝、機械設備操作及組裝人員         |     91.38|
|  2015| 電力及燃氣供應業-技藝、機械設備操作及組裝人員 |     91.69|
|  2014| 電力及燃氣供應業-技藝、機械設備操作及組裝人員 |     91.77|
|  2015| 教育服務業-服務及銷售工作人員                 |     91.90|
|  2015| 礦業及土石採取業-技術員及助理專業人員         |     92.42|
|  2014| 礦業及土石採取業-服務及銷售工作人員           |     92.57|
|  2015| 礦業及土石採取業-技藝、機械設備操作及組裝人員 |     93.10|

### 哪些行業女生薪資比男生薪資多?

-   程式碼說明＆步驟：
    -   Step1: 用filter()篩選出103~106年度男女薪資比例。要篩選SexRatio大於100的資料
    -   Step2: 用select()選出要顯示的欄位
    -   Step3: 用arrange()由**大到小**排序，比例數值越大女性薪資越高於男性
    -   Step4: 用head()顯示出差距越大的前10項職業
-   執行結果：
    -   可看出女性薪資大於男性的薪資的職業並不多。只有三個「資訊及通訊傳播業-服務及銷售工作人員」、「專業\_科學及技術服務業-技藝\_機械設備操作及組裝人員」、「金融及保險業-專業人員」

``` r
kable(filter(SexRatio,SexRatio >100)%>% 
       select(Year,Occupation,SexRatio)%>% 
       arrange(desc(SexRatio)))
```

|  Year| Occupation                                          |  SexRatio|
|-----:|:----------------------------------------------------|---------:|
|  2017| 資訊及通訊傳播業-服務及銷售工作人員                 |    100.33|
|  2015| 專業、科學及技術服務業-技藝、機械設備操作及組裝人員 |    100.26|
|  2016| 金融及保險業-專業人員                               |    100.11|

研究所薪資差異
--------------

以106年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#建立一個資料表，存106年度的資料
New106educate_salary<-data.frame(Year=X106educate_salary$Year,
                                 Occupation=X106educate_salary$Occupation,
                                 College_Salary=X106educate_salary$College_Salary,
                                 Master_Salary=X106educate_salary$Master_Salary,
                                 stringsAsFactors = FALSE)
```

-   程式碼說明＆步驟：
    -   Step1: 建立一個名稱為MasterDevideCollege的欄位
    -   Step2: 將研究所薪資（Master\_Salary）除 大學薪資（College\_Salary），算出薪資比例
    -   Step3: 用arrange()排序，desc()將MasterDevideCollege欄位從大到小排序
    -   Step4: 列出前10列資料
-   執行結果：
    -   比例越高，研究所的薪資和大學的薪資**差距越大**。
    -   從結果可以發現**「礦業及土石採集業-事物支援人員」**的比例最大，由此可以此職業念研究所最划算

``` r
New106educate_salary$MasterDevideCollege<-New106educate_salary$Master_Salary/New106educate_salary$College_Salary
kable(New106educate_salary %>% 
      arrange(desc(MasterDevideCollege)) %>% 
      head(10))
```

|  Year| Occupation                          |  College\_Salary|  Master\_Salary|  MasterDevideCollege|
|-----:|:------------------------------------|----------------:|---------------:|--------------------:|
|  2017| 礦業及土石採取業-事務支援人員       |            24815|           30000|             1.208946|
|  2017| 專業\_科學及技術服務業              |            29648|           35666|             1.202982|
|  2017| 其他服務業-技術員及助理專業人員     |            27929|           33500|             1.199470|
|  2017| 專業\_科學及技術服務業-事務支援人員 |            27035|           32234|             1.192306|
|  2017| 批發及零售業                        |            27611|           32910|             1.191916|
|  2017| 製造業                              |            28155|           33458|             1.188350|
|  2017| 藝術\_娛樂及休閒服務業-事務支援人員 |            24970|           29657|             1.187705|
|  2017| 工業部門                            |            28263|           33448|             1.183455|
|  2017| 工業及服務業部門                    |            28446|           33633|             1.182346|
|  2017| 服務業部門                          |            28715|           33922|             1.181334|

我有興趣的職業別薪資狀況分析
----------------------------

### 有興趣的職業別篩選，呈現薪資

-   感興趣的職業有：
    -   專業\_科學及技術服務業-專業人員
    -   資訊及通訊傳播業-專業人員
    -   金融及保險業-專業人員
-   程式碼說明＆步驟：
    -   Step1: 先利用**filter()**選出自己感興趣的**觀察值**
    -   Step2: 再用**select()**篩選出要呈現出來的**欄位**
    -   Step3: 利用**mutate()**新增一個Master-Bachelor（研究所薪資-大學薪資）欄位，來看薪資差異
-   執行結果： -大學畢業(Bachelor)的薪資跟我預期的差不多，但碩士畢業(Master)所得到的薪資比我預期的還來的低一些，我認為研究所薪資應該要比大學薪資高出7000~10000。

``` r
##有興趣的職業1.「專業_科學及技術服務業-專業人員」

kable(filter(X106educate_salary,Occupation=="專業_科學及技術服務業-專業人員") %>% 
      select(Year,Occupation,College_Salary,Master_Salary))
```

|  Year| Occupation                      |  College\_Salary| Master\_Salary |
|-----:|:--------------------------------|----------------:|:---------------|
|  2017| 專業\_科學及技術服務業-專業人員 |            33384| 38415          |

``` r
##有興趣的職業2.「資訊及通訊傳播業-專業人員」

kable(filter(X106educate_salary,Occupation=="資訊及通訊傳播業-專業人員") %>% 
      select(Year,Occupation,College_Salary,Master_Salary))
```

|  Year| Occupation                |  College\_Salary| Master\_Salary |
|-----:|:--------------------------|----------------:|:---------------|
|  2017| 資訊及通訊傳播業-專業人員 |            31817| 36545          |

``` r
##有興趣的職業3.「金融及保險業-專業人員」

kable(filter(X106educate_salary,Occupation=="金融及保險業-專業人員") %>% 
      select(Year,Occupation,College_Salary,Master_Salary))
```

|  Year| Occupation            |  College\_Salary| Master\_Salary |
|-----:|:----------------------|----------------:|:---------------|
|  2017| 金融及保險業-專業人員 |            33646| 38542          |

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
##有興趣的職業1. 「專業_科學及技術服務業-專業人員」

kable(filter(X106educate_salary,Occupation=="專業_科學及技術服務業-專業人員") %>% 
      select(Year,Occupation,College_Salary,Master_Salary) %>% 
      mutate(`Master-Bachelor`=as.numeric(Master_Salary)-as.numeric(College_Salary)))
```

|  Year| Occupation                      |  College\_Salary| Master\_Salary |  Master-Bachelor|
|-----:|:--------------------------------|----------------:|:---------------|----------------:|
|  2017| 專業\_科學及技術服務業-專業人員 |            33384| 38415          |             5031|

``` r
##有興趣的職業2. 「資訊及通訊傳播業-專業人員」

kable(filter(X106educate_salary,Occupation=="資訊及通訊傳播業-專業人員") %>% 
      select(Year,Occupation,College_Salary,Master_Salary) %>%
      mutate(`Master-Bachelor`=as.numeric(Master_Salary)-as.numeric(College_Salary)))
```

|  Year| Occupation                |  College\_Salary| Master\_Salary |  Master-Bachelor|
|-----:|:--------------------------|----------------:|:---------------|----------------:|
|  2017| 資訊及通訊傳播業-專業人員 |            31817| 36545          |             4728|

``` r
##有興趣的職業3. 「金融及保險業-專業人員」

kable(filter(X106educate_salary,Occupation=="金融及保險業-專業人員") %>% 
      select(Year,Occupation,College_Salary,Master_Salary) %>%
      mutate(`Master-Bachelor`=as.numeric(Master_Salary)-as.numeric(College_Salary)))
```

|  Year| Occupation            |  College\_Salary| Master\_Salary |  Master-Bachelor|
|-----:|:----------------------|----------------:|:---------------|----------------:|
|  2017| 金融及保險業-專業人員 |            33646| 38542          |             4896|

-   執行結果說明：
    -   觀察**Master-Bachelor**欄位可發現我所感興趣的職業類別，研究所畢業和大學畢業的薪資大約差了5000元。這個差距雖然比我預期來的小，但這個結果**並不會改變我未來是否要讀研究所的決定**。
