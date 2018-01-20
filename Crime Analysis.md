In this project, I will use exploratory data analysis (EDA) to analyze the crime data from San Francisco.

1: How many rows and columns of data do I have?

``` r
library(data.table)
SFPD<-fread("SFPD_Incidents_from_January_2003.csv")
```

    ## 
    Read 39.6% of 2047932 rows
    Read 69.3% of 2047932 rows
    Read 2047932 rows and 13 (of 13) columns from 0.391 GB file in 00:00:04

    ## Warning in require_bit64(): Some columns are type 'integer64' but
    ## package bit64 is not installed. Those columns will print as strange
    ## looking floating point data. There is no need to reload the data. Simply
    ## install.packages('bit64') to obtain the integer64 print method and print
    ## the data again.

``` r
dim(SFPD)
```

    ## [1] 2047932      13

2: How many different types of crime categories are there?

``` r
length(unique(SFPD$Category))
```

    ## [1] 39

``` r
print(unique(SFPD$Category))
```

    ##  [1] "NON-CRIMINAL"                "ROBBERY"                    
    ##  [3] "ASSAULT"                     "SECONDARY CODES"            
    ##  [5] "VANDALISM"                   "BURGLARY"                   
    ##  [7] "LARCENY/THEFT"               "DRUG/NARCOTIC"              
    ##  [9] "WARRANTS"                    "VEHICLE THEFT"              
    ## [11] "OTHER OFFENSES"              "WEAPON LAWS"                
    ## [13] "ARSON"                       "MISSING PERSON"             
    ## [15] "DRIVING UNDER THE INFLUENCE" "SUSPICIOUS OCC"             
    ## [17] "RECOVERED VEHICLE"           "DRUNKENNESS"                
    ## [19] "TRESPASS"                    "FRAUD"                      
    ## [21] "DISORDERLY CONDUCT"          "SEX OFFENSES, FORCIBLE"     
    ## [23] "FORGERY/COUNTERFEITING"      "KIDNAPPING"                 
    ## [25] "EMBEZZLEMENT"                "STOLEN PROPERTY"            
    ## [27] "LIQUOR LAWS"                 "FAMILY OFFENSES"            
    ## [29] "LOITERING"                   "BAD CHECKS"                 
    ## [31] "TREA"                        "GAMBLING"                   
    ## [33] "RUNAWAY"                     "BRIBERY"                    
    ## [35] "PROSTITUTION"                "PORNOGRAPHY/OBSCENE MAT"    
    ## [37] "SEX OFFENSES, NON FORCIBLE"  "SUICIDE"                    
    ## [39] "EXTORTION"

3: Use the **ggplot** package to plot the total incidence of crime over time by year.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday,
    ##     week, yday, year

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
SFPD$Cdate<-mdy(SFPD$Date)
SFPD$Cyear<-year(SFPD$Cdate)
SFPD$Count<-1
df<-aggregate(Count~Cyear,SFPD,sum)

library(ggplot2)
ggplot(df,aes(x=Cyear,y=Count))+geom_line(colour="cornflowerblue",lwd=1)+ggtitle("Total Incidence of Crime by Year")+xlab("Year")+ylab("Number of Incidence")
```
Please refer to the Total Incidence of Crime by Year chart.


4: Create a table to show the frequency of all crimes over time. Since all crimes may not occur every year, missing values need to be handled.

``` r
library(plyr)
```

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     here

``` r
x<-xtabs(~Cyear+Category,SFPD)
head(x,n=15)
```

    ##       Category
    ## Cyear  ARSON ASSAULT BAD CHECKS BRIBERY BURGLARY DISORDERLY CONDUCT
    ##   2003   293   13461        134      28     6047                886
    ##   2004   280   12899         79      39     6753                814
    ##   2005   231   11601        101      38     7071                687
    ##   2006   240   12449         87      36     7004                521
    ##   2007   246   12518         69      56     5454                581
    ##   2008   248   12681         78      49     5679                789
    ##   2009   222   12284         64      46     5379                956
    ##   2010   209   12387         54      61     4966                827
    ##   2011   203   12279         45      47     4987                762
    ##   2012   237   12181         49      64     6243                704
    ##   2013   248   12580         26      69     6195                464
    ##   2014   253   12402         34      56     6066                345
    ##   2015   311   13115         38      73     5931                551
    ##   2016   286   13546         34      65     5790                655
    ##   2017    68    3055          9      19     1432                100
    ##       Category
    ## Cyear  DRIVING UNDER THE INFLUENCE DRUG/NARCOTIC DRUNKENNESS EMBEZZLEMENT
    ##   2003                         289          9917         662          240
    ##   2004                         244          9897         600          201
    ##   2005                         196          8533         636          197
    ##   2006                         266          9069         703          229
    ##   2007                         313         10560         671          269
    ##   2008                         408         11648         710          242
    ##   2009                         579         11950         804          214
    ##   2010                         458          9205         706          155
    ##   2011                         470          6935         652          168
    ##   2012                         421          6444         644          169
    ##   2013                         448          6775         954          172
    ##   2014                         381          5408         620          150
    ##   2015                         430          4254         576          184
    ##   2016                         376          4237         465          157
    ##   2017                          84           847          78           29
    ##       Category
    ## Cyear  EXTORTION FAMILY OFFENSES FORGERY/COUNTERFEITING FRAUD GAMBLING
    ##   2003        43             116                   2320  3285       27
    ##   2004        57             109                   2292  2745       25
    ##   2005        34              64                   2209  2494       29
    ##   2006        49              89                   2306  2599       28
    ##   2007        41              79                   2592  2349       37
    ##   2008        52              72                   2468  2560       27
    ##   2009        92              83                   2209  2513       28
    ##   2010        58              95                   1089  2656       10
    ##   2011        41              58                    820  2756       17
    ##   2012        38              77                    990  2698       18
    ##   2013        30              76                    913  2752       22
    ##   2014        32              89                    750  2988        8
    ##   2015        39              66                    763  3136       29
    ##   2016        59              53                    613  2617       20
    ##   2017        14              10                    107   507        3
    ##       Category
    ## Cyear  KIDNAPPING LARCENY/THEFT LIQUOR LAWS LOITERING MISSING PERSON
    ##   2003        319         26393         281       175           3699
    ##   2004        277         24505         350       236           3675
    ##   2005        276         25319         278       273           3703
    ##   2006        318         27352         248        96           4253
    ##   2007        345         25770         401       430           4024
    ##   2008        345         25807         499       441           4335
    ##   2009        478         25585         394       126           3657
    ##   2010        355         24446         324       187           4513
    ##   2011        421         25905         261       124           4758
    ##   2012        282         30976         226       135           4372
    ##   2013        528         36412         227        54           4485
    ##   2014        523         38003         180        36           4735
    ##   2015        341         42068         166        27           4653
    ##   2016        257         40364         156        42           4311
    ##   2017         47         10278          18         5            902
    ##       Category
    ## Cyear  NON-CRIMINAL OTHER OFFENSES PORNOGRAPHY/OBSCENE MAT PROSTITUTION
    ##   2003        13149          21232                       3         1952
    ##   2004        13778          20710                       3         1527
    ##   2005        14055          17834                       5         1103
    ##   2006        13368          18306                       6         1290
    ##   2007        12677          19763                       4         1873
    ##   2008        12303          23457                       5         1673
    ##   2009        12395          24693                       0         1468
    ##   2010        13877          20990                       4         1299
    ##   2011        15586          19552                       3         1094
    ##   2012        16936          18646                       2          690
    ##   2013        21084          19480                       5          692
    ##   2014        19404          20740                       2          449
    ##   2015        19177          20382                       4          374
    ##   2016        17803          19521                       4          640
    ##   2017         3940           4497                       2           60
    ##       Category
    ## Cyear  RECOVERED VEHICLE ROBBERY RUNAWAY SECONDARY CODES
    ##   2003                 0    3204     366            1273
    ##   2004                 0    3380     431            1344
    ##   2005                 0    3566     388            1001
    ##   2006              1086    4131     382            1375
    ##   2007               987    4027     273            1475
    ##   2008               833    4229     259            1580
    ##   2009               655    3578     340            1783
    ##   2010               609    3324     293            1869
    ##   2011               693    3376     320            1815
    ##   2012               723    3955     329            1865
    ##   2013               760    4196     214            2001
    ##   2014                 0    3420     221            1879
    ##   2015               916    3759     149            2040
    ##   2016               735    3298     140            1832
    ##   2017               175     749      29             465
    ##       Category
    ## Cyear  SEX OFFENSES, FORCIBLE SEX OFFENSES, NON FORCIBLE STOLEN PROPERTY
    ##   2003                    698                         28             800
    ##   2004                    698                         31             641
    ##   2005                    638                         24             540
    ##   2006                    599                         29             575
    ##   2007                    631                         39             527
    ##   2008                    682                         20             518
    ##   2009                    682                         17             660
    ##   2010                    733                         30             680
    ##   2011                    735                         29             785
    ##   2012                    791                         28             932
    ##   2013                    760                         18            1224
    ##   2014                    845                         20            1084
    ##   2015                    931                         22             959
    ##   2016                    927                         40             880
    ##   2017                    215                         13             216
    ##       Category
    ## Cyear  SUICIDE SUSPICIOUS OCC  TREA TRESPASS VANDALISM VEHICLE THEFT
    ##   2003      93           4196     0     1434      6448         15325
    ##   2004     102           4489     0     1191      6496         17884
    ##   2005      79           4693     0     1034      7013         18194
    ##   2006      86           4775     0     1102      7688          7291
    ##   2007     111           4800     0     1198      7566          6460
    ##   2008     100           4751     0     1151      7342          6053
    ##   2009      83           4627     0     1232      7604          5183
    ##   2010      82           6004     2     1150      7934          4346
    ##   2011      76           6207     0     1072      7243          4762
    ##   2012      86           5860     2     1288      7808          6183
    ##   2013      73           5677     4     1129      6921          6241
    ##   2014      70           5230     1     1125      7167          7108
    ##   2015      80           5500     1     1403      7675          7944
    ##   2016      69           5762     3     1809      8582          6414
    ##   2017      18           1471     0      436      2289          1467
    ##       Category
    ## Cyear  WARRANTS WEAPON LAWS
    ##   2003     9079        1281
    ##   2004     8114        1252
    ##   2005     6708        1341
    ##   2006     6498        1324
    ##   2007     7105        1318
    ##   2008     5798        1419
    ##   2009     5764        1433
    ##   2010     6187        1349
    ##   2011     6311        1329
    ##   2012     6300        1455
    ##   2013     7362        1535
    ##   2014     6726        1580
    ##   2015     6817        1655
    ##   2016     5857        1656
    ##   2017     1081         407

``` r
df1<-as.data.frame.matrix(x)
```

5: Compute the correlations of all crimes and report top 10 pairs of crimes that are most correlated.

``` r
y<-cor(df1)
dt<-as.data.table(as.table(y))
correlation<-dt[order(N)]
print(correlation[1463:1482,])
```

    ##                         V1                     V2         N
    ##  1:            WEAPON LAWS              VANDALISM 0.9140897
    ##  2:              VANDALISM            WEAPON LAWS 0.9140897
    ##  3:            WEAPON LAWS         MISSING PERSON 0.9149553
    ##  4:         MISSING PERSON            WEAPON LAWS 0.9149553
    ##  5:           PROSTITUTION          DRUG/NARCOTIC 0.9211487
    ##  6:          DRUG/NARCOTIC           PROSTITUTION 0.9211487
    ##  7:           NON-CRIMINAL          LARCENY/THEFT 0.9260638
    ##  8:          LARCENY/THEFT           NON-CRIMINAL 0.9260638
    ##  9:                SUICIDE           EMBEZZLEMENT 0.9305213
    ## 10:           EMBEZZLEMENT                SUICIDE 0.9305213
    ## 11:        STOLEN PROPERTY           NON-CRIMINAL 0.9365495
    ## 12:           NON-CRIMINAL        STOLEN PROPERTY 0.9365495
    ## 13:                  FRAUD                ASSAULT 0.9407685
    ## 14:                ASSAULT                  FRAUD 0.9407685
    ## 15:         SUSPICIOUS OCC         MISSING PERSON 0.9444869
    ## 16:         MISSING PERSON         SUSPICIOUS OCC 0.9444869
    ## 17:            LIQUOR LAWS          DRUG/NARCOTIC 0.9461114
    ## 18:          DRUG/NARCOTIC            LIQUOR LAWS 0.9461114
    ## 19:            WEAPON LAWS SEX OFFENSES, FORCIBLE 0.9485275
    ## 20: SEX OFFENSES, FORCIBLE            WEAPON LAWS 0.9485275

6: Plot all the crimes by year to see how they evolve.

``` r
sa<-stack(as.data.frame(df1))
sa$year<-rep(seq_len(nrow(df1)),ncol(df1))
qplot(year,values,data=sa,group=ind,colour=ind,geom="line",xlab="Year (2003-2017)",ylab="Number of Crimes")
```
Please refer to the Numbers of All Crimes by Year chart.


7: Create a pie chart of all crimes (aggregate across all years).

``` r
df2<-aggregate(Count~Category,SFPD,sum)
ggplot(df2,aes(x="",y=Count,fill=Category))+geom_bar(width=1,stat="identity")+coord_polar(theta="y")+theme_void()+ggtitle("Pie Chart of All Crimes")
```
Please refer to the Pie Chart of All Crimes chart.


8: Plot the total crimes by lattitude and longitude, for all combinations of police district and crime category, using a scatter plot. Make the scatter plot interactive using the **rbokeh** package.

``` r
library(rbokeh)
colnames(SFPD)[10]<-"lng"
colnames(SFPD)[11]<-"lat"
data <- subset(SFPD, Category=='BRIBERY'|Category=='SUICIDE')
gmap(lat=37.78,lng=-122.42,zoom=12,width=480,height=480,map_type="hybrid")%>% ly_points(lng,lat,data=data,col='red',hover=c(PdDistrict,Category))
```

    ## Warning in structure(x, class = unique(c("AsIs", oldClass(x)))): Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    ##   Consider 'structure(list(), *)' instead.

    ## Warning in structure(x, class = unique(c("AsIs", oldClass(x)))): Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    ##   Consider 'structure(list(), *)' instead.

    ## Warning in structure(x, class = unique(c("AsIs", oldClass(x)))): Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    ##   Consider 'structure(list(), *)' instead.

    ## Warning in structure(x, class = unique(c("AsIs", oldClass(x)))): Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    ##   Consider 'structure(list(), *)' instead.

<!--html_preserve-->

If I use the whole data to plot on google map, the size of the HTML file will become too large. I used only two kinds of crimes in this graph. Please refer to the google map graph.


9: Using annual counts, how much of DRUG/NARCOTIC offences can I explain using the number of LIQOUR LAWS, PROSTITUTION, and WEAPON LAWS violations?

``` r
AnnualCounts<-as.data.frame.matrix(x)
reg<-lm(AnnualCounts[,8]~AnnualCounts[,18]+AnnualCounts[,24]+AnnualCounts[,39])
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = AnnualCounts[, 8] ~ AnnualCounts[, 18] + AnnualCounts[, 
    ##     24] + AnnualCounts[, 39])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1005.88  -596.01   -23.24   399.58  1426.21 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)         389.8031  1098.2128   0.355  0.72934   
    ## AnnualCounts[, 18]   14.3374     4.0158   3.570  0.00439 **
    ## AnnualCounts[, 24]    2.4294     0.7877   3.084  0.01039 * 
    ## AnnualCounts[, 39]    0.6411     0.8375   0.765  0.46009   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 831.8 on 11 degrees of freedom
    ## Multiple R-squared:  0.9438, Adjusted R-squared:  0.9285 
    ## F-statistic: 61.61 on 3 and 11 DF,  p-value: 3.649e-07

``` r
plot(residuals(reg))
abline(h=0,col="gray")
```
The output of this regression indicates that WEAPON LAWS is not significant, since its p-value is larger than 0.05. The p-values of LIQOUR LAWS and PROSTITUTION shows that they are significant. However, when we look at the estimated values, we found that the value of intercept is much larger than the values of LIQOUR LAWS and PROSTITUTION, which indicates that if the number of these two crimes increase, the number of DRUG/NARCOTIC offences won't be affected too much. Please refer to the Residual plot.


10: Which crimes occur together the most?

``` r
Tmost<-subset(SFPD,select=c("Category","Cdate","Cyear","Count"))
ggplot(Tmost,aes(x=Cyear,y=Count,fill=Category))+geom_bar(stat="identity")+xlab("Year")+ylab("Number of Incidence")
```
Please refer to the Number of Incidence of Each Crime by Year chart.
