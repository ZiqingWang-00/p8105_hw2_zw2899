p8105_hw2_zw2899
================
Ziqing Wang
2022-10-02

``` r
library(tidyverse)
library(readxl)
```

### Problem 1 (Posted solutions)

Below we import and clean data from
`NYC_Transit_Subway_Entrance_And_Exit_Data.csv`. The process begins with
data import, updates variable names, and selects the columns that will
be used in later parts fo this problem. We update `entry` from `yes` /
`no` to a logical variable. As part of data import, we specify that
`Route` columns 8-11 should be character for consistency with 1-7.

``` r
trans_ent = 
  read_csv(
    "data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
  janitor::clean_names() %>% 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

As it stands, these data are not “tidy”: route number should be a
variable, as should route. That is, to obtain a tidy dataset we would
need to convert `route` variables from wide to long format. This will be
useful when focusing on specific routes, but may not be necessary when
considering questions that focus on station-level variables.

The following code chunk selects station name and line, and then uses
`distinct()` to obtain all unique combinations. As a result, the number
of rows in this dataset is the number of unique stations.

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 465 × 2
##    station_name             line    
##    <chr>                    <chr>   
##  1 25th St                  4 Avenue
##  2 36th St                  4 Avenue
##  3 45th St                  4 Avenue
##  4 53rd St                  4 Avenue
##  5 59th St                  4 Avenue
##  6 77th St                  4 Avenue
##  7 86th St                  4 Avenue
##  8 95th St                  4 Avenue
##  9 9th St                   4 Avenue
## 10 Atlantic Av-Barclays Ctr 4 Avenue
## # … with 455 more rows
```

The next code chunk is similar, but filters according to ADA compliance
as an initial step. This produces a dataframe in which the number of
rows is the number of ADA compliant stations.

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 84 × 2
##    station_name                   line           
##    <chr>                          <chr>          
##  1 Atlantic Av-Barclays Ctr       4 Avenue       
##  2 DeKalb Av                      4 Avenue       
##  3 Pacific St                     4 Avenue       
##  4 Grand Central                  42nd St Shuttle
##  5 34th St                        6 Avenue       
##  6 47-50th Sts Rockefeller Center 6 Avenue       
##  7 Church Av                      6 Avenue       
##  8 21st St                        63rd Street    
##  9 Lexington Av                   63rd Street    
## 10 Roosevelt Island               63rd Street    
## # … with 74 more rows
```

To compute the proportion of station entrances / exits without vending
allow entrance, we first exclude station entrances that do not allow
vending. Then, we focus on the `entry` variable – this logical, so
taking the mean will produce the desired proportion (recall that R will
coerce logical to numeric in cases like this).

``` r
trans_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
## [1] 0.3770492
```

Lastly, we write a code chunk to identify stations that serve the A
train, and to assess how many of these are ADA compliant. As a first
step, we tidy the data as alluded to previously; that is, we convert
`route` from wide to long format. After this step, we can use tools from
previous parts of the question (filtering to focus on the A train, and
on ADA compliance; selecting and using `distinct` to obtain dataframes
with the required stations in rows).

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 60 × 2
##    station_name                  line           
##    <chr>                         <chr>          
##  1 Times Square                  42nd St Shuttle
##  2 125th St                      8 Avenue       
##  3 145th St                      8 Avenue       
##  4 14th St                       8 Avenue       
##  5 168th St - Washington Heights 8 Avenue       
##  6 175th St                      8 Avenue       
##  7 181st St                      8 Avenue       
##  8 190th St                      8 Avenue       
##  9 34th St                       8 Avenue       
## 10 42nd St                       8 Avenue       
## # … with 50 more rows

trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 17 × 2
##    station_name                  line            
##    <chr>                         <chr>           
##  1 14th St                       8 Avenue        
##  2 168th St - Washington Heights 8 Avenue        
##  3 175th St                      8 Avenue        
##  4 34th St                       8 Avenue        
##  5 42nd St                       8 Avenue        
##  6 59th St                       8 Avenue        
##  7 Inwood - 207th St             8 Avenue        
##  8 West 4th St                   8 Avenue        
##  9 World Trade Center            8 Avenue        
## 10 Times Square-42nd St          Broadway        
## 11 59th St-Columbus Circle       Broadway-7th Ave
## 12 Times Square                  Broadway-7th Ave
## 13 8th Av                        Canarsie        
## 14 Franklin Av                   Franklin        
## 15 Euclid Av                     Fulton          
## 16 Franklin Av                   Fulton          
## 17 Howard Beach                  Rockaway
```

### Problem 2

First, we import and clean the Mr. Trash Wheel dataset:

``` r
mr_trash_wheel_sheet = read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", 
                            sheet = "Mr. Trash Wheel", skip = 1) %>% # skip the row that contains figure
  janitor::clean_names() %>%
  drop_na(c("dumpster")) %>% # drop the rows that computes monthly totals
  slice(1:(n()-1)) %>% # drop the last row, which computes the grand total
  select(-c(x15, x16, x17))%>%# drop the columns that contain notes
  mutate(sports_balls = as.integer(sports_balls),
         which_wheel = "mr") # round sports balls to the nearest integer and create new variable to note the dumpsters are from mr. trash wheel 

mr_trash_wheel_sheet
## # A tibble: 453 × 15
##    dumpster month  year date                weight_tons volume…¹ plast…² polys…³
##    <chr>    <chr> <dbl> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
##  1 1        May    2014 2014-05-16 00:00:00        4.31       18    1450    1820
##  2 2        May    2014 2014-05-16 00:00:00        2.74       13    1120    1030
##  3 3        May    2014 2014-05-16 00:00:00        3.45       15    2450    3100
##  4 4        May    2014 2014-05-17 00:00:00        3.1        15    2380    2730
##  5 5        May    2014 2014-05-17 00:00:00        4.06       18     980     870
##  6 6        May    2014 2014-05-20 00:00:00        2.71       13    1430    2140
##  7 7        May    2014 2014-05-21 00:00:00        1.91        8     910    1090
##  8 8        May    2014 2014-05-28 00:00:00        3.7        16    3580    4310
##  9 9        June   2014 2014-06-05 00:00:00        2.52       14    2400    2790
## 10 10       June   2014 2014-06-11 00:00:00        3.76       18    1340    1730
## # … with 443 more rows, 7 more variables: cigarette_butts <dbl>,
## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
## #   sports_balls <int>, homes_powered <dbl>, which_wheel <chr>, and abbreviated
## #   variable names ¹​volume_cubic_yards, ²​plastic_bottles, ³​polystyrene
```

Next, we import and clean the Professor Trash Wheel data:

``` r
prof_trash_wheel_sheet = read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", 
                            sheet = "Professor Trash Wheel", skip = 1) %>%  # skip the row that contains figure
  janitor::clean_names() %>%
  drop_na(c("dumpster")) %>% # drop the rows that computes monthly/grand totals
  mutate(sports_balls = as.integer(sports_balls),
         which_wheel = "prof") # round sports balls to the nearest integer and create a new variable to note that these dumpsters are from professor trash wheel


prof_trash_wheel_sheet
## # A tibble: 71 × 15
##    dumpster month     year date                weight_…¹ volum…² plast…³ polys…⁴
##       <dbl> <chr>    <dbl> <dttm>                  <dbl>   <dbl>   <dbl>   <dbl>
##  1        1 January   2017 2017-01-02 00:00:00      1.79      15    1950    6080
##  2        2 January   2017 2017-01-30 00:00:00      1.58      15    9540   11230
##  3        3 February  2017 2017-02-26 00:00:00      2.32      18    8350    9210
##  4        4 February  2017 2017-02-26 00:00:00      3.72      15    8590    1030
##  5        5 February  2017 2017-02-28 00:00:00      1.45      15    7830    9950
##  6        6 March     2017 2017-03-30 00:00:00      1.71      15    8210   10340
##  7        7 April     2017 2017-04-01 00:00:00      1.82      15    9830   11020
##  8        8 April     2017 2017-04-20 00:00:00      2.37      15    9240    8760
##  9        9 May       2017 2017-05-10 00:00:00      2.64      15    9540    8810
## 10       10 May       2017 2017-05-26 00:00:00      2.78      15    8230    7800
## # … with 61 more rows, 7 more variables: cigarette_butts <dbl>,
## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
## #   sports_balls <int>, homes_powered <dbl>, which_wheel <chr>, and abbreviated
## #   variable names ¹​weight_tons, ²​volume_cubic_yards, ³​plastic_bottles,
## #   ⁴​polystyrene
```

Before combining the two datasets by rows, we check the names and data
types of the columns of the two datasets:

``` r
# the datasets have the same column names if there sets of column names contain each other
FALSE %in% (names(mr_trash_wheel_sheet) %in% names(prof_trash_wheel_sheet))
## [1] FALSE
FALSE %in% (names(prof_trash_wheel_sheet) %in% names(mr_trash_wheel_sheet))
## [1] FALSE
```

We can see from the checks above that the two datasets inded have the
same column names.

Nest, we check if the variables in the two datasets have the same data
type before merging:

``` r
glimpse(mr_trash_wheel_sheet)
## Rows: 453
## Columns: 15
## $ dumpster           <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", …
## $ month              <chr> "May", "May", "May", "May", "May", "May", "May", "M…
## $ year               <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
## $ date               <dttm> 2014-05-16, 2014-05-16, 2014-05-16, 2014-05-17, 20…
## $ weight_tons        <dbl> 4.31, 2.74, 3.45, 3.10, 4.06, 2.71, 1.91, 3.70, 2.5…
## $ volume_cubic_yards <dbl> 18, 13, 15, 15, 18, 13, 8, 16, 14, 18, 15, 19, 15, …
## $ plastic_bottles    <dbl> 1450, 1120, 2450, 2380, 980, 1430, 910, 3580, 2400,…
## $ polystyrene        <dbl> 1820, 1030, 3100, 2730, 870, 2140, 1090, 4310, 2790…
## $ cigarette_butts    <dbl> 126000, 91000, 105000, 100000, 120000, 90000, 56000…
## $ glass_bottles      <dbl> 72, 42, 50, 52, 72, 46, 32, 58, 49, 75, 38, 45, 58,…
## $ grocery_bags       <dbl> 584, 496, 1080, 896, 368, 672, 416, 1552, 984, 448,…
## $ chip_bags          <dbl> 1162, 874, 2032, 1971, 753, 1144, 692, 3015, 1988, …
## $ sports_balls       <int> 7, 5, 6, 6, 7, 5, 3, 6, 5, 7, 6, 7, 6, 6, 6, 6, 5, …
## $ homes_powered      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ which_wheel        <chr> "mr", "mr", "mr", "mr", "mr", "mr", "mr", "mr", "mr…
glimpse(prof_trash_wheel_sheet)
## Rows: 71
## Columns: 15
## $ dumpster           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
## $ month              <chr> "January", "January", "February", "February", "Febr…
## $ year               <dbl> 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 201…
## $ date               <dttm> 2017-01-02, 2017-01-30, 2017-02-26, 2017-02-26, 20…
## $ weight_tons        <dbl> 1.79, 1.58, 2.32, 3.72, 1.45, 1.71, 1.82, 2.37, 2.6…
## $ volume_cubic_yards <dbl> 15, 15, 18, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,…
## $ plastic_bottles    <dbl> 1950, 9540, 8350, 8590, 7830, 8210, 9830, 9240, 954…
## $ polystyrene        <dbl> 6080, 11230, 9210, 1030, 9950, 10340, 11020, 8760, …
## $ cigarette_butts    <dbl> 19700, 17600, 12000, 13000, 16000, 14000, 17000, 15…
## $ glass_bottles      <dbl> 8, 14, 19, 21, 18, 23, 26, 14, 28, 22, 12, 24, 27, …
## $ grocery_bags       <dbl> 3100, 5630, 6430, 5870, 7450, 9560, 11500, 9970, 12…
## $ chip_bags          <dbl> 15600, 16700, 12400, 11030, 15340, 13470, 18620, 14…
## $ sports_balls       <int> 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ homes_powered      <dbl> 29.83333, 26.33333, 38.66667, 62.00000, 24.16667, 2…
## $ which_wheel        <chr> "prof", "prof", "prof", "prof", "prof", "prof", "pr…
```

We found an inconsistency where in the first dataset, the dumpster
variable was encoded as a character, while in the second dataset, the
dumpster variable was ended as a double. We convert the dumpster
variable in the first dataset into a double:

``` r
mr_trash_wheel_sheet = mr_trash_wheel_sheet %>% mutate(dumpster = as.numeric(dumpster))
```

Then we combine the two datasets together into one:

``` r
wheel_tidy = bind_rows(mr_trash_wheel_sheet, prof_trash_wheel_sheet)
wheel_tidy
## # A tibble: 524 × 15
##    dumpster month  year date                weight_tons volume…¹ plast…² polys…³
##       <dbl> <chr> <dbl> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
##  1        1 May    2014 2014-05-16 00:00:00        4.31       18    1450    1820
##  2        2 May    2014 2014-05-16 00:00:00        2.74       13    1120    1030
##  3        3 May    2014 2014-05-16 00:00:00        3.45       15    2450    3100
##  4        4 May    2014 2014-05-17 00:00:00        3.1        15    2380    2730
##  5        5 May    2014 2014-05-17 00:00:00        4.06       18     980     870
##  6        6 May    2014 2014-05-20 00:00:00        2.71       13    1430    2140
##  7        7 May    2014 2014-05-21 00:00:00        1.91        8     910    1090
##  8        8 May    2014 2014-05-28 00:00:00        3.7        16    3580    4310
##  9        9 June   2014 2014-06-05 00:00:00        2.52       14    2400    2790
## 10       10 June   2014 2014-06-11 00:00:00        3.76       18    1340    1730
## # … with 514 more rows, 7 more variables: cigarette_butts <dbl>,
## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
## #   sports_balls <int>, homes_powered <dbl>, which_wheel <chr>, and abbreviated
## #   variable names ¹​volume_cubic_yards, ²​plastic_bottles, ³​polystyrene
```

Here is a summary of the merged dataset: It has 524 observations and 15
variables. The variables include the attributes of the trash collected
by each dumpster in Mr. Trash Wheel and Professor Trash Wheel. These
attributes include weight in tons, volume in cubic yards, number of
plastic bottles collected, number of polystyrene containers collected,
number of cigarette butts collected, number of glass bottles collected,
number of grocery bags collected, number of chip bags collected, number
of sports balls collected, and number of homes powered by the trash
collected by the dumpster on the recorded date. The recorded dates for
dumpsters in Mr. Trash Wheel ranges from 2014-05-16 to 2021-01-04, while
for Professor Trash Wheel, they range from 2017-01-02 to 2021-01-04.

For the available data, Professor Trash Wheel collected 135.5 tons of
trash in total. In 2020, Mr. Trash Wheel collected 856 sports balls in
total.

### Problem 3

First, import and clean the pols-month.csv data:

``` r
pols_month_data = read_csv("data/fivethirtyeight_datasets/pols-month.csv") %>%
  separate(col = mon, into = c("year", "month", "day"), sep = "-") %>% # separate the mon variable into three variables
  mutate(year = as.integer(year), 
         month = month.abb[as.integer(month)], 
         day = as.integer(day)) %>% # convert year and day to integer; convert month to month names
  pivot_longer(cols = c(prez_gop, prez_dem),
               names_to = "president",
               values_to = "which_party") %>% # transform prez_gop and prez_dem into new variables president and which_party
  filter(which_party==1) %>% # delete duplicate rows resulted from the pivot_longer() operation, which contains information on to which party the president does NOT belong 
  mutate(president = recode(president, "prez_dem" = "dem", "prez_gop" = "gop")) %>% # recode the president variable
  select(-c(which_party, day)) # remove extra variables
  
pols_month_data
## # A tibble: 817 × 9
##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
##    <int> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
##  1  1947 Jan        23      51     253      23      45     198 dem      
##  2  1947 Feb        23      51     253      23      45     198 dem      
##  3  1947 Mar        23      51     253      23      45     198 dem      
##  4  1947 Apr        23      51     253      23      45     198 dem      
##  5  1947 May        23      51     253      23      45     198 dem      
##  6  1947 Jun        23      51     253      23      45     198 dem      
##  7  1947 Jul        23      51     253      23      45     198 dem      
##  8  1947 Aug        23      51     253      23      45     198 dem      
##  9  1947 Sep        23      51     253      23      45     198 dem      
## 10  1947 Oct        23      51     253      23      45     198 dem      
## # … with 807 more rows
```

Next, import and clean the snp.csv data:

``` r
snp_data = read_csv("data/fivethirtyeight_datasets/snp.csv") %>%
  separate(col = date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(year = as.integer(year), 
         month = month.abb[as.integer(month)], 
         day = as.integer(day)) %>%
  mutate(year = case_when(year %in% seq(0, 15) ~ year + 2000,
                          year %in% seq(50, 99) ~ year + 1900)) %>% # convert two-digit year to 4-digit year
  relocate(year) %>% # make year and month the first and secone column
  select(-day) # remove the day variable as in the previous dataset
  
snp_data
## # A tibble: 787 × 3
##     year month close
##    <dbl> <chr> <dbl>
##  1  2015 Jul   2080.
##  2  2015 Jun   2063.
##  3  2015 May   2107.
##  4  2015 Apr   2086.
##  5  2015 Mar   2068.
##  6  2015 Feb   2104.
##  7  2015 Jan   1995.
##  8  2014 Dec   2059.
##  9  2014 Nov   2068.
## 10  2014 Oct   2018.
## # … with 777 more rows
```

Third, tidy the unemployment data so that it can be merged with the
previous datasets. This process will involve switching from “wide” to
“long” format; ensuring that key variables have the same name; and
ensuring that key variables take the same values.

``` r
unemployment_data = read_csv("data/fivethirtyeight_datasets/unemployment.csv") %>%
  pivot_longer(cols = Jan:Dec,
               names_to = "month",
               values_to = "unemployment_rate_%") 

unemployment_data
## # A tibble: 816 × 3
##     Year month `unemployment_rate_%`
##    <dbl> <chr>                 <dbl>
##  1  1948 Jan                     3.4
##  2  1948 Feb                     3.8
##  3  1948 Mar                     4  
##  4  1948 Apr                     3.9
##  5  1948 May                     3.5
##  6  1948 Jun                     3.6
##  7  1948 Jul                     3.6
##  8  1948 Aug                     3.9
##  9  1948 Sep                     3.8
## 10  1948 Oct                     3.7
## # … with 806 more rows
```

Join the datasets by merging the snp data into the pols data, and
merging the unemployment data into the result.

``` r
merged_pol_snp_data = left_join(pols_month_data, snp_data, by=c("year"="year", "month"="month"))
merged_pol_snp_unemploymemt_data = left_join(merged_pol_snp_data, unemployment_data, by = c("year"="Year", "month"="month"))

merged_pol_snp_unemploymemt_data
## # A tibble: 817 × 11
##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president close
##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>
##  1  1947 Jan        23      51     253      23      45     198 dem          NA
##  2  1947 Feb        23      51     253      23      45     198 dem          NA
##  3  1947 Mar        23      51     253      23      45     198 dem          NA
##  4  1947 Apr        23      51     253      23      45     198 dem          NA
##  5  1947 May        23      51     253      23      45     198 dem          NA
##  6  1947 Jun        23      51     253      23      45     198 dem          NA
##  7  1947 Jul        23      51     253      23      45     198 dem          NA
##  8  1947 Aug        23      51     253      23      45     198 dem          NA
##  9  1947 Sep        23      51     253      23      45     198 dem          NA
## 10  1947 Oct        23      51     253      23      45     198 dem          NA
## # … with 807 more rows, and 1 more variable: `unemployment_rate_%` <dbl>
```
