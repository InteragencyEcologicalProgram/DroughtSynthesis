

```r
#DO data

library(tidyverse)
library(lubridate)
library(DroughtData)
library(viridis)

RRI = read_csv("data/RRI_2008-2021_WQ.csv")
```

```
## New names:
## Rows: 9293413 Columns: 10
## ── Column specification
## ───────────────────────────────────────────────────────────────────────────────────────────────── Delimiter: "," chr
## (4): StationID, Month, QAQCFlag, Analyte dbl (5): ...1, Year, Value, Depth, Julian dttm (1): DateTime
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ Specify the column types or set
## `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

```r
yrs = read_csv("data/yearassignments.csv")
```

```
## Rows: 117 Columns: 8
## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): Yr_type, Drought, DroughtYear, ShortTerm, Whitepaper
## dbl (3): Year, Index, SprNDOI
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
#calculate means, mins, maxes

#Get rid of the data that's flagged bad
RRI2 = filter(RRI, !QAQCFlag %in% c("X")) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Month, Year, Julian, Date, Depth, Analyte) %>%
  summarize(Mean = mean(Value, na.rm = T), Max = max(Value, na.rm = T), Min = min(Value, na.rm = T)) %>%
  mutate(Year = case_when(Month %in% c("Oct",
                                       "Nov","Dec") ~ Year +1,
                        TRUE ~ Year))
```

```
## `summarise()` has grouped output by 'Month', 'Year', 'Julian', 'Date', 'Depth'. You can override using the `.groups`
## argument.
```

```r
#Add water year type and make water year type a factor
RRI2a = left_join(RRI2, yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Wet")))
```

```
## Joining, by = "Year"
```

```r
#quick plot of all the data
ggplot(RRI2a, aes(x = Yr_type, y = Mean)) + geom_boxplot()+
  facet_grid(Depth~Analyte, scales = "free_y")
```

```
## Warning: Removed 124 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
#Just the DO data
ggplot(filter(RRI2a, Analyte == "Dissolved Oxygen"), aes(x = Yr_type, y = Mean, fill = Yr_type)) + 
  geom_boxplot()+ drt_color_pal_yrtype()+
  facet_wrap(~Depth, scales = "free_y")+ theme_bw()
```

```
## Warning: Removed 33 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
#filter out summer DO, when low DO is most problematic
summerDO = filter(RRI2a, Month %in% c("Sep", "Jun", "Jul", "Aug"),  Analyte == "Dissolved Oxygen")

ggplot(summerDO, aes(x = Yr_type, y = Mean)) + geom_boxplot()+
  facet_wrap(~Depth, scales = "free_y")
```

```
## Warning: Removed 13 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
#Look for changes over time
ggplot(summerDO, aes(x = as.factor(Year), y = Min, fill = Yr_type)) + geom_boxplot()+
  facet_wrap(~Depth, scales = "free_y")
```

```
## Warning: Removed 13 rows containing non-finite values (stat_boxplot).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
#changes in minimum over time
ggplot(summerDO, aes(x = Year, y = Min)) + geom_point(aes(color = Yr_type))+ geom_smooth(method = lm)+
  facet_wrap(~Depth, scales = "free_y")+ylab("Summer Daily MInimum DO mg/L")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 13 rows containing non-finite values (stat_smooth).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

```r
#More changes over time, all days
ggplot(filter(RRI2a, Analyte == "Dissolved Oxygen"), aes(x = Date, y = Mean)) + geom_point()+
  facet_wrap(~Depth, scales = "free_y")
```

```
## Warning: Removed 33 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-6.png)

```r
#color code by year type
ggplot(filter(RRI2a, Analyte == "Dissolved Oxygen"), aes(x = Date, y = Min, color = Yr_type)) + geom_point()+
  facet_wrap(~Depth)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-7.png)

```r
#Let's pivot it so I can regress water quality against each otehr
RRwide = pivot_wider(RRI2a, id_cols = c(Month, Year, Julian, Date, Depth, Yr_type), 
                     names_from = Analyte, values_from = Mean)

#plot DO versus temperature
ggplot(RRwide, aes(x = `Temperature`, y = `Dissolved Oxygen`, color = Yr_type)) + geom_point()+
  ylab("Dissolved Oxygen mg/L")+ xlab("Temperature C")+ 
  scale_color_brewer(palette = "Dark2", name = "Water Year\nType")+ theme_bw()
```

```
## Warning: Removed 72 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-8.png)

```r
#color code by fluorescence
ggplot(RRwide, aes(x = `Temperature`, y = `Dissolved Oxygen`, color = log(Fluorescence))) + geom_point()+
  scale_color_viridis()
```

```
## Warning: Removed 72 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-9.png)

```r
ggplot(RRwide, aes(x = log(Fluorescence), y = `Dissolved Oxygen`, color = Temperature)) + geom_point()+
  scale_color_viridis()
```

```
## Warning: Removed 282 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-10.png)

```r
# G - good 
# 
# X - bad
# 
# A - added (no data and  no date stamp, date time added for continuous date)
# 
# M - missing (no data, date stamp present)
# 
# U - unchecked 
# 
# Q - qualified 
```

