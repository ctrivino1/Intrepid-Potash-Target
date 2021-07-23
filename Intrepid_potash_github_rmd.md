Intrepid Potash
================
Chris Trivino
July 23, 2021

``` r
# Use this R-Chunk to import all your !



intrepid$NS1M1[intrepid$NS1M1=="[-11059] No Good Data For Calculation"]<-NA
intrepid$NS1M1 <-as.numeric(intrepid$NS1M1)

intrepid$NS2M1[intrepid$NS2M1=="[-11059] No Good Data For Calculation"]<-NA
intrepid$NS2M1 <-as.numeric(intrepid$NS2M1)

intrepid$NS2M2[intrepid$NS2M2=="[-11059] No Good Data For Calculation"]<-NA
intrepid$NS2M2 <-as.numeric(intrepid$NS2M2)

intrepid$RollGap1[intrepid$RollGap1=="[-11059] No Good Data For Calculation"]<-NA
intrepid$RollGap1 <-as.numeric(intrepid$RollGap1)

intrepid$RollGap2[intrepid$RollGap2=="[-11059] No Good Data For Calculation"]<-NA
intrepid$RollGap2 <-as.numeric(intrepid$RollGap2)

intrepid$NS1Gate[intrepid$NS1Gate=="[-11059] No Good Data For Calculation"]<-NA
intrepid$NS1Gate <-as.numeric(intrepid$NS1Gate)

intrepid$NS2Gate[intrepid$NS2Gate=="[-11059] No Good Data For Calculation"]<-NA
intrepid$NS2Gate <-as.numeric(intrepid$NS2Gate)

intrepid$BinLevel[intrepid$BinLevel=="[-11059] No Good Data For Calculation"]<-NA
intrepid$BinLevel <-as.numeric(intrepid$BinLevel)

intrepid$NS2Gate[intrepid$NS2Gate=="[-11059] No Good Data For Calculation"]<-NA
intrepid$NS2Gate <-as.numeric(intrepid$NS2Gate)



dat <- intrepid %>% filter(Method == "clamshells", intrepid$Product == 230)

dat <- intrepid %>% filter(Method == "clamshells", intrepid$Product == 230)


dat$NS1Diff <- dat$NS1M2-dat$NS1M1

data1 <- subset(dat, select = -c(Method, Product,Time))

data1 <- data1 %>% filter(AvgBelt6TPHTime>0)

data2 <- data1 %>% select(c(AvgBelt6TPHTime,NS1Diff, NS1M1, NS1M2,Tumbler))
```

## Trying to find linear models

This can help me get off to a start.

``` r
mylm <- lm(AvgBelt6TPHTime ~ . , data = data1 )

pander(summary(mylm))
```

|                        | Estimate | Std. Error | t value | Pr(&gt;\|t\|) |
|:----------------------:|:--------:|:----------:|:-------:|:-------------:|
|    **(Intercept)**     |  -58.01  |   126.4    | -0.4589 |    0.6463     |
| **CompactorRollSpeed** |  0.263   |  0.01662   |  15.83  |   2.584e-55   |
|       **NS1M1**        | -0.3855  |  0.04538   | -8.496  |   2.439e-17   |
|       **NS1M2**        |  0.6362  |  0.01034   |  61.52  |       0       |
|       **NS2M1**        |  -0.129  |  0.04214   | -3.061  |   0.002214    |
|       **NS2M2**        |  0.143   |  0.01486   |  9.619  |   9.475e-22   |
|      **RollGap1**      |  3.849   |   14.05    |  0.274  |    0.7841     |
|      **BinLevel**      | 0.06117  |  0.003651  |  16.75  |   1.303e-61   |
|   **PressureDrive**    | 0.01467  |  0.002631  |  5.575  |   2.575e-08   |
|  **PressureNonDrive**  | -0.01117 |   0.0026   | -4.298  |   1.754e-05   |
|      **Haulback**      |  0.1886  |  0.01613   |  11.69  |   3.111e-31   |
|      **NS1Gate**       | -0.1878  |  0.02056   | -9.132  |   8.941e-20   |
|      **NS2Gate**       | -0.08252 |  0.01744   | -4.731  |   2.28e-06    |
|      **Tumbler**       | -0.9339  |   0.169    | -5.524  |   3.441e-08   |

| Observations | Residual Std. Error | *R*<sup>2</sup> | Adjusted *R*<sup>2</sup> |
|:------------:|:-------------------:|:---------------:|:------------------------:|
|     6111     |        3.868        |     0.8122      |          0.8118          |

Fitting linear model: AvgBelt6TPHTime \~ .

``` r
mylm2 <-  lm(AvgBelt6TPHTime ~ .^2 , data = data1 )
#pander(summary(mylm2))

mylm3 <-  lm(AvgBelt6TPHTime ~ .^3 , data = data1 )
#pander(summary(mylm3))
```

## Lets take a look at the pairs plots

``` r
#pairs(cbind(Res = mylm3$res, Fit = mylm3$fit, data1),panel=panel.smooth)
pairs(data2,panel=panel.smooth)
```

![](Intrepid_potash_github_rmd_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## finding a linear model to work with

After much trial and tribulation I found a good lead, but there are
still some problems

``` r
# Use this R-Chunk to clean & wrangle your data!
lm8 <- lm(AvgBelt6TPHTime~I(NS1Diff^2)+I(NS1Diff^3)+(Tumbler)+CompactorRollSpeed,data1)
pander(summary(lm8)) #.80
```

|                        | Estimate  | Std. Error | t value | Pr(&gt;\|t\|) |
|:----------------------:|:---------:|:----------:|:-------:|:-------------:|
|    **(Intercept)**     |  -5.875   |   0.8868   | -6.625  |   3.778e-11   |
|    **I(NS1Diff^2)**    |  0.08499  |  0.001051  |  80.84  |       0       |
|    **I(NS1Diff^3)**    | -0.001876 | 3.072e-05  | -61.06  |       0       |
|      **Tumbler**       |  -1.744   |   0.1432   | -12.18  |   9.211e-34   |
| **CompactorRollSpeed** |  0.1553   |  0.01497   |  10.38  |   5.178e-25   |

| Observations | Residual Std. Error | *R*<sup>2</sup> | Adjusted *R*<sup>2</sup> |
|:------------:|:-------------------:|:---------------:|:------------------------:|
|     6113     |        3.895        |     0.8093      |          0.8092          |

Fitting linear model: AvgBelt6TPHTime \~ I(NS1Diff^2) + I(NS1Diff^3) +
(Tumbler) + CompactorRollSpeed

``` r
par(mfrow=c(1,3))
plot(lm8, which=1:2)
plot(lm8$residuals, main="Residuals vs Order", xlab="",
     ylab="Residuals")
```

![](Intrepid_potash_github_rmd_files/figure-gfm/tidy_data-1.png)<!-- -->

Good news: The model is simple and is 2D explainable.

Bad news: From the diagnostic plots almost every linear assumption is
being violated. There are lots of problems.

## Data Visualizations

Time to try and find what’s going on in the data that’s causing my
linear assumptions to fail.

``` r
# Use this R-Chunk to plot & visualize your data!

ggplot(data1, aes(x=CompactorRollSpeed,y=AvgBelt6TPHTime, color=Tumbler)) + geom_point(opacity=.3) + theme_bw()
```

    ## Warning: Ignoring unknown parameters: opacity

    ## Warning: Removed 8 rows containing missing values (geom_point).

![](Intrepid_potash_github_rmd_files/figure-gfm/plot_data-1.png)<!-- -->

hmm.. Why did the compactor roller speed stay at 65 for such a large
amount of time? I will have to email our contact with the company. The
grey dots are NA’s, I wonder if I can find out if they are supposed to
be zero’s.

``` r
data1 <- data1 %>% mutate(tumbler_no_na = case_when(Tumbler == 0 ~ "off",
                                           Tumbler ==1 ~ "on",
                                           is.na(Tumbler) ~ "unknown",
                                           TRUE~"unknown"))
```

``` r
#ggplot(data1, aes(x=CompactorRollSpeed,y=AvgBelt6TPHTime, color=Tumbler)) + geom_point()

data1 %>% 
  filter(CompactorRollSpeed>65.2 & CompactorRollSpeed <= 65.26) %>% 
  ggplot(aes(x=CompactorRollSpeed,y=AvgBelt6TPHTime, color=tumbler_no_na)) + geom_boxplot()+ facet_wrap(~tumbler_no_na)
```

![](Intrepid_potash_github_rmd_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

If I can try and find out if NA’s are zero’s that would help
significantly.

## Next step:

You could hire someone helping them understand the NA values in the data
set along with the information you provided us. This would help someone
with industry experience give you an appropriate regression model
faster. Or you could bring the project back to DSS and let students work
with the data to help create a linear model.
