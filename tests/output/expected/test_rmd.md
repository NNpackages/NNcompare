Output from parallel program: test\_rmd.
================
mzdb
Thu Oct 8 14:56:18 2020

  - [Summary of data.frames](#summary-of-data.frames)
  - [Summary of overall comparison](#summary-of-overall-comparison)
  - [Observations not shared](#observations-not-shared)
  - [Differences detected by
    variable](#differences-detected-by-variable)
  - [Differences detected (324 not
    shown)](#differences-detected-324-not-shown)
  - [Non-identical attributes](#non-identical-attributes)

## Summary of data.frames

| version | arg        | ncol | nrow |
| :------ | :--------- | ---: | ---: |
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   14 | 1492 |

Summary of data.frames

## Summary of overall comparison

| statistic                                                   | value |
| :---------------------------------------------------------- | ----: |
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    13 |
| Number of variables compared                                |    13 |
| Number of variables in x but not y                          |     0 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     2 |
| Number of variables compared with all values equal          |    11 |
| Number of observations in common                            |  1492 |
| Number of observations in x but not y                       |     7 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |   316 |
| Number of observations with all compared variables equal    |  1176 |
| Number of values unequal                                    |   344 |

Summary of overall comparison

|                         |
| :---------------------- |
| No variables not shared |

Variables not shared

|                                 |
| :------------------------------ |
| No other variables not compared |

Other variables not compared

## Observations not shared

| version |   case | observation |
| :------ | -----: | ----------: |
| x       | 105850 |        1076 |
| x       | 106182 |        1095 |
| x       | 106348 |        1102 |
| x       | 111608 |        1371 |
| x       | 111928 |        1400 |
| x       | 112240 |        1448 |
| x       | 112263 |           5 |

Observations not shared

## Differences detected by variable

| var.x       | var.y       |   n | NAs |
| :---------- | :---------- | --: | --: |
| age         | age         | 261 |   0 |
| arm         | arm         |   0 |   0 |
| sex         | sex         |   0 |   0 |
| race        | race        |   0 |   0 |
| fu.time     | fu.time     |   0 |   0 |
| fu.stat     | fu.stat     |   0 |   0 |
| ps          | ps          |   0 |   0 |
| hgb         | hgb         |   0 |   0 |
| bmi         | bmi         |  83 |   0 |
| alk.phos    | alk.phos    |   0 |   0 |
| ast         | ast         |   0 |   0 |
| mdquality.s | mdquality.s |   0 |   0 |
| age.ord     | age.ord     |   0 |   0 |

Differences detected by variable

## Differences detected (324 not shown)

| var.x | var.y |  case | values.x | values.y | row.x | row.y |
| :---- | :---- | ----: | :------- | :------- | ----: | ----: |
| age   | age   | 77355 | 42       | 37       |    33 |    32 |
| age   | age   | 78496 | 44       | 39       |    40 |    39 |
| age   | age   | 79178 | 48       | 43       |    47 |    46 |
| age   | age   | 79425 | 48       | 43       |    51 |    50 |
| age   | age   | 80048 | 41       | 36       |    61 |    60 |
| age   | age   | 80803 | 38       | 33       |    66 |    65 |
| age   | age   | 80805 | 46       | 41       |    67 |    66 |
| age   | age   | 81061 | 40       | 35       |    70 |    69 |
| age   | age   | 83255 | 47       | 42       |    76 |    75 |
| age   | age   | 83483 | 48       | 43       |    81 |    80 |
| bmi   | bmi   | 76240 | 19.91541 | 18.91541 |    27 |    26 |
| bmi   | bmi   | 79795 | 14.053   | 13.053   |    58 |    57 |
| bmi   | bmi   | 85029 | 18.54671 | 17.54671 |   122 |   121 |
| bmi   | bmi   | 85064 | 16.64932 | 15.64932 |   123 |   122 |
| bmi   | bmi   | 85078 | 19.82948 | 18.82948 |   124 |   123 |
| bmi   | bmi   | 86205 | 19.03673 | 18.03673 |     6 |     5 |
| bmi   | bmi   | 88989 | 19.7413  | 18.7413  |     9 |     8 |
| bmi   | bmi   | 89025 | 19.26006 | 18.26006 |   209 |   208 |
| bmi   | bmi   | 89703 | 18.81378 | 17.81378 |   247 |   246 |
| bmi   | bmi   | 90352 | 19.71332 | 18.71332 |   296 |   295 |

Differences detected (324 not shown)

## Non-identical attributes

| var.x | var.y | name  |
| :---- | :---- | :---- |
| age   | age   | label |
| bmi   | bmi   | label |

Non-identical attributes
