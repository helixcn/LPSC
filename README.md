# LPSC
Tools for searching List of plant species in China

# What is it?

It is an R package for searching the List of plant species in China (currently 2022). Most importantly, it could show the accepted name in a batch mode. This will help standardising the checklist of a given region.

# How to install

To install the package, please use the following commands:

```R
devtools::install_github("helixcn/LPSC")
```

If you haven't had `devtools` installed, please install it by typing `"install.packages("devtools")"` in the R console.


# How to use

```R
library(LPSC)
get_accepted_name("Cyclobalanopsis myrsinifolia")
aaaa <- get_accepted_name(c(
  "小叶青冈",
  "Cyclobalanopsis myrsinifolia",
  "Machilus chekiangensis",
  "宫廷胡萝卜"
))
make_taxa_table(aaaa)
get_accepted_name(c("Carex phaeopoda"))
get_accepted_name(c("Cyclobalanopsis myrsinifolia"))
get_accepted_name(c("Carex phaeopoda2",
                    "Carex phaeopoda"))
get_accepted_name(c("Carex phaeopoda",
                    "苹果", "华中山楂", "华中山楂2"))
# 直接查询出接受名，中文，拉丁文均可
get_accepted_name("华中山楂2")
xxx <- get_accepted_name(c("Carex phaeopoda",
                           "苹果", "华中山楂"))
make_taxa_table(xxx)
get_accepted_name("刺枝麻黄")
ttt <- get_accepted_name("单子麻黄2")
make_taxa_table(ttt)
```

# How to cite

- Zhang J (2022). _LPSC: Tools for searching List of plant
  species in China_. R package version 0.2.0,
  <https://github.com/helixcn/LPSC>.
  
As the data came from List of plant species in China (2022 Edition), the database must be cited when you use this package.

## Citation of the database in English

- List of plant species in China (2022 Edition), 2022, Plant Data Center of Chinese Academy of Sciences, DOI:10.12282/plantdata.0061

## Citation of the database  in Chinese
- 中国植物物种名录（2022版）,(中国科学院植物研究所), 2022, 中国科学院植物科学数据中心, DOI:10.12282/plantdata.0061


## Note

*THIS PACKAGE HAS NOT BEEN PEER REVIEWED, USE AT YOUR OWN RISK.* 

If you have any question or comments, feel free to send an email to the package maintainer **Dr. Jinlong Zhang** .

