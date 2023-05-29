# What is LPSC?

It is an R package for searching the Checklist of Plant Species in China (currently 2023). This will help standardising the checklist of a given region.

Note: The codes for this package have been rewritten due to the extensive changes in the database released in 2023.

# How to install?

```R
devtools::install_github("helixcn/LPSC", build_vignettes = TRUE)
```

If you haven't had `devtools` installed, please install it by typing `"install.packages("devtools")"` in the R console.


# How to use?

```R
library(LPSC)

# 查询物种的接受名（只限学名）

get_accepted_name("Cyclobalanopsis myrsinifolia")

aaaa <- get_accepted_name(c(
  "Cyclobalanopsis myrsinifolia",
  "Machilus chekiangensis"
))

get_accepted_name(c("Carex phaeopoda"))
get_accepted_name(c("Cyclobalanopsis myrsinifolia"))
get_accepted_name(c("Carex phaeopoda2",
                    "Carex phaeopoda"))
get_accepted_name(c("Carex phaeopoda",
                    "苹果", "华中山楂", "华中山楂2"))

# 查询中文名或学名（仅限接受名），显示科、属和分布等信息

show_detail("华中山楂2")
show_detail(c("Carex phaeopoda",
                           "苹果", "华中山楂"))
show_detail("刺枝麻黄")
show_detail("单子麻黄2")

# 查询物种的濒危等级，学名或者中文名均可
# Search species conservation status in 中国生物多样性红色名录—高等植物卷（2020）
# China Biodiversity Red List (2020) higher plants

show_iucn_status("竹叶青冈")
show_iucn_status("木姜叶青冈")
show_iucn_status("Quercus glauca")
show_iucn_status(c("樟叶泡花树", "香港木兰", "蛇藤", "匙叶黄杨", "海南姜"))
show_iucn_status(c("樟叶泡花树", "香港木兰", "蛇藤", "", "Wikstroemia nutans"))
```

# How to cite?

- Zhang J (2023). _LPSC: Tools for searching List of plant
  species in China_. R package version ,
  <https://github.com/helixcn/LPSC>.
  
As the data came from List of plant species in China (2023 Edition), the database must be cited when you use this package. 

Citation Information:

- 数据来源于：中国植物物种名录（2023版）,(中国科学院植物研究所), 2023, 中国科学院植物科学数据中心, DOI:10.12282/plantdata.1390, CSTR:34735.11.PLANTDATA.1390

- Normative references：Checklist of plant species in China (2023 Edition), 2023, Plant Data Center of Chinese Academy of Sciences, DOI:10.12282/plantdata.1390, CSTR:34735.11.PLANTDATA.1390


## Note

*THIS PACKAGE HAS NOT BEEN PEER REVIEWED, USE AT YOUR OWN RISK.* 

If you have any question or comments, feel free to send an email to the package maintainer **Dr. Jinlong Zhang**.

