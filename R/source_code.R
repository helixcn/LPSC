## To do:  1. 输入的结果，一定要按照顺序排列，也就是merge的结果不能排序

################# data prior to 0.1.5
# library(here)
# LPSC2022 <-
#  read.csv("LPSC2022.csv",
#           header = TRUE, row.names = 1)

## summary_sp2000cn <- function(x){
##   number_records <- nrow(x)
##   number_accepted_names <- nrow(x)
##   number_genera
##   number_families
##   number_accepted_names
##   number_genera_by_phylum
##   number_families_by_phylum
##   number_accepted_names_by_phylu
##   number_names_not_matched
## }

################# data since 0.1.6
## library(here)
## library(openxlsx)
## library(usethis)
## sheet1_scientific_names <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 1)
## sheet2_specialist_scientificname <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 2)
## sheet3_sp2000_statuses <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 3)
## sheet4_common_names <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 4)
## sheet5_families <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 5)
## sheet6_distribution <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 6)
## sheet7_distribution_code <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 7)
## sheet8_references <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 8)
## sheet9_specialists <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 9)
## sheet10_databases <- read.xlsx("sp2000-2022-scientific_names-1.052-完整版.xlsx", sheet = 10)
##
## res1 <- merge(sheet1_scientific_names,
##               sheet5_families,
##               by.x = "family_id",
##               by.y = "record_id",
##               all = TRUE)
## res2 <- merge(res1, sheet6_distribution,
##               by.x = "name_code",
##               by.y = "name_code",
##               all = TRUE)
## LPSC2022 <- merge(res2, sheet8_references,
##                   by.x = "name_code",
##                   by.y = "T-record_id",
##                   all = TRUE)
##
## # write.xlsx(res3, "data_merged_from_xlsx.xlsx")
##
## # package.skeleton(name = "SPSC", list = "LPSC2022")
##
## use_data(LPSC2022, overwrite = TRUE) # save in 'xz' in the 'data' folder
##
## prompt(LPSC2022) # generate the Rd file, edit the Rd file in the man folder.


############### Functions ###################

#' Capitalize the first letter
#'
#' @param x a character string
#'
#' @return a character string whose first letter is capitalized
#' @export
#'
#' @examples
#'
#' Cap("michelia")
#'
#'
Cap <- function(x) {
  paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2)), sep = "")
}



#' replace multiple white space
#'
#' @param x a character string
#'
#' @return a character string whose leading white space multiple white spaces have been replaced.
#' @export
#'
#' @examples
#'
#' REPLACE("   Michelia    alba   ")
#'
#'
REPLACE <- function(x) {
  temp <- gsub(" +", " ", gsub(",+", ", ",
                               gsub(", +", ",", x)))
  res <- gsub("^[[:space:]]+|[[:space:]]+$", "", temp)
  return(res)
}


#' Select and reorder and column names for the dataset
#'
#' @param x a dataframe derived from List of plant species in China with more columns
#'
#' @return a reordered List of plant species in China
#' @export
#'
#' @examples
#'
#' temp <- head(LPSC::LPSC2022)
#' temp2 <- cbind(temp, YOUR_SEARCH = paste0("something_entered", 1:nrow(temp)))
#' format_output(temp2)
#'
format_output <- function(x) {
  subset(
    x,
    select = c(
      "YOUR_SEARCH",
      "accepted_name_code",
      "name_code",
      "canonical_name",
      "genus",
      "genus_c",
      "species",
      "species_c",
      "infraspecies",
      "infraspecies2",
      "infraspecies_c",
      "infraspecies_c2",
      "infraspecies_marker",
      "infraspecies_marker2",
      "author.x",
      "distribution",
      "distribution_c",
      "hierarchy_code",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "superfamily",
      "kingdom_c",
      "phylum_c",
      "class_c",
      "order_c",
      "family_c",
      "superfamily_c",
      "database_name",
      "author.y",
      "author_c",
      "year",
      "title",
      "title_c",
      "source",
      "source_c"
    )
  )
}

#' Create a taxa table including family,genus and species
#'
#' @param x a dataframe containing the three columns, 1. YOUR_SEARCH, 2. family, 3. genus
#' @param file the name of the plain text file to generate
#' @param substitute.sp.white.space character string, by default -
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' aaaa <- get_accepted_name(c("小叶青冈",
#'  "Cyclobalanopsis myrsinifolia",
#'  "Machilus chekiangensis",
#'  "宫廷胡萝卜"))
#' make_taxa_table(aaaa)
#'
make_taxa_table <- function(x,
                            file = NULL,
                            substitute.sp.white.space = "_") {
  if (nrow(x) < 1) {
    stop("at least one row is needed")
  }
  res <-
    paste(x$family,
          x$genus,
          gsub(" ", substitute.sp.white.space, x$YOUR_SEARCH),
          sep = "/")
  if (!is.null(file)) {
    writeLines(res, file)
  }
  return(res)
}


# 为提高效率，查询应该允许输入多个

#' Search the List of plant species in China using the canonical name
#'
#' @param x a character vector, each element represents a scientific name
#' @param dat the dataset used, here the dataframe of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries
#' @export
#'
#' @examples
#'
#' search_canonical_name(x = "ginkgo biloba") # 全名匹配
#' search_canonical_name(x = "   ginkgo  biloba ") # 全名匹配
#' search_canonical_name(x = "Ginkgo biloba") # 全名匹配
#' search_canonical_name(x = "Ginkgo biloba ") # 全名匹配
#' search_canonical_name(x = "Ginkgo  biloba ") # 全名匹配
#' search_canonical_name(x = "Ginkgo biloba2") # 全名匹配
#' search_canonical_name(x = c("Ginkgo biloba2",
#' "Salix wangiana",
#' "Machilus chekiangensis",
#' "Ziziphus jujuba")) # 全名匹配

#' aaa <- c("Ginkgo biloba2",
#' "Salix wangiana",
#' "Machilus chekiangensis",
#' "Ziziphus jujuba")
#' search_canonical_name(aaa)
#'
#'
search_canonical_name <- function(x, dat = LPSC::LPSC2022) {
  x <- Cap(REPLACE(x))
  res <- subset(dat, dat$canonical_name %in% x)
  res$canonical_name2 <- res$canonical_name
  # 因为res在merge之后，canonical_name便会消失，
  # 为保持各函数输出一致，先保存为canonical_name2
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "canonical_name",
                sort = FALSE,
                all = TRUE)
  res2$canonical_name <- res2$canonical_name2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}


#' Search the List of plant species in China using genera
#'
#' @param x a character vector containing the genera's names
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries
#' @export
#'
#' @examples
#'
#' search_genus("Ginkgo")
#' search_genus("michelia")
#' search_genus("Lindera")
#' search_genus(c("Ginkgo", "木兰"))
#' search_genus(c("Ginkgo", "木兰", "Machilus", "Acer"))
#'
#'
search_genus <- function(x, dat = LPSC::LPSC2022) {
  x <- Cap(REPLACE(x))
  res <- subset(dat, dat$genus %in% x)
  res$genus2 <- res$genus
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "genus",
                sort = FALSE,
                all = TRUE)
  res2$genus <- res2$genus2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}


#' Search the List of plant species in China using a Chinese genus name
#'
#' @param x a character string containing the genera's Chinese names
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#'
#' search_genus_c("含笑属") # 完整匹配
#' search_genus_c("珙桐属") # 完整匹配
#' search_genus_c(c("珙桐属", "银杏属")) # 完整匹配
#'
#'
search_genus_c <- function(x, dat = LPSC::LPSC2022) {
  x <- REPLACE(x)
  res <- subset(dat, dat$genus_c %in% x)
  res$genus_c2 <- res$genus_c
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "genus_c",
                sort = FALSE,
                all = TRUE)
  res2$genus_c <- res2$genus_c2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}


#' Search List of plant species in China using a species' Chinese name
#'
#' @param x a character string containing species' Chinese names
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#'
#' search_species_c("银杏")
#' search_species_c(c("银杏",
#' "篦子三尖杉", "八角莲",
#' "扁果草", "黄樟", "黄樟2"))
#'
#'
search_species_c <- function(x, dat = LPSC::LPSC2022) {
  x <- REPLACE(x)
  res <- subset(dat, dat$species_c %in% x)
  res$species_c2 <- res$species_c
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "species_c",
                sort = FALSE,
                all = TRUE)
  res2$species_c <- res2$species_c2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}




#' Search the List of plant species in China using a infraspecies' Chinese name
#'
#' @param x a character string containing the infraspecies' Chinese name
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#'
#' search_infraspecies_c("刺枝麻黄")
#' search_infraspecies_c(c("刺枝麻黄", "云实"))
#'
search_infraspecies_c <- function(x, dat = LPSC::LPSC2022) {
  x <- REPLACE(x)
  res <- subset(dat, dat$infraspecies_c %in% x)
  res$infraspecies_c2 <- res$infraspecies_c
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "infraspecies_c",
                sort = FALSE,
                all = TRUE)
  res2$infraspecies_c <- res2$infraspecies_c2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}



# 获得接受名 ， 本函数可以接收中文名以及学名

#' Search the accepted name for a scientific name or a Chinese name in List of plant species in China
#'
#' @param x a character string containing the species' scientific name or Chinese name
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#'
#' aaaa <- get_accepted_name(c("小叶青冈",
#' "Cyclobalanopsis myrsinifolia",
#' "Machilus chekiangensis",
#' "宫廷胡萝卜"))
#' make_taxa_table(aaaa)

#' get_accepted_name(c("Carex phaeopoda"))
#' get_accepted_name(c("Cyclobalanopsis myrsinifolia"))
#' get_accepted_name(c("Carex phaeopoda2",
#' "Carex phaeopoda"))
#' get_accepted_name(c("Carex phaeopoda",
#' "苹果", "华中山楂", "华中山楂2"))
#' # 直接查询出接受名，中文，拉丁文均可
#' get_accepted_name("华中山楂2")
#' xxx <- get_accepted_name(c("Carex phaeopoda",
#'  "苹果", "华中山楂"))
#' make_taxa_table(xxx)

#' get_accepted_name("刺枝麻黄")
#' ttt <- get_accepted_name("单子麻黄2")
#' make_taxa_table(ttt)
#'
#'
#'
get_accepted_name <- function(x, dat = LPSC::LPSC2022) {
  x <- unique(REPLACE(x))
  res0 <- format_output(cbind(YOUR_SEARCH = "", dat[1,]))
  # Create an empty template for rbind (in the for loop)
  res0[] <- NA

  for (i in 1:length(x)){
    temp <- unique(rbind(
       search_canonical_name(x[i]),
       get_accepted_name_one(x[i]),
       search_species_c(x[i]),
       search_infraspecies_c(x[i])
     ))
    res0 <- rbind(res0, temp)
  }
  # One condition to get the accepted names  `
  res2 <- subset(res0, res0$name_code  %in% res0$accepted_name_code &!is.na(res0$name_code))

  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res2,
                by.x = "YOUR_SEARCH",
                by.y = "YOUR_SEARCH",
                sort = FALSE,
                all = TRUE)
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}


# 获得接受名

## Obtain the accepted name when a canonical name is provided

#' Search the accepted name for a scientific name in List of plant species in China
#'
#' @param x0 a character string containing one scientific name only
#' @param y0 a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#' get_accepted_name_one(c("Carex phaeopoda"))
#' get_accepted_name_one(c("Cyclobalanopsis myrsinifolia"))
#' get_accepted_name_one("Adiantum pedatum  ")
#' get_accepted_name_one("Adiantum pedatum2  ")
#' get_accepted_name_one("Machilus chekiangensis")
#' get_accepted_name_one("Quercus myrsinifolia")
#' get_accepted_name_one("Cyclobalanopsis myrsinifolia")
#'
#'
get_accepted_name_one <- function(x0, y0= LPSC::LPSC2022){
  x0 <- REPLACE(x0)
  if(length(x0) > 1){
    stop("Please provide one name only")
  }
  if(is.na(x0) | is.null(x0)| x0 == ""){
    stop("Please provide a valid scientific name")
  }
  res <- search_canonical_name(x0)
  res2 <- res[1,-1]
  res2[] <- NA
  #有可能一个名字，匹配了多个学名，种加词一样，但是命名人不同，这种情况就要调出来全部的记录，再检查每条记录的accepted name code是否与name code相同。
  # res is the result of the search using
    # canonical_name
  if(nrow(res) == 1 ){
       if(is.na(res$name_code)) {
          return(res)
       } else {
         res2 <- subset(y0, y0$name_code %in% res$accepted_name_code)
         if(nrow(res2) == 0){
           message(paste("Note: the accepted name for",
                         paste(res$canonical_name, res$author.x), "could not be found\n"))

           return(res) # In case if the accepted name does not exist

         }
         if(!res$name_code %in% res$accepted_name_code){
           message(paste("Note: the accepted name for",
                         paste(res$canonical_name, res$author.x), "is:\n",
                         paste(res2$canonical_name, res2$author.x), "\n"))
         }
         return(format_output(cbind(YOUR_SEARCH = x0, res2)))
       }
    } else {
        message(paste(x0, "matched multiple names (using search_canonical_name)!"))
    for (i in 1:nrow(res)){
      res2_i <- subset(y0, y0$name_code %in% res$accepted_name_code[i])
      # Obtain the accepted name，每个种，只可能对应一个accepted name，已经保存在res2_i中
      res2 <- rbind(res2, format_output(cbind(YOUR_SEARCH = "", res2_i))[,-1]) #res2 is a dataframe consisted by NA when i = 1.
          message(paste("Note: the accepted name for",
                    paste(res$canonical_name[i], res$author.x[i]), "is:\n",
                    paste(res2_i$canonical_name, res2_i$author.x), "\n"))
    }
        return(format_output(cbind(YOUR_SEARCH = x0, res2[-1,])))
    }
}




#' Search the List of plant species in China using Family Names
#'
#' @param x a character string representing the Family Names
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#'
#' search_family("Poaceae") # 完整匹配
#'
search_family <- function(x, dat = LPSC::LPSC2022) {
  res <- subset(dat, dat$family %in% x)
  res$family2 <- res$family
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "family",
                sort = FALSE,
                all = TRUE)
  res2$family <- res2$family2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}


#' Search the List of plant species in China using Chinese Family Names
#'
#' @param x a string in Chinese
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#' search_family_c("连香树科") # 完整匹配
#'
#'
search_family_c <- function(x, dat = LPSC::LPSC2022) {
  res <- subset(dat, dat$family_c %in% x)
  res$family_c2 <- res$family_c
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "family_c",
                sort = FALSE,
                all = TRUE)
  res2$family_c <- res2$family_c2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}



#' Search the literature where the name was published
#'
#' @param x a character string representing the name of the literature
#' @param dat a dataframe containing all the data of the List of plant species in China
#'
#' @return a subset of dat, only the matched entries

#' @export
#'
#' @examples
#'
#' search_source("Sargent") # 部分匹配
#'
search_source <- function(x, dat = LPSC::LPSC2022) {
  res <- subset(dat, grepl(pattern = x, dat$source, ignore.case = TRUE))
  res$source2 <- res$source
  x2 <- data.frame(number = 1:length(x), x)
  colnames(x2) <- c("number","YOUR_SEARCH")
  res2 <- merge(x2,
                res,
                by.x = "YOUR_SEARCH",
                by.y = "source",
                sort = FALSE,
                all = TRUE)
  res2$source <- res2$source2
  res2 <- res2[order(res2$number),]
  res3 <- format_output(res2)
  return(res3)
}

