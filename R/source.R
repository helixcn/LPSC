# library(here)
# library(openxlsx)
# xlsx_name <- "Sp2000cn-2023 植物完整版含简表 v1.043.xlsx"
# sheet_names <- getSheetNames(file = xlsx_name)

# dat_all_accepted_sp2023_original     <-
#   read.xlsx(xlsx_name, "scientific_names-物种接受名简表")

# 因最后一列的列名与之前某一列的列名重复，因此需要删除
# dat_all_accepted_sp2023 <- dat_all_accepted_sp2023_original[,-ncol(dat_all_accepted_sp2023_original)]
# dat_family_summary      <- read.xlsx(xlsx_name, "科统计")
# dat_genus_summary       <- read.xlsx(xlsx_name, "属统计")

# prompt(dat_all_accepted_sp2023)

# dat_all_sp2023           <- read.xlsx(xlsx_name, "scientific_names")
# prompt(dat_all_sp2023)

# dat_name_status         <-
#  read.xlsx(xlsx_name, "specialist_scientificname")
# dat_families            <- read.xlsx(xlsx_name, "families")
# dat_common_names        <- read.xlsx(xlsx_name, "common_names")
# dat_sp2000_status       <- read.xlsx(xlsx_name, "sp2000_statuses")
# dat_distribution        <- read.xlsx(xlsx_name, "distribution")
# dat_distribution_code   <- read.xlsx(xlsx_name, "distributioncode")
# dat_references          <- read.xlsx(xlsx_name, "references")
# dat_reference_links     <- read.xlsx(xlsx_name, "reference_links")
# dat_databases           <- read.xlsx(xlsx_name, "databases")
# dat_contact_specialists <- read.xlsx(xlsx_name, "specialists")

#' Look up the accepted name for given scientific names 
#'
#' @param x a character vector representing scientific names
#' @param db the database used, by default CPSC2023
#'
#' @return a dataframe showing the details of accepted names
#' @export
#'
#' @examples
#' 
#' x2 <- "Cinnamomum camphora"
#' get_accepted_name(x2)
#' get_accepted_name(c("Michelia maudiae", "Machilus", "Cinnamomum camphora"))
#' get_accepted_name(c("Michelia maudiae", "Cinnamomum camphora", "machilus", ""))
#' get_accepted_name("Michelia")
#' get_accepted_name(c("Michelia", "magnolia"))
#' get_accepted_name("Michelia maudiae")
get_accepted_name <- function(x, db = LPSC::dat_all_sp2023) {
  get_accepted_name_one <- function(x, db) {
    
    if (length(x) > 1) {
      stop("Only one name allowed")
    }
    
    db_empty_row <- db[1, ]
    db_empty_row[1, ] <- NA
    
    sub_dat_sci_names <- subset(db, db$canonical_name %in% x)
    
    sub_sub_dat_sci_names <- subset(
      db,
      db$name_code %in% c(
        sub_dat_sci_names$accepted_name_code,
        sub_dat_sci_names$name_code
      )
    ) # select the rows that the name_code/accepted_name_code appear for the query
    
    if (nrow(sub_sub_dat_sci_names) > 1) {
      # If x is a synonym
      comparison <- sub_sub_dat_sci_names$accepted_name_code ==
        sub_sub_dat_sci_names$name_code
      accepted_name <- subset(sub_sub_dat_sci_names, subset = comparison)
    } else{
      # x is an accepted species or could not be found in the database
      accepted_name <- sub_dat_sci_names
    }
    if (nrow(accepted_name) > 0) {
      print(paste(
        "The accepted name for",
        x,
        "is",
        paste(accepted_name$canonical_name,
              accepted_name$author)
      ))
    } else {
      print(paste(x, "could not be found in the databse"))
      accepted_name <- db_empty_row
    }
    return(cbind(YOUR_SEARCH = x, accepted_name))
  }
  
  ## obtain a subset of the db, the subsequent query will be based on this subset.
  ## This is the key to speedup the search
  sub_dat_sci_names <- subset(db, db$canonical_name %in% x)
  
  sub_sub_dat_sci_names <- subset(
    db,
    db$name_code %in% c(
      sub_dat_sci_names$accepted_name_code,
      sub_dat_sci_names$name_code
    )
  ) 
  
  x <- Cap(REPLACE(x)) # Standardise the search, in case,
  # there are multiple white spaces or the first letter is not capitalised.
  res_seed <- get_accepted_name_one(x[1], db = sub_sub_dat_sci_names)
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      res_temp <- get_accepted_name_one(x[i], db = sub_sub_dat_sci_names)
      res_seed <- rbind(res_seed, res_temp)
    }
  }
  return(res_seed)
}

#' Show details of species
#'
#' @param x a character string representing scientif names or Chinese names of plants
#' @param db database used
#'
#' @return a dataframe showing the details of accepted species of List of Plant Species China
#' @export
#'
#' @examples
#' 
#' res1 <- show_detail(c("木栏","香港木兰"))
#' show_detail(c("樟叶泡花树", "Machilus"))
#' show_detail("Machilus gamblei")
#' show_detail(c("大叶木兰","香港木兰","雾水葛"))
#' show_detail("滇青冈")
#' show_detail(c("滇青冈", "Quercus glaucoides"))
#' show_detail(c("", "Quercus glaucoides"))
#' show_detail(c("", ""))
show_detail <- function(x, db = LPSC::dat_all_accepted_sp2023) {
  ## For accepted names only (Chinese names work)
  show_detail_one <- function(x, db) {
    if (length(x) > 1) {
      stop("only one species allowed")
    }
    
    empty_row <- db[1, ]
    empty_row[1, ] <- NA # Empty row must be shown in the result, 
    # and the values for all colums are NAs.
    
    sub_dat1 <- subset(db, db$canonical_name %in% x) # Scientific name
    sub_dat2 <- subset(db, db$species_c %in% x) # Chinese Name
    res <- unique(rbind(sub_dat1, sub_dat2))
    if (nrow(res) < 1) {
      res <- empty_row
      print(paste("Note: ",
                  x[!x %in% unique(c(res$canonical_name, res$species_c))],
                  "could not be found in the database"))
    }
    return(cbind(YOUR_SEARCH = x, res))
  }
  
  x <- Cap(REPLACE(x)) # Standardise the search, in case
  # there are multiple whitespaces or the first letter is not capitalised.
  res_seed <- show_detail_one(x[1], db = db)
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      res_temp <- show_detail_one(x[i], db = db)
      res_seed <- rbind(res_seed, res_temp)
    }
  }
  return(res_seed)
}

#' Capitalize the first letter
#'
#' @param x a character string
#'
#' @return a character string whose first letter is capitalized
#'
#' @examples
#'
#' Cap("michelia")
Cap <- function(x) {
  paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2)), sep = "")
}


#' replace multiple white space
#'
#' @param x a character string
#'
#' @return a character string whose leading white space multiple white spaces have been replaced.
#'
#' @examples
#'
#' REPLACE("   Michelia    alba   ")
REPLACE <- function(x) {
  temp <- gsub(" +", " ", gsub(",+", ", ",
                               gsub(", +", ",", x)))
  res <- gsub("^[[:space:]]+|[[:space:]]+$", "", temp)
  return(res)
}
