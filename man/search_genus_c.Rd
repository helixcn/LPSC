% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source_code.R
\name{search_genus_c}
\alias{search_genus_c}
\title{Search the List of plant species in China using a Chinese genus name}
\usage{
search_genus_c(x, dat = LPSC::LPSC2022)
}
\arguments{
\item{x}{a character string containing the genera's Chinese names}

\item{dat}{a dataframe containing all the data of the List of plant species in China}
}
\value{
a subset of dat, only the matched entries
}
\description{
Search the List of plant species in China using a Chinese genus name
}
\examples{

search_genus_c("含笑属") # 完整匹配
search_genus_c("珙桐属") # 完整匹配
search_genus_c(c("珙桐属", "银杏属")) # 完整匹配


}
