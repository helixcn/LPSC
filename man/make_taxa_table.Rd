% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source_code.R
\name{make_taxa_table}
\alias{make_taxa_table}
\title{Create a taxa table including family,genus and species}
\usage{
make_taxa_table(x, file = NULL, substitute.sp.white.space = "_")
}
\arguments{
\item{x}{a dataframe containing the three columns, 1. YOUR_SEARCH, 2. family, 3. genus}

\item{file}{the name of the plain text file to generate}

\item{substitute.sp.white.space}{character string, by default -}
}
\value{
a character vector
}
\description{
Create a taxa table including family,genus and species
}
\examples{

aaaa <- get_accepted_name(c("小叶青冈",
 "Cyclobalanopsis myrsinifolia",
 "Machilus chekiangensis",
 "宫廷胡萝卜"))
make_taxa_table(aaaa)

}
