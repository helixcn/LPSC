\name{LPSC2022}
\alias{LPSC2022}
\docType{data}
\title{
List of plant species in China 2022
}
\description{
List of plant species in China (2022 Edition)
}
\usage{data("LPSC2022")}
\format{
  A data frame with 231945 observations on the following 75 variables.
  \describe{
    \item{\code{name_code}}{a character vector}
    \item{\code{family_id}}{a character vector}
    \item{\code{SP_ID}}{a numeric vector}
    \item{\code{accepted_name_code}}{a character vector}
    \item{\code{web_site}}{a numeric vector}
    \item{\code{EditTime.x}}{a numeric vector}
    \item{\code{Family}}{a numeric vector}
    \item{\code{canonical_name}}{a character vector}
    \item{\code{canonical_name1}}{a character vector}
    \item{\code{genus}}{a character vector}
    \item{\code{genus_c}}{a character vector}
    \item{\code{species}}{a character vector}
    \item{\code{species_c}}{a character vector}
    \item{\code{infraspecies}}{a character vector}
    \item{\code{infraspecies2}}{a numeric vector}
    \item{\code{infraspecies_c}}{a character vector}
    \item{\code{infraspecies_c2}}{a numeric vector}
    \item{\code{infraspecies_marker}}{a character vector}
    \item{\code{infraspecies_marker2}}{a numeric vector}
    \item{\code{author.x}}{a character vector}
    \item{\code{scrutiny_date}}{a numeric vector}
    \item{\code{sp2000_status_id}}{a numeric vector}
    \item{\code{database_id.x}}{a character vector}
    \item{\code{specialist_id}}{a character vector}
    \item{\code{is_accepted_name.x}}{a numeric vector}
    \item{\code{genus_c_py}}{a character vector}
    \item{\code{species_c_py}}{a character vector}
    \item{\code{infraspecies_c_py}}{a character vector}
    \item{\code{infraspecies_c_py2}}{a numeric vector}
    \item{\code{genuswithsubgenus}}{a character vector}
    \item{\code{specialist_code}}{a numeric vector}
    \item{\code{family_code.x}}{a numeric vector}
    \item{\code{comments}}{a character vector}
    \item{\code{hierarchy_code}}{a numeric vector}
    \item{\code{family_code.y}}{a character vector}
    \item{\code{kingdom}}{a character vector}
    \item{\code{phylum}}{a character vector}
    \item{\code{class}}{a character vector}
    \item{\code{order}}{a character vector}
    \item{\code{family}}{a character vector}
    \item{\code{superfamily}}{a numeric vector}
    \item{\code{kingdom_c}}{a character vector}
    \item{\code{phylum_c}}{a character vector}
    \item{\code{class_c}}{a character vector}
    \item{\code{order_c}}{a character vector}
    \item{\code{family_c}}{a character vector}
    \item{\code{superfamily_c}}{a numeric vector}
    \item{\code{family_common_name}}{a numeric vector}
    \item{\code{database_name}}{a character vector}
    \item{\code{is_accepted_name.y}}{a numeric vector}
    \item{\code{database_id.y}}{a character vector}
    \item{\code{kingdom_c_py}}{a character vector}
    \item{\code{phylum_c_py}}{a character vector}
    \item{\code{class_c_py}}{a character vector}
    \item{\code{order_c_py}}{a character vector}
    \item{\code{family_c_py}}{a character vector}
    \item{\code{superfamily_c_py}}{a numeric vector}
    \item{\code{record_id}}{a character vector}
    \item{\code{distribution}}{a character vector}
    \item{\code{distribution_c}}{a character vector}
    \item{\code{level4}}{a character vector}
    \item{\code{sea}}{a numeric vector}
    \item{\code{distribution_back}}{a character vector}
    \item{\code{EditTime.y}}{a numeric vector}
    \item{\samp{T-record_id}}{a character vector}
    \item{\code{author.y}}{a numeric vector}
    \item{\code{author_c}}{a character vector}
    \item{\code{year}}{a character vector}
    \item{\code{title}}{a character vector}
    \item{\code{title_c}}{a numeric vector}
    \item{\code{source}}{a character vector}
    \item{\code{source_c}}{a character vector}
    \item{\code{database_id}}{a character vector}
    \item{\code{reference_code}}{a numeric vector}
    \item{\code{EditTime}}{a numeric vector}
  }
}
\details{
List of plant species in China (2022 Edition), 2022, Plant Data Center of Chinese Academy of Sciences, DOI:10.12282/plantdata.0061
}
\source{
https://www.plantplus.cn/cn/datasetdatadown?guid=9d8e682f-968a-4192-b12c-dc018d5e70df
}
\references{
List of plant species in China (2022 Edition), 2022, Plant Data Center of Chinese Academy of Sciences, DOI:10.12282/plantdata.0061
}
\examples{
data(LPSC2022)
## maybe str(LPSC2022) ; plot(LPSC2022) ...
}
\keyword{datasets}
