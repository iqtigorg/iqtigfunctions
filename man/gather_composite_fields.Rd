% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gather-composite-fields.R
\name{gather_composite_fields}
\alias{gather_composite_fields}
\title{Gather composite fields}
\usage{
gather_composite_fields(data)
}
\arguments{
\item{data}{a data frame}
}
\description{
Certain fields can have multiple values that are often encoded in a sequence
of columns, typically named FIELD_1, FIELD_2, FIELD_3, etc. This function
identifies these columns and collapses them into a single list column
"FIELD". This is in particular necessary for \code{\link{\%any_like\%}},
\code{\link{\%all_like\%}} etc., which expect such columns as input.
}
\examples{
data <- data.frame(
  OPSCHLUESSEL_1 = c("C5923", "C5923", "C575"),
  OPSCHLUESSEL_2 = c("C599.3", "C59AB", NA_character_),
  OPSCHLUESSEL_3 = NA_character_,
  TDS_B = c(1, 2, 3),
  stringsAsFactors = FALSE
)
data <- gather_composite_fields(data)
data$OPSCHLUESSEL

}
