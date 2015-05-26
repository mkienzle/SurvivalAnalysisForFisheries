\name{Caaa2Coaa}
\alias{Caaa2Coaa}
\title{Convert catch-at-age to cohort-at-age}
\description{
  Convert a matrix of dimensions year x age into a matrix of cohort x age}
\usage{Caaa2Coaa(mat)}
\arguments{
  \item{mat}{a matrix of catch at age}
  }
\examples{
nb.at.age <- matrix(sample(1:10, 40, replace = TRUE), nrow = 10, ncol = 4)
Caaa2Coaa(nb.at.age)
}
\references{NONE}
\keyword{misc}
