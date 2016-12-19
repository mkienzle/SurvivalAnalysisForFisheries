\name{Coaa2Caaa}
\alias{Coaa2Caaa}
\title{Convert cohort-at-age to catch-at-age - the opposite of Caaa2Coaa}
\description{
  Convert a matrix of dimensions number of cohorts x age into a matrix of year x age}
\usage{Coaa2Caaa(cohort.mat)}
\arguments{
  \item{cohort.mat}{a matrix of cohort at age}
  }
\examples{
(nb.at.age <- matrix(sample(1:10, 40, replace = TRUE), nrow = 10, ncol = 4))
tmp <- Caaa2Coaa(nb.at.age)
Coaa2Caaa(tmp)
}

\references{NONE}
\keyword{misc}
