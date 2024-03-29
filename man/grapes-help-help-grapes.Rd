% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R00-Utilities_and_operators.R
\name{\%??\%}
\alias{\%??\%}
\title{Nullish Coalescing Operator}
\usage{
lhs \%??\% rhs
}
\arguments{
\item{lhs}{Any value.}

\item{rhs}{Any non-null and non-NA value.}
}
\value{
lhs or rhs
}
\description{
Returns the RHS operand if the LHS operand is NULL or NA.
}
\examples{
1 \%??\% 2
NULL \%??\% 3
NA \%??\% NA \%??\% 4

}
\author{
Michael Pascale
}
