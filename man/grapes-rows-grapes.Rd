% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R00-Utilities_and_operators.R
\name{\%rows\%}
\alias{\%rows\%}
\title{Binary Operator for Subsetting Rows While Preserving Attributes}
\usage{
x \%rows\% y
}
\arguments{
\item{x}{A data frame.}

\item{y}{An integer vector, logical vector, or character vector
specifying the rows in \code{x} to keep.}
}
\value{
A data frame, a subset of \code{x}.
}
\description{
The binary operator \code{\%rows\%} returns the subset of specified
rows for a data frame without removing column attributes.
}
\examples{
dtf <- data.frame(
  X1 = 1:4,
  X2 = LETTERS[1:4],
  X3 = c( TRUE, TRUE, FALSE, FALSE )
)
attributes( dtf$X1 ) <- list( Example_attr = "Integer" )
attributes( dtf$X2 ) <- list( Example_attr = "Character" )
attributes( dtf$X3 ) <- list( Example_attr = "Logical" )

# Each column has an attribute
str( dtf )

# Normal indexing removes attributes
str( dtf[1:2,] )

# Can use operator to avoid losing attributes
str( dtf \%rows\% 1:2 )

}
\author{
Kevin Potter
}
