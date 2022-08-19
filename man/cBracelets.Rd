\name{cBracelets}
\alias{cBracelets}
\title{Elements in a bracelet}
\description{The function generates the elements of a bracelet in equivalence relation with the vector given in input.}
\usage{
cBracelets(v=c(), bOut=FALSE)
}

\arguments{
  \item{v}{vector: input vector}
  \item{bOut}{boolean: if \code{TRUE}, the function produces a compact result}
}


\details{
The function generates the elements of a bracelet which are in equivalence relation with the vector given in input. The first parameter is the input vector. If the second parameter (\code{bOut}) is set equal to \code{TRUE},
the function produces a compact result.

Example: \code{cBracelets(c(1,0,2,1))} produces the following result:
\tabular{l}{\cr
\code{[1] 0 1 1 2} \cr
\code{[1] 0 2 1 1} \cr
\code{[1] 1 0 2 1} \cr
\code{[1] 1 1 0 2} \cr
\code{[1] 1 1 2 0} \cr
\code{[1] 1 2 0 1} \cr
\code{[1] 2 0 1 1} \cr
\code{[1] 2 1 1 0} \cr
}

\code{cBracelets(c(1,0,2,1),TRUE)} produces the following result:
\tabular{l}{\cr
\code{[ 0 1 1 2 ]  ( 1 )} \cr
\code{[ 0 2 1 1 ]  ( 2 )} \cr
\code{[ 1 0 2 1 ]  ( 3 )} \cr
\code{[ 1 1 0 2 ]  ( 4 )} \cr
\code{[ 1 1 2 0 ]  ( 5 )} \cr
\code{[ 1 2 0 1 ]  ( 6 )} \cr
\code{[ 2 0 1 1 ]  ( 7 )} \cr
\code{[ 2 1 1 0 ]  ( 8 )} \cr
}

Note that \code{0 1 1 2} is the representative of the class, that is the minimum in lexicographical order. 

}

\value{\item{list}{the list containing all the elements of the bracelet in equivalence  relation with the vector given in input}}

\references{

Di Nardo, E., and Guarino., G.  (2022) kStatistics: Unbiased Estimates of Joint Cumulant Products from the Multivariate Faa Di Bruno's Formula. The R journal - In press.  (\url{https://arxiv.org/abs/2206.15348})

Di Nardo, E., and Guarino., G.  (2022)  Necklaces and bracelets in R - (\url{https://arxiv.org/abs/2208.06855})

Flajolet, P., and Sedgewick, R. (2009) Analytic combinatorics. Cambridge University press.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
The function is called from the \code{\link{fBracelets}} function in the \code{Necklaces} package.
}


\seealso{
\code{\link{fBracelets}}
}


\examples{
# Generate the elements of the bracelet in equivalence relation with 
# the input vector (1,0,2,1) 
cBracelets(c(1,0,2,1)) 

# The previous result in a compact form
cBracelets(c(1,0,2,1),TRUE) 

}

% R KEYWORDS
\keyword{Necklaces}
\keyword{Bracelets}
\keyword{Lyndon words}
\keyword{de Bruijn sequences}

