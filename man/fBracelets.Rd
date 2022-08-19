\name{fBracelets}
\alias{fBracelets}
\title{Bracelets of a fixed configuration
}

\description{
The function generates all the representatives of the bracelets corresponding to a fixed configuration.
}
\usage{
fBracelets(pv=c(), bOut=FALSE, fn=1)
}
\arguments{
  \item{pv}{vector: the fixed configuration}
  \item{bOut}{boolean: if \code{TRUE}, the function produces a compact result}
  \item{fn}{integer: the first value of the alphabet, the default is 1}
}


\details{
The function generates all the representatives of the bracelets corresponding 
to a fixed configuration. If the second parameter (\code{bOut}) is set equal to \code{TRUE}, the function produces a compact result. The third parameter 
(\code{fn}) initializes the first value of the alphabet, which by default is equal to 1. For example, to generate all the representatives of the bracelets 
corresponding to the fixed configuration \code{(2,1,1)}, run \code{fBracelets(c(2,1,1))}. In such a case the alphabet is \code{{1,2,3}}. Using the \code{\link{nPerm}} function of the \code{kStatistics} package, the function first generates all the permutations of the vector \code{(1,1,2,3)} corresponding to the configuration \code{(2,1,1)}, that is 
\tabular{ll}{\code{(I)} \tab \code{(3,2,1,1), (2,3,1,1), (3,1,1,2), ..., (1,1,2,3)} (12 in total)}
Then the \code{\link{cBracelets}} function of the \code{Necklaces} package is called with input equal to each vector in \code{(I)}. For each obtained list, only the representative survives. At the end all the representatives of the  bracelets are printed, that are \code{[1 1 2 3], [1 2 1 3]}.

}

\value{\item{list}{the list containing all the representatives of the bracelets corresponding to a fixed configuration.}}

\references{
Di Nardo, E. (2014) On a symbolic representation of non-central Wishart random matrices with applications. Jour. Mult. Anal. Vol.125, 121--135. (\url{https://arxiv.org/abs/1312.4395})

Di Nardo, E., and Guarino., G.  (2022)  Necklaces and bracelets in R - (\url{https://arxiv.org/abs/2208.06855})

Di Nardo, E., and Guarino., G.  (2022) kStatistics: Unbiased Estimates of Joint Cumulant Products from the Multivariate Faa Di Bruno's Formula. The R journal - In press.  (\url{https://arxiv.org/abs/2206.15348})

Flajolet, P., and Sedgewick, R. (2009) Analytic combinatorics. Cambridge University press.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
The function calls the \code{\link{cBracelets}} function in the \code{Necklaces} package and the \code{\link{nPerm}} function in the \code{kStatistics} package.}
 

\seealso{
\code{\link{cBracelets}},
\code{\link{LyndonW}},
\code{\link{sBruijn}}
}


\examples{
# Generate all the bracelets of the configuration (2,1,1) 
# corresponding to the vector (1,1,2,3)
fBracelets(c(2,1,1))  

# The previous result in a compact form
fBracelets(c(2,1,1),TRUE)

# The first value of the alphabet is set equal to zero
fBracelets(c(2,1,1),TRUE,0)


}

% R KEYWORDS
\keyword{Necklaces}
\keyword{Bracelets}
\keyword{Lyndon words}
\keyword{de Bruijn sequences}

