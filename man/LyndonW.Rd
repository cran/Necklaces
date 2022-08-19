\name{LyndonW}
\alias{LyndonW}
\title{Lyndon words}
\description{
The function generates Lyndon words from necklaces of length \code{n} over an alphabet of \code{m} consecutive non-negative integers.
}
\usage{
LyndonW(n=1, m=2, bOut=FALSE, fn=1)
}

\arguments{
  \item{n}{positive integer: the length of the representatives}
  \item{m}{positive integer: the number of consecutive non-negative integers in the alphabet}
  \item{bOut}{boolean: if \code{TRUE}, the function produces a compact result}
  \item{fn}{integer: the first value of the alphabet, the default is 1}
}

\details{
The function generates Lyndon words from necklaces of length \code{n} over an alphabet of \code{m} consecutive non-negative integers. The last parameter 
(\code{fn}) initializes the first value of the alphabet, which by default is equal to 1. If the parameter (\code{bOut}) is set equal to \code{TRUE}, the function produces a compact result. As example, running \code{LyndonW(5,2, TRUE,0)}, the function  generates Lyndon words in compact form, from the binary necklaces of length \code{5}, that are \code{[0 0 0 0 1], [0 0 0 1 1], [0 0 1 0 1], 
[0 0 1 1 1], [0 1 0 1 1], [0 1 1 1 1]}.

}

\value{\item{list}{the list containing all the Lyndon words  of length \code{n} over an alphabet of \code{m} consecutive non-negative integers.}}

\references{
Di Nardo, E. (2014) On a symbolic representation of non-central Wishart random matrices with applications. Jour. Mult. Anal. Vol.125, 121--135. (\url{https://arxiv.org/abs/1312.4395})

Di Nardo, E., and Guarino., G.  (2022)  Necklaces and bracelets in R - (\url{https://arxiv.org/abs/2208.06855})

Flajolet, P., and Sedgewick, R. (2009) Analytic combinatorics. Cambridge University press.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
The function calls the \code{\link{cNecklaces} and \link{lSort}} functions in the \code{Necklaces} package.
}


\seealso{
\code{\link{cNecklaces}},
\code{\link{sBruijn}}
}


\examples{
# Generate all the Lyndon words of length 5 over the alphabet 
# {1,2} 
LyndonW(5) 
# or equivalently 
LyndonW(5,2) 

# The previous result in a compact form 
LyndonW(5,2,TRUE)

# Generate all the Lyndon words of length 5 over the alphabet 
# {0,1}
LyndonW(5,2,TRUE,0)

}

% R KEYWORDS
\keyword{Necklaces}
\keyword{Lyndon words}
\keyword{de Bruijn sequences}