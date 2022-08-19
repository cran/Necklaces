\name{sBruijn}
\alias{sBruijn}
\title{The de Bruijn sequence}

\description{
The function generates the (minimum) de Bruijn sequence of length \code{n} over an alphabet of \code{m} consecutive non-negative integers.
}
\usage{
sBruijn(n=1,m=2, fn=0, bSep=FALSE)
}

\arguments{
  \item{n}{positive integer: the length of the representatives}
  \item{m}{positive integer: the number of consecutive non-negative integers in the alphabet}
  \item{fn}{integer: the first value of the alphabet, the default is 0}
  \item{bSep}{boolean: if \code{TRUE}, a separator is inserted between the output blocks}
}

\details{
The function generates the (minimum) de Bruijn sequence of order \code{n} over an alphabet of \code{m} consecutive non-negative integers. The parameter 
(\code{fn}) assigns the first value of the alphabet, which by default is equal to 0. If (\code{bSep}) is set equal to \code{TRUE}, a 
separator is inserted between the output blocks.  

}

\value{\item{string}{the de Bruijn sequence}}

\references{

Di Nardo, E., and Guarino., G.  (2022)  Necklaces and bracelets in R - (\url{https://arxiv.org/abs/2208.06855})

Flajolet, P., and Sedgewick, R. (2009) Analytic combinatorics. Cambridge University press.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
The function calls the \code{\link{Necklaces} function and the \link{cNecklaces}} function in the \code{Necklaces} package.
}

\seealso{
\code{\link{Necklaces}},
\code{\link{cNecklaces}},
\code{\link{LyndonW}},
\code{\link{sBruijn}}
}

\examples{
# Generate the de Bruijn sequence of length 4 on the binary alphabet
# {0,1}
sBruijn(4) 
# or equivalently 
sBruijn(4,2) 

# Generate the de Bruijn sequence of length 2 over the alphabet {0,1,2} 
sBruijn(2,3)

# Generate the de Bruijn sequence of length 2 over the alphabet {1,2,3}
sBruijn(2,3,1)

# Generate the de Bruijn sequence of length 2 over the alphabet {1,2,3} 
# with a block separator. 
sBruijn(2,3,1,TRUE)

}

% R KEYWORDS
\keyword{Necklaces}
\keyword{Bracelets}
\keyword{Lyndon words}
\keyword{de Bruijn sequences}