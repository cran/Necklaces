\name{lSort}
\alias{lSort}
\title{Sort a list of vectors
}
\description{The function takes in input a list of vectors and returns the same list ordered in a lexicographical way.
}
\usage{
lSort(pL = list())
}
\arguments{
  \item{pL}{list of vectors to be ordered}
}

\details{The function takes as input a list of vectors and returns the same list ordered in a lexicographical way.
For example if the input list is \code{(2,2,3),(3,2,3),(1,2,3)}, then the output of the function \code{\link{lSort}} produces the following result:
\code{(1,2,3),(2,2,3),(3,2,3).} 

	
}

\value{\item{list}{the input list ordered in lexicographical way}}

\references{

Flajolet, P., and Sedgewick, R. (2009) Analytic combinatorics. Cambridge University press.

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{\link{cNecklaces}}, \code{\link{cBracelets}}, \code{\link{fNecklaces}}, \code{\link{Necklaces}}, \code{\link{Bracelets}},  \code{\link{LyndonW}}, \code{\link{sBruijn}} functions in the \code{Necklaces} package.
}


\seealso{
\code{\link{cNecklaces}},
\code{\link{cBracelets}},
\code{\link{fNecklaces}},
\code{\link{Necklaces}},
\code{\link{Bracelets}},
\code{\link{LyndonW}},
\code{\link{sBruijn}}
}

\examples{
# Sort the following list (2,2,3),(3,2,3),(1,2,3) 
#
lSort(list(c(2,2,3),c(3,2,3),c(1,2,3)))
}

% R KEYWORDS
\keyword{Necklaces}
\keyword{Bracelets}
\keyword{Lyndon words}
\keyword{de Bruijn sequence}

