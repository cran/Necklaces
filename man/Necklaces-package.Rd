\name{Necklaces-package}
\alias{Necklaces-package}
\alias{Necklaces-package}
\docType{package}
\title{
\packageTitle{Necklaces}
}
\description{
\packageDescription{Necklaces}
}
\details{
Using multi-index compositions, necklaces and bracelets are generated 
as well as Lyndon words and de Bruijn sequences. For multi-index compositions, this package refers to the \code{kStatistics} package. 

}
\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}


\packageAuthor{Necklaces}

Maintainer: \packageMaintainer{Necklaces}
}

\references{

Di Nardo, E. (2014) On a symbolic representation of non-central Wishart random matrices with applications. Jour. Mult. Anal. Vol.125, 121--135. (\url{https://arxiv.org/abs/1312.4395})

Di Nardo, E., and Guarino., G.  (2022) kStatistics: Unbiased Estimates of Joint Cumulant Products from the Multivariate Faa Di Bruno's Formula. The R journal - In press. (\url{https://arxiv.org/abs/2206.15348})  

Di Nardo, E., and Guarino., G.  (2022)  Necklaces and bracelets in R - (\url{https://arxiv.org/abs/2208.06855})

Flajolet, P., and Sedgewick, R. (2009) Analytic combinatorics. Cambridge University press.

}
\examples{

# Sort the following list [2,2,3],[3,2,3],[1,2,3] 
#
lSort(list(c(2,2,3),c(3,2,3),c(1,2,3)))

# Generate the elements of the necklace in equivalence relation with 
# the input vector c(1,0,2,1) 
cNecklaces(c(1,0,2,1)) 

# The previous result in a compact form
cNecklaces(c(1,0,2,1),TRUE)

# Generate the elements of the bracelet in equivalence relation with 
# the input vector (1,0,2,1) 
cBracelets(c(1,0,2,1)) 

# The previous result in a compact form
cBracelets(c(1,0,2,1),TRUE)  

# Generate all the necklaces of the configuration (2,1,1) 
# corresponding to the vector (1,1,2,3)
fNecklaces(c(2,1,1))  

# The previous result in a compact form
fNecklaces(c(2,1,1),TRUE)

# The first value of the alphabet is set equal to zero 
fNecklaces(c(2,1,1),TRUE,0)

# Generate all the bracelets of the configuration (2,1,1) 
# corresponding to the vector (1,1,2,3)
fBracelets(c(2,1,1))  

# The previous result in a compact form
fBracelets(c(2,1,1),TRUE)

# The first value of the alphabet is set equal to zero
fBracelets(c(2,1,1),TRUE,0)

# Generate the list of all the representatives of all the necklaces 
# of length 4 over the alphabet {1,2}. 
Necklaces(4,2)  

# Generate the list of  all the representatives of all the necklaces 
# of length 5 over the alphabet {1,2,3}. 
Necklaces(5,3)  

# Generate the list of  all the representatives of all the necklaces 
# of length 5 over the alphabet {0,1,2}. 
Necklaces(5,3,0) 

# Generate the list of all the representatives of all the bracelets 
# of length 4 over the alphabet {1,2}. 
Bracelets(4,2)  

# Generate the list of  all the representatives of all the bracelets 
# of length 5 over the alphabet {1,2,3}. 
Bracelets(5,3)  

# Generate the list of  all the representatives of all the bracelets 
# of length 5 over the alphabet {0,1,2}. 
Bracelets(5,3,0)  

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
