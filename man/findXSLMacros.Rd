\name{findXSLMacros}
\alias{findXSLMacros}
\title{Find macro-like XSL templates}
\description{
  This is a heuristic function that attempts to find
  XSL templates that correspond to macro or text substitution
  operations.  The idea is that we use XML elements such
  as \code{<js>} or \code{<R>}  in our XML documents and then have
  these mapped to literals in HTML or LaTeX.
}
\usage{
findXSLMacros(doc)
}
\arguments{
  \item{doc}{this can be either the name of an XML document,
    a directory or a parsed XML document.
    If this is a directory, we process all the files that end in "xsl"
    in that directory.
}
}
\value{
 A list of the templates that appear to correspond to literal text expansion.
}
\references{
  XSL
}
\author{
Duncan Temple Lang
}
\seealso{
}
\examples{
f = system.file("XSL", "OmegahatXSL", "latex", package = "XDynDocs")
if(f != "") {
  o = findXSLMacros(f)
  o
  names(o)
  table(sapply(o, XML::docName))
}
}
\keyword{programming}
\keyword{documentation}
