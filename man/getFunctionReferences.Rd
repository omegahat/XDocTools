
\name{getFunctionReferences}
\alias{getFunctionReferences}

\title{Get information about references to R functions in text}

\description{
    This function returns information about references
to R functions within the text of the document. These are
r:func elements in the XML. The function returns the name of
the function and its package if that is specified via
the pkg attribute.
     
  }

\usage{
getFunctionReferences(doc, namespace = c(r = "http://www.r-project.org"))

}

\arguments{
    \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
    \item{namespace}{the XML namespace pairing of prefix and URI as a named character vector that is used in the call to <r:func>getNodeSet</r:func>}
  }

\value{
    
     
  }

\references{
   }

\seealso{
   }

   \author{
     
   }
 
\examples{
  
    
    
	
  
}

\keyword{}
