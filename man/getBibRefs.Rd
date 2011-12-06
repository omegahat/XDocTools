
\name{getBibRefs}
\alias{getBibRefs}
\alias{getXRefs}

\title{Get references to bibliography or simple cross references }

\description{
    These functions allow one to query all the reference to other elements
  of a document or the bibliography.  This is different  from \code{\link{getLinks}}
  which identifies the links to other documents.
     
  }

\usage{
getBibRefs(doc, nodes = TRUE)

getXRefs(doc, location = FALSE)

}

\arguments{
    \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
    \item{nodes}{
      a logical value indicating whether we should return the XML include nodes, or the names of the files.
       This is useful if one wants to modify the nodes or get the xpointer attribute.
    }
    \item{location}{a logical value indicating whether we want the file and line number or just the names of the ids and references that mismatch}
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
