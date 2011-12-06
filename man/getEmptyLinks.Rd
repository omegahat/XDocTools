
\name{getEmptyLinks}
\alias{getEmptyLinks}
\alias{getMalformedLinks}

\title{Find Docbook links that are not valid}

\description{
    These functions find hyper-links within the Docbook document
  that are either empty in that they have no or an empty url attribute,
  or that have the wrong attribute, e.g. href.
     
  }

\usage{
getEmptyLinks(doc)

getMalformedLinks(doc)

}

\arguments{
   \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
  }

\value{
   a list of the ulink nodes that are considered incomplete or malformed.
     This is an object of class NodeSet.
     
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
