
\name{getLinks}
\alias{getLinks}
\alias{getIds}

\title{Get external links and internal identifiers}

\description{
    These functions get all the hyper-link nodes or values
      or the identities of any node that has an id attribute.
      These can be used to verify the hyper-links and to 
      ensure that links to internal nodes correspond to actual identifiers.
     
  }

\usage{
getLinks(doc, nodes = FALSE)

getIds(doc, nodes = FALSE)

}

\arguments{
    \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
    \item{nodes}{
      a logical value indicating whether one wants the nodes to be returned
       or the value of the links or identifiers as strings.
      
    }
  }

\value{
    if nodes is \code{TRUE}, a NodeSet
    object which is a list of the matching nodes.
    If nodes is \code{FALSE}, the actual values 
    for the links, i.e. the url attribute, or the identifier, i.e. the id attribute.
     
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
