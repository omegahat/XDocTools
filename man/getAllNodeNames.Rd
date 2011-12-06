
\name{getAllNodeNames}
\alias{getAllNodeNames}

\title{Compute the names of all XML nodes in a document}

\description{
    This function is useful for finding the names of all the
XML nodes used in the specified XML document.
This allows us to verify that there are XSL templates for all of
them, find which are most widely used, etc.
     
  }

\usage{
getAllNodeNames(doc, full = TRUE)

}

\arguments{
   \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }  
   \item{full}{a logical value that indicates whether
to use the full name with the XML namespace prefix,
or the simple name. This is passed to 
\code{\link[XML]{xmlName}}
.
}
   }

\value{
    a frequency table giving the number  of times
each node name appears in the document.
     
  }

\seealso{
 \code{\link{wordCount}} and \code{\link{getWords}}
  }

   \author{
     
   }
 
\examples{
  
    
     #  This examines this very document to find all the nodes it uses.
  f = system.file("XMLHelp", "getAllNodeNames.xml", package = "XDocTools")
  getAllNodeNames(f)
	
  
}

\keyword{IO}
