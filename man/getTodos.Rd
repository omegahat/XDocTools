
\name{getTodos}
\alias{getTodos}

\title{Access elements in a To-Do file}

\description{
    This is the start of working with To-Do files
from within R. These To-Do files are created as XML
by the author and rendered on-the-fly in a Web browser
using the associated style sheet.
Because  of the reordering (based on status), 
accessing the elements by index/position doesn't make a lot
of sense. This function allows one to fetch all the nodes.
     
  }

\usage{
getTodos(doc, path = "//fixme|//todo|//Note|//question", nodes = TRUE)

}

\arguments{
   \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }  
   \item{path}{
}
    \item{nodes}{
      a logical value indicating whether one wants the nodes to be returned
       or the value of the links or identifiers as strings.
      
    }
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

\keyword{documentation}

\keyword{IO}
