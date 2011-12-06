
\name{findMissingTitles}
\alias{findMissingTitles}
\alias{findMissingFigureTitles}
\alias{findMissingTableTitles}
\alias{toc,XMLInternalDocument-method}
\alias{toc,character-method}

\title{Identify empty title elements}

\description{
    These functions identify title elements within a document
     that have no content.
       We are also interested in sections, chapters, examples, figures, tables, etc.    
       that have no title elements
     
  }

\usage{
findMissingTitles(doc)
findMissingFigureTitles(doc, nodes = TRUE)
findMissingTableTitles(doc, nodes = TRUE)
}

\arguments{
    \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
    \item{nodes}{a logical value indicating whether we want the nodes or the text values}
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
