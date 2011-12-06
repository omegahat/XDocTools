
\name{toc}
\alias{toc}

\title{Extract Table of Contents of a document}

\description{
    This function and its methods allow us to query the document for its table of contents.
These are the titles within chapter and section headings.
     
  }

\usage{
toc(file, ...)
}

\arguments{
    \item{file}{
the document whose table of contents is to be computed. This can be either the name of the document file, or and already parsed XML document (using \code{\link[XML]{xmlParse}}}.
    \item{\dots}{
      additional parameters for methods to define
    }
  }

\value{
    An object of S3 class
     "HierarchicalTableOfContents", "TableOfContents".
    This is a list 
     a factor giving the titles of the chapters, sections, etc.
     the indentation or nesting level of the corresponding elements in titles
     
  }

\references{
 Docbook reference manual
   }

\seealso{\code{\link[XML]{xmlParse}}, \code{\link[XML]{getNodeSet}}
   }

  \author{Duncan Temple Lang \email{duncan@wald.ucdavis.edu}
}
 
\examples{
  
    
        
  
}

\keyword{documentation}

\keyword{}
