
\name{getDocLocation}
\alias{getDocLocation}

\title{Find the chapter, section, sub-section, etc. of a node}

\description{
    This function allows the caller to find the context of a node within the document. 
  The idea is that she has identified a node having some characteristic of interest, e.g. 
   a link to Web site, a bibliographic reference, a typo.  She wants to be able to locate
  that relative to the document hierarchy, i.e. the chapter, section, subsection and so on.

    This function can return the nodes, the titles or the numbers of the
 chapter and sections to provide different "views" of this information.

    Note that we can use 
\code{\link[XML]{findXInclude}}
 to identify the 
   file containing this actual node. We can also use 
\code{\link[XML]{getLineNumber}}

   to find the exact line in that file.  This function  
     \code{getDocLocation}
   
  is more concerned with understanding where the node is in the overall document, and not within a
  file.
   
  }

\usage{
getDocLocation(node, style = c("nodes", "titles", "position", "line"))

}

\arguments{
   \item{node}{the XMLInternalNode object whose context is to be found.
}
   \item{style}{one of "nodes", "titles" and "position".
  This controls what is returned.
}
   }

\value{
    The value depends on the value of style. If this is 
      "nodes", the result is a list of the matching section and chapter nodes.
     If style is "titles", the result is a character
    vector giving the titles of the subsections, sections and chapters.  
    The character vector has names identifying the XML elements, i.e. section and chapter.
    Finally, for "position", the value is an integer vector giving the
    numbers of the subsection, section, and chapter, again as a named vector.
     
  }

\references{
This uses \url{http://www.w3.org/TR/xpath} and \url{http://www.xmlsoft.org}.
   }

\seealso{
  }

   \author{
     
   }
 
\examples{
  
    
    
	
  
}

\keyword{}
