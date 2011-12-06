
\name{addFunctionID}
\alias{addFunctionID}
\alias{addFunctionPackage}


\title{Annotate nodes in an XML document defining or referring to a function}

\description{
    These functions update elements within a document 
  that refer to R variables or R function definitions.
  
     \code{addFunctionID}
    adds an id attribute to each 
  r:function node that does not have one and calculates this
  by parsing the code.

    
   
     \code{addFunctionPackage}
    finds the r:func nodes
  that have no pkg attribute and adds the package corresponding to the
  function being referenced.  This function can also update nodes with an existing
  pkg attribute.  This is controlled by the all parameter.
     
  }

\usage{
addFunctionID(doc, nodes = "//r:function[not(@id)]", save = TRUE)

addFunctionPackage(doc, nodes = if (all)
                                   "//r:func"
                                else
                                   "//r:func[not(@pkg)]",
                    save = TRUE, all = FALSE,
                     skip = character()))

}

\arguments{
    \item{doc}{
      the XML document containing the documentation. This can be either the name of the
  file (or even URL), or an already parsed XML document, i.e. an XMLInternalDocument
  object.

    }
    \item{nodes}{
      the XPath expression that will be used to retrieve the nodes.
 This is a character string. See 
\code{\link[XML]{getNodeSet}}
.

    }
    \item{save}{
      a logical value which indicates whether the modified document should be written back to the file
  from which it was originally read. If one is making several changes in succession, it is slightly
  more efficient to save the document at the end of all of these changes.

    }
    \item{all}{
      a logical which if \code{TRUE} indicates that all r:func nodes should be processed even if they have an existing pkg attribute.

    }
    \item{skip}{}
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
  
    
    
addFunctionID(system.file("sampleDocs", "functionRefs.xml", package = "XDocTools"), save = FALSE)

     #  This adds the package to the document.
addFunctionPackage(system.file("sampleDocs", "functionRefs.xml", package = "XDocTools"), save = FALSE)

  
}

\keyword{IO}

\keyword{documentation}
