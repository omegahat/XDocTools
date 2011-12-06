
\name{getXIncludeFiles}
\alias{getXIncludeFiles}

\title{Find all documents XInclude'd in this one}

\description{
    This function finds all the XInclude   nodes within this document
     and identifies the included documents. It can operate recursively, 
     finding the included documents within each of those and so on.
     This is useful for determining if the files are included more than once,
     if some files in a directory are not included at all, and for obtaining
     a dependency list, e.g. for use with make rules.
  
     This doesn't deal with XPointer qualifications at this point in time. See \url{#rh-Details}.
     
  }

\usage{
getXIncludeFiles(doc, recursive = TRUE, nodes = FALSE, full.names = TRUE, 
                  namespace = c(xi = "http://www.w3.org/2003/XInclude", xo = "http://www.w3.org/2001/XInclude"), 
                 verbose = FALSE, table = FALSE)

}

\arguments{
    \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
    \item{recursive}{
      a logical value indicting whether to find the included documents within the included documents.
    }
    \item{nodes}{
      a logical value indicating whether we should return the XML include nodes, or the names of the files.
       This is useful if one wants to modify the nodes or get the xpointer attribute.
    }
    \item{full.names}{
      a logical value indicating whether we are to give full paths for the files.
     If this is \code{FALSE}, we use the names as they appear in the href attribute within the include nodes within the document.
    }
    \item{namespace}{
      a named character vector giving the namespace definitions for identifying XInclude. There are two in reasonably common use.
    }
    \item{verbose}{
      a logical indicating whether to emit information about which files are being processed as the search proceeds.
    }
    \item{table}{
      a logical value which indicates whether to return a frequency table of the include file names.
     This makes the result more compact if there are many repeated includes.
    }
  }

\value{
    The return value depends on the value of different arguments.
  If recursive is true  and table is \code{TRUE},  a frequency table of the included files is 
  returned. If table is \code{FALSE}  however, then the character vector of all 
  included file names is returned.
  If recursive is \code{FALSE}, then either the include nodes or the 
   character vector of included files is returned. This is controlled by the value
  of nodes.   In all cases, if the names of the files are being returned,
  the names are converted to normalized path names (typically fully-qualified names) if full.names is
  specified.
     
  }

\seealso{

\code{\link[XML]{getNodeSet}}

\url{http://www.w3.org/TR/xinclude/}
   }

   \author{
     
   }
 
\examples{
  
    
  
     #  Finding XIncludes in a book
 #  This identifies the included files in a sample book that uses XInclude to 
 #  bring in the chapters from separate files and a section from separate file.
 #  
 #  It converts the file names to fully-qualified paths and operates recursively.
 #  This gives us the dependency list for the entire book (at the present instance).
f = system.file("sampleDocs", "book.xml", package = "XDocTools")
getXIncludeFiles(f, recursive = TRUE, full.names = TRUE)

  
}

\keyword{IO}
