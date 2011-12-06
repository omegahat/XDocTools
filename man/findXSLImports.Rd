
\name{findXSLImports}
\alias{findXSLImports}

\title{Find imported XSL documents}

\description{
This function, similar to  \code{\link{getXIncludeFiles}},
  finds the names of the documents that an XSL file imports or includes.
 This allows the caller to find the dependencies between XSL files and
 understand which set of XSL files are used by a top-level XSL document.
     
}

\usage{
findXSLImports(dir, 
                files = list.files(dir, pattern = ".*\\\\.xsl$", recursive = TRUE, full.names = TRUE), 
               filter.out = "docbook-xsl", simplify = TRUE, recursive = FALSE)

}

\arguments{
   \item{dir}{
the directory in which to look for .xsl files.
}
   \item{files}{the names of one or more XSL files to process
and find the XSL files they import.
}
   \item{filter.out}{
a string providing a regular expression which is used to identify
elements within files that should be discarded and 
not searched for their import/include nodes.
This is useful in conjunction with the default value for files
so that one can include all but a few files.
}
   \item{simplify}{a logical value that controls
whether the result is left as a list with an element for each document in
files, or simplified to a single character vector with no duplicates.
}
   \item{recursive}{a logical value controlling
whether each of the imported/included files are also searched
for their import/include files and so on with those files.
}
   }

\value{

If simplify is \code{FALSE}, a list with an element
for each of the values in files. Each element is either a
character vector if recursive is \code{FALSE} or
a list containing lists for each of the import/include'd files.

If simplify is \code{TRUE}, a character vector is returned.

}

\references{
\url{http://www.w3.org/Style/XSL/}
   }

\seealso{
\code{\link{getXIncludeFiles}}

\code{\link[Sxslt]{xsltApplyStyleSheet}}

   }

   \author{
     
   }
 
\examples{
  

 #  File
 #  This searches the XSL file in the XDynDocs package.
 #  Note that we are using a URL and allowing a catalog file
 #  map this to the local file, or read it from the URL.
findXSLImports( , "http://www.omegahat.org/XDynDocs/XSL/html.xsl")
findXSLImports(files = "http://www.omegahat.org/DynDocs.xsl")


 #  Directories
 #  This version reads all the XSL files in a directory and
 #  finds all their imports.
 #  This makes most sense when the .xsl files in the directory are unrelated
 #  and refer to .xsl files in other directories. This is the case, for example,
 #  when we have top-level XSL files for HTML, FO, LaTeX and text in one
 #  directory and each refers to other XSL files in separate directories.
findXSLImports("~/Classes/StatComputing/XDynDocs/inst/XSL", simplify = FALSE)
findXSLImports("~/Classes/StatComputing/XDynDocs/inst/XSL", simplify = FALSE, recursive = TRUE)



}

\keyword{IO}
