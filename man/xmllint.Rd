
\name{xmllint}
\alias{xmllint}

\title{Check an XML document is parseable}

\description{
    This function can be used to determine the errors in an XML document
 which make it not parseable. This is helpful when one has a document that
cannot be read and one wants to find where the errors are.
     
  }

\usage{
xmllint(doc, system = TRUE)

}

\arguments{
   \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }  
   \item{system}{a logical value which indicates whether to use the
xmllint program system via a system call or whether to use our own internal parser
and cumulate the errors. These are very similar, relying on the same basic parser.}
   }

\value{
    
     
  }

\references{
   }

\seealso{
  
\code{\link[XML]{xmlParse}}

   }

   \author{
     
   }
 
\examples{
  
    
    
	
  
}

\keyword{}
