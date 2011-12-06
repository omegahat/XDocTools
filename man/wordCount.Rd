
\name{getWords}
\alias{getWords}
\alias{wordCount}

\title{Get or count the words in an XML document}

\description{
    These functions make it easy to get access to all the words
  in the text of an XML document or to merely get the number of words.
 These functions discard all the XML markup  and focus only on the text nodes.
     
  }

\usage{
getWords(doc, split = "[[:space:][:punct:]]+")

wordCount(doc, split = "[[:space:][:punct:]]+")

}

\arguments{
    \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
    \item{split}{
      a string giving a regular expression that is used to split a text string into words.
    }
  }

\value{
    
 
     \code{getWords}
    returns a character vector of all the words in the document.
    
    
    
     \code{wordCount}
    returns an integer giving the total number of words in the document.
     
  }

\seealso{

\code{\link[XML]{getNodeSet}}

   }

   \author{
     
   }
 
\examples{
  
    
     #  This computes the number of words in a mid-size book.
   wordCount("~/Books/XMLTechnologies/book.xml")
	
     #  This reads the contents of the XML and Web technologies book and finds all the included files.
 #  Then it computes the word count for each of those separate files.
 inc = getXIncludeFiles("~/Books/XMLTechnologies/book.xml", recursive = TRUE, full.names = TRUE)
 counts = sapply(names(inc), wordCount)
 barchart(counts)

     #  Here we read the actual words in the book. Then we look at the distribution of these words
 #  and look at the words that are not excessively common, but occur numerous times.
 words = getWords("~/Books/XMLTechnologies/book.xml")
 sort(table(words), decreasing = TRUE)
 x = log(table(words))
 barchart(x)
 names(x) [ x > 2 & x < 4] 

  
}

\keyword{IO}
