
\name{getXSLTemplates}
\alias{getXSLTemplates}
\alias{getXSLParameters}

\title{Get information about an XSL document}

\description{
These functions allow the caller to discover what templates, parameters and
 variables are defined within a given XSL file. They act as a table of contents
 for the file.


 Note that these are quite different from related functions available via the Sxslt
 package.  The functions here work on the XML documents defining the XSL stylesheet.
 The function 
\code{\link[Sxslt]{getTemplate}}
  works at run-time when an XSL stylesheet is being used to 
 transform an XML document.  There we can see all the templates (after include's and import's) and ask the 
 XSL engine to match a node as it would during the processing.

}

\usage{
getXSLTemplates(doc)

getXSLParameters(doc)

}

\arguments{
   \item{doc}{
      the XML document to be processed. This can be either the file name (or URL) or the already parsed XML document
      returned from 
\code{\link[XML]{xmlParse}}
.
    }
}

\value{


     \code{getXSLTemplates}
    returns list with two elements

 matcha character vector giving the match attribute of the xsl:template nodes
 namea character vector giving the 




     \code{getXSLParameters}
    returns a named character vector
giving the names of the XSL parameters and variables. The names on the vector
are either param or variable indicating the type of XSL element.

}

\references{
   }

\seealso{

\code{\link[XML]{getNodeSet}}
 and 
\code{\link[XML]{xpathApply}}

}

   \author{
     
   }
 
\examples{
  

 #  This reads one of the XDynDocs XSL files either locally
 #  or remotely.
getXSLTemplates("http://www.omegahat.org/XDynDocs/XSL/html.xsl")

$match
 [1] "r:code|r:function|r:test|r:init"        "r:expr"                                 "r:data|r:value|r:object"               
 [4] "programlisting[@lang='r' or @lang='R']" "r:commands"                             "r:plot"                                
 [7] "s:var|r:var"                            "r:func|s:func"                          "r:package"                             
[10] "dots"                                   "sh:code"                                "make:code"                             
[13] "latex"                                  "ignore"                                 "invisible"                             
[16] "omg:func[@pkg]"                         "omg:func[@url]"                         "omg:package"                           
[19] "omg:package[@url]"                      "rwx:func"                               "r:class"                               
[22] "interactive"                            "r:run"                                 

$name
[1] "user.footer.content" "makeVerbatimCode"    "getSVGDim"           "omg:func"           




 #  Parameters
getXSLParameters("http://www.omegahat.org/XDynDocs/XSL/html.xsl")





}

\keyword{}
