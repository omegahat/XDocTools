setGeneric("renameXML",
            function(from, to = names(from), doc, type, ...) {
                 standardGeneric("renameXML")
            })

setMethod("renameXML", c(doc = "character"),
            function(from,  to = names(from), doc, type, ...) {
                 # if a directory, process all the .xml files.
                if(length(doc) == 1 && file.exists(doc[1]) && file.info(doc)[1, "isdir"]) 
                   doc = list.files(doc, pattern = "\\.xml$", full.names = TRUE )
                
                w = sapply(doc, function(i) {
                                    pdoc = xmlParse(i)
                                    n = renameXML(from, to, pdoc, type, ...)
                                    if(length(n)) {
                                       saveXML(pdoc, i)
                                       TRUE
                                    } else
                                       FALSE
                            })
                doc[w]
            })

setMethod("renameXML", c(doc = "XMLInternalDocument"),
            function(from, to = names(from), doc, type, ...) {
               if(length(from) > 1) {
                  mapply(renameXML, from, to, MoreArgs = list(type = type, doc = doc))
               } else {
                  renameContent(from, to, doc, type)
               }
            })


renameFunction =
  #
  #  rename a function by changing all the <r:func>from</r:func> to <r:func>to</r:func>
  #
function(from, to, doc, ...)
{
   renameXML(from, to, doc, "r:func", ...)
}

renameClass =
  #
  #  rename a function by changing all the <r:class>from</r:class> to <r:func>to</r:func>
  #
function(from, to, doc, ...)
{
   renameXML(from, to, doc, "r:class", ...)
}

renameContent =
  #
  # general function that does the transformation via XPath.
  #
function(from, to, doc, xpath, namespaces = "r")
{
  xp = paste("//", xpath, "[string(.) = ", sQuote(from), "]", sep = "") 
  nodes = getNodeSet(doc, xp, namespaces)
  if(length(nodes)) 
     sapply(nodes, function(node) xmlValue(node) = to)
  nodes
}
