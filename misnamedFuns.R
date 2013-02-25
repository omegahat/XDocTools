library(XML)
fun = function(filename, unique = TRUE)
{  
  doc = xmlParse(filename)
  nodes = getNodeSet(doc, "//r:func", "r")
  ids = sapply(nodes, xmlValue)

  library(RCurl); library(XMLRPC); library(SSOAP); library(Rcompression)
  i = sapply(ids, exists)
  if(unique)
     unique(ids[!i])
  else {
#browser()    
    tapply(nodes[!i], ids[!i], lapply, getNodeLocation)
#     ids[!i]
  }
}



