tangle = 
function(filename, ...)
{
  if(is.character(filename))
     doc = xmlParse(filename)
  else
     doc = as(filename, "XMLInternalDocument")
  
  nodes = getNodeSet(doc, "//r:code[@file]", c("r" = "http://www.r-project.org"))
  tapply(nodes, sapply(toFile, xmlGetAttr, "file"), writeNodesToFile, ...)
}

writeNodesToFile =
function(nodes, sep = "\n", ...)
{
  out = xmlGetAttr(nodes[[1]], "file")
  cat(unlist(lapply(nodes, getCode)), file = out, sep = sep, ...)
  out
}

getCode =
function(node)
{
  if(all(i <- names(node) %in% c("text", "cdata")))
     xmlValue(node)
  else 
    sapply(xmlChildren(node)[i], xmlValue)
}
