getFunctionDefs =
function(doc,  nodes = getNodeSet(doc, "//r:function[@r:id]"))
{
   if(is.character(doc))
      doc = xmlParse(doc)
   ids = sapply(nodes, identifyFunction)
   tapply(nodes, ids, function(nodes) xmlValue(nodes[[length(nodes)]]))
}
