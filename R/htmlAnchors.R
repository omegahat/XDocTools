# This crashes but maybe after we have done lots of things.
# doc = htmlParse("../Web/examples.html", error = function(...){}); addTOC(doc); saveXML(doc, "../Web/examples1.html")

addAnchorToDL =
  # Have to be very relaxed about the xpath as if we don't close the
  # dd and dt tags, they become nested because of the parser.
function(doc, xpath = "//dt/li", ids = seq(along = nodes),
          nodes = getNodeSet(doc, xpath))
{
  if(is.function(ids))
    ids = sapply(nodes, ids)
  
  mapply(function(node, id)
              newXMLNode("a", attrs = c(name = id),
                          parent = node,
                          at = 1),
         nodes, ids)

  invisible(doc)
}

makeTOC =
function(doc, xpath = "//dt/li[./a[@name]]", 
          nodes = getNodeSet(doc, xpath))
{
  n = newXMLNode("ol")
  sapply(nodes, makeTOCItem, parent = n)

  n
}

makeTOCItem =
function(node, parent = NULL)
{
  newXMLNode("li",
             newXMLNode("a", xmlValue(node[[1]]),
              attrs = c(href = paste("#", xmlGetAttr(node[["a"]], "name"), sep = ""))),
            parent = parent)
}

addTOC =
  # doc = htmlParse("../Web/examples.html", error = function(...){})
  # addTOC(doc)
  # saveXML(doc, "/tmp/foo.html")
function(doc, addAnchors = length(getNodeSet(doc, "//dt/li/a[@name]")) == 0,
          toc = makeTOC(doc), at = getNodeSet(doc, "//body")[[1]])
{
  if(addAnchors)
    addAnchorToDL(doc)

  addChildren(at, toc, at = 0)
  invisible(doc)
}
