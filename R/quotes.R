IgnoreTextNodeParents =
  c("r:code", "r:output", "r:expr", "r:error", "r:attr",
    "r:function", "r:init", "r:plot",
    "fixme",
    "literal", "programlisting", "proglisting", 
    "ulink", "url", 
    "xml:attr", "xml:expr", "xp:expr", "xml:code",
    "js:code",
    "sh:code", "sh:output", "js:code",
    "xp:code",
    "ignore", "invisible", "comment", "duncan", "deb")

CommonNamespaceDefs =
  c(r = "http://www.r-project.org",
    bioc="http://www.bioconductor.org",
    omg="http://www.omegahat.org",
    xp="http://www.w3.org/TR/xpath",
    xml = "http://www.w3.org/XML/1998/namespace", 
    js="http://www.ecma-international.org/publications/standards/Ecma-262.htm",
    xp="http://www.w3.org/TR/xpath",
    c="http://www.C.org",
    db="http://docbook.org/ns/docbook",
    sh = "http://www.shell.org")

findQuotes =
  #
  # Find text nodes that  have quotes so we can figure out whether to put the sub-text in <quote> or some other elements.
  #
function(doc,
         pattern = '\"', nodes = IgnoreTextNodeParents,
          namespaces = CommonNamespaceDefs)
{
  findPatternInTextNodes(doc, pattern, nodes, namespaces)
}

findPatternInTextNodes =   
function(doc,
         pattern = '\"', nodes = IgnoreTextNodeParents,
          namespaces = CommonNamespaceDefs
          )
{
  if(is.character(doc))
    doc = xmlParse(doc)
  
  txt = paste(sprintf("not(ancestor::%s)", nodes), collapse = " and ")
  query = sprintf("//text()[contains(., \'%s\') and %s]", pattern, txt)

  q = getNodeSet(doc, query, namespaces)
  names(q) = sapply(q, getNodePosition)
  q
#  ff = sapply(q, function(x) getNodeLocation(x)$file[1])
}


quotesToElement =
  #
  #  Process any text nodes in the document that have text within double quotes 
  #  and put each of them in elementName.
  #
function(doc, elementName = "quote", ...,
          quotes = findQuotes(doc, ...))
{
   mapply(replaceQuotesInText, quotes, elementName)
#   doc
}

replaceQuotesInText =
  #
  #  Finds the text in quotes within this text element and then replaces them
  #  with nodes of the form <elementName>content</elementName>
  #
function(node, elementName = "quote")
{
   txt = xmlValue(node)
   ntxt = gsub('("([^"]*)")', sprintf("<%s>\\2</%s>", elementName, elementName), txt)

   doc = xmlParse(sprintf("<tmp>%s</tmp>", ntxt), asText = TRUE)
   i = XML:::indexOfNode(node)
   p = xmlParent(node)
   removeNodes(node)
   kids = xmlChildren(xmlRoot(doc))
   addChildren(p, kids = kids)
   p
}

