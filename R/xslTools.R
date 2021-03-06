findXSLImports =
 # dir = catalogResolve("http://www.omegahat.org/XSL/html")
 # o = findXSLImports(dir, simplify = FALSE)
 # o = findXSLImports(dir, simplify = FALSE, recursive = TRUE)
function(dir, files =  list.files(dir, pattern = ".*\\.xsl$", recursive = TRUE, full.names = TRUE),
          filter.out = "docbook-xsl",
          simplify = TRUE,
          recursive = FALSE, full.names = FALSE)
{
  dir = catalogResolve(dir, asIs = TRUE)
  
  
  if(length(filter.out)) {
    i = grep(filter.out, files)
    if(length(i))
      files = files[ - i]
  }

  refs = lapply(files, getXSLImports, recursive = recursive)
  names(refs) = files
  
  if(simplify)
    unique(unlist(refs))
  else if(full.names)
     refs
  else
     structure(refs, names = basename(files))
}

XSLNamespace = c(xsl = "http://www.w3.org/1999/XSL/Transform")

getXSLImports =
  #
  # v = getXSLImports("../inst/XSL/html.xsl", TRUE)
  #
function(doc, recursive = FALSE)
{
  xmldoc = if(is.character(doc)) xmlParse(doc) else doc
  els = xpathSApply(xmldoc, "(//xsl:import|//xsl:include)", xmlGetAttr, "href", namespaces = XSLNamespace, noMatchOkay = TRUE)
  if(!recursive)
    return(els)

  ans = structure(list(els), names = doc)
  if(length(els) == 0)
    return(ans)

     # do this iteratively
  
  remaining = unique(fixBase(els, doc))
   
  while(length(remaining)) {
    d = remaining[1]
    remaining = remaining[-1]
    tmp = getXSLImports(d, FALSE)
    if(length(tmp) == 0)
      tmp = character()
    
    ans[[d]] = tmp
    if(length(tmp))
      remaining = unique(c(remaining, fixBase(tmp, d)))
  }

  ans
}

fixBase =
  function(els, doc) {
                                        # fix up the relative links
      i = grep("^(http|ftp|/)", els)
      tmp = els # debugging
      if(length(i) < length(els)) {
        base = dirname(doc)
        w = !(1:length(els) %in% i)
        els[ w ] = paste(base, els[w], sep = .Platform$file.sep)
      }
         # Resolve via catalog to get canonical name.
      catalogResolve(els, asIs = TRUE)
    }



XSL.ns = c(xsl="http://www.w3.org/1999/XSL/Transform")

getXSLTemplates =
# show what the templates in an XSL file
# match, i.e. get their match attribute.
# Also get named templates

function(doc, split = FALSE, ...)
{
  if(is.character(doc) && file.info(doc)[1, "isdir"])
    return(getXSLTemplates(list.files(doc, full.names = TRUE, pattern = "xsl$"), split = split, ...))

  if(is.character(doc) && length(doc) > 1)
     return(structure(lapply(doc, getXSLTemplates, split = split, ...), names = doc))

  
  if(is.character(doc))
    doc = xmlInternalTreeParse(doc)
  else
    doc = as(doc, "XMLInternalDocument")

  tm = xpathSApply(doc, "//xsl:template[@match]", xmlGetAttr, "match", namespaces = XSL.ns)
  if(split)
     tm = lapply(tm, strsplit, "[[:space:]]*\\|[[:space:]]*")

  list(match = tm,
       name = xpathSApply(doc, "//xsl:template[@name]", xmlGetAttr, "name", namespaces = XSL.ns)  )
}  


getXSLParameters =
function(doc)
{
  if(is.character(doc))
    doc = xmlInternalTreeParse(doc)
  else
    doc = as(doc, "XMLInternalDocument")

  els = getNodeSet(doc, "//xsl:variable | //xsl:param", XSL.ns)
  structure(sapply(els, xmlGetAttr, "name"), names = sapply(els, xmlName))
}


getNSDefs =
  #
  #  hxsl = list.files("~/Classes/StatComputing/XDynDocs/inst/XSL/OmegahatXSL/html", pattern = "xsl$", full.names = TRUE)
  #  tt = getNSDefs(hxsl, simplify = TRUE)
  #  tt = getNSDefs("~/Classes/StatComputing/XDynDocs/inst/XSL/OmegahatXSL/html", simplify = TRUE)  
  #
function(doc, ...)
{
  if(is.character(doc) && file.info(doc)[1, "isdir"])
    return(getNSDefs(list.files(doc, full.names = TRUE, pattern = "xsl$"), ...))

  if(is.character(doc) && length(doc) > 1)
     return(structure(lapply(doc, getNSDefs, ...), names = doc))
  
  if(is.character(doc))
    doc = xmlInternalTreeParse(doc)
  else
    doc = as(doc, "XMLInternalDocument")

  xmlNamespaceDefinitions(xmlRoot(doc), ...)
}

checkUniqueNSDefs =
  # Also want to compare across fo and 
function(..., .docs = list(...))
{
  ns = unlist(lapply(.docs, getNSDefs, simplify = TRUE))
  by(ns, names(ns), function(x) unique(as.character(x)))
}
  



#  find macro-like

findXSLMacros =
function(doc)
{
  if(is.character(doc)) {
    if(file.info(doc)[1, "isdir"]) {
      return(unlist(lapply(list.files(doc, pattern = "\\.xsl$", full.names = TRUE), findXSLMacros)))
    }

    doc = xmlParse(doc)
  }

  v = getNodeSet(doc, "//xsl:template[not(*) and text() and @match]", "xsl")
  if(length(v))
    names(v) = sapply(v, xmlGetAttr, "match", "")

  v
}
