# 
#

setAs("character", "XMLInternalDocument",
        function(from) {
          xmlParse(from)
        })

getIds =
function(doc, nodes = FALSE)
{
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = !nodes, xinclude = FALSE)

   if(nodes)
      getNodeSet(doc, "//*[@id]")
   else
      as.character(unlist(getNodeSet(doc, "//@id")))
}

getSections =
function(doc)
{
    doc = as(doc, "XMLInternalDocument")
    sectionNames = c("section", paste("sect", 1:5, sep = ""))
    getNodeSet(doc, paste("//", sectionNames, collapse = "|", sep = "")) 
}


getIdUsage =
function(doc, nodes = FALSE)
{
  getIds()
}


getXRefCounts =
  # For each defined id, find how many are linking to it.
function(doc, nodes = FALSE)  
{
  book = as(doc, "XMLInternalDocument")
  ids = getIds(book)
  n = lapply(ids, function(id) getNodeSet(book, sprintf("//xref[@linkend='%s']", id)))
  if(nodes)
    structure(n, names = ids)
  else
    structure(sapply(n, length), names = ids)
}


getXRefs =
  #
  # location = TRUE returns each element of the list as a data frame
  # giving the linkend or id and the file in which it occurred and
  # the line number
  #
function(doc, location = FALSE)
{
  book = as(doc, "XMLInternalDocument")  
  o = getNodeSet(book, "//xref/@linkend")
  ends = as.character(unlist(o))
  ids = as.character(unlist(getIds(book)))

  if(location) {
    nodes = getNodeSet(book, "//xref[@linkend]")
    w = ! ( ends %in% ids )
    xinc = sapply(nodes[w], findXInclude)
    undef = data.frame(id = ends[w],
                       include = sapply(xinc, `[`, 1),
                       line = sapply(nodes[w], getLineNumber),
                        stringsAsFactors = FALSE)

    nodes = getNodeSet(book, "//*[@id]")
    w = ! ( ids %in% ends)
    unused = data.frame(id = ids[w],
                       include = sapply(nodes[w], function(x) findXInclude(x)[1]),
                       line = sapply(nodes[w], getLineNumber),
                       stringsAsFactors = FALSE)      
    list(undef = undef, unused = unused)
  } else
            # refs that don't exist.
    list(undefined = setdiff(ends, ids),
         unused = setdiff(ids, ends))
}



getXIncludeFiles =
  #
  # recursive means to get the XIncludes within the included files and so on
  #
  # normalizeNames
  #
  #
function(doc, recursive = TRUE, nodes = FALSE, full.names = TRUE,
         namespace = c(xi = "http://www.w3.org/2003/XInclude",
                       xo = "http://www.w3.org/2001/XInclude"),  # both versions for completeness
          verbose = FALSE, table = FALSE, hierarchical = FALSE,
          query = "include[not(ancestor::ignore)]")
{
   normalizeNames = full.names
   
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = !nodes, xinclude = FALSE)

   res = structure(list(name = docName(doc), children = list()),
                    class = c("XIncludeInfo", "Hierarchy"))

   if(!inherits(query, "AsIs"))
      query = paste(sprintf("//%s:%s", names(namespace), query), collapse = " | ")

   els = getNodeSet(doc, query, namespace)

   if(length(els) == 0) {
      if(hierarchical) {
         res[[docName(doc)]] = if(nodes) list() else character()
         return(res)
      } else
        return(if(nodes) list() else character())
    }

   ans = sapply(els, xmlGetAttr, "href") 
   isText = sapply(els, function(x) xmlGetAttr(x, "parse", "") == "text")

   xp = sapply(els, xmlGetAttr, "xpointer", NA) 

   if(recursive && !all(isText)) {

      dir = dirname(docName(doc))
      sub = ans[!isText]
       ## Keep a list of ones we have seen already to avoid cycles.
       # Some cycles may be legit, if there is an xpointer to get a subset.
      if(verbose)
         cat(docName(doc), "->", paste(sub, collapse = ", "), "\n")
      
      tmp = lapply(getRelativeURL(sub, dir),
                   function(f) {
                           if(verbose) cat(f, "\n")
                           getXIncludeFiles(f, TRUE, nodes, table = FALSE, hierarchical = hierarchical, query = I(query))
                         })
      if(hierarchical)
         res[["children"]] = structure(tmp, names = as.character(getRelativeURL(sub, dir)),
                                         xpointer = xp)
      else {
         ans = structure(c(ans, unlist(tmp)),
                          xpointer = c(xp, unlist(lapply(tmp, attr, "xpointer"))))
      }

      if(normalizeNames)  {
         tmp = getRelativeURL(ans, dir)
         isURL = grepl("^http:", tmp)
         
         if(!all(isURL))
           tmp[!isURL] = normalizePath(path.expand(tmp[!isURL]))
         ans = structure(tmp, xpointer = attr(ans, "xpointer"))
       }

       if(hierarchical)
           res
       else { if(table)
                table(unlist(ans))
              else
                ans
            }
          
   } else {  # not recursive or all text includes
      if(nodes)
        els
      else {
        if(normalizeNames)
           ans = getRelativeURL(ans, dirname(docName(doc)))
        structure(ans, xpointer = xp)
      }
   }
}

findMissingTitles =
function(doc)
{
  paths = c("//section[not(./title)]",
            "//chapter[not(./title)]",
            "//section[normalize-space(./title/text()) = '']")
                  # Last one misses <section><title><markup>text text text</markup></title></section>
  unlist(lapply(paths, function(path) getNodeSet(doc, path)), recursive = FALSE)
}


findMissingTableTitles =
  #
  # Find table elements with no or an empty caption
  #
function(doc, nodes = TRUE)
{
    findMissingElementTitles(doc, "table", nodes)
}


findMissingFigureTitles =
  #
  # Find table elements with no or an empty caption
  #
function(doc, nodes = TRUE)
{
  findMissingElementTitles(doc, "figure", nodes)  
}

findMissingElementTitles =
function(doc, elType, nodes = TRUE)
{
  if(is.character(doc))
    doc = xmlParse(doc, addFinalizer = !nodes)
  
  path = sprintf("//%s[not(./title)]|//%s[normalize-space(string(./title)) = '']", elType, elType)
  ans = getNodeSet(doc, path)
  if(nodes)
    ans
  else
    structure(sapply(ans, getLineNumber), names = sapply(ans, findXInclude))  
}



getAllNodeNames =
  #
  #
  #
function(doc, full = TRUE)
{
  book = as(doc, "XMLInternalDocument")
  elNames = xpathSApply(book, "//*", xmlName, full)
  sort(table(elNames), decreasing = TRUE)
}
  
getTodos =
function(doc, path = "//fixme|//todo|//Note|//question", nodes = TRUE)
{
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = !nodes)

   els = getNodeSet(doc, path)   
   if(nodes)
      structure(els, names = sapply(els, xmlName))
   else
      structure(xpathSApply(doc, path, xmlValue), names = sapply(els, xmlName))     
      
 }

getBibRefs =
function(doc, nodes = TRUE)
{
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = !nodes)  

    if(nodes)
      getNodeSet(doc, "//biblioref")
    else
      unlist(getNodeSet(doc, "//biblioref/@linkend") )
}

getMissingLinks =
function(doc, nodes = TRUE)
{
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = FALSE)
   
   ans = getNodeSet(doc, "//xref[not(@linkend)]")
   if(nodes)
     ans
   else
     unlist(ans)   
}

getLinks =
function(doc, nodes = FALSE)
{
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = FALSE)

   ans = getNodeSet(doc, if(nodes) "//ulink" else "//ulink/@url")

   if(nodes)
     ans
   else
     unlist(ans)
}

getEmptyLinks =
function(doc)
{
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = FALSE)

   getNodeSet(doc, "//ulink[not(@url) or @url='']")
}

validateLink =
  #
  # Download the link to see if it is valid.
  # We don't need the body, just the header.
  #  If it is 200, okay and we return TRUE.
  #  If it is 300, return the redirect.
  # If it is 400, we return FALSE.
  #
  # An example of redirection is
  #   http://eeyore.ucdavis.edu/Redirect/bob.html
  #
function(url)
{
  h = getURL(url, nobody = TRUE, header = TRUE)
  header = parseHTTPHeader(h)
  st = as.integer(header["status"])
    # 301 means permanent. 302 means temporary so still go to the original one.
  if(st == 301)
    return(header["Location"]) 
  level = as.integer(st / 100)
  switch(level, "2" = TRUE, "3" = TRUE, "4" = FALSE)
}

validateLinks =
function(doc, nodes = TRUE)
{
  doc = as(doc, "XMLInternalDocument")
  links = getNodeSet(doc, "//ulink[@url]")
  lapply(links, function(x) validateLink(xmlGetAttr(x, "url")))
}

getMalformedLinks =
  # Find docbook link elements that have the wrong attribute
  # or no url attribute.
function(doc)
{  
   if(is.character(doc))
      doc = xmlParse(doc, addFinalizer = FALSE)
   getNodeSet(doc, "//ulink[@href or not(@url)]")
}


getDocLocation =
  #
  # Given 
  #
function(node, style = c("nodes", "titles", "position", "line"))
{
   exprs = list(nodes = "./ancestor::chapter|./ancestor::section",
                titles = "./ancestor::section/title/text()|./ancestor::chapter/title/text()",
                position = c("count(./ancestor::section/preceding-sibling::*)+1", "count(./ancestor::chapter/preceding-sibling::*)+1"),
                lines = "")

   i = match.arg(style, names(exprs))
   if(i == "line")  {
       structure(getLineNumber(node), names = findXInclude(node))
   }
   if(i == "position") {
     structure(sapply(exprs[[i]], function(xp) getNodeSet(node, xp)),
                 names = xpathSApply(node, exprs[["nodes"]], xmlName))
   } else
     unlist(getNodeSet(node, exprs[[i]]))
}


xmllint =
  #
  # Determine the errors in an XML document.
  # 
function(doc, system = TRUE)
{  
  if(system)
    system( paste( "xmllint", doc))
  else {
     errors = list()
     err = function(...)  {
       errors[[length(errors) + 1]] <<- structure(list(...),
                                                  names = c("message", "code", "domain", "line", "int2", "level", "file"),
                                                  class = "XMLParseError")
     }
     try(xmlParse(doc, error = err))
     errors
   }
}

wordCount =
  #
  # Get the number of words in the document
  #
function(doc, split = "[[:space:][:punct:]]+")
{
   length(getWords(doc, split))
}

getWords =
  #
  # Get the words in a document
  #
function(doc, split = "[[:space:][:punct:]]+")
{
   doc = as(doc, "XMLInternalDocument")

   els = getNodeSet(doc, "//text()[not(ancestor::ignore) or not(ancestor::invisible)]")
   words = unlist(lapply(els, function(x) strsplit(xmlValue(x), split)))
   words[words != ""]
}
