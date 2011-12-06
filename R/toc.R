if(!exists("toc") || !isGeneric("toc"))
  setGeneric("toc", function(file, ...) standardGeneric("toc"))

setMethod("toc", "character",
function(file, ...)
{
   doc = xmlParse(file, ...)
   toc(doc)
})

# xmlValue.NULL = function (x, ignoreComments = FALSE, recursive = TRUE) { NA}

setMethod("toc", "XMLInternalDocument",
function(file, ...)
{
   nodes = getNodeSet(file, "//chapter|//section")
   levels = sapply(nodes, function(x) length(getNodeSet(x, "./ancestor::chapter|./ancestor::section")))
   titles = sapply(nodes, function(x) { title = x[["title"]]; if(is.null(title)) NA else xmlValue(title)})
   structure(data.frame(titles = titles, levels = levels),
                class = c("HierarchicalTableOfContents", "TableOfContents"))
})

print.HierarchicalTableOfContents =
function(x, indent = "   ", ...)
{
  prefixes = sapply(x$levels, function(i) paste(rep(indent, i), collapse = ""))  
  ans = paste(prefixes, x$titles, sep = "")
  cat(ans, sep = "\n")
}



getEmptySections =
  #
  # This finds the <section> nodes which have no content or just a title.
  #
  #
function(doc)
{
   doc = as(doc, "XMLInternalDocument")

   getNodeSet(doc, "//section[not(title) and normalize-space(string(.)) = '']|//section[./title and normalize-space(string(.)) = normalize-space(string(./title))]")
}

getEmptyParagraphs =
function(doc)
{
   doc = as(doc, "XMLInternalDocument")
   getNodeSet(doc, "//para[normalize-space(string(.)) = '']")
}

