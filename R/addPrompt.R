#
# Idea is to add R> prompt and + to lines of code in an XML document.
#
#

setGeneric("addCodePrompt",
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), ...)
               standardGeneric("addCodePrompt"))

setMethod("addCodePrompt", "character", 
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), ...) {
              addCodePrompt( xmlParse(doc), prompt, ...)
            })

setOldClass(c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode"))

setOldClass("AsIs")
setOldClass("XMLNodeSet")


setMethod("addCodePrompt", "AsIs", 
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), ...) {
              addCodePrompt( xmlParse(doc), prompt, ...)
            })

setMethod("addCodePrompt", "XMLInternalDocument", 
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), replace = TRUE, ...) {
              code = getNodeSet(doc, "//r:code[not(ancestor::ignore) and not(ancestor::invisible)] | //r:plot[not(ancestor::ignore) and not(ancestor::invisible)]", c("r" = "http://www.r-project.org"))
              addCodePrompt(code, prompt, replace = replace, ...)
            })


setMethod("addCodePrompt", "XMLNodeSet", 
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), replace = TRUE, ...) {
              lapply(doc, addCodePrompt, prompt, replace = replace, ...)
            })


addPrompt.language =
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), ...) {
              els = deparse(doc, ...)
              paste(sprintf("%s %s", c(prompt[1], rep(prompt[2], length(els)-1)), els), collapse = "\n")
            }

getRCode =
function(node)
{
  w <- xmlSApply(node, inherits, c("XMLInternalTextNode", "XMLInternalCDataNode"))
  if(all(w))
    xmlValue(node)
  else
    paste(sapply(xmlChildren(node)[w], xmlValue), collapse = "\n")
}



addCodePrompt.Text =
function(doc, prompt = unlist(options()[c("prompt", "continue")]), replace = TRUE,
          asIs = NA,
           width.cutoff = 55L, ...)
{
  txt = xmlValue(doc)
#  txt = XML:::trim(txt)
  
  if(length(txt) == 0 || nchar(txt) == 0 || !grepl("[^[:space:]]", txt))
    return(FALSE)

  txt = unlist(strsplit(txt, "\\\n"))

    # We leave blank lines alone. They could be part of a string.
  
#  txt = gsub("^\\n", "", txt)
  if(txt[1] == "")
    txt = txt[-1]

  txt = sprintf("\n%s", txt)
  txt = gsub("\\\n([^[:space:]])", sprintf("\\\n%s\\1", prompt[1]), txt)              
  txt = gsub("\\\n([[:space:]])", sprintf("\\\n%s\\1", prompt[2]), txt)
#  content = sprintf("\n%s%s\n", prompt[1], paste(txt, collapse = "\n"))
  if(XML:::indexOfNode(doc) == xmlSize(xmlParent(doc)) && !grepl("\\\n$", txt[length(txt)]))
    txt = c(txt, "\n")
  
  content = paste(txt, collapse = "")
  if(replace) {
    replaceNodes(doc, newXMLCDataNode(content))
  }

  content
}

setMethod("addCodePrompt", "XMLInternalTextNode", addCodePrompt.Text)
setMethod("addCodePrompt", "XMLInternalCDataNode", addCodePrompt.Text)  
          

setMethod("addCodePrompt", "XMLInternalElementNode", 
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), replace = TRUE,
                      asIs = NA,
                       width.cutoff = 55L, ...) {

          if(xmlName(doc) == "output")
             return(FALSE)

          if(is.na(asIs))
             asIs = xmlGetAttr(doc, "addPrompt", "") %in% c("I", "AsIs")

          if(asIs) {
                 # ?FIXED? assume no r:output, etc. i.e. sub nodes other than text
              ans = xmlSApply(doc, addCodePrompt, prompt = prompt, replace = replace, asIs = TRUE, width.cutoff = width.cutoff, ...)
              return(ans)
          }
              
          width.cutoff = xmlGetAttr(doc, "width.cutoff", formals(deparse)[[2]], as.integer)
          
          ans = if(!all(w <- xmlSApply(doc, inherits, c("XMLInternalTextNode", "XMLInternalCDataNode")))) {
                   kids = xmlChildren(doc)[w]
                   for( i in kids) {
                     val = addCodePrompt(parse(text = xmlValue(i)), prompt, width.cutoff = width.cutoff, ...)
                     replaceNodes(i, newXMLCDataNode(sprintf("\n%s\n", val)))
                   }
                } else {
                   text = xmlValue(doc)
                   txt = parse(text = text)
                   val = addCodePrompt(txt, prompt, width.cutoff = width.cutoff, ...)
                   if(replace) {
                      removeNodes(xmlChildren(doc))
                      newXMLCDataNode(sprintf("\n%s\n", val), parent = doc)
                   }
                   val
               }

            if(replace)
                 xmlAttrs(doc) = c(hasPrompt = "TRUE")

             val
          })

setMethod("addCodePrompt", "expression",
            function(doc, prompt = unlist(options()[c("prompt", "continue")]), ...) {
                paste(sapply(doc, addCodePrompt, prompt, ...), collapse = "\n")
              })

setMethod("addCodePrompt", "call", addPrompt.language)
setMethod("addCodePrompt", "=", addPrompt.language)
setMethod("addCodePrompt", "for", addPrompt.language)
setMethod("addCodePrompt", "while", addPrompt.language)
setMethod("addCodePrompt", "if", addPrompt.language)
setMethod("addCodePrompt", "name", addPrompt.language)



isValidRCode =
function(text)
{
  if(is(text, "XMLInternalElementNode"))
    text = getRCode(text)
  
  ans = try(parse(text = text), silent = TRUE)
  !inherits(ans, "try-error")
}
