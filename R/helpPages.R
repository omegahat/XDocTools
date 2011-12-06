
#
# In an XML/Rdb document, we map an r:func node to
#  an <a href="..."> so that the reader can click on the
# name of the function and pull up the help. To implement
# this we need to generate the HTML files in the appropriate
# place. Also, some help pages will be in a file named for a different
# function, so we need to find those

makeHTMLDoc = 
function(funcName, package = NA, out = paste(dir, .Platform$file.sep, funcName, if(isClass) "-class", ".html", sep = ""),
          dir  = ".", isClass = FALSE)
{
  library(tools)

  if(is(funcName, "XMLInternalNode")) {
    if(missing(package))
       package = xmlGetAttr(funcName, "pkg", NA)
    if(missing(isClass) && xmlName(funcName) == "class")
      isClass = TRUE
    
    funcName = xmlValue(funcName)
  }
  
  if(is.na(package) || length(package) == 0)
    if(isClass) {
       package = findClass(funcName)
       if(length(package))
         package = attr(package[[1]], "name")
    } else  package = find(funcName)

  if(length(package) == 0) {
    warning("Cannot find the object named ", funcName)
    return(chracter())
  }

  package = package[length(package)]
  package = gsub("^package:", "", package)

  rd = Rd_db(package)
  aliases = lapply(rd, tools:::.Rd_get_metadata, "alias")
  aname = if(isClass) sprintf("%s-class", funcName) else funcName
  doc = rd[[ which(sapply(aliases, function(x) any(aname == x))) ]]
  if(length(doc) == 0 || is.null(doc))
    stop("Cannot find Rd file for ", funcName)

  invisible(Rd2HTML(doc, out, package))
}

generateRdFiles =
  #
  # Get the references to R fnctions and classes and generate the
  # help files for each. We may end up with duplicates and certainly
  # we don't bother dealing with multiple objects documented in a single
  # file. We create a separate file for each symbol/object.
function(doc, dir = "HTML")
{
  if(is.character(doc))
     doc = xmlParse(doc)
  
  funcs = getNodeSet(doc, "//r:func|//r:class", "r")

  sapply(funcs, makeHTMLDoc, dir = dir)

  invisible(doc)
}
