# Synchronize with getXIncludeFiles() in ids.R

if(TRUE) {
getXIncludes =
  #
  # Read the given document and find the documents which it includes.
  #
  #
function(doc, base = dirname(doc), recursive = TRUE)
  UseMethod("getXIncludes")

getXIncludes.character =
function(doc, base = dirname(doc), recursive = TRUE)
{
  getXIncludes(xmlTreeParse(doc, useIntern = TRUE, xinclude = FALSE),
                base, recursive)
}

getXIncludes.XMLInternalDocument =
function(doc, base = dirname(doc), recursive = TRUE)
{

  if(!recursive)
     return(unique(xpathApply(doc, "//xi:include[@href]", xmlGetAttr, "href", namespaces = c(xi = "http://www.w3.org/2003/XInclude") )))

      # keep as nodes and don't just get the href as we want to be able to work with the xpointer
      # also to restrict our search to that subset of the included document.
  inc.nodes = getNodeSet(doc, "//xi:include[@href]", c(xi="http://www.w3.org/2003/XInclude"))

  ans = lapply(inc.nodes,
              function(node) {
                  u = xmlGetAttr(node, "href")
                  if(length(grep("^(/|http:|ftp:)", u)) == 0)
                     u = paste(base, u, sep = .Platform$file.sep)

                  if(length(grep("^(http|ftp):", u)) == 0 && !file.exists(u))
                    return(list())

                   #XXX  if there is an xpointer attribute on node, restrict to it.
                  
                  c(u, if(xmlGetAttr(node, "parse", "") != "text") getXIncludes(u, recursive = TRUE))
              })

  unique(unlist(ans))
}
}

getFunctionReferences =
  #
  # namespace allow us to do this for other languages, e.g. javascript
  # matlab, python, shell, etc. by providing the namespace and prefix.
  #
  #
function(doc, namespace = c("r" = "http://www.r-project.org"))
{
  if(is.character(doc))
    doc = xmlParse(doc)

  nodes = getNodeSet(doc, sprintf("//%s:func", names(namespace)), namespace)
  structure(sapply(nodes, xmlValue), names = sapply(nodes, xmlGetAttr, "pkg", ""))
}  


addFunctionID =
  #
  # See also the new addFunctionName().
  # Put an @id attribute on a node based on the R variable being assigned.
  #
function(doc, nodes = "//r:function[not(@id)]", save = TRUE)
{
   fun = function(node) {
              txt = xmlValue(node)

              e = parse(text = txt)
                  #XXX Need to check if this is a simple name.
              id = e[[1]][[2]]
              if(is.name(id))
                 xmlAttrs(node)["id"] = as.character(id)
           }

   processNodes(doc, nodes, fun, save)
}  

ReposNamespaces = c(r = 'http://www.r-project.org', omg = "http://www.omegahat.org",
                    bioc = "http://www.bioconductor.org", rforge = "http://www.r-forge.net")
loadDocPackages =
 #  finds the packages referenced in the document, discards those that have a noload="true"
 #  and optionally loads the packages. 
 # This is convenient when we are going to try to find the functions/objects/symbols
 # referenced in the document.
 #
function(doc, nodes = "//r:pkg|//omg:pkg|//bioc:pkg|//r:func/@pkg", namespaces = ReposNamespaces, load = TRUE)
{
  els = getNodeSet(doc, nodes, namespaces)
  pkgs = sapply(els, function(x) if(is.character(x)) x else xmlValue(x))

    # find the packages that have a noload attribute on any node mentioning them.
  omit = !is.na(sapply(els, function(x) if(is.character(x)) NA else xmlGetAttr(x, "noload", NA)))
  pkgs = setdiff(pkgs, pkgs[omit])

   # if we are asked to load them, do so but ensure no errors terminate the execution.
  if(load)
    structure(sapply(pkgs, tryLoadPackage), names = pkgs)
  else
    pkgs
}

tryLoadPackage = 
 # not exported
function(name)
{
  require(name, character.only = TRUE)
}


checkFunctions =
 # optionally loads the packages that are referenced in the document.
 #
function(doc, nodes =  if(all) "//r:func[not(@exists='false')]" else "//r:func[not(@exists='false') and not(@pkg)]",
          all = FALSE, skip = character(), load = TRUE)
{
  if(load)
     loadDocPackages(doc)

  fun = function(node) {
      id = xmlValue(node)
      p = find(id)
      if(length(p) == 0)
         p = NA
      structure(p[length(p)], names = id)
  }
  ans = processNodes(doc, nodes, fun, FALSE)
 
  names(ans)[is.na(ans)] 
}

addFunctionPackage =
  #
  # process the r:func nodes that have no pkg attribute and resolve
  # it given the current search path.
  #
  # We could also process the r:func nodes that do have a pkg argument
  # and verify they are correct.
  #
  # Want to compute the nodes in processNodes after we have evaluated doc
  # to an XMLInternalDocument.
  #
function(doc, nodes =  if(all) "//r:func" else "//r:func[not(@pkg)]",
         save = TRUE, all = FALSE, skip = character(), load = TRUE)
{
  if(load)
     loadDocPackages(doc)

  notFound = character()
  fun = function(node) {
             p = find(xmlValue(node))

	     if(length(p) == 0)
                notFound <<- c(notFound, xmlValue(node))


             p = gsub("^package:", "", p[length(p)])
             
             if(length(skip) && any(p == skip))
                return(FALSE)
                # p = p [ ! (p %in% skip) ]
             
             if(length(p) == 0)  {
                 # Could check the r:function[@id] values to see if they are locally defined.
               return(FALSE)
             }

             xmlAttrs(node)["pkg"] = p
             p
           }
  
  ans = processNodes(doc, nodes, fun, save)
  if(length(notFound))
    warning("didn't find function(s) named: ", paste(unique(notFound), collapse = ", "))

  invisible(ans)
}

processNodes =
  #
  # Generic work horse that is parameterized by the fun.
  #
function(doc, nodes, fun, save = TRUE)
{
    isFileName = is.character(doc)
    if(isFileName) 
      doc = xmlTreeParse(doc, useInternal = TRUE)

    if(is.character(nodes)) 
       nodes = getNodeSet(doc, nodes, c(r = "http://www.r-project.org"))
    
    ans = sapply(nodes, fun)

    if(is.logical(save) && !save)
      return(if(isFileName) doc else ans)
    else
       save = docName(doc)

    if(is.character(save) && length(save))
       saveXML(doc, save)

    doc
}  
