findRawFunctionNames =
  #
  # Find words in paragraphs that look like they might be
  # the names of R functions and should be in <r:func> nodes.
  # We determine what looks like a function by a) looking
  # at the names that are actually in <r:func> nodes, b)
  # names of R functions on the search path.
  #
  # Words like "for", "apply", "system", ... are real function
  # names but also so common in text that we discard these
  # or give them a low weight. We identify these via the stopWords.
  #
  # We could do more here and use latent semantic analysis to try to understand
  # the context of the reference to the would be function name.
  #  e.g. "call the apply function", "the function apply "
  #
  #
  #  Do the same for functions in other languages, e.g. js:func, etc.
  #
  
function(doc, referencedFunctions = getFunctionReferences(doc),
          knownFunctions = getAllFunctionNames(stopWords = stopWords), stopWords = character())
{
   doc = as(doc, "XMLInternalDocument")

   words = getParagraphWords(doc)

   i = words %in% referencedFunctions

      # Could look at the counts in referencedFunctions
      # and favor those that are really commonly found.
   ref = table(words[i])
   
   list(referenced = ref, search = table(words[!i][ words[!i] %in% knownFunctions ]))
}

getParagraphWords =
function(doc)
{
  doc = as(doc, "XMLInternalDocument")  
  txt = xpathSApply(doc, "//para/text()", xmlValue)
  unlist(strsplit(txt, "[[:space:][:punct:]]+"))
}

getAllFunctionNames =
function(pkgs = search(), doc = NULL, stopWords = character(), omit.pattern = "^.__[CM]__")
{
    #XXX should get only functions.
  ans = unlist(sapply(pkgs, objects, all = TRUE))
  #XXX double sapply should probably be rolled into 1 ~GB
  ans = ans[sapply(ans, function(x) is.function(get(x)))]
  
  if(!is.null(doc))
    ans = c(ans, getDocPackageObjects(doc, funcsOnly = TRUE))
  
  if(length(stopWords))
     ans = ans[! (ans %in% stopWords) ]

  i = grep(omit.pattern, ans)
  if(length(i))
    ans = ans [ - i ]
  
  unique(ans)
}

getDocPackageObjects =
function(doc, funcsOnly = FALSE)
{
  doc = as(doc, "XMLInternalDocument")    
      # Now load packages that are referenced but not loaded
      # Can load them and get the objects and then unload them immediately
  funcs = getNodeSet(doc, "//r:func")  
  pkgs = unique(c(sapply(funcs, xmlGetAttr, "pkg", NA),
                  xpathSApply(doc, "//r:pkg|//omg:pkg", xmlValue, namespaces = Namespaces[c("r", "omg")])
                 ))
  pkgs = pkgs[!is.na(pkgs)]
  tmp = paste("package", pkgs, sep = ":")
  pkgs = pkgs[!(tmp %in% search())]
  
  unlist(sapply(pkgs, getPackageObjects, funcsOnly = funcsOnly))
} 
  




Namespaces = c(r = "http://www.r-project.org",
               omg = "http://www.omegahat.org")

getErroneousFunctions =
  #
  # This finds the r:func nodes that have a function name
  # that doesn't correspond to an actual function.
  # This may miss functions that are legitimate but which 
  # are not in the search path. This is up to the caller
  # or for the author to specify the intended package via the @pkg
  # attribute in the r:func.
  #
  #  All will control whether we return funNames as well.
  #
  #
  #  force causes us to use getAnywhere() on just the ones we can't match.
  #
  # Note that when we don't get a match, we might try to suggest possible matches
  # within the r:func values we did match.
  #
  #
  function(doc, nodes = FALSE, loadPackages = TRUE, quietly = TRUE, all = TRUE, force = TRUE, additionalPackages = c("XML", "XDocTools"), additionalFunctions = unlist(tryCatch(checkRCodeFunctions(doc), error=function(e) NULL)))
{
  doc = as(doc, "XMLInternalDocument")
  funcs = getNodeSet(doc, "//r:func")

   # Get all function names
  funNames = getAllFunctionNames(doc = doc)
  if(length(additionalFunctions))
    
  refNames = sapply(funcs, xmlValue)
  i = !(refNames %in% funNames)

  ok = unique(refNames[ ! i ])
  
  if(force)
     i = which(i)[ sapply(refNames[i], function(x) length(eval(substitute(getAnywhere(x), list(x = x)))$where)) == 0 ]

  possibles = sapply(unique(refNames[i]), agrep, ok, value = TRUE)
  general.possibles = sapply(unique(refNames[i]), agrep, funNames, value = TRUE)
  possibles = possibles[sapply(possibles, length) > 0]
  general.possibles = general.possibles[sapply(general.possibles, length) > 0]  
  files = sapply(funcs[i], function(node) findXInclude(node)[1])
  filesPerMistake = split(files, refNames[i])
  if(nodes) 
     funcs[i]
  else {
     ans = sort(table(refNames[i]), decreasing = TRUE)
     if(all)
        list(unmatched = ans,
             within.matches = possibles,
             general.matches = general.possibles,
             allNames = funNames,
             files = filesPerMistake)
     else
        ans
  }
}

# The following two functions are support for getErroneousFunctions
# and manipulate the search path temporarily and ensure it is restored
# to its original

getPackageObjects =
  #
  # Get the names of all the objects in a package, attaching it to the search path if necessary
  # and detaching it along with any other packages that were attached as a result
  # of attaching this one.
  #
  # funcsOnly indicates whether we should return the names of all
  # objects in the package, or only those that are functions.
  # Defaults to previous behavior (all objects) ~GB
function(pkg, all = TRUE, quietly = TRUE, funcsOnly = FALSE)
{
  orig = search()
  ans = character()
  full.name = paste("package", pkg, sep = ":")

  #initialize to TRUE because according to comment above we want objects whether or not we have to load the package to get them ~GB
  ok = TRUE
  if(!full.name %in% orig) {
    ok = require(pkg, warn.conflicts = FALSE, character.only = TRUE, quietly = quietly)
  }
  if(ok)
    {
      ans = objects(full.name, all = all)
      if(funcsOnly)
        ans = ans[sapply(ans, function(nm) is.function(get(nm)))]
    }
   # We need to clean up packages even if require() failed because
   # it may have attached some dependent packages and then failed
   # and so left them there!
  
   # Moved this outside of the if(!full.name %in% orig).
   #detachPackages knows how to deal with empty set (return TRUE) ~GB 
  detachPackages(setdiff(search(), orig), TRUE) 
  ans
}


detachPackages =
function(pkgs, hasPrefix = length(grep("^package:", pkgs)))
{
  if(length(pkgs) == 0)
    return(TRUE)

  if(!hasPrefix)
    pkgs = paste("package", pkgs, sep = ":")

  pos = match(pkgs, search())
  pos = pos[!is.na(pos)]

    #We need to start with the lowest position due to package
    #dependencies. We calculate cumulative offsets, complicated,
    #but using names or starting from the highest position
    #don't work ~GB
  offset = cumsum(c(0,rep(1, times = length(pos) - 1)))

  sapply(sort(pos) - offset, function(i) detach( pos = i ))
}


checkRCodeFunctions =
  #
  # parse the r:code nodes and get the code (ignoring r:output, etc.)
  #  and find all the functions being referenced.
  #
function(doc)
{
   library(codetools)
   doc = as(doc, "XMLInternalDocument")

   code = xmlSource(doc, eval = FALSE)

   f = function(){}
   sapply(code, function(x) {
                   body(f) = as.expression(x)
                   findGlobals(f, merge = FALSE)$functions
                 })
}

###############

# Change r:pkg to omg:pkg for certain nodes.
if(FALSE) {
 nodes = getNodeSet(bk, "//r:pkg[string(.) = 'XML']")
   # Ensure the namespace up to the xinclude node
 invisible(lapply(nodes, function(x) xmlNamespace(x) = "omg"))
}


