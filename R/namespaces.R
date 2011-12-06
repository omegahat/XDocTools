checkNamespaces =
  #
  #  Find what namespace URIs have multiple prefixes and
  #  what prefixes map to multiple URIs
  #
function(doc, nodes = FALSE, defaults = character())
{
   doc = as(doc, "XMLInternalDocument")

   nodes = getNodeSet(doc, "//*[namespace::*]")
   defs = sapply(nodes, xmlNamespaceDefinitions, simplify = TRUE)
   defs = defs[ sapply(defs, length) > 0 ]

   allDefs = unlist(defs, recursive = FALSE)
   df = data.frame(id = sapply(allDefs, "[[", "id"), 
                   uri = sapply(allDefs, "[[", "uri"),
                   stringsAsFactors = FALSE)

   out = by(df$uri, df$id, function(x) as.character(unique(x)))
   dups = out[ sapply(out, length) > 1 ]

   out = by(df$id, df$uri, function(x) as.character(unique(x)))
   two = out[ sapply(out, length) > 1 ]

   structure(list(prefix = dups, uri = two), class = "XMLNamespaceSummary")
}

print.XMLNamespaceSummary =
function(x, ...)
{
  if(length(x$prefix)) {
     cat("Prefixes mapped to multiple URIs\n")
     cat(paste("   ", names(x$prefix), ": ", sapply(x$prefix, paste, collapse = ", "), sep = "", collapse = "\n"), "\n\n")
  }
  if(length(x$uri)) {
     cat("URIs with multiple prefixes\n")
     cat(paste("   ", names(x$uri), ": ", sapply(x$uri, paste, collapse = ", "), sep = "", collapse = "\n"), "\n")
  }  
  invisible(x)
}
