# Spell check a graffle document from Omnigraffle, a Mac diagram editor/drawing
# app.
# The .graffle files are regular XML files.
#
# This just spell-checks the Text nodes which also contain a lot of RTF.
#

if(!isGeneric("spellCheck"))
  setGeneric("spellCheck",
              function(doc, ...)
                   standardGeneric('spellCheck'))

spellCheck.Omnigraffle =
function(doc, asNodes = TRUE, ...)
{
   if(is.character(doc))
     doc = xmlParse(doc)

   nodes = getNodeSet(doc, "//dict/key[text() = 'Text']/following-sibling::string[1]")
   txt = gsub("(.*\\\\cf0 |}$)", "", sapply(nodes,  xmlValue))

   library(Aspell)
   mis = lapply(txt, function(txt) {
                 words = strsplit(txt, "[[:space:]]+")[[1]]
                 ok = aspell(words)
                 words[!ok]
               })

   w = sapply(mis, length) > 0
   if(asNodes)
     structure(nodes[w], names = sapply(mis[w], paste, collapse = ", "))
   else
     unlist(mis)
}

setMethod("spellCheck", "OmniGraffleDoc", spellCheck.Omnigraffle)

