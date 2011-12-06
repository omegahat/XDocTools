
findLatexComments =
  #
  # Find latex comments in the text
  # These may have come during a transition from latex to XML.
  #
function(doc)  
{
   doc = as(doc, "XMLInternalDocument")

   percent = getNodeSet(doc, "//text()[parent::para and contains(., '%')]")
   
   percent
}


findLatexMarkup =
  #
  # Find latex comments in the text
  # These may have come during a transition from latex to XML.
  #
  # 
function(doc, pattern = "\\\\[a-zA-Z]({}|\\.)|\\\\[a-zA-Z]+{[^}]+}")  
{
   doc = as(doc, "XMLInternalDocument")

   txt = getNodeSet(doc, "//para/text()")
   vals = sapply(txt, xmlValue)
   i = grep(pattern, vals, perl = TRUE)
   txt[i]
}
