getBibliographyProblems =
function(doc)
{
  doc = as(doc, "XMLInternalDocument")  
#  getNodeSet
}

biblioDuplicateTitles =
function(doc, nodes = FALSE)
{
  duplicateTitles(doc, nodes, "//biblioentry/title")
}

duplicateTitles =
  #
  # Find duplicate titles
  #
  #
function(doc, nodes = FALSE, xpath = "//section/title")
{
  doc = as(doc, "XMLInternalDocument")  
  titles = getNodeSet(doc, xpath)
  txt = sapply(titles, xmlValue)
  w = duplicated(txt)

  if(nodes) {
    tapply(titles, txt, function(x) x)
  } else
    txt[w]
}
