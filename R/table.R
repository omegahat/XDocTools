
addColumn =
  #
  #  tbl - the parsed table
  #  content is the content for the new column
  #  col - the index for the new column. By default, it will be added to the end.
  #      this is used as the at argument for addChildren and so the new nodes
  #      go after the specified column. So to add at the beginning of each row
  #      we'd use 0.
function(tbl, content = "", col = NA, cellTemplate = NA, format = NA)
{
    # For docbook, html
  rows = getNodeSet(tbl, ".//row | .//tr")

  mapply(addCell, rows, content, MoreArgs = list(col = col, template = cellTemplate))

  grp = getNodeSet(tbl, ".//tgroup[@cols]")
  if(length(grp))
     xmlAttrs(grp[[1]]) = c(cols = as.integer(xmlGetAttr(grp[[1]], "cols")) + 1L)

  tbl
}

addCell =
function(row, content, col = NA, template = NA)
{
  if(is.na(col))
    col = xmlSize(row)

  if(is.na(template)) {
     template = xmlClone(row[[1]])
     xmlValue(template) = ""
  } else if(is.character(template))
     template = newXMLNode(template)
  else {
     template = xmlClone(template)
   }

  if(!is.na(content) && content != "")
    addChildren(template, content)
  
  addChildren(row, template, at = col)

  template
}
