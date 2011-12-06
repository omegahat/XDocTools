
addFunctionName =
function(doc, nodes = getNodeSet(doc, "//r:function[not(@r:id)]"))
{
   if(is.character(doc))
     doc = xmlParse(doc)
   
   ids = sapply(nodes, identifyFunction)
   mapply(setRId, nodes, ids)

   invisible(doc)
}

setRId =
function(node, id)
{
  xmlAttrs(node) = c("r:id" = id)
  node
}

identifyFunction =
function(node, env = new.env())
{
  existing = if(!missing(env))
               ls(env)
             else
               character()
  
  e = eval(parse(text = xmlValue(node)), env)
  setdiff(ls(env), existing)
}
