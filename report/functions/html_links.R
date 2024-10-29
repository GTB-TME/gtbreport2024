# Simple functions to create call-outs in html text to the figures/tables and references.
# Display names have non-alphanumeric characters changed to hyphens in the html anchor name
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


lnk <- function(display_name){

  # Create a link from text to a named anchor
  # Don't use markdown short cut because Sitefinity needs to have the span within the anchor text
  # Also can't have <a class="red" ...  in Sitefinity
  
  return(paste0('<a href="#',
                gsub("[^a-zA-Z0-9]", "-", tolower(display_name)),
                '"><span class="red">',
                display_name,
                '</span></a>'))
}

anch <- function(display_name){

  # Create a named anchor (can be inserted above a figure or table or references section)
  
  return(paste0('<a name="',
                gsub("[^a-zA-Z0-9]", "-", tolower(display_name)),
                '"></a>'))
}


ref_lnk <- function(display_name){

  # Create a link from text to the references in the WHO style (black, italic)
  # Don't use markdown short cut because Sitefinity needs to have the span within the anchor text
  # Also can't have <a class="refs" ...  in Sitefinity
  
  return(paste0('<a href="#refs"><span class="refs">', 
                display_name,
                '</span></a>'))
}
