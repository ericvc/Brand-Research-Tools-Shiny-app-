#Function for horizontal placement of textInput objects.
textInputRow <- function(inputId, label, value = ""){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

#Function for horizontal placement of numericInput objects.
numericInputRow <- function(inputId, label, value = ""){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "numeric", value = value,class="input-small"))
}
