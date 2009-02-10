setMethod("formula", signature(x = "GoGARCH"), function(x, ...)
  x@garchf
)

setMethod("formula", signature(x = "Goestica"), function(x, ...)
  x@garchf
)

setMethod("formula", signature(x = "Goestmm"), function(x, ...)
  x@garchf
)

setMethod("formula", signature(x = "Goestnls"), function(x, ...)
  x@garchf
)

setMethod("formula", signature(x = "Goestml"), function(x, ...)
  x@garchf
)
