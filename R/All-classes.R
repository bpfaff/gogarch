##
## Class definition of initial GO-GARCH objects
##
setClass(Class = "Goinit", representation(X = "matrix", V = "matrix", P = "matrix", Dsqr = "matrix", garchf = "formula"))
##
## Class definition of GO-GARCH objects
##
setClass(Class = "GoGARCH", representation(Z = "matrix", Y = "matrix", H = "list", models = "list", estby = "character"), contains = "Goinit")
##
## Class definition of GO-GARCH objects, estimated by Maximum-Likelihood
##
setClass(Class = "Goestml", representation(opt = "list"), contains = "GoGARCH")
##
## Class definition of orthogonal matrices
##
setClass(Class = "Orthom", representation(M = "matrix"))


