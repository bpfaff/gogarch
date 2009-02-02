##
## Class definition of initial GO-GARCH objects
##
setClass(Class = "Goinit", representation(X = "matrix", V = "matrix", P = "matrix", Dsqr = "matrix", garchf = "formula"))
##
## Class definition of GO-GARCH objects
##
setClass(Class = "GoGARCH", representation(Z = "matrix", Y = "matrix", H = "list", models = "list", estby = "character", CALL = "call"), contains = "Goinit")
##
## Class definition of GO-GARCH objects, estimated by Maximum-Likelihood
##
setClass(Class = "Goestml", representation(opt = "list"), contains = "GoGARCH")
##
## Class definition for summary objects from GoGARCH
##
setClass(Class = "Gosum", representation(name = "character", method = "character", model = "formula", garchc = "list", Zinv = "matrix"))
##
## Class definition of orthogonal matrices
##
setClass(Class = "Orthom", representation(M = "matrix"))


