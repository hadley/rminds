library(rggobi)

.GGobiCall <- getNamespace("rggobi")$.GGobiCall

 ggobi_display_set_tour_projection <- 
function (gd, value) 
{
    normal <- all(colSums(value^2) - 1 < 0.001)
    orthog <- all(crossprod(value, value) - diag(ncol(value)) < 
        0.001)
    if (!normal) 
        stop("Matrix is not normal (colSums do not equal 1)")
    if (!orthog) 
        stop("Matrix is not orthogonal")
    invisible(.GGobiCall("setTourProjection", gd, pmode(gd), 
        value))
}

 ggobi_display_get_tour_projection <- 
function (gd) 
{
    mat <- .GGobiCall("getTourProjection", gd, pmode(gd))
    mat[, 1:2]
}

