NIC <- function(mdl){
  tryCatch({
    b <- coef(mdl)[!is.na(coef(mdl))]
    # b <- b[setdiff(names(b), "(Intercept)")]
    x <- as.matrix(model.matrix(mdl))[,names(b)]
    # x <- x[,setdiff(names(b), "(Intercept)")]
    y <- mdl$y
    c <- mdl$c
    res <- vcov.robust.xy(x, y, b, c)
    nic <- res$nic
    aic <- res$aic
    dev <- res$deviance
    return(list("dev"=dev,
                "aic"=aic,
                "nic"=nic))
    
  },error=function(e){
    print(e)
  })
}
