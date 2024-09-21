#' Glioma deconv
#'
#' This function generatea a deconvolution result of bulk RNA-seq
#'
#' @param exp the expression matrix, with no log scale
#' @export
#' @examples
#' res <- glioma_deconv(exp)

glioma_deconv<-function(exp){
  library(InstaPrism)
  library(Biobase)
  library(data.table)
  load(system.file("data",
                   "refPhi_obj_glioma_fourstate.Rda",
                   package = "gliomadeconv"))

  ref<-refPhi_obj
  bulk.eset<-ExpressionSet(assayData = as.matrix(exp))
  example_res = InstaPrism(bulk_Expr = exprs(bulk.eset),refPhi_cs = ref)
  estimated_frac = t(example_res@Post.ini.ct@theta)
  estimated_frac <-as.data.frame(estimated_frac)
  return(estimated_frac)
}

