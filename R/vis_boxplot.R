#' Glioma deconv
#'
#' This function generatea a boxplot of deconvolution result
#'
#' @param res the deconvolution result
#' @param phe phe, rownames should be in rownames of deconvolution result
#' @param group group name, should be in phe
#' @param celltype default is "all", can be "NPClike", "AClike", "OPClike","MESlike","Microglia","Granulocyte","Dendritic cells","MHC.TAM", "Ribo.TAM", "Prolife.TAM","Lipid.TAM", "Hypoxia.TAM","Senescent.TAM","Oligodendrocyte","Fibroblast","Lymphocyte", "Endothelial"
#' @param ncol n columns of the plot
#' @export
#' @examples
#' vis_boxplot(res)

vis_boxplot<-function(res,phe,group,celltype="all",ncol=5){
  library(ggplot2)
  library(ggpubr)
  all_colors<-c("#5DA2CB","#9F9359","#366B4B","#8D3A4B",
                "#B282B9","#F5CFE5","#8FA5AD","#FCEE82",
                "#F5D3A8","#BBDE77","#EE934F","#7DBFA8","#A08B98",
                "#CA934E","#EFD8C0","#A22725","#BBDE77")

  all_levels<-c("NPClike", "AClike", "OPClike","MESlike",
                "Microglia",
                "Granulocyte","Dendritic cells",
                "MHC.TAM", "Ribo.TAM", "Prolife.TAM",
                "Lipid.TAM", "Hypoxia.TAM",
                "Senescent.TAM",
                "Oligodendrocyte","Fibroblast",
                "Lymphocyte", "Endothelial")

  estimated_frac<-res

  if (!setequal(rownames(estimated_frac), rownames(phe))) {
    stop("Rownames are not same, please re-check!")
  }
  estimated_frac <-estimated_frac[rownames(phe),]
  estimated_frac$ID<-rownames(estimated_frac)
  estimated_frac$group<-as.character(phe[,group])
  estimated_frac_long<-reshape2::melt(estimated_frac)
  palette<-all_colors
  estimated_frac_long$variable<-factor(estimated_frac_long$variable,levels = all_levels)

  if (celltype=="all"){
    p<- ggpubr::ggboxplot(estimated_frac_long,
                          x="group",
                          y="value",
                          fill = "group",
                          add = "jitter",
                          palette = "nejm",
                          ylab = "Cell Proportions"
    )+stat_compare_means()+facet_wrap(.~variable,ncol = ncol,scales = "free_y")+
      theme_bw()+
      theme(
        axis.text.x = element_text(angle = 45,hjust = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "right"
      )
  }
  if (!celltype=="all"){
    estimated_frac_long<-estimated_frac_long[which(estimated_frac_long$variable %in% celltype),]
    p<- ggpubr::ggboxplot(estimated_frac_long,
                          x="group",
                          y="value",
                          fill = "group",
                          add = "jitter",
                          palette = "nejm",
                          ylab = "Cell Proportions"
    )+stat_compare_means()+facet_wrap(.~variable,ncol = ncol,scales = "free_y")+
      theme_bw()+
      theme(
        axis.text.x = element_text(angle = 45,hjust = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "right"
      )
  }
  return(p)
}
