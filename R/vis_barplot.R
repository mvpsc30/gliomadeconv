#' Glioma deconv
#'
#' This function generatea a barplot of deconvolution result
#'
#' @param res the deconvolution result
#' @param use_phe if using phe, default is False
#' @param phe phe, rownames should be in rownames of deconvolution result
#' @export
#' @examples
#' vis_barplot(res)
#'

vis_barplot<-function(res,use_phe=F,phe=NULL){
  library(ggplot2)
  library(dplyr)
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

  if (!use_phe){
    estimated_frac<-res
    estimated_frac$ID<-rownames(estimated_frac)
    estimated_frac_long<-reshape2::melt(estimated_frac)
    palette<-all_colors
    estimated_frac_long$variable<-factor(estimated_frac_long$variable,levels = all_levels)
    estimated_frac_long$ID<-factor(estimated_frac_long$ID,levels = rownames(estimated_frac))

    p <- estimated_frac_long %>%
      ggplot(aes(ID,value))+
      geom_bar(stat = "identity",position = "stack",aes(fill=variable),
               width = 1)+
      labs(x=NULL)+
      scale_y_continuous(expand = c(0,0))+
      scale_fill_manual(values = palette,name=NULL)+
      theme_bw()+
      theme(
        axis.text.x = element_text(angle = 45,hjust = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "right"
      )
  }
  if (use_phe){
    estimated_frac<-res
    if (!setequal(rownames(estimated_frac), rownames(phe))) {
      stop("Rownames are not same, please re-check!")
    }
    estimated_frac <-estimated_frac[rownames(phe),]
    estimated_frac$ID<-rownames(estimated_frac)
    estimated_frac_long<-reshape2::melt(estimated_frac)
    palette<-all_colors
    estimated_frac_long$variable<-factor(estimated_frac_long$variable,levels = all_levels)
    estimated_frac_long$ID<-factor(estimated_frac_long$ID,levels = rownames(phe))

    p <- estimated_frac_long %>%
      ggplot(aes(ID,value))+
      geom_bar(stat = "identity",position = "stack",aes(fill=variable),
               width = 1)+
      labs(x=NULL)+
      scale_y_continuous(expand = c(0,0))+
      scale_fill_manual(values = palette,name=NULL)+
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
