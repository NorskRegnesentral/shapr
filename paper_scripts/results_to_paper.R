
#### Run all paper experiments and put the results in a table
rm(list=ls())

library(xtable)

experiment.num <- 1:7

res_df_to_paper <- NULL
paperfolder <- "paper_scripts"


for (OK in experiment.num){
    source(paste0("paper_scripts/paper_experiment_",OK,".R"))

    res_df_to_paper <- rbind(res_df_to_paper,res_to_paper)


    #### Produce tables ####
   # rownames(res_df_to_paper) = rep("",nrow(res_df_to_paper))

    xtab <- xtable(res_df_to_paper,
                   caption = "",
                   label = "tab:result_table",
                   digits = 3)

    print.xtable(xtab,
                 include.rownames = F,
                 include.colnames = F,
                 only.contents = T,
                 hline.after = c(0,nrow(xtab)),
                 file = file.path(paperfolder,paste0("paper_table_after_",OK,".tex")))

    print(OK)

}




