library(data.table)

#setwd("/disk/home/jullum/Dropbox/Local_work/Projects/groupShapley")
Hallmark_genes = qusage::read.gmt("h.all.v7.4.symbols.gmt") # HALLMARK


colitis = GEOquery::getGEO(GEO = "GPL96",filename = "GDS1615_full.soft.gz")


Y = data.table(patient_id = colitis@dataTable@columns$sample,
               response = colitis@dataTable@columns$disease.state)

X <- t(as.matrix(colitis@dataTable@table[,colnames(colitis@dataTable@table) %in% Y[,patient_id]]))
all.equal(rownames(X),Y[,patient_id])
X <- as.data.table(X,keep.rownames = T)
setnames(X,"rn","patient_id")

gene_names <- colitis@dataTable@table$`Gene symbol`#gds1615@dataTable@table$IDENTIFIER#gds1615@dataTable@table$`Gene symbol`
colnames(X)[-1] <- gene_names


Hallmark_dt <- as.data.table(reshape2::melt(Hallmark_genes),)
setnames(Hallmark_dt, c("value", "L1"), c("gene", "gene_set"))

all_gene_names <- unique(unlist(lapply(as.list(gene_names),stringr::str_split_fixed,pattern="///",n=2),recursive = T))

geneset_dt <- merge(data.table(gene=all_gene_names), Hallmark_dt,by="gene",all=F)
geneset_dt <- unique(geneset_dt)


## Assign genes which belong to more than one group to the biggest one ### NO I DON'T DO THAT ALREADY!
#geneset_dt[,gene_set_N:=.N,by=gene_set]
#setorder(geneset_dt,-gene_set_N)

#geneset_dt[,gene_dup := duplicated(geneset_dt[,gene])]
#geneset_dt <- geneset_dt[gene_dup==FALSE]

#geneset_dt[,.N,by=gene_set]

# Only using genes found in the Hallmark dataset
these_genes <- names(X)[names(X)%in%geneset_dt[,gene]]
X <- X[,..these_genes]


# Set response and trainign data
set.seed(123)
n_train <- 100
Y[,binary_response:=(response!="normal")*1]
train_samps <- sample(1:nrow(Y),size = n_train,replace=F)
Y[,train_test:="test"]
Y[train_samps,train_test:="train"]

### Here I use glmnet instead

library(glmnet)

Y[,binary_response_factor :=as.factor(binary_response)]

glmnet_mod <- glmnet(x=as.matrix(X),y=Y$binary_response_factor,family = "binomial",alpha=1)

set.seed(123)
glmnet_cv <- cv.glmnet(x=as.matrix(X[which(Y$train_test=="train")]),y=Y$binary_response_factor[which(Y$train_test=="train")],family = "binomial",alpha=1,lambda = exp(seq(-1,-15,length.out=500)))
plot(glmnet_cv)
which_best_lambda <- which(glmnet_cv$lambda==glmnet_cv$lambda.1se)

pred_test <- as.vector(predict(glmnet_cv,newx = as.matrix(X[which(Y$train_test=="test")]),type="response"))
response_test <- Y$binary_response[(Y$train_test=="test")]

cbind(pred_test,response_test)
pROC::auc(response_test,pred_test)
brier <- mean((response_test-pred_test)^2)

best_beta <- glmnet_cv$glmnet.fit$beta[,which_best_lambda]
nonzero_betas <- names(best_beta)[best_beta!=0]

best_alpha <- glmnet_cv$glmnet.fit$a0[which_best_lambda]


# Could


# Using shapr from branch jens/explain_batch

# Define gene sets being used (in practice)
used_genesets_glmnet0 <-  unique(geneset_dt[data.table(gene=nonzero_betas),on="gene"])
geneset_dt0 <- copy(geneset_dt)
geneset_dt0[,gene_set_N:=.N,by=gene_set]

used_genesets_glmnet0 <- merge(used_genesets_glmnet0,unique(geneset_dt0[,.(gene_set,gene_set_N)]),by="gene_set",all.x=T)
setorder(used_genesets_glmnet0,gene,-gene_set_N)
used_genesets_glmnet0[,gene_dup:= duplicated(gene)]
used_genesets_glmnet0 <- used_genesets_glmnet0[gene_dup==FALSE]
used_genesets_glmnet0[,unique(gene_set)]


#geneset_dt[,gene_dup := duplicated(geneset_dt[,gene])]
#geneset_dt <- geneset_dt[gene_dup==FALSE]


used_genesets_glmnet0[,.N,by=gene_set]

used_genesets_glmnet <-used_genesets_glmnet0$gene_set
#geneset_dt00[gene_set %in%used_genesets_glmnet]

gene_gene_sets_glmnet <- unique(merge(geneset_dt,data.table(gene_set=used_genesets_glmnet),by="gene_set",all.y=F,all.x=F))
gene_gene_sets_glmnet[,gene_set_N:=.N,by=gene_set]
setorder(gene_gene_sets_glmnet,gene,-gene_set_N)
gene_gene_sets_glmnet[,gene_dup:= duplicated(gene)]
gene_gene_sets_glmnet_final <- gene_gene_sets_glmnet[gene_dup==FALSE]
gene_gene_sets_glmnet_final[,gene_set_N:=NULL]
gene_gene_sets_glmnet_final[,gene_dup:=NULL]


gene_cols_logical <- names(X) %in% gene_gene_sets_glmnet_final[,gene]
gene_cols <- names(X)[gene_cols_logical]


# Define training data
x_train <- X[which(Y$train_test=="train"),..gene_cols]
x_test <- X[which(Y$train_test=="test"),..gene_cols]
y_train <- Y[train_test=="train"]
y_test <- Y[train_test=="test"]


# Need unique column names
gene_cols_names <- make.unique(gene_cols,sep="__")

names(x_train) <- gene_cols_names
names(x_test) <- gene_cols_names

# Define grouping

# Merge in the new gene names used
gene_gene_sets_glmnet2 <- merge(gene_gene_sets_glmnet_final,data.table(gene=gene_cols,gene_col_name=gene_cols_names),by="gene",all.y=F,all.x=F)
# This seems to be in the correct order now

gene_groups0 <- gene_gene_sets_glmnet2[,list(list=list(gene_col_name)),by=gene_set]
gene_groups <- gene_groups0[,list]
names(gene_groups) <- gene_groups0[,gene_set]

# Define model object to use in shapr
best_beta_gene_cols <- best_beta[gene_cols_logical]
these_nonzero_gene_cols <- which(best_beta_gene_cols!=0)

lasso_model <- list(alpha=best_alpha,
                    these_nonzero_betas=these_nonzero_gene_cols,
                    nonzero_beta_vals = best_beta_gene_cols[these_nonzero_gene_cols])
class(lasso_model) = "lasso_manual_model"

predict_model.lasso_manual_model <- function(x, newdata) {
  z <- as.vector(x$alpha + as.matrix(newdata)[,x$these_nonzero_betas]%*%x$nonzero_beta_vals)
  exp(z)/(exp(z)+1)
}

# Just checking correc predicton
#all.equal(predict_model.lasso_manual_model(lasso_model,x_train),
#          as.vector(predict(glmnet_cv,newx = as.matrix(X[which(Y$train_test=="train")]),type="response")))#
# TRUE
#these <- c(4,1,7,4,2)
#all.equal(predict_model.lasso_manual_model(lasso_model,x_test[these,]),
#          as.vector(predict(glmnet_cv,newx = as.matrix(X[which(Y$train_test=="test")][these,]),type="response")))
# TRUE

gene_info_cols <- c("ID_REF","IDENTIFIER","Gene title","Gene symbol","Gene ID","Nucleotide Title","GI","GenBank Accession")

gene_info <- as.data.table(colitis@dataTable@table)[,..gene_info_cols]
setnames(gene_info,"Gene symbol","gene")

full_gene_info <- merge(gene_gene_sets_glmnet2,gene_info,by="gene",all.x = T,all.y=F)
basic_gene_info <- copy(gene_gene_sets_glmnet2)
full_gene_info_also_unused <- gene_info

object_list <- c("x_train","x_test","y_train","y_test","lasso_model","gene_groups","full_gene_info","basic_gene_info")
save(list=object_list,file = "shapley_objects_new.RData")
#saveRDS(full_gene_info,file="full_gene_info.rds")
#saveRDS(gene_info,file="gene_info_all.rds")

