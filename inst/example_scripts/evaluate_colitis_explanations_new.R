
# Evaluate the colitis explanations

library(data.table)
library(ggplot2)


load("/disk/home/jullum/Dropbox/Local_work/Projects/groupShapley/shapley_objects_new.RData")
explanation <- readRDS(file = "/disk/home/jullum/Dropbox/Local_work/Projects/groupShapley/colitis_explanation_empirical_5.rds")
y_test


gg_normal <- plot(explanation,plot_phi0 = F,index_x_test = which(y_test$response=="normal"))
gg_uc <- plot(explanation,plot_phi0 = F,index_x_test = which(y_test$response=="ulcerative colitis"))
gg_crohn <- plot(explanation,plot_phi0 = F,index_x_test = which(y_test$response=="Crohn's disease"))



explanation$dt[,id:=.I]
explanation$dt[,response:=y_test$response]

dat.melted <- data.table::melt(explanation$dt,id.vars=c("id","response"))

ggplot(data=dat.melted[variable!="none"],aes(x=variable,y=value,col=response))+#geom_point()
  geom_boxplot(position="dodge")+#geom_jitter(width = 0.2)+#facet_wrap(facets = vars(response))+
  ggplot2::coord_flip()+theme_gray()


full_gene_info[,`Gene ID`:=as.numeric(`Gene ID`)]
#### Here I try to find genes from papers that are "known" to play a vital role for UC vs Crohn's disease

# Gene conversion tool: https://www.ncbi.nlm.nih.gov/gene/

# Original data paper (Burzynski et al):
#https://www.sciencedirect.com/science/article/pii/S1525157810602929?via%3Dihub
# Conclusions from discussion section:

# SERPINB2 (PAI2) and to some extent PAI1 (PAIP1?) are common distinctions for both diseases (so bigger Shapley values for both diseases)
#https://www.ncbi.nlm.nih.gov/gene/5055
full_gene_info[gene=="SERPINB2",gene_set] # "HALLMARK_TNFA_SIGNALING_VIA_NFKB"
# PAI1 also mentioned here:
unique(full_gene_info[grep("PAI",gene),.(gene,gene_set)])
#gene                           gene_set
#1: PAIP1 HALLMARK_UNFOLDED_PROTEIN_RESPONSE

# Specially for CD patients:
full_gene_info[grep("PTGS1",gene)] # Not in our data
full_gene_info[grep("PTGS2",gene),gene_set] # Not mentioned directly, but in our data
#[1] "HALLMARK_TNFA_SIGNALING_VIA_NFKB"
full_gene_info[grep("PTGDS",gene),unique(gene_set)] # Yes, in our data
#[1] "HALLMARK_XENOBIOTIC_METABOLISM"
full_gene_info[grep("PGE2",gene),] # NOT IN OUR DATA

full_gene_info[`Gene ID`==5196,gene_set] # Yes, in our data
#HALLMARK_ALLOGRAFT_REJECTION
full_gene_info[`Gene ID`==5473,gene_set] # not in our data
full_gene_info[`Gene ID`==5197,gene_set] # not in our data

# Check also the 7 gene identified by Mannick and colleagues: Gene expression in mononuclear cells from patients with inflammatory bowel disease
full_gene_info[grep("TSC22",gene),gene_set] # Yes, in our data
#[1] "HALLMARK_TNFA_SIGNALING_VIA_NFKB"

full_gene_info[grep("YAP",gene)] # NOT IN OUR DATA


### Specific for UC patients:

full_gene_info[grep("LGG",gene)] # NOT IN OUR DATA
full_gene_info[grep("HG3",gene)] # NOT IN OUR DATA
full_gene_info[`Gene ID`>3500 & `Gene ID`<=3600] # NOT IN OUR DATA
full_gene_info[grep("G1",gene)] # NOT IN OUR DATA

full_gene_info[`Gene ID`==3500] # NOT IN OUR DATA


### THIS PAPER: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4714968/

#COMMON: NOD2, MHC, and MST1 3p21
full_gene_info[grep("NOD",gene)] # Not NOD2, but NOD1 we have

# HLA mentioed as Crohn's disease are distinct entities because variants in NOD2 are associated with small bowel disease and HLA alleles with colonic disease
full_gene_info[grep("HLA",gene)] # LOTS OF THESE IN OUR DATA

full_gene_info[grep("MST",gene)] # NOT IN OUR DATA
full_gene_info[`Gene ID`==4485] # NOT IN OUR DATA

#### Arguments:

#This paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6930003/
# states that TNFA_SIGNALING_VIA_NFKB was supportive of chrohn's disease, which is also supported in our stud

hallmarks <-substring(unique(dat.melted$variable),first = 10)[-1]
hallmarks[grep("SIGNAL",hallmarks)]

# Interesting gene sets:
INTERFERON_ALPHA_RESPONSE# Maybe here: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjDt-OQpabzAhWIl4sKHfMDC2MQFnoECAQQAQ&url=https%3A%2F%2Fwww.ncbi.nlm.nih.gov%2Fpmc%2Farticles%2FPMC7049232%2F&usg=AOvVaw3s-mcIRb_obv_x7nT8qWaN
COMPLEMENT
MYC_TARGETS_V1
MYOGENESIS
MITOTIC_SPINDLE
P53_PATHWAY # Meta study https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5655860/ suggests that this is present from both CD and UC
KRAS_SIGNALING_DN
UV_RESPONSE_UP
TNFA_SIGNALING_VIA_NFKB

### Chrons disease study: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6930003/, both TNFA_SIGNALING_VIA_NFKB and UV_RESPONSE_UP were found

### Original paper with single genes:

# important for CD, but not for UC:
#HALLMARK_XENOBIOTIC_METABOLISM throuhg PTGDS
#HALLMARK_ALLOGRAFT_REJECTION includes chemokines which are mentioned as clear indicators of CD by oriignal apper.
# HALLMARK_ALLOGRAFT_REJECTION through full_gene_info[`Gene ID`==5196,gene_set] Several chemokines (C-X-C ligands 4, etc)
# HALLMARK_TNFA_SIGNALING_VIA_NFKB through TSC22

#THIS PAPER: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6930003/ studies Chron's disease only, and find: TNFA_SIGNALING_VIA_NFKB and UV_RESPONSE_UP as important



#### Try to find which gene set has the largest differences, and whether genes from that set
#### are found in the literature
#DIFFERENCES ACCORIDNG TO ORG PAPER

# HALLMARK_TNFA_SIGNALING_VIA_NFKB Contains both NFKB2, COPEB (KLF6) which are mentioned by original paper

#CUGBP1 and CUGBP2, COPEB, ELK3, and Meis 1.

#HALLMARK_TNFA_SIGNALING_VIA_NFKB
full_gene_info[grep("NFKB2",gene)] # HALLMARK_TNFA_SIGNALING_VIA_NFKB
COPEB#

full_gene_info[grep("CELF",gene)] # HALLMARK_TNFA_SIGNALING_VIA_NFKB
full_gene_info[grep("ELK",gene)] # HALLMARK_TNFA_SIGNALING_VIA_NFKB
full_gene_info[grep("MEI",gene)] # HALLMARK_TNFA_SIGNALING_VIA_NFKB
full_gene_info[`Gene ID`==2004] # NOT IN OUR DATA

#### SUMMARY:

#P53_PATHWAY: Meta study https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5655860/ suggests that this is present from both CD and UC
#TNFA_SIGNALING_VIA_NFKB: Contains both NFKB2, COPEB (KLF6) which are mentioned by original paper as distinguishing genes (UC vs CD), and by
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6930003/ as important fro CD

#XENOBIOTIC_METABOLISM: contains PTGDS whihc is mentioned by original paper as CD specific
#UV_RESPONSE_UP: mentioned by https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6930003/ as identifier of CD

key_gene_sets <- c("P53_PATHWAY","TNFA_SIGNALING_VIA_NFKB","XENOBIOTIC_METABOLISM","UV_RESPONSE_UP")

dat.melted[variable!="none",variable := substring(variable,first = 10)]


ggplot(data=dat.melted[variable %in%key_gene_sets],aes(x=variable,y=value,col=response))+#geom_point()
  #geom_boxplot(position="dodge")+
  geom_jitter(width = 0.2)+#facet_wrap(facets = vars(response))+
  ggplot2::coord_flip()+theme_gray()


UC_id <- c(16,17)
CD_id <- c(19,21)
normal_id <- c(7,10,11)
these_ids <- c(UC_id,CD_id,normal_id)

level_order <- dat.melted[response=="normal",min(value),by=variable]
setorder(level_order,V1)
level_order$variable[c(3,4,1,2,5:23)]

dat.melted[,gene_set:=factor(variable,levels=rev(level_order$variable[c(3,4,1,2,5:23)]))]


size = 5.5
dat.melted[response=="normal",response:="healty control"]
dat.melted[response=="healty control",resp2:="control"]
dat.melted[response=="Crohn's disease",resp2:="CD"]
dat.melted[response=="ulcerative colitis",resp2:="UC"]
dat.melted[,resp2:=factor(resp2,levels=c("control","CD","UC"))]

gg0 <- ggplot(data=dat.melted[!(variable %in% "none")],aes(x=gene_set,y=value,col=resp2))+#geom_point()
  geom_boxplot(position="dodge",outlier.size = 0.5,lwd=0.25)+
#  geom_point()+#facet_wrap(facets = vars(response))+
  ggplot2::coord_flip()+#theme_gray()+
  theme(axis.text.y = element_text(face = c(rep("plain",6),"plain",rep("plain",11),"bold.italic","plain","plain","bold.italic","bold.italic")))+
  xlab("Gene set")+ylab("groupShapley value")+
  theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size),
    legend.position="right"
  )+
  guides(col=guide_legend(title="Patient    \n class ",title.position = "top",title.vjust=6,label.position ="bottom",keywidth = 0.00002,keyheight=1.5,direction="vertical"))+
  theme(plot.margin=unit(c(0,0,0,0),"cm"),legend.key.size =unit(0.1, 'cm'))
gg0
ggsave(
  "colitis-full_new2.png",
  plot = gg0,
  device = 'png',
  path = 'plots',
  scale = 1,
  width = 13,
  height = 14,
  units = "cm"
)


# size = 7 # this is a good size for the paper
# theme_set(theme_bw()) # this makes a white background
# p1 = plot(explanation_group, plot_phi0 = F) + ggtitle("") +
#   ggplot2::facet_wrap(~header,  labeller = "label_value", ncol = 2) + # scales = "free_x",
#   ggplot2::theme(
#     legend.text = element_text(size = size),
#     legend.title = element_text(size = size),
#     axis.text = element_text(size = size),
#     axis.text.y = element_text(size = size),
#     axis.title = element_text(size = size),
#     strip.text = element_text(size = size)
#   ) + xlab("Feature group") + ylab("Feature group contribution")
# p1
# #
# ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
#   "car-insurance-glm-3-groups-new2.png",
#   plot = p1,
#   device = 'png',
#   path = 'plots',
#   scale = 1,
#   width = 13,
#   height = 9,
#   units = "cm"
# )


















# Conclusion: Not found in our data, but we found PAI-1, which is also mentioned there

all_gene_ids <- sort(as.numeric(unique(full_gene_info$`Gene ID`)))

full_gene_info[grep("PAI",gene)]

all_genes <- names(x_train)

#prostaglandin
#PTGS1
all_genes[grep("PTG",all_genes)]

#PTGDS
all_genes[grep("PTGDS",all_genes)]

#TSC
all_genes[grep("TSC",all_genes)]

all_genes[grep("MST",all_genes)]

all_genes[grep("BAH",all_genes)]

names(gene_groups)

gene_groups$HALLMARK_MYOGENESIS

gene_groups$HALLMARK_P53_PATHWAY

gene_groups$HALLMARK_MYC_TARGETS_V1

gene_groups$HALLMARK_COMPLEMENT

gene_groups$HALLMARK_KRAS_SIGNALING_DN
