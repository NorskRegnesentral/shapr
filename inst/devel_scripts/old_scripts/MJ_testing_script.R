

rm(list=ls())
#save(fmla.,df.,controls.,file = "inst/devel_scripts/MJ_testing_script_data.RData")
load("inst/devel_scripts/old_scripts/MJ_testing_script_data.RData")

library(party)
library(partykit)

minbucket = 7
mincriterion = 0.95

set.seed(123)
start <- proc.time()
datact <- party::ctree(fmla., data = df., controls = party::ctree_control(minbucket = minbucket,
                                                                          mincriterion = mincriterion))
end <- proc.time()
end - start

set.seed(123)
start <- proc.time()
datact_partykit <- partykit::ctree(fmla., data = df., control = partykit::ctree_control(minbucket = minbucket,
                                                                                      mincriterion = mincriterion,
                                                                                      splitstat = "maximum"))
end <- proc.time()
end - start
datact
datact_partykit

aa=predict(datact_partykit,df.,type="prob")
bb=matrix(unlist(treeresponse(datact,newdata=df.)),nrow=1000,byrow=T)

colMeans(abs(aa-bb)) # Identical

aaa=predict(datact_partykit,df.,type="node")

bbb=where(datact,df.)



(irisct_party <- party::ctree(Species ~ .,data = iris))

(irisct_partykit <- partykit::ctree(Species ~ .,data = iris,
                                    control = partykit::ctree_control(splitstat = "maximum")))


controls = controls.
data  = df.
formula = fmla.
subset = NULL
xtrafo = party::ptrafo
ytrafo = party::ptrafo
scores = NULL
weights = NULL


ls <- modeltools:::dpp(party::conditionalTree, formula, data, subset, xtrafo = xtrafo,
          ytrafo = ytrafo, scores = scores)

party:::ctreefit(object = ls, controls = controls, weights = rep(1,1000))

debugSource("inst/devel_scripts/MJ_testing_script_source.R")

#asd2 = 2
#bb()






if (!extends(class(object), "LearningSample"))
  stop(sQuote("object"), " is not of class ", sQuote("LearningSample"))
if (!extends(class(controls), "TreeControl"))
  stop(sQuote("controls"), " is not of class ", sQuote("TreeControl"))
if (is.null(weights))
  weights <- object@weights
storage.mode(weights) <- "double"
if (length(weights) != object@nobs || storage.mode(weights) !=
    "double")
  stop(sQuote("weights"), " are not a double vector of ",
       object@nobs, " elements")
if (max(abs(floor(weights) - weights)) > sqrt(.Machine$double.eps))
  stop(sQuote("weights"), " contains real valued elements; currently\n             only integer values are allowed")
tree <- .Call(R_TreeGrow, object, weights, controls)


party::conditionalTree, ls, controls = controls, weights = weights


