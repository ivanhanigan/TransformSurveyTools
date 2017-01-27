
xtab_by_dimensions <- function(
  dat = dat
  ,
  pid = "cSURVID"
  ,
  mainvar = "cNATDISdroughthappen12moV2" 
  ,
  dimension = "overall"
  ,
  weights = "GReg_Weight_NoEdu_Cut" # or NULL
  ,
  ci_prop = NULL # the name of the col with the proportion to give CI for
  ,
  overall = TRUE
  ,
  extra_exclusions_mainvar = NULL # i.e. -88 N/A along with standard -999
  ,
  extra_exclusions_dimension = NULL # i.e. '' as well as standard -777
                                    # and -999
  ){
invar <- c(pid, dimension, mainvar, weights)
matrix(invar)

# data.frame(table(dat[,dimension]))
if(!is.null(extra_exclusions_dimension)){
  xtra <- paste("& dat[,dimension] != ", extra_exclusions_dimension, sep = "")
} else {
  xtra <- ""
}
txt <- paste("datin <- dat[             
             dat[,dimension] != '-999' &
             dat[,dimension] != '-777'              
             ",xtra,"
             , invar
             ]", sep ="")
#cat(txt)
eval(parse(text=txt))  
#str(datin)
datin[,dimension] <- factor(datin[,dimension])
#data.frame(table(datin[,dimension]))
  

#mainvar
#data.frame(table(datin[,mainvar]))  
#head(datin)
#str(datin)
#summary(datin)
if(!is.null(extra_exclusions_mainvar)){
  xtra <- paste("& datin[,mainvar] != ", extra_exclusions_mainvar, sep = "")
} else {
  xtra <- ""
}
txt <- paste("datin <- datin[             
             datin[,mainvar] != '-999' &
             datin[,mainvar] != '-777'              
             ",xtra,"
             , 
             ]", sep ="")
#cat(txt)
eval(parse(text=txt))  
#str(datin)

datin[,mainvar] <- factor(datin[,mainvar])
#data.frame(table(datin[,mainvar]))

#outcols <-
#summa_out <- matrix(NA, nrow = 0, ncol = outcols)
#head(datin)
#nrow(datin)
# 6558
#head(datin[,1:4])
if(is.null(weights)){
# n
n <- table(datin[,dimension])
#n

txt <- paste("summa <- cbind('",mainvar,"', '",dimension,"', n, t(prop.table(table(datin[,c('",mainvar,"','",dimension,"')]),2)))", sep = '')
#txt
} else {  

# unweighted n
n <- table(datin[,dimension])
#n
#dat_wt <- svydesign(ids = ~Notfarmer1DrylandFarmer2Irrigator3, data
#= datin, weights = datin$GReg_Weight_NoEdu_Cut)
txt1 <- paste("dat_wt <- svydesign(id = ~1, data = datin, weights = datin$",weights,")", sep = "")
#txt1
eval(parse(text=txt1))    
#summary(dat_wt)
#str(dat_wt)

txt <- paste("summa <- cbind('",mainvar,"', '",dimension,"', n, t(prop.table(svytable(~",mainvar,"+",dimension,", design = dat_wt),2)))", sep = '')
}

#txt
#qc <- t(prop.table(svytable(~cNATDISdroughthappen12mo+Notfarmer1DrylandFarmer2Irrigator3, design = dat_wt),2))
#qc
#cbind(qc*100,qc[,1]+qc[,2])
eval(parse(text=txt))
#summa
## summa <- c(mainvar, table(datin[,mainvar])/n, n)
#as.data.frame(summa)
#summa
summa  <- cbind(summa, row.names(summa))
##   str(summa)
#summa_out <- summa

## }
#summa
#head(summa)
# TODO modify for number of cols
nc <- ncol(summa)
#nc
if(nrow(summa) == 1){
  d2  <- sapply(summa[,3:(nc-1)], as.numeric)
  
  d2  <- data.frame(
    matrix(summa[,c(1:2, nc)], nrow = 1),
                    matrix(round(d2, 3), nrow = 1)
                    )
  names(d2)[4:(nc)]  <- names(summa[,3:(nc-1)])  
#d2
} else {
  d2  <- apply(summa[,3:(nc-1)], 2, as.numeric)
  d2  <- data.frame(summa[,c(1:2, nc)], round(d2, 3))
}
#head(d2)

#names(d2)  <- c("variable", "dimension", "No", "Yes", "n")
#d2

#d2  <-  cbind(summa[,nc], d2)
names(d2)  <- c("variable", "dimension", "state", names(d2)[4:(nc)])
row.names(d2) <- NULL
## d3  <- data.frame(variable = d2$variable, Disagree = rowSums(d2[,c(2:4)]),
##                   Neither_agree_or_disagree = d2[,5],
##                   Agree = rowSums(d2[,6:8])
##                   )
## d3
## d2 <- merge(d2, d3)  
#d2 <- merge(mainvars_df, d2, by.x = "var", by.y = "variable")

## d2  <- sqldf("select *
## from d2
## where Yes > 0
## order by var, state")
## d2  
## #names(d2) <- gsub("Neither_agree_or_disagree", "Neither agree/disagree", names(d2))
## str(d2)
## paste(names(d2), sep = "", collapse = "', '")
## d2 <- d2[,c('var', 'state', 'dimension', 'sortorder', 'Item stem - exact wording seen by respondents', 'Item - exact wording seen by respondent OR description of variable', 'No', 'Yes', 'n', 'Piping or display logic')]
## d2[,2] <- paste(d2[,2], " (n=", d2$n, ")", sep = "")
if(!is.null(ci_prop)){
# adjusted wald from http://www.measuringux.com/adjustedwald.htm
z <- 2
# rather than 1.96
padj <- (d2$n * d2[,ci_prop] + (z^2)/2)/(d2$n + z^2)

nadj <- d2$n + z^2

# And finally, the calculation of the confidence interval:

d2$z_se  <- z * sqrt(padj*(1 - padj)/nadj)  
d2$lci  <- d2[,ci_prop] - (z * sqrt(padj*(1 - padj)/nadj))
d2$uci  <- d2[,ci_prop] + (z * sqrt(padj*(1 - padj)/nadj))
# NB we will use this +/- on the proportion (not the padj as recommended, because the asymmetry 
# might look odd to our audience)
}
return(d2)
}
