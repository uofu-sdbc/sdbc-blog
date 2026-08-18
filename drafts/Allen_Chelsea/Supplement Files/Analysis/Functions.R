
library("coin");library(Hmisc);library(rtf);library(clinfun)
library(MASS);library(statmod);library(dplyr);
library(nlme);library(lmerTest); library(readxl);
library(lubridate);library(rmdHelpers) #printList

############### Import, review, and recode data ################

sort.size <- function(xx) {
  factor(xx,levels=names(sort(table(xx),decreasing=T)))
}

self_name <- function(xx) {
  names(xx) <- xx
  return(xx)
}

read_excel_allsheets <- function(filename, ...) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, ...))
  x <- lapply(x, as.data.frame, stringsAsFactors=T)
  names(x) <- sheets
  x
}

calc_age <- function(birthDate, refDate = Sys.Date()) {
  period <- as.period(interval(birthDate, refDate),unit = "year")  ### unit="day"
  period$year+period$month/12+period$day/365.25
}

review <- function(df) {
  # reviews a dataframe and PRINTS:
  # tallies logicals and  most factors (unless > 20 levels, then table of # of occurances)
  # gives range and amount missing for numeric and date data
  for (i in 1:dim(df)[2]) {
    print(c(names(df)[i],class(df[,i])))
    if(sum(c("factor","character","logical") %in% class(df[,i]))) {
      if(length(table(df[,i])) <= 20) print(table(df[,i],useNA="ifany"))
      else {
        print("Too Many Levels. Table of occurences:")
        print(table(table(df[,i])))
        print(paste("Missing =",sum(is.na(df[,i]))))
      }
    }
    else   #numbers and dates
      print(summary(df[,i]))
    print("",quote=F)
  }
}


# assign to new variable (comes out as character vector if previously a factor)
# x = vector to have replacements
# prev = previous entries to be replaced
# new = new entries to replace previous (same order and length)
replace_all <- function(x,prev,new){  # can this be done with case_when?
  if("factor" %in% class(x)) x <- as.character(x)
  if(length(prev) != length(new)) stop("lengths of prev and new must be the same")
  if(sum(is.na(prev))>0){
    na.ind <- which(is.na(prev))
    for(i in 1:length(prev)){
      if(i %in% na.ind) x <- replace(x,which(is.na(x)),new[i])
      else x <- replace(x,which(x==prev[i]),new[i])
    }
  }
  else for(i in 1:length(prev))
    x <- replace(x,which(x==prev[i]),new[i])
  x
}


sub_all <- function(pattern, replacement, x,...) {
  if("factor" %in% class(x)) x <- as.character(x)
  if(length(pattern) != length(replacement)) stop("lengths of pattern and replacement must be the same")
  for(i in 1:length(pattern))
    x <- gsub(pattern[i],replacement[i],x,...)
  x
}


expand_range <- function(xx,mult=0.1) {
  temp <- range(xx,na.rm=T)
  temp + c(-1,1)*mult*diff(temp)
}


mytab.long <- function(tab,ind="  ") {
  if(!class(tab) %in% c("data.frame","matrix")) stop("tab must be a data frame or matrix")
  var.row <- c(which(tab[,1]!=""),nrow(tab)+1)
  newtab <- data.frame(tab[1,-1],stringsAsFactors=F,check.names=F)
  newtab <- as.data.frame(do.call(cbind,lapply(newtab, function(x) if(is.factor(x)) as.character(x) else x)),stringsAsFactors=F)
  tab <- as.data.frame(do.call(cbind,lapply(tab, function(x) if(is.factor(x)) as.character(x) else x)),stringsAsFactors=F)
  tab[,2] <- paste0(ind,tab[,2])
  for(i in 1:(length(var.row)-1)) {
    if(tab[var.row[i],2]==ind) newtab <- rbind(newtab,as.character(tab[var.row[i],-2]))
    else {
      newtab <- rbind(newtab,c(tab[var.row[i],1],rep("",ncol(tab)-2)))
      newtab <- rbind(newtab,tab[var.row[i]:(var.row[i+1]-1),-1])
    }}
  names(newtab)[1] <- names(tab)[1]
  return(data.frame(newtab[-1,],stringsAsFactors=F,check.names=F))
}


uneven.cbind <- function(xx,yy) {
  ## needs to be updated to use data frames without specific column names
  nx <- nrow(xx)
  ny <- nrow(yy)
  if(all(xx$Variable==yy$Variable[1:nx],xx$Level==yy$Level[1:nx])) {
    output <- data.frame(cbind(yy[,1:2],rbind(as.matrix(xx[,-(1:2)]),matrix(NA,nrow=ny-nx,ncol=ncol(xx)-2)),yy[,-(1:2)]),stringsAsFactors=F,check.names=F)
    names(output) <- c(names(yy)[1:2],names(xx)[-(1:2)],names(yy)[-(1:2)])
    return(output)
  } else print("Variables do not match")
}



myForm <- function(x,digitsx=2,lim=NULL) {
  if(is.null(lim)) lim <- 10^(c(-digitsx,digitsx+2))
  # Number 'nice' formatting to be used in regression table functions
  trimws(case_when(abs(x) >= min(lim) & abs(x) <= max(lim) ~ formatC(round(x,digitsx),format="f",digits=digitsx), 
                   x==0 ~ "0", abs(x) < min(lim) | abs(x) > max(lim)~ formatC(x,format="e",digits=digitsx)))
}

myperc <- function(perc,digitsx=0) {
  lim <- c(1*10^(-digitsx),100-1*10^(-digitsx))
  f <- function(xx) 
    case_when(xx==0 | xx==100 ~ as.character(xx),
              xx<lim[1] ~ paste0("<",lim[1]), xx>lim[2] ~ paste0(">",lim[2]),
              T ~ myForm(xx,digitsx=digitsx,lim=c(0,Inf)))
  out <- NULL
  if(is.null(dim(perc))) {
    out <- f(perc)
  } else if(length(dim(perc))==2){ 
    for(i in 1:dim(perc)[2]) 
      out <- cbind(out,f(perc[,i]))
  }
  return(out)
  # print(perc)
  # return(round(perc))
}

ratio.print <- function(EstCI,p,type="OR",conf.level=0.95) {
  ratio.c <- sub(" \\(.*$","",EstCI)
  ratio <- as.numeric(ratio.c)
  HL <- ifelse(ratio>1,paste0(100*(ratio-1),"% increase"),paste0(100*(1-ratio),"% decrease"))
  CI <- sub_all(c("^.*\\(","\\)"),c("",""),EstCI)
  p <- ifelse(p %in% c(">0.99","<0.001"),paste0("p",p), paste0("p=",p))
  paste0(HL," (",type,": ",ratio.c,"; ",conf.level*100,"% CI: ",CI,"; ",p,")")
}

diff.print <- function(EstCI,p,conf.level=0.95) {
  coeff.c <- sub(" \\(.*$","",EstCI)
  coeff <- as.numeric(coeff.c)
  HL <- ifelse(coeff<0,paste("decrease of",sub("^-","",coeff.c)),paste("increase of",coeff.c))
  CI <- sub_all(c("^.*\\(","\\)"),c("",""),EstCI)
  p <- ifelse(p %in% c(">0.99","<0.001"),paste0("p",p), paste0("p=",p))
  paste0(HL, " (",conf.level*100,"% CI: ",CI,"; ",p,")")
}

##### Table functions #####

#Function to format any p-value (used here and in table functions)
fpvalue = function(xpval){
  ifelse(xpval <= 0.1,ifelse(xpval < 0.001,"<0.001",paste0(" ",myForm(xpval,digitsx=3,lim=0:1))),
         ifelse(xpval > 0.99,">0.99",paste0(" ",myForm(xpval,digits=2))))
}


#Capitalize first words.  Adapted from function at https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    if(is.na(s[1])) return(NA)
    else return(paste(toupper(substring(s, 1, 1)),
                      {s <- substring(s, 2); if(strict) tolower(s) else s},
                      sep = "", collapse = " " ))
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}



############### Chong's Table Functions (edited) ###############
# These functions were created by Chong Zhang
# Some changes were made by Chelsea Allen to change some formatting and add options.


#CONTENTS#########################################################################################
#This file contains code to create the following types of descriptive tables:
#myNOgrtab(): Function to print out a pretty table with No grouping variable. 
#my2grtab() = Function to print out a pretty table with a two-level grouping variable. 
#myMultiGrtab() = Function to print out a pretty table with a >2 level grouping variable. 
#
#Following these three main functions there are several supporting functions.  In particular:
#fisherChi(): Function that determines whether fisher's or chi-square test is appropriate. 
#armitageSim(): Function that determines whether a Cochran-armitage trend test or a Monte Carlo simulation 
#version of it is appropriate.
#fpvalue() : Function that formats p-values.
##################################################################################################
##################################################################################################
#myNOgrtab(): Function to print out a pretty table with No grouping variable. 
#xdat = the data frame, 
#dnames = an optional vector of variable labels, otherwise original variable names are used.
#typev = indicates the type of variable: 0=parametric, 1=nonparametric,2=categorical, 3=Categorical with two levels but cannot be treated as binary (usually you don't need this). 
#annot = T/F include annotation "- mean (SD)".
#allSums = T, for continuous vars, summarize: median (IQR), mean (SD), range.  
#          If F, summarize mean(SD) for parametric and median (IQR) for nonparametric. 
#oneLineBinary=T, for categorical variables with only two categories, print count (%) 
#              for only TRUE/1/or second level of variable, but also add name of level 
#              using "- T" or "-1".  If F, print both levels.
#footMissing=T, print missing values as a footnote below the table.  If F, print missing 
#            values as an additional column in the table. 
#output = "table1.doc", print table to a .rtf file.  If NULL don't print to a file.
#myNOgrtab(xdat,dnames,typev) #typev: 0=parametric, 1=nonparametric,2=categorical, 3=2-level categorical force both
##################################################################################################
myNOgrtab = function(xdat,dnames=NULL,typev=NULL,annot=T, allSums=F,onelinebinary=T,showbinarylevel=T, footMissing=T, output=NULL, digitsx=1, digitsp=0)
{
  xdat <- as.data.frame(xdat)
  if(length(typev) != dim(xdat)[2]) {
    print("Warning: Dimensions of data frame and typev don't match. Types assumed based on classes.")
    classes <- sapply(xdat,function(xx) tail(class(xx),1))
    typev <- case_when(
      classes %in% c("ordered","factor","logical","character") ~ 2,
      classes %in% c("integer","numeric") ~ 0
    )
    if(sum(is.na(typev))>0) stop("Unknown class type encountered. Must give types.")
  }
  if(is.null(dnames)|(length(dnames) != dim(xdat)[2])){ 
    print("Warning: Data set names not provided or do not match length of data frame.  Original variables names will be used.")
    dnames = names(xdat)
  }#end if
  if(!is.null(output) && !endsWith(output,".doc")){
    if (endsWith(output,".docx")) output = substring(output,1,nchar(output)-1)
    else output = paste0(output,".doc")
  }
  dnames0=names(xdat) #still need column names of xdat.
  tabx=NULL
  allmiss=NULL
  
  ### add histograms
  if (sum(typev<2)>0) {
    dnum=as.data.frame(xdat[,typev<2]) # subset with only numeric variables
    names(dnum)=names(xdat)[typev<2]
    nv=dim(dnum)[2] # number of continuous variables
    np=ceiling(nv/4) # number of pages/figures
    rtf<-RTF("histograms.doc",width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
    addNewLine(rtf,"Plots for Descriptive Summary Table")
    for  (i in 1:np){
      a=(i-1)*4+1
      b=a+3
      if (b>nv) {b=nv}
      vgroup=as.data.frame(dnum[,a:b])
      names(vgroup)=names(dnum)[a:b]
      addPlot(rtf,plot.fun=hist4,width=6,height=6,res=300, vgroup)
      ta4=sum4var(vgroup)
      addTable(rtf,ta4,font.size=9,row.names=FALSE,NA.string="-")}
    done(rtf)} #end add histograms
  
  #create rows
  for(k in 1:length(dnames0)){
    cv=xdat[,k]
    # For continuous variables
    if(allSums==F & typev[k]<2){ # do not summarize all statistics
      if(typev[k]==0){ #parametric
        if(annot) cdnames = c(dnames[k],"[mean (SD)]")
        else cdnames = c(dnames[k], "")   #end if annot
        crow = c(cdnames,paste0(myForm(mymean(cv),digitsx)," (",myForm(mysd(cv),digitsx),")"),sum(is.na(cv)) )
      }
      else if(typev[k]==1){ #non-parametric
        qcv1 = as.numeric(myForm(quantile(cv,c(0.5,0.25,0.75),na.rm=T),digitsx))
        if(annot) cdnames = c(dnames[k],"[median (IQR)]")
        else cdnames = c(dnames[k], "")   #end if annot
        crow = c(cdnames,paste0(qcv1[1]," (",qcv1[2],", ",qcv1[3],")"),sum(is.na(cv)))
      }}
    else if(allSums==TRUE & typev[k]<2) { #provide all numeric summaries regardless of non-parametric or parametric
      crow1 = c(dnames[k],"[mean (SD)]",paste0(myForm(mymean(cv),digitsx)," (",myForm(mysd(cv),digitsx),")"),sum(is.na(cv)))
      qcv1 = as.numeric(myForm(quantile(cv,c(0.5,0.25,0.75),na.rm=T),digitsx))
      crow2 = c("", "[median (IQR)]",paste0(qcv1[1]," (",qcv1[2],", ",qcv1[3],")"),"")
      crow3= c("","[range]", paste0("(", myForm(mymin(cv),digitsx), ", ", myForm(mymax(cv),digitsx),")"), "")
      crow=rbind(crow1, crow2, crow3)
    }
    else if(typev[k]>=2){ #categorical (use "3" for forced non-binary)
      t1=table(cv)
      t1n = names(t1)
      if(length(t1)==2 & typev[k]==2 & onelinebinary){ #For 2-level variables, summarize level=1 or TRUE, or if these don't exist, summarize 2nd level.
        usev=mymin(c(length(t1),match("1",t1n),match("TRUE",t1n)))
        ccv1 = paste0(t1[usev]," (",myperc(t1[usev]/sum(t1)*100),"%)")
        if (showbinarylevel){crow = c(dnames[k],t1n[usev], ccv1,sum(is.na(cv)))} 
        else crow = c(dnames[k], "", ccv1 ,sum(is.na(cv)))
      }
      else if(length(t1)>=2){ #For multi-level, summarize all
        crow = NULL
        for(j in 1:dim(t1)[1]){
          ccv1 = paste0(t1[j]," (",myperc(t1[j]/sum(t1)*100),"%)")
          if(j==1){ 
            cname = c(dnames[k],t1n[1])
            miss=sum(is.na(cv))
          }
          else{cname = c("",t1n[j])
          miss=""
          }#end if
          crow= rbind(crow,c(cname,ccv1,miss))
        }#end for
      } 
      else { #For results with only one value:
        if(t1n=="0"|t1n==FALSE)  crow = c(dnames[k],"","0 (0%)",sum(is.na(cv))  )
        else if(t1n=="1"|t1n==TRUE)  crow = c(dnames[k],"",paste(t1,"(100%)"),sum(is.na(cv)) )
        else   crow = c(dnames[k],names(t1),paste(t1,"(100%)"),sum(is.na(cv)) )
      }#end if
    }else crow=NULL
    # end if
    tabx=rbind(tabx,crow)
    
    ## get missing values:
    if (footMissing==T){
      if(!is.null(crow)&sum(is.na(cv))>0){
        allmiss = c(allmiss,paste0(" ",dnames[k]," = ",sum(is.na(cv))))
      }}#end all miss
  }#end for
  #print("Loop completed")
  tabx = data.frame(tabx,stringsAsFactors=F)
  v1n = paste0("N=",dim(xdat)[1])
  names(tabx) = c(v1n,"Type/Level","Summary","# Missing")
  if (footMissing==T) {
    if (!is.null(output)){
      rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
      # table 1
      addParagraph(rtf,"Table: Descriptive Summary. \n ")
      addTable(rtf,tabx[,1:3],font.size=9,row.names=FALSE,col.justify=c("L","L","L"),header.col.justify=c("L","L","L"),NA.string="")
      addNewLine(rtf)
      if(is.null(allmiss)) addParagraph(rtf,"No Missing Values")  
      else addParagraph(rtf,paste0("Missing values:",paste(allmiss,collapse=",")))
      done(rtf)}
    return(list(tabx[,1:3], paste0("Missing values:",paste(allmiss,collapse=",")) ))
  }
  else if (footMissing==F) {
    if (!is.null(output)){
      rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
      addParagraph(rtf,"Table: Descriptive Summary. \n ")
      addTable(rtf,tabx,font.size=9,row.names=FALSE,col.justify=c("L","L","L","C"),header.col.justify=c("L","L","L","C"),NA.string="")
      addNewLine(rtf)
      done(rtf)}
    return(tabx)
  }
}
#end myNOgrtab





#######################
#my2grtab() = Function to print out a pretty table with a two-level grouping variable. 
#xdat is the data frame, 
#gvar is a TRUE/FALSE group variable,
#dnames is an optional vector of data set names, otherwise original names are used.
#typev indicates the type of variable: #0=parametric, 1=nonparametric,2=categorical
#gnames = optional vector of group names (TRUE comes first).
#annot = T/F whether to add "- Mean (SD)" and "- Median (IQR)" to names.
#For 2-level variables, summarize level=1/TRUE/Yes, or if these don't exist, summarize 2nd level.
my2grtab = function(xdat,gvar,dnames=NULL,typev=NULL,gnames=NULL,annot=T, showbinarylevel=T, onelinebinary=T,
                    output=NULL, allSums=F, testname=F, footMissing=T, sumCol=T, printpvalue=T, digitsx=1,digitsp=0){
  #typev: 0=parametric, 1=nonparametric,2=categorical
  xdat <- as.data.frame(xdat)
  if(length(typev) != dim(xdat)[2]) {
    print("Warning: Dimensions of data frame and typev don't match. Types assumed based on classes.")
    classes <- sapply(xdat,function(xx) tail(class(xx),1))
    typev <- case_when(
      classes %in% c("ordered","factor","logical","character") ~ 2,
      classes %in% c("integer","numeric") ~ 0
    )
    if(sum(is.na(typev))>0) stop("Unknown class type encountered. Must give types.")
  }
  if(is.null(dnames)|(length(dnames) != dim(xdat)[2])){ 
    print("Warning: Data set names not provided or do not match length of data frame.  Original variables names will be used.")
    dnames = names(xdat)
  }#end if
  if(!is.null(output) && !endsWith(output,".doc")){
    if (endsWith(output,".docx")) output = substring(output,1,nchar(output)-1)
    else output = paste0(output,".doc")
  }
  
  #Remove values for which gvar is missing.
  xdat = xdat[!is.na(gvar),]
  gvar = gvar[!is.na(gvar)]
  testnote=NULL
  if (testname==T) {pos=2}  # position of fisherChi object.
  else if (testname==F) {pos=3; testnote="1=t.test, 2=Wilcox, 3=Chi-square, 4=Fisher's exact, 5=Fisher's exact test with simulation, 6=Exact wilcoxon rank sum test" }
  
  if(length(unique(gvar)) != 2) stop("gvar must have exactly 2 levels (besides NA)")
  if(any(is.na(as.logical(gvar)))) {
    if(is.null(gnames)) gnames <- sort(unique(gvar),decreasing=T)
    gvar <- gvar==sort(unique(gvar))[2]
  }
  
  dnames0=names(xdat) #still need column names of xdat.
  tabx=NULL;allmiss=NULL;
  for(k in 1:length(dnames0)){
    cv=xdat[,dnames0[k]]
    cv1=cv[!(gvar)]
    cv2=cv[gvar==1]
    
    
    ### Add histgrams
    if (sum(typev<2)>0) {
      dnum=as.data.frame(xdat[,typev<2]); # subset with only numeric variables
      names(dnum)=names(xdat)[typev<2];
      nv=dim(dnum)[2]; # number of continuous variables
      np=ceiling(nv/4); # number of pages/figures
      rtf<-RTF("histograms.doc",width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
      for  (i in 1:np)
      {a=(i-1)*4+1;b=a+3;if (b>nv) {b=nv}
      #if (a!=b){
      vgroup=as.data.frame(dnum[,a:b])
      names(vgroup)=names(dnum)[a:b]
      addPlot(rtf,plot.fun=hist4,width=6,height=6,res=300, vgroup)
      ta4=sum4var(vgroup)
      addTable(rtf,ta4,font.size=9,row.names=FALSE,NA.string="-")}
      #else {addPlot(rtf,plot.fun=hist1,width=6,height=6,res=300, dnum[,nv])}
      done(rtf)}
    
    # For continuous variable:
    if(typev[k]<2){
      # calculate all descriptive statistics
      msd1=paste0(myForm(mymean(cv1),digitsx)," (",myForm(mysd(cv1),digitsx),")")
      msd2=paste0(myForm(mymean(cv2),digitsx)," (",myForm(mysd(cv2),digitsx),")")
      qcv1 = as.numeric(myForm(quantile(cv1,c(0.5,0.25,0.75),na.rm=T),digitsx))
      qcv2 = as.numeric(myForm(quantile(cv2,c(0.5,0.25,0.75),na.rm=T),digitsx))
      miqr1= paste(qcv1[1]," (",qcv1[2],", ",qcv1[3],")",sep="")
      miqr2= paste(qcv2[1]," (",qcv2[2],", ",qcv2[3],")",sep="")
      rcv1=paste0("(", myForm(mymin(cv1),digitsx), ", ", myForm(mymax(cv1),digitsx),")")
      rcv2=paste0("(", myForm(mymin(cv2),digitsx), ", ", myForm(mymax(cv2),digitsx),")")
      row3 =c("","[range]", rcv2, rcv1, NA, NA, NA)
      if(typev[k]==0){ #parametric
        if (printpvalue==T) {uap=fpvalue(t.test(eval(parse(text=dnames0[k]))~gvar,data=xdat)$p.value)
        if (testname==T) {test="T.test"} 
        else {test=1} }
        else {uap=NA ; test=NA }
        num1=msd1; num2=msd2;statname="[mean (SD)]";
        row2 =c("","[median (IQR)]",miqr2,miqr1,NA, NA, NA) } #end parametric
      else if(typev[k]==1){ #non-parametric
        if (printpvalue==T) {
          
          if (sum(!is.na(cv1))>=50 & sum(!is.na(cv2))>=50 )
          {uap=fpvalue(wilcox.test(eval(parse(text=dnames0[k]))~gvar,data=xdat)$p.value)
          if (testname==T) {test="Wilcox"}
          else {test=2}}
          else { uap=fpvalue(pvalue(wilcox_test(cv~as.factor(gvar), distribution="exact")))
          if (testname==T) {test="Exact Wilcox"}
          else {test=6} }
        }
        else {uap=NA ; test=NA }
        num1=miqr1; num2=miqr2;statname="[median (IQR)]";
        row2= c("","[mean (SD)]", msd2, msd1, NA, NA, NA)
      }#end non-parametric
      if (allSums==F) {
        if(annot){
          cdnames = c(dnames[k],statname)
        }else{
          cdnames = c(dnames[k],"")
        } # end annot
        crow = c(cdnames,num2,num1, uap, test, sum(is.na(cv))) } 
      else {
        row1=c(dnames[k],statname ,num2,num1, uap, test,sum(is.na(cv)) )
        if (typev[k]==0){crow=rbind(row1, row2, row3)}
        else if (typev[k]==1) {crow=rbind(row2,row1,row3)
        crow[1,1:2]=c(dnames[k],"[mean (SD)]")
        crow[2,1:2]=c("", "[median (IQR)]")
        }
      }
    }# end continuous variable
    else{ #categorical
      if (sumCol!=T) {
        t1=table(cv,gvar)
        if (printpvalue==T){
          uap=fpvalue(fisherChi(t1)[[1]])
          test=fisherChi(t1)[[pos]]}
        else { uap=NA; test=NA  }
        allc = NULL
        for(j in 1:dim(t1)[1]){
          ccv1 = paste(t1[j,1]," (",myperc(t1[j,1]/sum(t1[j,])*100),"%)",sep="")
          ccv2 = paste(t1[j,2]," (",myperc(t1[j,2]/sum(t1[j,])*100),"%)",sep="")
          if (j==1) {crow = c(dnames[k],row.names(t1)[j], ccv2,ccv1,uap,test, sum(is.na(cv)))}
          else {crow = c("",row.names(t1)[j], ccv2,ccv1,NA,NA,NA)}
          allc=rbind(allc, crow)}
        crow=allc
      }
      else {
        t1=table(cv,gvar)
        if(dim(t1)[1]==2){
          if (printpvalue==T){
            uap=fpvalue(fisherChi(t1)[[1]])
            test=fisherChi(t1)[[pos]]}
          else { uap=NA; test=NA}
          if (onelinebinary==T){ #For 2-level variables, summarize level=1 or TRUE, or if these don't exist, summarize 2nd level.
            usev=row.names(t1) %in% c("1","TRUE","T","true","YES","Yes","yes")
            if(sum(usev)==0) usev=2
            ccv1 = paste(t1[usev,1]," (",myperc(t1[usev,1]/sum(t1[,1])*100),"%)",sep="")
            ccv2 = paste(t1[usev,2]," (",myperc(t1[usev,2]/sum(t1[,2])*100),"%)",sep="")
            if (showbinarylevel==T){
              crow = c(dnames[k],row.names(t1)[usev], ccv2,ccv1,uap,test,sum(is.na(cv)) )}
            else if (showbinarylevel==F){crow = c( dnames[k],"", ccv2,ccv1,uap, test, sum(is.na(cv)))} #re-order TRUE, FALSE
          }
          else if (onelinebinary==F) {
            allc = NULL
            for(j in 1:dim(t1)[1]){
              ccv1 = paste(t1[j,1]," (",myForm(t1[j,1]/sum(t1[,1])*100,digitsp),"%)",sep="")
              ccv2 = paste(t1[j,2]," (",myForm(t1[j,2]/sum(t1[,2])*100,digitsp),"%)",sep="")
              if (j==1) {crow = c(dnames[k],row.names(t1)[j], ccv2,ccv1,uap,test, sum(is.na(cv)))}
              else {crow = c("",row.names(t1)[j], ccv2,ccv1,NA,NA,NA)}
              allc=rbind(allc, crow)}
            crow=allc}
        }
        else if(dim(t1)[1]>2){ #For multi-level, summarize all
          #print("Begin multi-level analysis:")
          t1n = row.names(t1)
          fqp = apply(t1,2,sum)[2];fqn = apply(t1,2,sum)[1]
          allc = NULL
          for(j in 1:dim(t1)[1]){
            ccv1 = paste(t1[j,1]," (",myForm(t1[j,1]/fqn*100,digitsp),"%)",sep="")
            ccv2 = paste(t1[j,2]," (",myForm(t1[j,2]/fqp*100,digitsp),"%)",sep="")
            if(j==1){ 
              cname = c(dnames[k],t1n[1])
            }else{
              cname = c("",t1n[j])
            }#end if
            #print(c(cname,ccv2,ccv1))
            allc = rbind(allc,c(cname,ccv2,ccv1))
          }#end for
          if (printpvalue==T){
            uap=c(fpvalue(fisherChi(t1)[[1]]),rep(NA,dim(t1)[1]-1))
            test=c(fisherChi(t1)[[pos]], rep(NA, dim(t1)[1]-1))}
          else { uap=NA; test=NA }
          nmiss=c(sum(is.na(cv)), rep(NA, dim(t1)[1]-1))
          crow=cbind(allc,uap, test, nmiss)
        }else crow = c(dnames[k],row.names(t1)[1],paste(t1[1,2:1],"(100%)"),rep("-",2),sum(is.na(cv)))
        #end if
      } #end sumCol=T
    }# end categorical 
    #print("###########");print(crow);print(tabx);
    tabx=rbind(tabx,crow)
    #print(tabx);
    if (footMissing==T){
      if(!is.null(crow)&sum(is.na(cv))>0){
        allmiss = c(allmiss,paste0(" ",dnames[k]," = ",sum(is.na(cv))))
      }}#end all miss
  }#end for
  #print("Loop completed")
  tabx = data.frame(tabx,stringsAsFactors=F)
  missnote <- ifelse(footMissing,ifelse(is.null(allmiss[1]),"No Missing Values",paste("Missing values:",paste(allmiss,collapse=", "))),"")
  if(is.null(gnames)){
    names(tabx) <- c("Variable","Type/Level",paste("TRUE (N=",sum(gvar,na.rm=T),")",sep=""),paste("FALSE (N=",sum(!(gvar),na.rm=T),")",sep=""),"P-value", "Test", "#Missing")
  }else{
    names(tabx) <- c("Variable","Type/Level",paste(gnames[1], " (N=",sum(gvar,na.rm=T),")",sep=""),paste(gnames[2], " (N=",sum(!(gvar),na.rm=T),")",sep=""),"P-value", "Test", "#Missing")
  }#end if
  
  ind <- as.vector(na.omit(c(1:4,eval(parse(text=ifelse(printpvalue,"5:6",NA))),ifelse(footMissing,NA,7))))
  just <- c(rep("L",4),"R","C","C")[ind]
  tabx1 <- tabx[,ind]
  if (!is.null(output)){
    rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
    addParagraph(rtf,"Table. Summary by group. \n ")
    addTable(rtf,tabx1,font.size=9,row.names=FALSE,col.justify=just,header.col.justify=just,NA.string="")
    if(footMissing) {
      addNewLine(rtf)
      addParagraph(rtf,missnote) 
    }
    addParagraph(rtf, testnote)
    done(rtf)}
  return(list(tabx1,missnote, testnote))
}#end my2grtab

##############################
# myMultiGrtab() = Function to print out a pretty table with a >2 level grouping variable. 
# dnames is an optional vector of data set names, otherwise original names are used.
# typev indicates the type of variable: #0=parametric, 1=nonparametric,2=categorical,3=categorical (override onelinebinary for multiple rows)
# annot = T/F whether to add "- Mean (SD)" and "- Median (IQR)" to names.
# showbinarylevel=T/F whether to print out the level name for binary variables 
# onelinebinary=T/F: If T, just display summary for one of the two levels, i.e level=1/TRUE/Yes, or if these don't exist, summarize 2nd level.. This is ignored when sumCol=F.
# sumCol=T/F, if T, display col percent, if F, display row percent
# footMissing=T/F: if T, display # missings as a footnote, if F, list #missing as a column.
# allSums=T: produce mean(SD), median (IQR) and Range for numeric variables, if allSums=F, produce only mean(SD) if parametric, or median (ISR) if nonparametric.
# printpvalue=T, perform a test, printpvalue=F: do not perform a test.
# testname=T : list the name of the test as a column, if testname=F: list the number instead of the test (i,e, 1=ANOVA, 
# 2=Kruskal Wallis, 3=Chi-square, 4=Fisher, 5=Simulated.Chi-square, 6=J.K perm, 7=ANOVA trend, 8=Cochran-Armitage, 9=Cochran-Armitage.Sim) 
# output: the file name (eg. "table2.doc") to export your table.
# orderdGro=T/F, if T: perform ANOVA test for linear trend if variable is type 0, Jonckheere-Terpstra test with permutation if variable is type 1, Cochran-Armitage (C-A) test 
# for trend if variable is binary; if expected cell count is less than 5, use a Monte Carlo simulation version of Cochran-Armitage test for trend. 

myMultiGrtab= function(xdat,gvar,dnames=NULL,typev=NULL,annot=T, showbinarylevel=T, orderedGro=F,onelinebinary=T,footMissing=T,
                       allSums=F, testname=F, sumCol=T, printpvalue=T, digitsx=1, digitsp=0, output=NULL){
  
  xdat <- as.data.frame(xdat)
  if(length(typev) != dim(xdat)[2]) {
    print("Warning: Dimensions of data frame and typev don't match. Types assumed based on classes.")
    classes <- sapply(xdat,function(xx) tail(class(xx),1))
    typev <- case_when(
      classes %in% c("ordered","factor","logical","character") ~ 2,
      classes %in% c("integer","numeric") ~ 0
    )
    if(sum(is.na(typev))>0) stop("Unknown class type encountered. Must give types.")
  }
  if(is.null(dnames)|(length(dnames) != dim(xdat)[2])){ 
    print("Warning: Data set names not provided or do not match length of data frame.  Original variables names will be used.")
    dnames = names(xdat)
  }#end if
  if(!is.null(output) && !endsWith(output,".doc")){
    if (endsWith(output,".docx")) output = substring(output,1,nchar(output)-1)
    else output = paste0(output,".doc")
  }
  
  #Remove values for which gvar is missing.
  xdat = xdat[!is.na(gvar),]
  gvar = gvar[!is.na(gvar)]
  
  n.grp <- dim(table(gvar))
  
  ### Add histgrams
  if (sum(typev<2)>0) {
    dnum=as.data.frame(xdat[,typev<2]); # subset with only numeric variables
    names(dnum)=names(xdat)[typev<2];
    nv=dim(dnum)[2]; # number of continuous variables
    np=ceiling(nv/4); # number of pages/figures
    rtf<-RTF("histograms.doc",width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
    for  (i in 1:np)
    {a=(i-1)*4+1;b=a+3;if (b>nv) {b=nv}
    #if (a!=b){
    vgroup=as.data.frame(dnum[,a:b])
    names(vgroup)=names(dnum)[a:b]
    addPlot(rtf,plot.fun=hist4,width=6,height=6,res=300, vgroup)
    ta4=sum4var(vgroup)
    addTable(rtf,ta4,font.size=9,row.names=FALSE,NA.string="-")}
    #else {addPlot(rtf,plot.fun=hist1,width=6,height=6,res=300, dnum[,nv])}
    done(rtf)}
  
  if(testname==T){
    pos=2 ; testnote=" " # position of fisherChi object.
  }else if(testname==F){
    pos=3; testnote="1=ANOVA, 2=Kruskal-Wallis, 3=Chi-square, 4=Fisher, 5=Sim.Chisq, 6=Jonckheere-Terpstra, 7=ANOVA.L.trend, 8=Cochran-Armitage, 9=Cochran-Armitage.Sim" 
  } #end if
  
  dnames0=names(xdat) #still need column names of xdat.
  tabx=NULL;allmiss=NULL;
  
  for(k in 1:length(dnames0)){#typev: 0=parametric, 1=nonparametric,2=categorical
    cv=xdat[,dnames0[k]]
    #print(dnames0[k]);print(k);
    # For continuous variable:
    if(typev[k]<2){
      # calculate all descriptive statistics
      nsum1=tapply(cv, gvar, msd)
      nsum2=tapply(cv, gvar, miqr)
      nsum3=tapply(cv, gvar, mrange)
      if (allSums==T) { 
        row=rbind(c(dnames[k],"[mean (SD)]",nsum1),c("","[median (IQR)]",nsum2), c("","[range]",nsum3))
        if (footMissing==F) { row=cbind(row, c(sum(is.na(cv)),rep(NA, nrow(row)-1)) )}
      }
      if (allSums==F) { 
        if(annot){ if (typev[k]==0) {
          row=cbind(dnames[k],"[mean (SD)]",t(nsum1))
          if (footMissing==F) { row=cbind(row, t(sum(is.na(cv))))}
        }
          if (typev[k]==1 ){row=cbind(dnames[k],"[median (IQR)]",t(nsum2))
          if (footMissing==F) { row=cbind(row, t(sum(is.na(cv))))}
          }}
        if (annot==FALSE) { if (typev[k]==0) {
          row=cbind(dnames[k],"",t(nsum1))
          if (footMissing==F) { row=cbind(row, t(sum(is.na(cv))))}
        }
          if (typev[k]==1 ){
            row=cbind(dnames[k],"",t(nsum2))
            if (footMissing==F) { row=cbind(row, t(sum(is.na(cv))))}
          }
        }}
      if (printpvalue==T) { # if requiring test
        if (typev[k]==0){ # if parametric
          if (orderedGro==T) {   # if groups are ordered
            gvar=as.factor(gvar)
            contrasts(gvar)=contr.poly(levels(gvar))
            hh=lm(cv~gvar)
            uap=fpvalue(summary(lm(cv~gvar))$coefficients[2,4])
            if (testname==T) {test="ANOVA Trend"}
            else {test=7}
          } # end orderedGro==T
          else { 
            sfit=summary(aov(cv~as.factor(gvar)))
            uap=fpvalue(unlist(sfit)["Pr(>F)1"])
            if (testname==T) {test="ANOVA"} 
            else {test=1} }
        } # end parametric
        else if (typev[k]==1) { # if nonparametric
          if (orderedGro==T) { # if groups are ordered
            xdd=na.omit(cbind(cv,gvar))
            uap=fpvalue(jonckheere.test(xdd[,1], xdd[,2], nperm=50000)$p.value)
            if (testname==T) {test="Jonckheere-Terpstra"} 
            else {test=6} }
          else { # if groups are not ordered
            tmp=na.omit(data.frame(cv,gvar))
            uap=fpvalue(kruskal.test(cv~ gvar, data=tmp)$p.value)
            if (testname==T) {test="Kruskal-Wallis"}
            else {test=2}}
        } # end non-parametric 
        pcal=c(uap, rep(NA, nrow(row)-1))
        tcal=c(test, rep(NA, nrow(row)-1))
        row=cbind(row, pcal, tcal)
      }  # end print p value
    }# end continuous variable
    else { # categorical
      t1=table(cv,gvar)
      if(dim(t1)[1]>1){ #For multi-level, summarize all
        if(dim(t1)[1]==2 & onelinebinary==T & typev[k]==2){ # display only one level (binary & onelinebinary)
          usev=row.names(t1) %in% c("1","TRUE","T","true","YES","Yes","yes")
          if(sum(usev)==0) usev=2
          ccv=paste0(t1[usev,]," (", myperc(t1[usev,]/apply(t1,2,sum)*100),"%)")
          if (sumCol==F) { ccv=paste0(t1[usev,]," (", myperc(t1[usev,]/sum(t1[usev,])*100),"%)") }
          if (showbinarylevel==T){
            row = c(dnames[k],row.names(t1)[usev], ccv )
            if (footMissing==F) {row=c(row, sum(is.na(cv)))}
          } 
          else {row = c( dnames[k], ccv)
          if (footMissing==F) {row=c(row, t(sum(is.na(cv))))}
          }
        } # end binary
        else { # show 2 or more levels
          sum=apply(t1,2,sum)
          ccv=matrix(paste0(t1,paste0(" (",  myperc(t(apply(t1,1,function(x){x/sum}))*100),"%)")), nrow=dim(t1)[1])
          if (sumCol==F) {
            sum=apply(t1,1,sum)
            ccv=matrix(paste0(t1,paste0(" (",  myperc(apply(t1,2,function(x){x/sum})*100),"%)")), nrow=dim(t1)[1])
          }
          row=cbind(c(dnames[k],rep("", dim(t1)[1]-1)),row.names(t1), ccv)
          if (footMissing==F) { row=cbind(row, c(sum(is.na(cv)),rep(NA, nrow(row)-1)))}
        } # end more than 2 levels
        if (printpvalue==T){ # if requiring test
          if (orderedGro==T & dim(t1)[1]==2) # only for binary outcomes we care about ordered groups 
          {gvar=ordered(gvar)
          uap=fpvalue(armitageSim(cv, gvar)[[1]])
          test=armitageSim(cv,gvar)[[pos]] }
          else{ # if groups are not ordered or variable is not binary, perform chi-square/fisher's exact test
            uap=fpvalue(fisherChi(t1)[[1]])
            test=fisherChi(t1)[[pos]] }
          if(dim(t1)[1]==2 & onelinebinary==T & typev[k]==2){
            row = c(row,uap,test)
          } else {
            pcal=c(uap, rep(NA, dim(row)[1]-1))
            tcal=c(test, rep(NA, dim(row)[1]-1))
            row=cbind(row, pcal, tcal) 
          }
        }# end printpvalue
      } else {
        if (footMissing) row = c(dnames[k],row.names(t1)[1],paste0(t1[1,]," (100%)"),rep("-",2))
        else row = c(dnames[k],row.names(t1)[1],paste0(t1[1,]," (100%)"),sum(is.na(cv)),rep("-",2))}
    } # end categorical
    rownames(row)=NULL
    colnames(row)=NULL
    
    tabx=rbind(tabx,row)
    if (footMissing==T){
      if(!is.null(row)&sum(is.na(cv))>0){
        allmiss = c(allmiss,paste(dnames[k],"=",sum(is.na(cv))))
      }}#end all miss
  }#end for
  tabx = data.frame(tabx,stringsAsFactors=F)
  
  missnote <- ifelse(footMissing,ifelse(is.null(allmiss[1]),"No Missing Values",paste("Missing values:",paste(allmiss,collapse=", "))),"")
  
  ind <- as.vector(na.omit(c(1:(n.grp+2),ifelse(footMissing,NA,n.grp+3),eval(parse(text=ifelse(printpvalue,"n.grp+4:5",NA))))))
  names(tabx) <- c("Variable","Type/Level",paste0(names(table(gvar)), paste0(": N=",table(gvar))), "#Missing", "P-value","Test")[ind]
  just <- c(rep("L",dim(table(gvar))+2),"C","R","C")[ind]
  
  if (!is.null(output)){
    rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
    addParagraph(rtf,"Table 2. Summary by group. \n ")
    addTable(rtf,tabx,font.size=9,row.names=FALSE,col.justify=just,header.col.justify=just,NA.string="")
    addNewLine(rtf)
    if(footMissing) addParagraph(rtf,missnote)
    if (testname==F){addParagraph(rtf,testnote)}
    done(rtf)
  }
  return(list(tabx,missnote,testnote)[as.vector(na.omit(c(1,ifelse(footMissing,2,NA),ifelse(testname,NA,3))))])
} #end myMultiGrtab

###############################
#fisherChi(): Function that determines whether fisher's or chi-square test is appropriate.  
#If there are low cell counts, but data set is too large for Fisher's, conducts
#a permutation version of chi-square.
#Function that determines whether fisher's or chi-square test is appropriate.  
#If there are low cell counts, but data set is too large for Fisher's, conducts
#a permutation version of chi-square.
#Returns p-value and test-type: fisher (4), chi-square (3), sim.chi-square (5)
fisherChi = function(tabx){
  options(warn=-1)
  uap = chisq.test(tabx,correct=F)
  testtype = "Chi-square"
  testnumber=3
  #print(c("print1",testtype))
  if(sum(uap$expected<5)>0|sum(tabx)<30){
    uap=NULL
    uap = try(fisher.test(tabx),silent=TRUE)
    testtype = "Fisher" 
    testnumber=4
    #print(c("print2",testtype,is.null(uap)))
    if(length(uap)==1){ #this means there was an error with fisher
      uap = chisq.test(tabx,simulate.p.value=TRUE, B = 50000)
      testtype = "Sim.Chi"
      testnumber=5
      #print(c("print3",testtype))
    }#end if
  }#end if
  options(warn=0)
  print(testtype)
  return(list(uap$p.value,testtype, testnumber))
}#end fisherChi



#Some supporting functions:
mymin=function(x){min(x, na.rm=T)}
mymean=function(x){mean(x,na.rm=T)}
mysd=function(x){sd(x,na.rm=T)}
mymax=function(x){max(x, na.rm=T)}
mymedian=function(x){median(x, na.rm=T)}
msd=function(x){paste(myForm(mymean(x),1)," (",myForm(mysd(x),1),")",sep="")}
miqr=function(x){qcv=as.numeric(myForm(quantile(x,c(0.5,0.25,0.75),na.rm=T),1))
paste(qcv[1]," (",qcv[2],", ",qcv[3],")",sep="") }
mrange=function(x) {paste("(", myForm(mymin(x),1), ", ", myForm(mymax(x),1),")", sep="")}

#armitageSim(): Function that determines whether a Cochran-armitage trend test or a Monte Carlo simulation 
#version of it is appropriate. Returns p-value and test-type.
armitageSim=function(y,group)
{ options(warn=-1)
  tab=table(y,group)
  if (sum(chisq.test(tab)$expected<5)>0){
    uap=pvalue(independence_test(y ~ group, teststat = "quad", distribution = approximate(B = 50000) ) )
    testtype="C-A Sim"
    testnumber=9
  }
  else {uap=fpvalue(pvalue(independence_test(y ~ group, teststat = "quad") ))
  testtype="C-A"
  testnumber=8
  }
  options(warn=0)
  print(testtype)
  return(list(uap,testtype, testnumber))
} #end armitageSim

### produce a frequency table, with rowpercent
freqtab=function(var, group)
{mt=table(var, group)
margin.table(mt,1)
margin.table(mt,2)
mpt=myForm(100*prop.table(mt,2),1)
mtm=matrix(1, nrow=dim(mt)[2], ncol=length(table(var)))
for (i in 1:length(table(var))){
  mtm[,i]=paste(mt[,i],"(",mpt[i,],"%)")
};
tab=as.data.frame(mtm);
names(tab)=rownames(mt)
tab$group=names(table(group));
row.names(tab)=NULL
newtab=tab[,c(length(table(var))+1,1:dim(mt)[1])];
names(newtab)[1]="";
return (newtab)}

#### For histograms:#
hist4=function(x)
{par(mfrow=c(2,2))
  for (i in 1:dim(x)[2])
    hist(x[,i], main=names(x)[i], xlab=" ")
}
# calculate all descriptive statistics
sum4var=function(x){
  tab4=NULL;
  for (i in 1:dim(x)[2]){y=x[,i]
  msd=paste(myForm(mymean(y),1)," (",myForm(mysd(y),1),")",sep="")
  nmiss=sum(is.na(y))
  tot=length(y); pct=myForm(100*nmiss/tot)
  missing=paste(nmiss,"(", pct, "%)")
  b1=myForm(mymin(y),1)
  b2=myForm(mymax(y),1)
  qcv = as.numeric(myForm(quantile(y,c(0.25,0.5,0.75),na.rm=T)))
  Q1=qcv[1]; Q2=qcv[2]; Q3=qcv[3]
  if (tot-nmiss>5000) {sp=fpvalue(shapiro.test(sample(y, 5000))$p.value)}
  else if (tot-nmiss<3 || sd(y,na.rm=T) == 0) {sp=NA}
  else sp=fpvalue(shapiro.test(y)$p.value)
  crow=c(names(x)[i],missing, msd,b1,Q1,Q2,Q3,b2, sp)
  tab4=rbind(tab4,crow)}
  tab4=as.data.frame(tab4); rownames(tab4)=NULL
  names(tab4)=c("Variable", "missing","mean (SD)","Min","Q1","Q2","Q3","Max","Shapiro.P")
  return(tab4)
}





