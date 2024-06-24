
#==============================================================================
#         CALCULATING THE CONNECTANCE, MASS SURF, AND ENERGY SURF INDEX
#==============================================================================

# LOAD PACKAGES, PACKAGES MAY NEED TO BE INSTALLED FIRST
library(openxlsx) 
library(readxl)
library(plyr)
library(here)
library(stringr)

options(scipen=999)                                                            # PREVENT R FROM USING SCIENTIFIC NOTATION

#======= IMPORT DATA ==========================================================

# SET DIRECTORIES AND LOAD DATA
directory <- here::here("data","raw")
output <- here::here("data","output")

dietfiles = list.files(path=directory, pattern="diet*", full.names=TRUE)
diet.list = lapply(dietfiles, read_excel)
names(diet.list) = str_match(dietfiles, "diet_\\s*(.*?)\\s*.xlsx")[,2]

groupdatafiles = list.files(path=directory, pattern="groupdata*", full.names=TRUE)
groupdata.list = lapply(groupdatafiles, read_excel)
names(groupdata.list) = str_match(groupdatafiles, "groupdata_\\s*(.*?)\\s*.xlsx")[,2]

rm(dietfiles, groupdatafiles)

#==============================================================================
#       SET UP FOR LOOP TO RUN ALL GEOGRAPHIES
#==============================================================================

for (geog in 1:length(diet.list)) {
diet.mat <- as.data.frame(diet.list[[geog]], col.names=names(x))               # IMPORT AND CREATE DATA FRAME FROM DIET MATRIX
group.data <- as.data.frame(groupdata.list[[geog]], col.names=names(x))        # IMPORT AND CREATE TABLE FROM GROUP PARAMETERS AS USED IN THE ECOPATH MODEL (FUNCTIONAL GROUP NAMES, BIOMASS (B), AND CONSUMPTION/BIOMASS (QB)) IN COMBINATION WITH DIET DATA
n.groups=dim(group.data)[1]                                                    # SET NUMBER OF GROUPS
geography=names(diet.list[geog])                                               # SET GEOGRAPHY NAME

#==============================================================================
#        STEP 4.1. DISAGGREGATED MODEL                                                                         
#==============================================================================

#======= STEP 4.1.1. CREATE PARAMETERS AND DIET MATRICES ======================

B=as.matrix(as.numeric(group.data$B))                                          # CREATE VECTOR FOR BIOMASS (B) VALUES PER GROUP IN T/KM2

QB=as.matrix(as.numeric(group.data$QB))                                        # CREATE VECTOR FOR CONSUMPTION (Q) PER BIOMASS (B) PER GROUP IN T/YEAR

group.names=as.vector(unlist(group.data$group.name))                           # CREATE VECTOR OF GROUP NAMES
ED=as.matrix(group.data$ED)                                                    # CREATE VECTOR FOR ENERGY DENSITY (ED) VALUES PER GROUP IN KCAL/G
p=as.matrix(diet.mat[,2:(n.groups+1)])                                         # CREATE DATA FRAME OF ONLY DIET COMPOSITION (pij) VALUES EXTRACTED FROM DIET MATRIX (IGNORING FIRST COLUMN WITH GROUP NAMES)

CTOT=c(B*QB)                                                                   # CREATE VECTOR FOR ACTUAL TOTAL CONSUMPTION IN Tonnes PER GROUP AS PRODUCT OF BIOMASS AND CONSUMPTION/BIOMASS
na.index=which(is.na(CTOT)==TRUE)                                              # CHECK FOR GROUPS WITHOUT CONSUMPTION (E.G., PHYTOPLANKTON AND DETRITUS)
CTOT[na.index]=rep(0,length(na.index))                                         # FOR THESE GROUPS, SET CONSUMPTION TO BE "0"

CTOT<-t(CTOT)                                                                  # TRANSPOSE VECTOR FROM COLUMN TO ROW
CTOT <- as.data.frame(lapply(CTOT, rep, length(group.names)))                  # CREATE DATA FRAME OF TOTAL CONSUMPTION PER GROUP BY REPLICATING THIS ROW AS MANY TIMES AS THERE ARE GROUPS
C=p*CTOT                                                                       # CREATE DATA FRAME OF ACTUAL CONSUMPTION OF EACH PREY (ROW) BY EACH PREDATOR (COLUMN) AS PRODUCT OF TOTAL CONSUMPTION AND PIJ
colnames(C)=colnames(p)                                                        # GIVE THE DATA FRAME THE COLUMN HEADERS OF GROUP NAMES

EDmat <- t(sapply(ED, rep, length(group.names)))                               # CREATE DATA FRAME OF ENERGY DENSITY PER GROUP BY REPLICATING THIS COLUMN AS MANY TIMES AS THERE ARE GROUPS
EC= C*EDmat                                                                    # CREATE DATA FRAME OF ACTUAL ENERGY CONSUMPTION OF EACH PREY (ROW) BY EACH PREDATOR (COLUMN) AS PRODUCT OF CONSUMPTION AND ENERGY DENSITY
ECTOT=colSums(EC)                                                              # GIVES SUM OF ENERGY CONSUMPTION FOR EVERY COLUMN (PREDATOR)
ECTOT<-t(ECTOT)                                                                # CONVERT COLUMN TO ROW
ECTOT<- as.data.frame(lapply(ECTOT, rep, length(group.names)))                 # REPLICATE ROW FOR TOTAL NUMBER OF GROUPS
colnames(ECTOT) <- c(1:length(group.names))                                    # change column names to group numbers
pE<-EC/ECTOT                                                                   # CREATE P (DIET PROPORTION) FOR ENERGY 
pE <- replace(pE, is.na(pE), 0)                                                # FOR GROUPS OF WHICH EC/ECpredtot = NA (I.E. NON-CONSUMERS SUCH AS PHYTOPLANKTON), SET CONSUMPTION TO BE "0"
pE<-as.matrix(pE)                                                              # convert energy diet matrix from data frame to a matrix for further calculations 

SUMpE<-colSums(pE)                                                             # CHECK WHETHER ALL COLUMNS SUM UP TO 1

#==============================================================================
#         CALCULATE THE 3 INDICES FOR DISAGGREGATED MODEL
#==============================================================================

#======= STEP 4.1.2. COUNT TROPHIC LINKAGES FOR ALL GROUPS ====================


S<-vector(mode='numeric')                                                      # CREATE EMPTY VECTOR FOR S (# OF TROPHIC LINKAGES IN MASS DIET MATRIX FOR A GROUP AS PREY WITH OTHER GROUPS AS PREDATORS)
S2<-vector(mode='numeric')                                                     # CREATE EMPTY VECTOR FOR S2 (TOTAL TROPHIC LINKAGES AS S + # OF TROPHIC LINKAGES IN MASS DIET MATRIX FOR A GROUP AS PREDATOR WITH OTHER GROUP AS PREY)
ES<-vector(mode='numeric')                                                     # CREATE EMPTY VECTOR FOR ES (# OF TROPHIC LINKAGES IN ENERGY DIET MATRIX FOR A GROUP AS PREY WITH OTHER GROUP AS PREDATORS)


for (i in 1:n.groups)                                                          # RUN A LOOP WITH NUMBER OF RUNS (i) EQUAL TO NUMBER OF GROUPS 
{                                                                              # OPEN LOOP
  
  S=rbind(S,length(which(p[i,]>0)))                                            # POPULATE VECTOR S WITH VALUE BEING # OF TROPHIC LINKAGES IN MASS DIET MATRIX (p) FOR GROUP i AS PREY WITH OTHER GROUP AS PREDATORS GREATER THAN 0
  ES=rbind(ES,length(which(pE[i,]>0)))                                         # POPULATE VECTOR ES WITH VALUE BEING # OF TROPHIC LINKAGES IN ENERGY DIET MATRIX (pE) FOR GROUP i AS PREY WITH OTHER GROUP AS PREDATORS GREATER THAN 0
  
  if (group.names[i]=='Import'|group.names[i]=='Discards'
      |group.names[i]=='By-catch')                                             # CONDITION: IF THE GROUP NAME OF GROUP i IS 'IMPORT', 'BY-CATCH', OR 'DISCARDS' (OUTSIDE (NATURAL) ECOSYSTEM) THEN
  {
    S[i]=0                                                                     # # OF TROPHIC LINKAGES IN MASS DIET MATRIX (p) FOR GROUP i AS PREY WITH OTHER GROUP AS PREDATORS (S) IS SET AT 0
    ES[i]=0                                                                    # # OF TROPHIC LINKAGES IN ENERGY DIET MATRIX (p) FOR GROUP i AS PREY WITH OTHER GROUP AS PREDATORS (ES) IS SET AT 0
  }
  
  S2=rbind(S2,(S[i]+length(which(p[,i]>0))))                                   # POPULATE VECTOR S2 FOR GROUP i WITH VALUE BEING Si + # OF TROPHIC LINKAGES IN MASS DIET MATRIX FOR GROUP i AS PREDATOR WITH OTHER GROUP AS PREY
}                                                                              # CLOSE LOOP

#======= STEP 4.1.3. CALCULATE INDICES AND DETERMINE KEYNESS ==================

connectance<-vector(mode='numeric')                                            # CREATE EMPTY VECTOR FOR CONNECTANCE INDEX VALUE
KEYconnectance<-vector(mode='numeric')                                         # CREATE EMPTY VECTOR FOR POTENTIAL DETERMINATION OF A KEY GROUP BY CONNECTANCE INDEX 
sumpij<-vector(mode='numeric')                                                 # CREATE EMPTY VECTOR FOR VARIABLE sumpij TO CALCULATE MASS SURF INDEX
SURF<-vector(mode='numeric')                                                   # CREATE EMPTY VECTOR FOR SURF INDEX VALUE
KEYSURF<-vector(mode='numeric')                                                # CREATE EMPTY VECTOR FOR POTENTIAL DETERMINATION OF A KEY GROUP BY MASS SURF INDEX
esumpij<-vector(mode='numeric')                                                # CREATE EMPTY VECTOR FOR VARIABLE esumpij TO CALCULATE ENERGY SURF INDEX
energy<-vector(mode='numeric')                                                 # CREATE EMPTY VECTOR FOR ENERGY SURF INDEX VALUE
KEYenergy<-vector(mode='numeric')                                              # CREATE EMPTY VECTOR FOR POTENTIAL DETERMINATION OF A KEY GROUP BY ENERGY SURF INDEX


for (j in 1:n.groups)                                                          # RUN A LOOP WITH NUMBER OF RUNS (i) EQUAL TO NUMBER OF GROUPS 
{
  connectance=rbind(connectance,(S2[j]/sum(S)))                                # POPULATE 'CONNECTANCE' INDEX FOR GROUP i WITH TOTAL TROPHIC LINKAGES (S2) OF GROUP i DIVIDED BY TOTAL TROPHIC LINKAGES IN FOOD WEB
  
  if (connectance[j]>0.04)                                                     # CONDITION: IF 'CONNECTANCE' INDEX FOR GROUP i IS HIGHER THAN 0.04, THEN
  {KEYconnectance=rbind(KEYconnectance,KEYconnectance[j]<-'KEY')}              # POPULATE 'KEYCONNECTANCE' FOR GROUP i WITH 'KEY' AS THIS GROUP IS IDENTIFIED AS BEING KEY BY THE CONNECTANCE INDEX
  
  else                                                                         # IF 'CONNECTANCE' INDEX FOR GROUP i IS NOT HIGHER THAN 0.04 (Smith et al., 2011), THEN
  {KEYconnectance=rbind(KEYconnectance,KEYconnectance[j]<-'')}                 # LEAVE 'KEYCONNECTANCE' FOR GROUP i EMPTY AS THIS GROUP IS NOT IDENTIFIED AS BEING KEY BY THE CONNECTANCE INDEX
  
  sumpij<- rbind(sumpij,sum((p[j,])^2))                                        # POPULATE 'sumpij' FOR GROUP i WITH VALUE CALCULATED AS THE SUM OF ALL SQUARED DIET PROPORTION GROUP i REPRESENTS FOR EACH GROUP AS PREDATOR
  SURF<- rbind(SURF,sumpij[j]/sum(S))                                          # POPULATE 'SURF' INDEX FOR GROUP i AS 'sumpij' OF GROUP i DIVIDED BY TOTAL TROPHIC LINKAGES IN FOOD WEB
  
  if (SURF[j]>0.001)                                                           # CONDITION: IF 'SURF' INDEX OF GROUP i IS HIGHER THAN 0.001 (Plaganyi and Essington, 2014), THEN
  {KEYSURF=rbind(KEYSURF,KEYSURF[j]<-'KEY')}                                   # POPULATE 'KEYSURF' FOR GROUP i WITH 'KEY' AS THIS GROUP IS IDENTIFIED AS BEING KEY BY THE MASS SURF INDEX
  else                                                                         # IF 'SURF' INDEX FOR GROUP i IS NOT HIGHER THAN 0.001, THEN
  {KEYSURF=rbind(KEYSURF,KEYSURF[j]<-'')}                                      # LEAVE 'KEYSURF' FOR GROUP i EMPTY AS THIS GROUP IS NOT IDENTIFIED AS BEING KEY BY THE MASS SURF INDEX
  
  esumpij<- rbind(esumpij,sum((pE[j,])^2))                                     # POPULATE 'esumpij' FOR GROUP i WITH VALUE CALCULATED AS THE SUM OF ALL SQUARED DIET PROPORTION GROUP i REPRESENTS FOR EACH GROUP AS PREDATOR
  energy<- rbind(energy,esumpij[j]/sum(ES))                                    # POPULATE 'energy' SURF INDEX FOR GROUP i AS 'esumpij' OF GROUP i DIVIDED BY TOTAL TROPHIC LINKAGES IN FOOD WEB
  
  if (energy[j]>0.001)                                                         # CONDITION: IF 'SURF' INDEX OF GROUP i IS HIGHER THAN 0.001 (Surma et al., 2022), THEN 
  {KEYenergy=rbind(KEYenergy,KEYenergy[j]<-'KEY')}                             # POPULATE 'KEYSURF' FOR GROUP i WITH 'KEY' AS THIS GROUP IS IDENTIFIED AS BEING KEY BY THE ENERGY SURF INDEX
  else                                                                         # IF 'ENERGY' INDEX FOR GROUP i IS NOT HIGHER THAN 0.001, THEN
  {KEYenergy=rbind(KEYenergy,KEYenergy[j]<-'')}                                # LEAVE 'KEYENERGY' FOR GROUP i EMPTY AS THIS GROUP IS NOT IDENTIFIED AS BEING KEY BY THE ENERGY SURF INDEX
  
  if (group.names[j]=='Import'|group.names[j]=='Discards'|
      group.names[j]=='By-catch')                                              # CONDITION: IF GROUP i IS EITHER 'IMPORT', 'DISCARDS' or 'BY-CATCH', THEN 
  {
    KEYconnectance[j]<-''                                                      # LEAVE 'KEYCONNECTANCE' EMPTY
    KEYSURF[j]<-''                                                             # LEAVE 'KEYSURF' EMPTY
    KEYenergy[j]<-''                                                           # LEAVE 'KEYENERGY' EMPTY
  }
}

# NAME COLUMNS
colnames(connectance)<-'connectance'
colnames(KEYconnectance)<-'KEYconnectance'  
colnames(SURF)<-'SURF mass' 
colnames(KEYSURF)<-'KEYSURF mass'  
colnames(energy)<-'SURF energy' 
colnames(KEYenergy)<-'KEYSURF energy' 

# PASTE ALL COLUMNS WITH DATA INTO ONE DATA FRAME
indices<-data.frame(group.names,round(connectance,10),KEYconnectance,round(SURF,10),KEYSURF, round(energy,10),KEYenergy)   
worksheetmass<-data.frame(group.names,S,S2,sumpij,round(connectance,10),KEYconnectance,round(SURF,10),KEYSURF,p)
worksheetenergy<-data.frame(group.names,ES,esumpij,round(energy,10),KEYenergy,pE)

# CREATE LIST OF DATA FOR SEPERATE EXCEL DATASHEETS
list_of_datasets <- list("Table A - Model Parameters" = group.data, 
                         "Table B - Diet Mass" = worksheetmass, 
                         "Table C - Diet Energy" = worksheetenergy)

# *** WRITE OUTPUT FILES ***
write.xlsx(indices, file.path(output, paste0(geography, "_indices.xlsx")))     # WRITE EXCEL FILE WITH THE THREE INDICES VALUES FOR ALL FUNCTIONAL GROUPS IN DISAGGREGATED MODEL
write.xlsx(list_of_datasets, file.path(output, paste0(geography, ".xlsx")))    # WRITE EXCEL FILE OF THE COMPLETE WORKBOOK INCLUDING THE SEPERATE DATASHEETS



#==============================================================================
#         STEP 4.2. INDEX CALCULATIONS FOR AGGREGATED MODEL (IF REQUIRED)                                      
#==============================================================================

#======= STEP 4.2.1. DEFINE QBE ===============================================

group.data$QBE<-(t(ECTOT[1,]/group.data$B))                                    # ADD A VECTOR TO THE GROUP DATA FOR THE ENERGY CONSUMPTION/BIOMASS (QBE) CALCULATED BY DIVIDING THE TOTAL ENERGY CONSUMPTION PER GROUP (CALCULATE AT LINES 40-45) BY ITS BIOMASS  
QBE<-group.data$QBE                                                            # EXTRACT THESE DATA AS SEPERATE VECTOR
QBE[is.na(QBE)] <- 0                                                           # CONVERT NA VALUES TO ZEROS


#======= STEP 4.2.2. CREATE FUNCTION FOR AGGREGATING GROUPS IN DIET MATRICES ================================


aggregate.groups<-function(combine.groups,new.groupname,C,EC,B,QB,QBE,p,pE,group.names) {                     # CREATE FUNTION THAT USES THE ASSIGNED PARAMETERS, SOME ARE BEING GENERATED IN LINES BELOW: COMBINE.GROUPS -> 264 AND NEW.GROUPNAME -> 270
  
  ngroups=length(B)                                                                                           # DETERMINE NUMBER OF GROUPS FROM NUMBER OF BIOMASSES
  
  other.groups=c()                                                                                            # CREATE VECTOR OF GROUPS THAT ARE NOT AGGREGATED
  for (i in 1:ngroups){                                                                                       # RUN A LOOP WITH NUMBER OF RUNS (i) EQUAL TO # ORIGINAL GROUPS
    group.index=which(combine.groups==i)                                                                      # DETERMINE WHETHER GROUP i IS GETTING COMBINED (TRUE) OR NOT (FALSE)
    if (length(group.index)==0){                                                                              # IF GROUP i IS NOT COMBINED (AND THUS INDEX IS FALSE AND LENGTH INDEX =0), THEN
      other.groups=c(other.groups,i)                                                                          # GROUPS NAME REMAINS UNCHANGED
    }
  }
  
  
  #======= CREATE TEMP VECTORS FOR BASIC MODEL PARAMETERS ADDING NEW GROUP =====
  
  
  B.tmp=as.matrix(c(B,sum(B[combine.groups])),nrow=ngroups+1)                                                 # ADD SUMMED BIOMASS OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF BIOMASS (B) VALUES 
  QB.newgroup=sum(QB[combine.groups]*B[combine.groups]/sum(B[combine.groups]))                                # CALCULATE CONSUMPTION/BIOMASS (QB) BY CALCULATING ACTUAL CONSUMPTION PER SEPERATE GROUPS DIVIDED BY BIOMASS OF AGGREGATED GROUP AND SUM THESE
  QB.tmp=as.matrix(c(QB,QB.newgroup),nrow=ngroups+1)                                                          # ADD CONSUMPTION/BIOMASS (QB) OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF QB VALUES
  QBE.newgroup=sum(QBE[combine.groups]*B[combine.groups]/sum(B[combine.groups]))                              # CALCULATE ENERGY CONSUMPTION/BIOMASS (QBE) BY CALCULATING ACTUAL ENERGY CONSUMPTION PER SEPERATE GROUPS DIVIDED BY BIOMASS OF AGGREGATED GROUP AND SUM THESE
  QBE.tmp=as.matrix(c(QBE,QBE.newgroup),nrow=ngroups+1)                                                       # ADD ENERGY CONSUMPTION/BIOMASS (QBE) OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF QBE VALUES
  group.names.tmp=as.matrix(c(group.names,new.groupname),nrow=ngroups+1)                                      # ADD GROUP NAME OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF GROUP NAMES
  
  
  #======= MASS DIET MATRIX =============================
  
  
  C.tmp.pred=rowSums(C[,combine.groups])                                                                      # CALCULATE ACTUAL CONSUMPTION IN MASS OF EACH PREY BY AGGREGATED GROUP AS PREDATOR BY SUMMING ACTUAL CONSUMPTION OF PREY IN MASS BY SEPERATE GROUPS 
  p.tmp.pred=C.tmp.pred/sum(C.tmp.pred)                                                                       # CALCULATE DIET PROPORTION (p) OF EACH PREY IN DIET AGGREGATED GROUP BY DIVIDING ACTUAL CONSUMPTION OF THAT PREY IN MASS BY TOTAL ACTUAL MASS CONSUMPTION OF AGGREGATE GROUP
  p.tmp.new=cbind(p,p.tmp.pred)                                                                               # ADD DIET COMPOSITION (p) OF AGGREGATE GROUP AS COLUMN TO EXISTING DIET MATRIX
  
  p.tmp.prey=colSums(p.tmp.new[combine.groups,])                                                              # CALCULATE DIET PROPORTION (p) THAT AGGREGATED GROUP REPRESENT AS PREY FOR EACH PREDATOR BY SUMMING THEIR PROPORTION (p) AS SEPERATE GROUPS
  p.tmp.new=rbind(p.tmp.new,p.tmp.prey)                                                                       # ADD ROW OF p-VALUES OF AGGREGATED GROUP AS PREY TO BOTTOM OF DIET MATRIX
  
  
  #======= ENERGY DIET MATRIX =============================
  
  
  EC.tmp.pred=rowSums(EC[,combine.groups])                                                                    # CALCULATE ACTUAL CONSUMPTION IN ENERGY OF EACH PREY BY AGGREGATED GROUP AS PREDATOR BY SUMMING ACTUAL CONSUMPTION OF PREY IN ENERGY BY SEPERATE GROUPS 
  pE.tmp.pred=EC.tmp.pred/sum(EC.tmp.pred)                                                                    # CALCULATE DIET PROPORTION (p) OF EACH PREY IN DIET AGGREGATED GROUP BY DIVIDING ACTUAL CONSUMPTION OF THAT PREY IN ENERGY BY TOTAL ACTUAL ENERGY CONSUMPTION OF AGGREGATE GROUP
  pE.tmp.new=cbind(pE,pE.tmp.pred)                                                                            # ADD DIET COMPOSITION (pE) OF AGGREGATE GROUP AS COLUMN TO EXISTING DIET MATRIX
  
  pE.tmp.prey=colSums(pE.tmp.new[combine.groups,])                                                            # CALCULATE ENERGY DIET PROPORTION (pE) THAT AGGREGATED GROUP REPRESENT AS PREY FOR EACH PREDATOR BY SUMMING THEIR PROPORTION (pE) AS SEPERATE GROUPS
  pE.tmp.new=rbind(pE.tmp.new,pE.tmp.prey)                                                                    # ADD ROW OF pE-VALUES OF AGGREGATED GROUP AS PREY TO BOTTOM OF DIET MATRIX
  
  
  #======= UPDATE NEW DIET MATRICES WITH NEW GROUP NAMES ==================================   
  
  
  other.groups=c(other.groups,n.groups+1)                                                                     # CREATE VECTOR WITH EXISTING GROUP NAMES PLUS ONE ROW
  p.new=p.tmp.new[other.groups,other.groups]                                                                  # CREATE NEW DIET MATRIX FOR SURF MASS-BALANCED INCLUDING AGGREGATED GROUP
  p.new[is.na(p.new)] <- 0                                                                                    # REPLACE NA'S WITH 0'S
  pE.new=pE.tmp.new[other.groups,other.groups]                                                                # CREATE NEW DIET MATRIX FOR SURF ENERGY-BALANCED INCLUDING AGGREGATED GROUP
  pE.new[is.na(pE.new)] <- 0                                                                                  # REPLACE NA'S WITH 0'S
  new.colnames=colnames(p.new)                                                                                # ASSIGN NEW COLUMN NAMES
  nr=length(new.colnames)                                                                                     # DETERMINE NUMBER OF GROUPS
  new.colnames[nr]=new.groupname                                                                              # DETERMINE NEW GROUP NAME AS THE LAST ONE
  colnames(p.new)=new.colnames                                                                                # GIVE NEW DIET MATRIX FOR SURF MASS-BALANCED NEW COLUMN NAMES
  rownames(p.new)=c()                                                                                         # REMOVE ROW NAMES
  colnames(pE.new)=new.colnames                                                                               # GIVE NEW DIET MATRIX FOR SURF ENERGY-BALANCED NEW COLUMN NAMES
  rownames(pE.new)=c()                                                                                        # REMOVE ROW NAMES
  
  B.new=B.tmp[other.groups]                                                                                   # ADJUST BIOMASS (B) AS BASIC PARAMETER
  QB.new=QB.tmp[other.groups]                                                                                 # ADJUST CONSUMPTION/BIOMASS (QB) AS BASIC PARAMETER
  QBE.new=QBE.tmp[other.groups]                                                                               # ADJUST ENERGY CONSUMPTION/BIOMASS (QBE) AS BASIC PARAMETER
  group.names.new=group.names.tmp[other.groups]                                                               # ADJUST GROUP NAMES AS BASIC VECTOR
  
  Agg.model=list(B=B.new,QB=QB.new,QBE=QBE.new,group.names=group.names.new,p=p.new,pE=pE.new)                 # CREATE A LIST OF THE FUNCTION'S OUTCOMES WITH THE PARAMETERS YOU WANT TO BE PRODUCED 
}

#======= STEP 4.2.3. DEFINE GROUPS TO AGGREGATE ===============================

obs1<-length(group.data$agg.group.name)                                        # SET LENGTH FOR FOLLOWING LOOP TO NUMBER OF GROUP NAMES (INCLUDING DOUBLES)
combine.group.names.all<-vector("character")                                   # CREATE NEW VECTOR FOR LIST OF NEW NAMES OF GROUPS THAT NEED TO BE AGGREGATED

for (k in 1:obs1){                                                             # RUN LOOP WITH NUMBER OF RUNS (k) EQUAL TO # OF GROUP NAMES
  if(length(which(unique(group.data$agg.group.name[k])==
                  group.data$agg.group.name))>1)                               # IF GROUP NAME k OCCURS MORE THAN ONCE (AS FOR THE NEW AGGREGATED GROUPS) IN THE GROUP.DATA TABLE
  {        
    combine.group.names.all<-append(combine.group.names.all,
                                    group.data$agg.group.name[k])              # EXTRACT THOSE AGGREGATED GROUP NAMES FROM GROUP DATA AND ADD TO LIST (THIS WILL CONTAIN NAME OF AGGREAGETED GROUP MULTIPLE TIMES)
  }
}

combine.group.names<- unique(combine.group.names.all)                          # CREATE LIST OF EXISTING GROUPS THAT NEED TO BE AGGREGATED WITH EACH AGGREGATED GROUP NAME APPEARING ONLY ONCE IN THE NEW LIST 

obs2<-length(combine.group.names)                                              # SET LENGTH FOR FOLLOWING LOOP TO # OF (NAMES FOR) NEW AGGREGATED GROUPS
combine.group.list<-list()                                                     # CREATE NEW VECTOR FOR LIST OF GROUP CODES OF GROUPS THAT ARE INCLUDED IN THE NEW AGGREGATED GROUPS

for (m in 1:obs2){                                                             # RUN LOOP WITH NUMBER OF RUNS (m) EQUAL TO # OF (NAMES FOR) NEW AGGREGATED GROUPS
  if(length(which(combine.group.names[m]==group.data$agg.group.name))>1)       # IF NEW GROUP NAME m OCCURS MORE THAN ONCE (AS FOR THE NEW AGGREGATED GROUPS) IN THE GROUP.DATA TABLE
  {        
    combine.group.list<-append(combine.group.list,
                               tryCatch(list(group.data$group.code[which(combine.group.names[m]==group.data$agg.group.name)]))) # LIST THE GROUP CODE OF THOSE GROUPS WITH NEW GROUP NAME m AND ADD THESE CODES PER GROUP NAME TO THE LIST
  }
}

n.groups.2.combine<-length(combine.group.list)                                 # ASSIGN NUMBER OF NEW GROUPS CREATED THROUGH AGGREGATION

aggregated.group.list=vector("list",n.groups.2.combine)                        # CREATE EMPTY LIST OF LENGTH OF GROUP NAMES

if (n.groups.2.combine==0) {
  next
}                                                                              # MOVE TO NEXT ITERATION OF OUTSIDE LOOP IF THERE ARE NO GROUPS TO COMBINE FOR THIS GEOGRAPHY

for (q in 1:n.groups.2.combine){                                               # RUN A LOOP WITH NUMBER OF RUNS (q) EQUAL TO # OF AGGREGATED GROUPS
  aggregated.group.list[[q]]=group.names[combine.group.list[[q]]]              # POPULATE LIST WITH NAMES OF AGGREGATED NEW GROUPS
}


#=== STEP 4.2.4. AGGREGATE GROUPS AND CREATE PARAMETERS AND DIET MATRICES =====

for(r in 1:n.groups.2.combine){                                                # RUN A LOOP WITH NUMBER OF RUNS (r) EQUAL TO # OF AGGREGATED GROUPS
  combine.groups.names<-aggregated.group.list[[r]]                             # DETERMINE THE NAMES OF GROUPS TO BE AGGREGATED TO CREATE AGGREGATED GROUP r
  n.combined.groups<-length(combine.groups.names)                              # DETERMINE NUMBER OF GROUPS INCLUDED TO CREATED AGGREGATED GROUP r BY COUNTING NUMBER OF GROUP NAMES
  combine.groups<-c()                                                          # CREATE EMPTY VECTOR WITH NAMES OF COMBINED GROUPS
  
  for (s in 1:n.combined.groups){                                              # RUN A LOOP WITH NUMBER OF RUNS (s) EQUAL TO # OF ORIGINAL GROUPS TO COMBINE TO CREATE AGGREGATED GROUP
    combine.groups[s]<-which(group.names==combine.groups.names[[s]])           # LIST THE GROUP CODES OF THE GROUPS NAMES THAT EQUAL THE GROUP NAMES OF THOSE TO BE COMBINED TO CREATE AGGREGATED GROUP K
  }
  
  new.groupname<-combine.group.names[r]                                        # ASSIGN NEW GROUPNAME r FROM LIST CREATED IN LINE 223
  
  agg.model=aggregate.groups(combine.groups,
                             new.groupname,C,EC,B,QB,QBE,p,pE,group.names)     # RUN FUNCTION 'AGGREGATE.GROUPS' PER 'group_aggregating_fxn.R' TO CREATE DATA FOR AGGREGATED MODEL
  
  B=agg.model$B                                                                # EXTRACT BIOMASS (B) VALUES FROM AGGREGATED MODEL
  QB=agg.model$QB                                                              # EXTRACT CONSUMPTION/BIOMASS (QB) VALUES FROM AGGREGATED MODEL
  QBE=agg.model$QBE
  group.names=agg.model$group.names                                            # EXTRACT GROUP NAMES FROM AGGREGATED MODEL
  p=agg.model$p                                                                # EXTRACT DIET MATRIX (SURF MASS-BALANCED) FROM AGGREGATED MODEL
  pE=agg.model$pE                                                              # EXTRACT DIET MATRIX (SURF ENERGY-BALANCED) FROM AGGREGATED MODEL
  
  n.groups=length(B)	                                                         # DETERMINE NUMBER OF GROUPS AGGREGATED MODEL AS NUMBER OF NEW GROUP NAMES
  
  CTOT=c(B*QB)                                                                 # CREATE VECTOR FOR ACTUAL TOTAL CONSUMPTION IN Tonnes PER GROUP AS PRODUCT OF BIOMASS AND CONSUMPTION/BIOMASS
  na.index=which(is.na(CTOT)==TRUE)                                            # CHECK FOR GROUPS WITHOUT CONSUMPTION (E.G., PHYTOPLANKTON AND DETRITUS)
  CTOT[na.index]=rep(0,length(na.index))                                       # FOR THESE GROUPS, SET CONSUMPTION TO BE "0"
  CTOT<-t(CTOT)                                                                # TRANSPOSE VECTOR FROM COLUMN TO ROW
  CTOT <- as.data.frame(lapply(CTOT, rep, length(group.names)))                # CREATE DATA FRAME OF TOTAL CONSUMPTION PER GROUP BY REPLICATING THIS ROW AS MANY TIMES AS THERE ARE GROUPS
  C=p*CTOT                                                                     # CREATE DATA FRAME OF ACTUAL CONSUMPTION OF EACH PREY (ROW) BY EACH PREDATOR (COLUMN) AS PRODUCT OF TOTAL CONSUMPTION AND PIJ
  
  ECTOT=c(B*QBE)                                                               # CREATE VECTOR FOR ACTUAL TOTAL ENERGY CONSUMPTION IN Tonnes PER GROUP AS PRODUCT OF BIOMASS AND ENERGY CONSUMPTION/BIOMASS
  na.index=which(is.na(ECTOT)==TRUE)                                           # CHECK FOR GROUPS WITHOUT CONSUMPTION (E.G., PHYTOPLANKTON AND DETRITUS)
  ECTOT[na.index]=rep(0,length(na.index))                                      # FOR THESE GROUPS, SET CONSUMPTION TO BE "0"
  ECTOT<-t(ECTOT)                                                              # TRANSPOSE VECTOR FROM COLUMN TO ROW
  ECTOT <- as.data.frame(lapply(ECTOT, rep, length(group.names)))              # CREATE DATA FRAME OF TOTAL CONSUMPTION PER GROUP BY REPLICATING THIS ROW AS MANY TIMES AS THERE ARE GROUPS
  EC=pE*ECTOT                                                                  # CREATE DATA FRAME OF ACTUAL CONSUMPTION OF EACH PREY (ROW) BY EACH PREDATOR (COLUMN) AS PRODUCT OF TOTAL CONSUMPTION AND PIJ
  
}

#==============================================================================
#         CALCULATE THE 3 INDICES OF AGGREGATED MODEL
#==============================================================================

#======= STEP 4.2.5. COUNT TROPHIC LINKAGES FOR ALL GROUPS ====================

S<-vector(mode='numeric')                                                      # CREATE EMPTY VECTOR FOR S (# OF TROPHIC LINKAGES IN MASS DIET MATRIX FOR A GROUP AS PREY WITH OTHER GROUPS AS PREDATORS)
S2<-vector(mode='numeric')                                                     # CREATE EMPTY VECTOR FOR S2 (TOTAL TROPHIC LINKAGES AS S + # OF TROPHIC LINKAGES IN MASS DIET MATRIX FOR A GROUP AS PREDATOR WITH OTHER GROUP AS PREY)
ES<-vector(mode='numeric')                                                     # CREATE EMPTY VECTOR FOR ES (# OF TROPHIC LINKAGES IN ENERGY DIET MATRIX FOR A GROUP AS PREY WITH OTHER GROUP AS PREDATORS)

n.groups<-dim(p)[1]                                                            # SET NUMBER OF GROUPS IN AGGREGATED MODEL

for (u in 1:n.groups)                                                          # RUN A LOOP WITH NUMBER OF RUNS (u) EQUAL TO NUMBER OF GROUPS 
{                                                                              # OPEN LOOP
  
  S=rbind(S,length(which(p[u,]>0)))                                            # POPULATE VECTOR S WITH VALUE BEING # OF TROPHIC LINKAGES IN MASS DIET MATRIX (p) FOR GROUP u AS PREY WITH OTHER GROUP AS PREDATORS GREATER THAN 0
  ES=rbind(ES,length(which(pE[u,]>0)))                                         # POPULATE VECTOR ES WITH VALUE BEING # OF TROPHIC LINKAGES IN ENERGY DIET MATRIX (pE) FOR GROUP u AS PREY WITH OTHER GROUP AS PREDATORS GREATER THAN 0
  
  if (group.names[u]=='Import'|group.names[u]=='Discards'|
      group.names[u]=='By-catch')                                              # CONDITION: IF THE GROUP NAME OF GROUP u IS 'IMPORT','BY-CATCH' OR 'DISCARDS' (OUTSIDE (NATURAL) ECOSYSTEM) THEN
  {
    S[u]=0                                                                     # # OF TROPHIC LINKAGES IN MASS DIET MATRIX (p) FOR GROUP u AS PREY WITH OTHER GROUP AS PREDATORS (S) IS SET AT 0
    ES[u]=0                                                                    # # OF TROPHIC LINKAGES IN ENERGY DIET MATRIX (p) FOR GROUP u AS PREY WITH OTHER GROUP AS PREDATORS (ES) IS SET AT 0
  }
  
  S2=rbind(S2,(S[u]+length(which(p[,u]>0))))                                   # POPULATE VECTOR S2 FOR GROUP u WITH VALUE BEING Si + # OF TROPHIC LINKAGES IN MASS DIET MATRIX FOR GROUP u AS PREDATOR WITH OTHER GROUP AS PREY
}                                                                              # CLOSE LOOP


#======= STEP 4.2.6. CALCULATE INDICES AND DETERMINE KEYNESS ==================

connectance<-vector(mode='numeric')                                            # CREATE EMPTY VECTOR FOR CONNECTANCE INDEX VALUE
KEYconnectance<-vector(mode='numeric')                                         # CREATE EMPTY VECTOR FOR POTENTIAL DETERMINATION OF A KEY GROUP BY CONNECTANCE INDEX 
sumpij<-vector(mode='numeric')                                                 # CREATE EMPTY VECTOR FOR VARIABLE sumpij TO CALCULATE MASS SURF INDEX
SURF<-vector(mode='numeric')                                                   # CREATE EMPTY VECTOR FOR MASS SURF INDEX VALUE
KEYSURF<-vector(mode='numeric')                                                # CREATE EMPTY VECTOR FOR POTENTIAL DETERMINATION OF A KEY GROUP BY MASS SURF INDEX
esumpij<-vector(mode='numeric')                                                # CREATE EMPTY VECTOR FOR VARIABLE esumpij TO CALCULATE ENERGY SURF INDEX
energy<-vector(mode='numeric')                                                 # CREATE EMPTY VECTOR FOR ENERGY SURF INDEX VALUE
KEYenergy<-vector(mode='numeric')                                              # CREATE EMPTY VECTOR FOR POTENTIAL DETERMINATION OF A KEY GROUP BY ENERGY SURF INDEX


for (v in 1:n.groups)                                                          # RUN A LOOP WITH NUMBER OF RUNS (v) EQUAL TO NUMBER OF GROUPS 
{
  connectance=rbind(connectance,(S2[v]/sum(S)))                                # POPULATE 'CONNECTANCE' INDEX FOR GROUP v WITH TOTAL TROPHIC LINKAGES (S2) OF GROUP v DIVIDED BY TOTAL TROPHIC LINKAGES IN FOOD WEB
  
  if (connectance[v]>0.04)                                                     # CONDITION: IF 'CONNECTANCE' INDEX FOR GROUP v IS HIGHER THAN 0.04, THEN
  {KEYconnectance=rbind(KEYconnectance,KEYconnectance[v]<-'KEY')}              # POPULATE 'KEYCONNECTANCE' FOR GROUP v WITH 'KEY' AS THIS GROUP IS IDENTIFIED AS BEING KEY BY THE CONNECTANCE INDEX
  
  else                                                                         # IF 'CONNECTANCE' INDEX FOR GROUP v IS NOT HIGHER THAN 0.04 (Smith et al., 2011), THEN
  {KEYconnectance=rbind(KEYconnectance,KEYconnectance[v]<-'')}                 # LEAVE 'KEYCONNECTANCE' FOR GROUP v EMPTY AS THIS GROUP IS NOT IDENTIFIED AS BEING KEY BY THE CONNECTANCE INDEX
  
  sumpij<- rbind(sumpij,sum((p[v,])^2))                                        # POPULATE 'sumpij' FOR GROUP v WITH VALUE CALCULATED AS THE SUM OF ALL SQUARED DIET PROPORTION GROUP v REPRESENTS FOR EACH GROUP AS PREDATOR
  SURF<- rbind(SURF,sumpij[v]/sum(S))                                          # POPULATE 'SURF' INDEX FOR GROUP v AS 'sumpij' OF GROUP v DIVIDED BY TOTAL TROPHIC LINKAGES IN FOOD WEB
  
  if (SURF[v]>0.001)                                                           # CONDITION: IF 'SURF' INDEX OF GROUP v IS HIGHER THAN 0.001 (Plaganyi and Essington, 2014), THEN
  {KEYSURF=rbind(KEYSURF,KEYSURF[v]<-'KEY')}                                   # POPULATE 'KEYSURF' FOR GROUP v WITH 'KEY' AS THIS GROUP IS IDENTIFIED AS BEING KEY BY THE MASS SURF INDEX
  else                                                                         # IF 'SURF' INDEX FOR GROUP v IS NOT HIGHER THAN 0.001, THEN
  {KEYSURF=rbind(KEYSURF,KEYSURF[v]<-'')}                                      # LEAVE 'KEYSURF' FOR GROUP v EMPTY AS THIS GROUP IS NOT IDENTIFIED AS BEING KEY BY THE MASS SURF INDEX
  
  esumpij<- rbind(esumpij,sum((pE[v,])^2))                                     # POPULATE 'esumpij' FOR GROUP v WITH VALUE CALCULATED AS THE SUM OF ALL SQUARED DIET PROPORTION GROUP v REPRESENTS FOR EACH GROUP AS PREDATOR
  energy<- rbind(energy,esumpij[v]/sum(ES))                                    # POPULATE 'energy' INDEX FOR GROUP v AS 'esumpij' OF GROUP v DIVIDED BY TOTAL TROPHIC LINKAGES IN FOOD WEB
  
  if (energy[v]>0.001)                                                         # CONDITION: IF 'SURF' INDEX OF GROUP v IS HIGHER THAN 0.001 (Surma et al., 2022), THEN 
  {KEYenergy=rbind(KEYenergy,KEYenergy[v]<-'KEY')}                             # POPULATE 'KEYSURF' FOR GROUP v WITH 'KEY' AS THIS GROUP IS IDENTIFIED AS BEING KEY BY THE ENERGY SURF INDEX
  else                                                                         # IF 'ENERGY' SURF INDEX FOR GROUP v IS NOT HIGHER THAN 0.001, THEN
  {KEYenergy=rbind(KEYenergy,KEYenergy[v]<-'')}                                # LEAVE 'KEYENERGY' FOR GROUP v EMPTY AS THIS GROUP IS NOT IDENTIFIED AS BEING KEY BY THE ENERGY SURF INDEX
  
  if (group.names[v]=='Import'|group.names[v]=='Discards'|
      group.names[v]=='By-catch')                                              # CONDITION: IF GROUP v IS EITHER 'IMPORT','BY-CATCH', OR 'DISCARDS', THEN 
  {
    KEYconnectance[v]<-''                                                      # LEAVE 'KEYCONNECTANCE' EMPTY
    KEYSURF[v]<-''                                                             # LEAVE 'KEYSURF' EMPTY
    KEYenergy[v]<-''                                                           # LEAVE 'KEYENERGY' EMPTY
  }
}

# NAME COLUMNS
colnames(connectance)<-'connectance'                                                                          
colnames(KEYconnectance)<-'KEYconnectance'
colnames(SURF)<-'SURF mass'
colnames(KEYSURF)<-'KEYSURF mass'
colnames(energy)<-'SURF energy'
colnames(KEYenergy)<-'KEYSURF energy'

# PASTE ALL COLUMNS WITH DATA INTO ONE DATA FRAME
indices.agg<-data.frame(group.names,round(connectance,10),KEYconnectance,round(SURF,10),KEYSURF, round(energy,10),KEYenergy)   
worksheetmassagg<-data.frame(group.names,S,S2,sumpij,round(connectance,10),KEYconnectance,round(SURF,10),KEYSURF,p)
worksheetenergyagg<-data.frame(group.names,ES,esumpij,round(energy,10),KEYenergy,pE)
list_of_datasets <- list("Table A - Model Parameters" = group.data, 
                         "Table B - Diet Mass" = worksheetmass, 
                         "Table C - Diet Energy" = worksheetenergy, 
                         "Table D - Diet Mass Agg" = worksheetmassagg, 
                         "Table E - Diet Energy Agg" = worksheetenergyagg)     # CREATE LIST OF DATA FOR SEPERATE EXCEL DATASHEETS

# *** WRITE OUTPUT FILES ***
write.xlsx(indices.agg, 
           file.path(output, paste0(geography, "_indices_agg.xlsx")))          # WRITE EXCEL FILE WITH THE THREE INDICES VALUES FOR ALL FUNCTIONAL GROUPS IN AGGREGATED MODEL
write.xlsx(list_of_datasets, 
           file.path(output, paste0(geography, "_workbook.xlsx")))             # WRITE EXCEL FILE OF THE COMPLETE WORKBOOK INCLUDING THE SEPERATE DATASHEETS

}
