#===== STEP 4.2.2. CREATE FUNCTION TO USE LATER FOR AGGREGATING GROUPS IN DIET MATRICES ====

# CREATE FUNCTION THAT USES THE ASSIGNED PARAMETERS, SOME ARE BEING GENERATED IN LINES BELOW: COMBINE.GROUPS -> 264 AND NEW.GROUPNAME -> 270
aggregate.groups<-function(combine.groups,new.groupname,C,EC,B,QB,QBE,p,pE,group.names) {                     
  
  ngroups=length(B)                                                            # DETERMINE NUMBER OF GROUPS FROM NUMBER OF BIOMASSES
  
  other.groups=c()                                                             # CREATE VECTOR OF GROUPS THAT ARE NOT AGGREGATED
  for (i in 1:ngroups){                                                        # RUN A LOOP WITH NUMBER OF RUNS (i) EQUAL TO # ORIGINAL GROUPS
    group.index=which(combine.groups==i)                                       # DETERMINE WHETHER GROUP i IS GETTING COMBINED (TRUE) OR NOT (FALSE)
    if (length(group.index)==0){                                               # IF GROUP i IS NOT COMBINED (AND THUS INDEX IS FALSE AND LENGTH INDEX =0), THEN
      other.groups=c(other.groups,i)                                           # GROUPS NAME REMAINS UNCHANGED
    }
  }
  
  
  #====== CREATE TEMP VECTORS FOR BASIC MODEL PARAMETERS ADDING NEW GROUP =====
  
  
  B.tmp=as.matrix(c(B,sum(B[combine.groups])),nrow=ngroups+1)                  # ADD SUMMED BIOMASS OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF BIOMASS (B) VALUES 
  QB.newgroup=sum(QB[combine.groups]*B[combine.groups]/sum(B[combine.groups])) # CALCULATE CONSUMPTION/BIOMASS (QB) BY CALCULATING ACTUAL CONSUMPTION PER SEPERATE GROUPS DIVIDED BY BIOMASS OF AGGREGATED GROUP AND SUM THESE
  QB.tmp=as.matrix(c(QB,QB.newgroup),nrow=ngroups+1)                           # ADD CONSUMPTION/BIOMASS (QB) OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF QB VALUES
  QBE.newgroup=sum(QBE[combine.groups]*B[combine.groups]/sum(B[combine.groups])) # CALCULATE ENERGY CONSUMPTION/BIOMASS (QBE) BY CALCULATING ACTUAL ENERGY CONSUMPTION PER SEPERATE GROUPS DIVIDED BY BIOMASS OF AGGREGATED GROUP AND SUM THESE
  QBE.tmp=as.matrix(c(QBE,QBE.newgroup),nrow=ngroups+1)                        # ADD ENERGY CONSUMPTION/BIOMASS (QBE) OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF QBE VALUES
  group.names.tmp=as.matrix(c(group.names,new.groupname),nrow=ngroups+1)       # ADD GROUP NAME OF AGGREGATE GROUP TO THE BOTTOM OF EXISTING LIST OF GROUP NAMES
  
  
  #======= MASS DIET MATRIX =============================
  
  
  C.tmp.pred=rowSums(C[,combine.groups])                                       # CALCULATE ACTUAL CONSUMPTION IN MASS OF EACH PREY BY AGGREGATED GROUP AS PREDATOR BY SUMMING ACTUAL CONSUMPTION OF PREY IN MASS BY SEPERATE GROUPS 
  p.tmp.pred=C.tmp.pred/sum(C.tmp.pred)                                        # CALCULATE DIET PROPORTION (p) OF EACH PREY IN DIET AGGREGATED GROUP BY DIVIDING ACTUAL CONSUMPTION OF THAT PREY IN MASS BY TOTAL ACTUAL MASS CONSUMPTION OF AGGREGATE GROUP
  p.tmp.new=cbind(p,p.tmp.pred)                                                # ADD DIET COMPOSITION (p) OF AGGREGATE GROUP AS COLUMN TO EXISTING DIET MATRIX
  
  p.tmp.prey=colSums(p.tmp.new[combine.groups,])                               # CALCULATE DIET PROPORTION (p) THAT AGGREGATED GROUP REPRESENT AS PREY FOR EACH PREDATOR BY SUMMING THEIR PROPORTION (p) AS SEPERATE GROUPS
  p.tmp.new=rbind(p.tmp.new,p.tmp.prey)                                        # ADD ROW OF p-VALUES OF AGGREGATED GROUP AS PREY TO BOTTOM OF DIET MATRIX
  
  
  #======= ENERGY DIET MATRIX =============================
  
  
  EC.tmp.pred=rowSums(EC[,combine.groups])                                     # CALCULATE ACTUAL CONSUMPTION IN ENERGY OF EACH PREY BY AGGREGATED GROUP AS PREDATOR BY SUMMING ACTUAL CONSUMPTION OF PREY IN ENERGY BY SEPERATE GROUPS 
  pE.tmp.pred=EC.tmp.pred/sum(EC.tmp.pred)                                     # CALCULATE DIET PROPORTION (p) OF EACH PREY IN DIET AGGREGATED GROUP BY DIVIDING ACTUAL CONSUMPTION OF THAT PREY IN ENERGY BY TOTAL ACTUAL ENERGY CONSUMPTION OF AGGREGATE GROUP
  pE.tmp.new=cbind(pE,pE.tmp.pred)                                             # ADD DIET COMPOSITION (pE) OF AGGREGATE GROUP AS COLUMN TO EXISTING DIET MATRIX
  
  pE.tmp.prey=colSums(pE.tmp.new[combine.groups,])                             # CALCULATE ENERGY DIET PROPORTION (pE) THAT AGGREGATED GROUP REPRESENT AS PREY FOR EACH PREDATOR BY SUMMING THEIR PROPORTION (pE) AS SEPERATE GROUPS
  pE.tmp.new=rbind(pE.tmp.new,pE.tmp.prey)                                     # ADD ROW OF pE-VALUES OF AGGREGATED GROUP AS PREY TO BOTTOM OF DIET MATRIX
  
  
  #======= UPDATE NEW DIET MATRICES WITH NEW GROUP NAMES ======================
  
  
  other.groups=c(other.groups,n.groups+1)                                      # CREATE VECTOR WITH EXISTING GROUP NAMES PLUS ONE ROW
  p.new=p.tmp.new[other.groups,other.groups]                                   # CREATE NEW DIET MATRIX FOR SURF MASS-BALANCED INCLUDING AGGREGATED GROUP
  pE.new=pE.tmp.new[other.groups,other.groups]                                 # CREATE NEW DIET MATRIX FOR SURF ENERGY-BALANCED INCLUDING AGGREGATED GROUP
  new.colnames=colnames(p.new)                                                 # ASSIGN NEW COLUMN NAMES
  nr=length(new.colnames)                                                      # DETERMINE NUMBER OF GROUPS
  new.colnames[nr]=new.groupname                                               # DETERMINE NEW GROUP NAME AS THE LAST ONE
  colnames(p.new)=new.colnames                                                 # GIVE NEW DIET MATRIX FOR SURF MASS-BALANCED NEW COLUMN NAMES
  rownames(p.new)=c()                                                          # REMOVE ROW NAMES
  colnames(pE.new)=new.colnames                                                # GIVE NEW DIET MATRIX FOR SURF ENERGY-BALANCED NEW COLUMN NAMES
  rownames(pE.new)=c()                                                         # REMOVE ROW NAMES
  
  B.new=B.tmp[other.groups]                                                    # ADJUST BIOMASS (B) AS BASIC PARAMETER
  QB.new=QB.tmp[other.groups]                                                  # ADJUST CONSUMPTION/BIOMASS (QB) AS BASIC PARAMETER
  QBE.new=QBE.tmp[other.groups]                                                # ADJUST ENERGY CONSUMPTION/BIOMASS (QBE) AS BASIC PARAMETER
  group.names.new=group.names.tmp[other.groups]                                # ADJUST GROUP NAMES AS BASIC VECTOR
  
  Agg.model=list(B=B.new,QB=QB.new,QBE=QBE.new,
                 group.names=group.names.new,p=p.new,pE=pE.new)                # CREATE A LIST OF THE FUNCTION'S OUTCOMES WITH THE PARAMETERS YOU WANT TO BE PRODUCED 
}

