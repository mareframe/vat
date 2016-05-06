create_pred_prey_encounter<-function(obj){
  #reads in the object previously created using create_vadt()
  PREY_SELECT<-NULL
  PRED_SELECT<-NULL
  
  for(g in 1:(length(obj$fgnames))){
    thisCode<-obj$fun_group$Code[g]
    thisName<-obj$fgnames[g]
    cat("\n",thisName,"-->")
    if(thisCode %in% obj$invert_names$Code){
      thisNumCohorts<-obj$invert_names$NumCohorts[obj$invert_names$Code==thisCode]
      if(thisNumCohorts>1){
        thisVars<-paste(thisName,"_N",seq(1,2),sep="")
        #add up the data for both juvenile and adult 
        for(j in 1:(length(thisVars))){
          #thisI <- thisVars[j]  ## is this what thisI should be?
          refErla<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==thisVars[j]]
          thisData<-obj$erla_plots[[refErla]]
          if(j==1){
            tmp <- thisData
          } else{
            tmp$number<-tmp$number+thisData$number
          }
        }
      } else{
        thisVar<-paste(thisName,"_N",sep="")
        refErla<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==thisVar]
        thisData<-obj$erla_plots[[thisVar]]
        tmp <- thisData
      }
      
    } else{
      thisVars<-paste(thisName,c(" Juvenile"," Adult"),sep="")
      #add up the data for both juvenile and adult 
      for(j in 1:(length(thisVars))){
        refErla<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==thisVars[j]]
        thisData<-obj$erla_plots[[refErla]]
        if(j==1){
          tmp <- thisData
        } else{
          tmp$number<-tmp$number+thisData$number
        }
      }
    }
      
    tmp$Time <- as.numeric(as.character(tmp$Time)) * obj$toutinc / 365 + obj$startyear
      
      ##DO THE PREDATORS OF THIS GROUP FIRST
      cat("Getting predators...")
      #this is the potential predators for the selected prey
      pPRED<-obj$pPREY[rownames(obj$pPREY)==thisCode,]
      index<-pPRED>0
      if(length(pPRED[index])>0){
        thisPredators<-colnames(pPRED)[index]
        thisPredatorNames<-thisPredators
        
        #try getting data for all other species and testing for overlap
        test<-tmp
        
        test$FullInt<-rep(1,nrow(test))
        fullByBox<-tapply(test$FullInt,test$Box,sum,na.rm=TRUE)
        
        byBox<-data.frame(matrix(NA,nrow=obj$numboxes,ncol=length(thisPredators)))
        colnames(byBox)<-thisPredators
        
        for(i in 1:(length(thisPredators))){
          ##NEED TO HANDLE INVERTS IN HERE
          #######################
          thisPredCode<-gsub("[\\d]","",thisPredators[i],perl=TRUE)
          thisPredAge<-get_first_number(thisPredators[i])
          thisPredName<-str_trim(obj$fgnames[match(thisPredCode,obj$fun_group$Code)])
          varPredName<-ifelse(thisPredAge==1,paste(thisPredName," Juvenile",sep=""),paste(thisPredName," Adult",sep=""))
          cat(varPredName,"-->")
          thisI<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==varPredName]
          thisData<-obj$erla_plots[[thisI]]
          
          test$numberPred<-thisData$number
          test$interact<-mapply(FUN=function(x,y){ifelse((x>0) & (y>0),1,0)},tmp$number,thisData$number)
          
          thisByBox<-tapply(test$interact,test$Box,sum,na.rm=TRUE)
          byBox[,i]<-thisByBox/fullByBox
          
        }
        PREY_SELECT[[thisCode]]<-byBox
      }
    
        ##NOW DO THE PREYS OF THIS GROUP FIRST
        cat("Getting preys ...")
        #this is the potential predators for the selected prey
        pPRED<-obj$pPREY[,grep(thisCode,colnames(obj$pPREY))] ##NEED TO ADD INVERTS INTO obj$pPREY AS PREDATORS
        pPRED$total<-apply(pPRED,1,sum)
        index<-pPRED$total>0
        if(length(pPRED[index,1])>0){
          thisPreys<-rownames(pPRED)[index]
          thisPreyNames<-thisPreys
          #get data for all available prey species and test for overlap
          test<-tmp
          
          test$FullInt<-rep(1,nrow(test))
          fullByBox<-tapply(test$FullInt,test$Box,sum,na.rm=TRUE)
          
          byBox<-data.frame(matrix(NA,nrow=obj$numboxes,ncol=length(thisPreys)))
          colnames(byBox)<-thisPreys
          
          for(i in 1:(length(thisPreys))){
            thisPreyCode<-gsub("[\\d]","",thisPreys[i],perl=TRUE)
            thisPreyName<-str_trim(obj$fgnames[match(thisPreyCode,obj$fun_group$Code)])
            cat(thisPreyName,"-->")
            #I have prey grouped together for juveniles and adults, so will need to check to encounters with both where the prey is a vertebrate
            #DO INVERT PREYS FIRST
            if(thisPreyCode %in% obj$invert_names$Code){
              #WITH AGE-STRUCTURE
              thisNumCohorts<-obj$invert_names$NumCohorts[obj$invert_names$Code==thisPreyCode]
              if(thisNumCohorts>1){
                test$numberPrey1<-0*(test$number)
                test$numberPrey2<-0*(test$number)
                for(age in 1:2){
                  varPreyName<-ifelse(age==1,paste(thisPreyName,"_N",age,sep=""),paste(thisPreyName,"_N",age,sep=""))
                  thisI<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==varPreyName]
                  thisData<-obj$erla_plots[[thisI]]
                  
                  test[,paste("numberPrey",age,sep="")]<-thisData$number
                  
                }
                #this bit is slow. replace..?
                test$interact<-mapply(FUN=function(x,y,z){ifelse((x>0) & ((y>0) | (z>0)),1,0)},test$number,test$numberPrey1,test$numberPrey2)
                
                thisByBox<-tapply(test$interact,test$Box,sum,na.rm=TRUE)
                byBox[,i]<-thisByBox/fullByBox
                
              } else{
                #INVERT PREYS WITHOUT AGE-STRUCTURE
                varPreyName<-paste(thisPreyName,"_N",sep="")
                thisI<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==varPreyName]
                thisData<-obj$erla_plots[[thisI]]
                
                test[,"numberPrey"]<-thisData$number
     
                test$interact<-mapply(FUN=function(x,y){ifelse((x>0) & (y>0),1,0)},test$number,test$numberPrey)
                
                thisByBox<-tapply(test$interact,test$Box,sum,na.rm=TRUE)
                byBox[,i]<-thisByBox/fullByBox
                
              }
              
              
            } else{
              #NOW DO VERTEBRATE PREYS
              test$numberPrey1<-0*(test$number)
              test$numberPrey2<-0*(test$number)
              for(age in 1:2){
                varPreyName<-ifelse(age==1,paste(thisPreyName," Juvenile",sep=""),paste(thisPreyName," Adult",sep=""))
                thisI<-seq(1,(length(obj$erla_plots)))[names(obj$erla_plots)==varPreyName]
                thisData<-obj$erla_plots[[thisI]]
                test[,paste("numberPrey",age,sep="")]<-thisData$number
              }

              test$interact<-mapply(FUN=function(x,y,z){ifelse((x>0) & ((y>0) | (z>0)),1,0)},test$number,test$numberPrey1,test$numberPrey2)
              
              thisByBox<-tapply(test$interact,test$Box,sum,na.rm=TRUE)
              byBox[,i]<-thisByBox/fullByBox

            }
            
          }
          
          PRED_SELECT[[thisCode]]<-byBox 
      }
    }
  
  output <- list(disagg = obj$vars,invert_vars = obj$invert_vars, invert_mnames = obj$invert_mnames, trace_vars = obj$trace_vars, trace_names = obj$trace_names, var_names = obj$tot_num, max_layers = obj$max_layers, max_time = obj$max_time, bioagg_names = obj$bioagg_names, rs_names = obj$rs_names, tot_pred = obj$tot_pred, ssb_names = obj$ssb_names, yoy_names = obj$yoy_names, islands = obj$islands, rel_bio = obj$rel_bio, tot_bio = obj$tot_bio, ssb = obj$ssb, yoy = obj$yoy, structN = obj$structN, reserveN = obj$reserveN, totalnums = obj$totalnums, map_base = obj$map_base, numboxes = obj$numboxes, fun_group = obj$fun_group, invert_names = obj$invert_names, invert_l = obj$invert_l, vert_l = obj$vert_l, ab_params = obj$ab_params, diet_l = obj$diet_l, erla_plots = obj$erla_plots, toutinc = obj$toutinc, startyear = obj$startyear, tot_bio_v = obj$tot_bio_v, tot_bio_i = obj$tot_bio_i, biomass_by_box = obj$biomass_by_box, fgnames = obj$fgnames,fgcodes=obj$fgcodes,pPREY=obj$pPREY,PREY_SELECT=PREY_SELECT,PRED_SELECT=PRED_SELECT)
  cat("### ------------ vat object created, you can now run the vat application ------------ ###\n") 
  return(output)
  class(output) <- "vadt"

}