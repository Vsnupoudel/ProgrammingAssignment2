rankhospital<-function(st, outcome, ranking){
    
    condition<-search_list_return_name( toupper(outcome))
    if (st %in% data$State){
        if (!is.null(condition) ){
            data<-read.csv("outcome-of-care-measures.csv")
            cn<-colnames(data)
            column<-cn[intersect( grep("(^Hospital.30.Day.Death)([.]+)",cn) , grep( paste0("[.]+",condition,"$"),cn)  ) ]  #get Field
            data1<-data[which(data$State==st), c("Hospital.Name","State",column) ]
            
            #Change the column attributes to Character and Numeric
            data1[,column]=as.numeric(as.character( data1[,column]))
            #removing NA in the input column
            data1<-data1[!is.na(data1[[column]]),]
            data1$Hospital.Name=as.character( data1$Hospital.Name)
            data1$State=as.character( data1$State)
            #start ordering the subset 
            #first sort by Hospital Name
            data1<-data1[order(data1$Hospital.Name),]
            data2<-data1[order(data1[[column]])    ,]
            data2$rank<-rank(data2[[column]], ties.method="first")
            
            #return the name for best and worst
            # worst<-data2$Hospital.Name[ which( max(data2[[column]] , na.rm=TRUE)== data2[[column]] )]
            #worst_sorted<- sort( as.character(worst) )
            if(ranking=="worst")  return( data2$Hospital.Name[max(data2$rank)] )
            if(ranking=="best")  return( data2$Hospital.Name[1] )
            
            #for the rest
            data2$Hospital.Name[which(data2$rank==ranking)]
            
        }
        else{stop("invalid outcome")}
    }
    else{stop("invalid state")}
}


search_list_return_name<- function(item_name=string){
    list_name<- list(c("HEARTATTACK" , "HEART ATTACK", "HEART_ATTACK" ,"HEART-ATTACK" ,"ATTACK"),
                     c("HEARTFAILURE" ,"HEART FAILURE", "HEART_FAILURE", "HEART-FAILURE" ,"FAILURE" ),
                     c("PNEUMONIA"))
    names(list_name)<-c("Heart.Attack","Heart.Failure","Pneumonia")
    for(i in names(list_name)){ if (item_name %in% list_name[[i]]) return(i) }
}