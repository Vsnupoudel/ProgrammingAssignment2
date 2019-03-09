best<-function(st, condition){
    data<-read.csv("outcome-of-care-measures.csv")
    cn<-colnames(data)
    condition<-search_list_return_name( toupper(condition))
    if (st %in% data$State){
        if (!is.null(condition) ){
           column<-cn[intersect( grep("(^Hospital.30.Day.Death)([.]+)",cn) , grep( paste0("[.]+",condition,"$"),cn)  ) ]  #get Field
           data1<-data[which(data$State==st), c("Hospital.Name","State",column) ]
           data1[,column]=as.numeric(as.character( data1[,column]))
           best<-data1$Hospital.Name[ which( min(  data1[[column]] , na.rm=TRUE )== data1[[column]] )]
           as.character(best)
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