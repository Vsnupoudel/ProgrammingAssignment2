rankall <- function(outcome, num ="best") {
    if(num=="best") num=as.numeric(1)
   
    condition<-search_list_return_name( toupper(outcome))
    if (is.null(condition) ){stop("invalid outcome")}
   #else
        data<-read.csv("outcome-of-care-measures.csv")
        
        #isolate the column for the given outcome
        cn<-colnames(data)
        column<-cn[intersect( grep("(^Hospital.30.Day.Death)([.]+)",cn) , grep( paste0("[.]+",condition,"$"),cn)  ) ] 
        data1<-data[, c("Hospital.Name","State",column) ]
        
        #Change the column attributes to Character and Numeric
        data1[,column]=as.numeric(as.character( data1[,column]))
        
        #After converting to numeric, REMOVE  NAs in the input column
        data1<-data1[!is.na(data1[[column]]),]
        
        #get the ranks of each state in a dataset
        data1<-data1[order(data1$Hospital.Name),]
        orders<-tapply(data1[[column]], data1$State, order)
        data2<-  split(data1, data1$State) 
        #final result with ranks in a new complete dataset
        dataset<-order_by_state(data2,orders)
        
        #in case of worst
        #gave up using vectorized for here.. using sqldf
        if(num=="worst"){ 
        library(sqldf)
           final<- sqldf(" select [Hospital.Name], State from (select [Hospital.Name], State, max(rank) 
                  from dataset group by State) order by State")
           return(final)
}
        #return only the asked rank 
        else{ final<-dataset[which(dataset$rank==num),]
        return(final)}
        
}

#function2

order_by_state<-function(data_in,orders){
    #set up an empty data frame
    f<-head(data1,0)
    f$rank<-numeric(0)
    for(VAR in names(orders)){
        dout<- as.data.frame(data_in[[VAR]])[orders[[VAR]],]  
        dout$rank<-rank(dout[[column]], ties.method="first")
    f<-    rbind(f,dout)
    }
    f
}

#function3
search_list_return_name<- function(item_name=string){
    list_name<- list(c("HEARTATTACK" , "HEART ATTACK", "HEART_ATTACK" ,"HEART-ATTACK" ,"ATTACK"),
                     c("HEARTFAILURE" ,"HEART FAILURE", "HEART_FAILURE", "HEART-FAILURE" ,"FAILURE" ),
                     c("PNEUMONIA"))
    names(list_name)<-c("Heart.Attack","Heart.Failure","Pneumonia")
    for(i in names(list_name)){ if (item_name %in% list_name[[i]]) return(i) }
}