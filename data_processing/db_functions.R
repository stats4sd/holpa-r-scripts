## 24 May
## functions.R
## Sam
## Some convenience functions to help connecting R to an SQL server and working with the data

## set default values for things likely to be constant across different servers

DB_HOST<-'127.0.0.1'
DB_PORT<-3307
SSH_USER<-'forge'
SSH_PORT<-3306
DB_USER<-'forge'

## forge_tunnel() sets up the connection between the private SSH key and the server
# this uses the ssh package to build the connection and then the sys package to run it in the background
# I am not sure how well this will work on a mac - definitely needs testing
# Only required arguments are IP and SSH local filepath
# Other arguments follow the defaults set above
forge_tunnel<-function(ip,
                       ssh_priv_local,
                       db_host=DB_HOST,
                       db_port=DB_PORT,
                       ssh_user=SSH_USER,
                       ssh_port=SSH_PORT){
  
tryCatch(file.exists(ssh_priv_local))
  if(file.exists(ssh_priv_local)==FALSE){
    stop("No file found at location ssh_priv_local")
  }
  target <- paste(db_host,ssh_port,sep=":")
  ProxySev <- paste(ssh_user,ip,sep="@") 
  
  cmd <- paste0('ssh::ssh_tunnel(ssh::ssh_connect(host = "',ProxySev,'", 
              keyfile ="',ssh_priv_local, '"),
              port = ',db_port,
                ', target = "', target, '")')
  
return(cmd)
}

## get_all_data() is a quick way of grabbing all data from an SQL database, putting it into a list of data frames, and naming the list to match the table names

get_all_data<-function(con=conn){
 
  table_list<-dbGetQuery(con, "SHOW TABLES")
  all_tables<-map(table_list[[1]],
                  function(table)dbGetQuery(con,paste("SELECT * FROM",table)))
  
  names(all_tables)<-table_list[[1]]
  
  return(all_tables)
  
}

## un_json_column() takes a json column containing multiple fields, splits it up, and then merges it back into the original data frame
#Big assumption on having a flat json column - may be worth extending this to deal with nested json columns in the future

un_json_column<-function(df,column){
  
  df$new_uid<-1:nrow(df)
  
  
  un_jsoned<-map_df(df$new_uid,
             function(x)fromJSON(select(df,{{column}})[[1]][df$new_uid==x]))
  
  df %>%
    select(-{{column}}) %>%
    bind_cols(un_jsoned) %>%
    return()
}


## format_df_fromclass() converts a data frame based on column names and matching R class names

#The ordering of factors and ordered factors is not neccessarily going to be
#sensible, but it is a quick fix - particularly for numerics - to do something
#efficiently Error message specifically highlights if you are trying to do
#anything with dates - not really sensible here since it is more likely to mess
#up than fix things - you'd need to think a bit more! This is also liable to
#mess up if converting from a factor into anything else (except a character).
#Assumption is that this would only every be used to convert from a bit df full of characters

valid_classes<-c("character",
                 "complex",
                 "double",
                 "expression",
                 "integer",
                 "list",
                 "logical",
                 "numeric",
                 "single",
                 "raw",
                 "factor",
                 "ordered")

format_df_fromclass<-function(df,cols,classes){
  
  if(length(cols)!=length(classes)){
    stop("length(cols)!=length(classes)")
  }
  if(any(classes=="Date")){
   stop("I've not allowed for dates to be coerced in this way - please pay attention those columns individually.")
  }
  
  if(!any(unique(classes)%in%valid_classes)){
    fail_class<-paste(unique(classes)[!unique(classes)%in%valid_classes],collapse="; ")
    stop(paste(fail_class,"not a valid class type for function."))
  }
  
  for(i in cols){
    if(i%in%colnames(df)){
      df[[i]]<-eval(call(paste0("as.",classes[cols==i]),df[[i]]))
    }
    if(!i%in%colnames(df)){
      warning(paste("Skipping column",i,". Not found in df"))
    }
    
  }
return(df)
  
}


