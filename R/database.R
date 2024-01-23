#
# database
#
# table:
#
#library(RSQLite)

db_connect <- function(filename=paste0(path.expand('~'),"/","keira.db")) {
  con <- dbConnect(SQLite(), dbname = filename)
  return(con)
}

db_item_in_database <- function(con, item_text) {

}

get_all_items <- function(con) {
  query <- "SELECT * from item"
  dbExecute(con, query)
}

get_all_exams <- function(con) {
  query <- "SELECT * from exam"
  rs <- dbSendQuery(con, query)
  dbFetch(rs)
}

#db_add_get_next_exam_id <- function(con) {
#  query <-
#}
qt <- function(x) {
   paste0("'",x,"'",sep="",collapse="")
}

db_add_exam <- function(con, module, author,
                        name, date) {
  query <- paste0("INSERT INTO exam (module, author, name, date) VALUES (",
  paste(c(qt(module), qt(author), qt(name), qt(date)),collapse=","),")")
  cat(query)
  result <- dbExecute(con, query)

  query <- "SELECT max(id) from exam"
  dbFetch(dbSendQuery(con, query))[1,1]
}

db_add_item <- function(con, item_text) {
  query <- paste0("INSERT INTO item (text,version) VALUES (",
  paste(c(item_text,version),collapse=","),")")
  result <- dbSendQuery(con, query)
  data <- dbFetch(result)

}

db_create <- function(con)
{
  query <- "CREATE TABLE item (
   id INTEGER PRIMARY KEY AUTOINCREMENT,
   text TEXT,
   version INT,
   active BOOLEAN,
   md5 CHAR(32)
  )"
  result <- dbSendQuery(con, query)
  data <- dbFetch(result)

  query <- "CREATE TABLE exam (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  module VARCHAR(250),
  author VARCHAR(250),
  name VARCHAR(250),
  date DATE)"

  result <- dbSendQuery(con, query)


  query <- "CREATE TABLE result (
  exam_id INT,
  item_id INT,
  difficulty FLOAT
  )"
  result <- dbSendQuery(con, query)

  query <- "CREATE TABLE keywords (
  item_id INT,
  keyword VARCHAR(100)
  )"
  result <- dbSendQuery(con, query)
}

# add items to a database
if (FALSE) {
con <- db_connect()
db_create(con)

exam_id <- db_add_exam(con, "M25","Brandmaier","Forschungsmethodik I", "2022-10-08")
#get_all_exams(con)
item_id <- db_add_item(con, item_text = "Hallo wie geht's?!")
db_add_link_exam_item(con, exam_id, item_id)
dbDisconnect(con)
}
