library(RMySQL)
system('ssh -f root@backoffice.com.es  -L 3350:localhost:3306 -N')
conn=dbConnect(RMySQL::MySQL(), dbname = 'hotsql1', username = 'hotsql1',
          password = 'B6f505xNKd3pm7b', host = '127.0.0.1',port=3350)
