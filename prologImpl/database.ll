%% File: database.ll

%% A simple database update scheme.  

db <- write('Command: ')  x read(Command) x do(Command).
db <- write('Try again.') x nl x db.

do(enter(Entry))   <- entry(Entry) -> db.
do(commit(Entry))  <- entry(Entry) => db.
do(retract(Entry)) <- entry(Entry) x db.
do(update(Old,New))   <- entry(Old) x (entry(New) -> db).
do(check(Q)) <-
  (entry(Q)  x write(Q) x
   write(' is an entry.') x nl x erase) & db.
do(necessary(Q)) <-
  (bang(entry(Q)) x write(Q) x 
   write(' is a necessary entry') x nl x erase) &
   db.
do(quit) <- erase.

%% Try the following commands to the data base prompt.
%enter(enrolled(jane,cs1)).
%check(enrolled(jane,X)).
%update(enrolled(jane,cs1),enrolled(jane,cs2)).
%check(enrolled(jane,X)).
%commit(student(jane)).
%enter(student(bob)).
%necessary(student(X)).
%retract(enrolled(jane,X)).
%necessary(student(bob)).
%quit.