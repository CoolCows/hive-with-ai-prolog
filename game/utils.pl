:-module(utils,[ push/3,
				 pop/3]).

% Some usefull predicates
push(X,Y,[X|Y]).

pop([X|_],_).
