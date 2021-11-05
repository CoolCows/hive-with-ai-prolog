:-module(utils,[ push/3,
				 pop/2
				 ]).

push(X,Y,[X|Y]).

pop([_|T],T).
