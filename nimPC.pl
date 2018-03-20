:- dynamic palitos/1.

palitos([*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*]).
palitos2([*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*]).

empieza:-
	write("\n Puedes quitar 1, 2 o 3 palitos por turno \n
			PIERDE QUIEN RETIRA EL ULTIMO PALILLO \n\n"),
	retractall(palitos(_)), palitos2(X),assertz(palitos(X)),
	jugar.

jugar:- largo(L), L > 0 , jugador, compu, jugar.
jugar:- largo(L), L = 0.

largo(Y):-palitos(X), length(X,Y).

jugador:- largo(L), L = 1, write(" Ya perdiste queda solo 1 palito \n").
jugador:- largo(L), L > 1, turnoJugador.

compu:- imprimeQuedan, piensa(X), quitapalitos(X),
	write("Computadora quito "),
	write(X), write(" palitos. \n").

imprimeQuedan:- largo(X) ,
	write("\nQuedan "), write(X), write(" palitos \n"),
	palitos(Y), imprime(Y).

piensa(X,L):-  (L > 8; L = 5),  random(1,3,X).
piensa(X,L):-  L > 5, L < 9, X is L - 5.
piensa(X,L):-  L > 1, L < 5, X is L - 1.

quitapalitos(X):- palitos(P), quitar(X,P,Ps),
	retractall(palitos(_)), assertz(palitos(Ps)).

quitar(N, L, R):- numlist(1,N,Aux), quita(Aux,L,R).
quita([_|T],[_|Xs],R):- quita(T, Xs,R).
quita([],X,X).

turnoJugador:-
	imprimeQuedan,
	write("Cuantos palitos quitaras?  "),
	read(C), valida123(C,C1),
	quitapalitos(C1).

valida123(C,C):- C>0, C<4.
valida123(C,R):-
	(C<1; C>3),
	write("\n Validos solo 1,2 y 3... teclee otro: "),
	read(C1),
	valida123(C1, R).

imprime([H|T]):- write(H), imprime(T).
imprime([]):- nl.
