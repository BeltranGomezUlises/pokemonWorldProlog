/* Mini Pokemon World */

%Base de Conocimiento

%Pokemons
% (Id, Nombre, Tipo, Fuerza, Estado, EXP)
%fuego
pokemon(1, charmander, fuego, 10, normal, 0).
pokemon(2, charmeleon, fuego, 20, normal, 100).
pokemon(3, charizard, fuego, 30, normal, 200).
pokemon(4, vulpix, fuego, 10, normal, 0).
pokemon(5, nineTails, fuego, 25, normal, 150).
pokemon(6, growlithe, fuego, 10, normal, 0).
pokemon(7, arcanine, fuego, 25, normal, 250).

%agua
pokemon(8, squirtle, agua, 9, normal, 0).
pokemon(9, wartortle, agua, 19, normal, 100).
pokemon(10, blastoise, agua, 30, normal, 190).
pokemon(11, marril, agua, 7, normal, 0).
pokemon(12, azulMarril, agua, 16, normal, 140).
pokemon(13, magikarp, agua, 1, normal, 0).
pokemon(14, giaradous, agua, 28, normal, 250).

%planta
pokemon(15, bulbasaur, planta, 8, normal, 0).
pokemon(16, ivysaur, planta, 18, normal, 80).
pokemon(17, venasaur, planta, 29, normal, 190).
pokemon(18, bellsprout, planta, 9, normal, 0).
pokemon(19, weepinbell, planta, 18, normal, 120).
pokemon(20, victreebel, planta, 26, normal, 240).
pokemon(21, chikorita, planta, 10, normal, 0).
pokemon(22, bayleef, planta, 20, normal, 90).
pokemon(23, meganium, planta, 30, normal, 190).

%electrico
pokemon(24, pikachu, electrico, 13, normal, 0).
pokemon(25, raichu, electrico, 29, normal, 150).
pokemon(26, voltrob, electrico, 12, normal, 0).
pokemon(27, electrode, electrico, 27, normal, 130).
pokemon(28, electabuzz, electrico, 27, normal, 0).

%normal
pokemon(14, rattata, normal, 12, normal, 0).
pokemon(15, raticate, normal, 25, normal, 90).
pokemon(16, meowth, normal, 13, normal, 0).
pokemon(17, persian, normal, 28, normal, 120).
pokemon(18, snorlax, normal, 30, normal, 0).

%imprime las posibles evoluciones
printEvoluciones:-
  forall(evolucion(X, Y, Z),
  (write("pokemon: "), write(X), write(" evoluciona a: "), write(Y), write( " con experiencia: "), write(Z), nl)).

% (Pokemon, Evolucion, EXP)
evolucion(charmander, charmeleon, 100).
evolucion(charmeleon, charizard, 200).
evolucion(squirtle, wartortle, 100).
evolucion(wartortle, blastoise, 190).
evolucion(bulbasaur, ivysaur, 80).
evolucion(ivysaur, venasaur, 190).
evolucion(pikachu, raichu, 150).
evolucion(voltrob, electrode, 130).
evolucion(rattata, raticate, 90).
evolucion(meowth, persian, 120).

estado(normal).
estado(debilitado).

tipo(fuego).
tipo(agua).
tipo(planta).
tipo(normal).
tipo(electrico).

%(nombre, poder, tipoPokemon)
%fuego
ataque(lanzallamas, 9, fuego).
ataque(ascuas, 6, fuego).
ataque(patadaIgnea, 7, fuego).
ataque(llamarada, 10, fuego).
%agua
ataque(pistolaAgua, 7, agua).
ataque(burbujas, 6, agua).
ataque(surf, 8, agua).
ataque(hidrobomba, 10, agua).
%planta
ataque(latigoCepa, 4, planta).
ataque(hojasNavajas, 7, planta).
ataque(rayoSolar, 10, planta).
ataque(semillas, 6, planta).
%electrico
ataque(trueno, 10, electrico).
ataque(chispa, 5, electrico).
ataque(rayo, 9, electrico).
ataque(impactrueno, 7, electrico).
%normal
ataque(placaje, 6, normal).
ataque(aranazo, 6, normal).
ataque(hiperrayo, 10, normal).
ataque(cabezazo, 7, normal).

%pokemon y ataques
pokemonAtaque(charmander, [ascuas, patadaIgnea, aranazo, placaje]).
pokemonAtaque(charmeleon, [ascuas, patadaIgnea, aranazo, lanzallamas]).
pokemonAtaque(charizard, [ascuas, patadaIgnea, llamarada, lanzallamas]).
pokemonAtaque(vulpix, [ascuas, patadaIgnea, aranazo, placaje]).
pokemonAtaque(nineTails, [ascuas, patadaIgnea, aranazo, hiperrayo]).
pokemonAtaque(growlithe, [ascuas, patadaIgnea, aranazo, placaje]).
pokemonAtaque(arcanine, [lanzallamas, patadaIgnea, llamarada, cabezazo]).
pokemonAtaque(squirtle, [pistolaAgua, burbujas, aranazo, placaje]).
pokemonAtaque(wartortle, [pistolaAgua, burbujas, surf, placaje]).
pokemonAtaque(blastoise, [pistolaAgua, burbujas, surf, hidrobomba]).
pokemonAtaque(marril, [pistolaAgua, burbujas, aranazo, cabezazo]).
pokemonAtaque(azulMarril, [pistolaAgua, burbujas, surf, cabezazo]).
pokemonAtaque(magikarp, [pistolaAgua, cabezazo, aranazo, burbujas]).
pokemonAtaque(giaradous, [pistolaAgua, hiperrayo, surf, hidrobomba]).
pokemonAtaque(bulbasaur, [latigoCepa, hojasNavajas, placaje, semillas]).
pokemonAtaque(ivysaur, [latigoCepa, hojasNavajas, placaje, rayoSolar]).
pokemonAtaque(venasaur, [latigoCepa, placaje, hojasNavajas, rayoSolar]).
pokemonAtaque(bellsprout, [latigoCepa, hojasNavajas, placaje, aranazo]).
pokemonAtaque(weepinbell, [latigoCepa, placaje, hojasNavajas, rayoSolar]).
pokemonAtaque(victreebel, [latigoCepa, hojasNavajas, placaje, rayoSolar]).
pokemonAtaque(chikorita, [latigoCepa, placaje, hojasNavajas, aranazo]).
pokemonAtaque(bayleef, [latigoCepa, hojasNavajas, placaje, rayoSolar]).
pokemonAtaque(meganium, [latigoCepa, placaje, hojasNavajas, rayoSolar]).
pokemonAtaque(pikachu, [placaje, aranazo, impactrueno, chispa]).
pokemonAtaque(raichu, [rayo, trueno, impactrueno, chispa]).
pokemonAtaque(voltrob, [placaje, trueno, impactrueno, chispa]).
pokemonAtaque(pikachu, [placaje, rayo, impactrueno, chispa]).
pokemonAtaque(electrode, [placaje, rayo, impactrueno, chispa]).
pokemonAtaque(electabuzz, [placaje, rayo, impactrueno, chispa]).

% (Tipo, KM)
kmHuevo(fuego, 7).
kmHuevo(agua, 5).
kmHuevo(planta, 3).
kmHuevo(normal, 3).
kmHuevo(electrico, 5).

huevo(Tipo, X) :-
    kmHuevo(Tipo, X).

%Pokemochila
pokemochila(pokemons).
pokemochila(medallas).
pokemochila(dinero).
pokemochila(pokebolas).

pokebola(pokeball).
pokebola(superball).
pokebola(ultraball).
pokebola(masterball).

medalla(puno).
medalla(tallo).
medalla(lluvia).
medalla(relampago).
medalla(volcan).
medalla(arcoiris).

%Ciudades

ciudadPokemon(1, paleta).
ciudadPokemon(2, verde).
ciudadPokemon(3, azulona).
ciudadPokemon(4, plateada).
ciudadPokemon(5, celeste).
ciudadPokemon(6, lavanda).
ciudadPokemon(7, carmin).

ciudadSiguiente(X, Y):- X1 is X + 1, ciudadPokemon(X1, Y).

distanciaCiudades(paleta, verde, 5).
distanciaCiudades(verde, azulona, 10).
distanciaCiudades(azulona, plateada, 8).
distanciaCiudades(plateada, celeste, 10).
distanciaCiudades(celeste, lavanda, 10).
distanciaCiudades(lavanda, carmin, 15).

%Poketienda

precio(pokeball, 10).
precio(superball, 15).
precio(ultraball, 20).

mostrarMenu :-
    write("*** PokeMenu ***"), nl,
    write("1.- Pokemons"), nl,
    write("2.- Pokemochila"), nl,
    write("3.- Medallas"), nl,
    write("4.- Info"), nl,
    write("¿Que deseas hacer? (Numero)"), nl,
    read(X), menuPrincipalController(X).

menuInfo :-
    write("--- Informacion ---"), nl,
    write("1.- Pokebolas"), nl,
    write("2.- Pokemons"), nl,
    write("3.- Huevos"), nl,
    write("4.- Ciudades"), nl,
    write("5.- Salir"), nl,
    write("Obtener info sobre: "),
    read(X), menuInfoController(X).

menuItemPokebolas :-
    write("----- Pokebolas -----"), nl,
    write(" Tipo         Precio"), nl,
    write("Pokebola   10 pokes"), nl,
    write("Superball   15 pokes "), nl,
    write("Ultraball    20 pokes ").

menuItemCiudades :-
    write("----- Ciudades ------"), nl,
    write("Ciudad       Siguiente       Distancia"), nl, nl,
    write("Paleta         Verde           5 Kms"), nl,
    write("Verde          Azulona       10 Kms"), nl,
    write("Azulona       Plateada      8 Kms"), nl,
    write("Plateada      Celeste       10 Kms"), nl,
    write("Celeste       Lavanda        10 Kms"), nl,
    write("Lavanda      Carmin          15 Kms"), nl.

menuItemHuevos :-
    write("----- Huevos ------"), nl,
    write(" Tipo        Kms"), nl, nl,
    write("Normal       3"), nl,
    write("Planta         3"), nl,
    write("Agua          5"), nl,
    write("Fuego         7"), nl,
    write("Electrico     5").

menuPrincipalController(X):-
(   (X = 1) ->
        menuItemPokemons;
    (X = 2) ->
        write("Menu Pokemochila");
    (X = 3) ->
        write("Menu Medallas");
    (X = 4) ->
        menuInfo;
    (X = 5) ->
        write("Saliendo del menu");
    ((X < 1); (X > 5)) ->
        vuelveAIntentar(Y),
        menuPrincipalController(Y)
).

menuInfoController(X) :-
(   (X = 1) ->
        menuItemPokebolas;
    (X = 2) ->
        write("Pokemons"), nl;
    (X = 3) ->
        menuItemHuevos;
    (X = 4) ->
        menuItemCiudades;
    ((X < 1); (X > 4)) ->
        vuelveAIntentar(Y),
        menuInfoController(Y)
).

menuInfoPokemon :-
    write("----- Evoluciones Pokemon ------"), nl.

menuItemPokemons :-
    write("----- Mis Pokemons ------"), nl,
    misPokemon(P),
    obtenerListaNumerada(P, 0),
    write("Obtener info sobre: "), nl,
    read(X), menuPokemonController(P, X).

menuPokemonController(P, X) :-
    numPokemons(N),
    (
        ((X > 0), (X < N + 1)) ->
            getElement(P, X, R),
            pokemon(_, R, Tipo, _, Estado, Exp),
            write("Nombre: "), write(R), nl,
            write("Tipo: "), write(Tipo), nl,
            write("Estado: "), write(Estado), nl,
            write("Exp: "), write(Exp);
        ((X < 1); (X > N)) ->
            write("No"),
            vuelveAIntentar(Y),
            menuPokemonController(P, Y)
    ).

%Param(Lista, 0)

obtenerListaNumerada([], _).
obtenerListaNumerada([X | T], C):-
    C1 is C + 1,
    write(C1), write(".- "), write(X), nl,
    obtenerListaNumerada(T, C1).

misPokemon([]).
numPokemons(N) :-
    misPokemon(P),
    len(P, N).

agregarPokemon(Pokemon) :-
    misPokemon(P),
    append(P,[Pokemon], L),
    asserta(misPokemon(L)),
    retract(misPokemon(P)).

vuelveAIntentar(Y) :-
    write("Valor invalido, vuelva a intentarlo."), nl,
    read(Y).

primerPokemon :-
    write("Elige tu primer Pokemon: "),nl,
    write("1. Charmander"), nl,
    write("2. Bulbasaur"), nl,
    write("3. Squirtle"), nl,
    read(X), primerPokemonController(X).

primerPokemonController(X) :-
    (
        (X = 1) ->
            agregarPokemon(charmander);
        (X = 2) ->
            agregarPokemon(bulbasaur);
        (X = 3) ->
            agregarPokemon(squirtle);
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            primerPokemonController(Y)
    ).

menuBatalla :-
    write("Elige el numero de tu accion:"),nl,
    write("1. Atacar"), nl,
    write("2. Cambiar Pokemon"), nl,
    write("3. Salir"), nl,
    read(X), batallaController(X).

batallaController(X) :-
    (
        (X = 1) ->
            write("Ataque");
        (X = 2) ->
            write("Cambiar");
        (X = 3) ->
            write("");
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            batallaController(Y)
    ).

/* Largo de una lista */
len([],0).
len([_|T],N)  :-  len(T,X),  N  is  X+1.

%retorna el elemnto n de una lista
getElement([X|_], 1, X).
getElement([_|T], N, X):-
          N2 is N - 1,
          getElement(T, N2, X).

:- dynamic batallaTerminada/1.
batallaTerminada(no).

pelear:- asserta(batallaTerminada(no)), pelea.
pelea:- batallaTerminada(no), peleoYo, pelea.
pelea:- write("batalla terminada").

peleoYo:- write("tu pokemon es: "), misPokemon([H|_]),
          write(H), write(", sus ataque son: \n"),
          pokemonAtaque(H, L), write("\n elige uno: \n"),
          obtenerListaNumerada(L, 0),
          read(X),
          (
            (X = 1) -> pokemonAtaque(H, [Ataque|_] );
            (X = 2) -> pokemonAtaque(H, [_, Ataque|_] );
            (X = 3) -> pokemonAtaque(H, [_, _, Ataque|_] );
            (X = 4) -> pokemonAtaque(H, [_, _, _, Ataque|_] ), retractall(batallaTerminada(no))
          ),
          write("elegiste: "), write(Ataque), nl, nl.
