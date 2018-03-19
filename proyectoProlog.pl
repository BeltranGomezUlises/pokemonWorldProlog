/* Mini Pokemon World */

%Base de Conocimiento

%Pokemons
% (Id, Nombre, Tipo, Fuerza, Estado, EXP)
pokemon(1, charmander, fuego, 10, normal, 0).
pokemon(2, charmeleon, fuego, 20, normal, 100).
pokemon(3, charizard, fuego, 30, normal, 200).
pokemon(4, squirtle, agua, 9, normal, 0).
pokemon(5, wartortle, agua, 19, normal, 100).
pokemon(6, blastoise, agua, 30, normal, 190).
pokemon(7, bulbasaur, planta, 8, normal, 0).
pokemon(8, ivysaur, planta, 18, normal, 80).
pokemon(9, venasaur, planta, 29, normal, 190).
pokemon(10, pikachu, electrico, 13, normal, 0).
pokemon(11, raichu, electrico, 29, normal, 150).
pokemon(12, voltrob, electrico, 12, normal, 0).
pokemon(13, electrode, electrico, 27, normal, 130).
pokemon(14, rattata, normal, 12, normal, 0).
pokemon(15, raticate, normal, 25, normal, 90).
pokemon(16, meowth, normal, 13, normal, 0).
pokemon(17, persian, normal, 28, normal, 120).
pokemon(18, snorlax, normal, 30, normal, 0).


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

poderAtaque(1).
poderAtaque(2).
poderAtaque(3).

tipo(fuego).
tipo(agua).
tipo(planta).
tipo(normal).
tipo(electrico).

%(nombre, poder, usos)

ataque(lanzallamas, 10, 8).
ataque(ascuas, 6, 15).
ataque(pistolaAgua, 9, 10).
ataque(burbujas, 6, 15).
ataque(latigoCepa, 9, 12).
ataque(hojasNavajas, 5, 15).
ataque(trueno, 10, 8).
ataque(chispa, 5, 13).
ataque(placaje, 6, 15).
ataque(aranazo, 6, 15).
ataque(hiperrayo, 10, 8).
ataque(cabezazo, 7, 10).

%ataques(charmander, ascuas, lanzallamas).

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
        write("Menu Pokemons");
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

menuItemPokemons :-
    write("----- Mis Pokemons ------"), nl,
    misPokemon(P),
    obtenerListaPokemons(P, 0),
    write("Obtener info sobre: "),
    read(X), menuPokemonController(P, X).

menuPokemonController(P, X) :-
    numPokemons(N),
    (
        ((X > 0); (X < N)) ->
            getElement(P, X, R),
            pokemon(_, R, Tipo, _, Estado, Exp),
            write("Nombre: "), write(R), nl,
            write("Tipo: "), write(Tipo), nl,
            write("Estado: "), write(Estado), nl,
            write("Exp: "), write(Exp);
        ((X < 1); (X > N)) ->
            vuelveAIntentar(Y),
            menuPokemonController(P, Y)
    ).
    


obtenerListaPokemons([], _).
obtenerListaPokemons([X | T], C):-
    C1 is C + 1,
    write(C1), write(".- "), write(X), nl,
    obtenerListaPokemons(T, C1).


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