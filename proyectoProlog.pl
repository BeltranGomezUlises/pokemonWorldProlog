/* Mini Pokemon World */

%Base de Conocimiento

%Pokemons
% (Id, Nombre, Tipo, Fuerza, Estado, EXP)
%fuego
pokemon(1, charmander, fuego, 10, normal, 0).
pokemon(2, charmeleon, fuego, 20, normal, 100).
pokemon(3, charizard, fuego, 30, normal, 200).

pokemon(4, bulpix, fuego, 10, normal, 0).
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

pokemon(28, electrode, electrico, 27, normal, 130).

%normal
pokemon(14, rattata, normal, 12, normal, 0).
pokemon(15, raticate, normal, 25, normal, 90).
pokemon(16, meowth, normal, 13, normal, 0).
pokemon(17, persian, normal, 28, normal, 120).
pokemon(18, snorlax, normal, 30, normal, 0).


printEvoluciones():-
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

poderAtaque(1).
poderAtaque(2).
poderAtaque(3).

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

%Pokebolas(nombre, cantidadObtenidas)
pokebola(pokeball, 5).
pokebola(superball, 0).
pokebola(ultraball, 0).
pokebola(masterball, 0).

%Medallas(nombre, ganada)
medalla(ninguna, si).
medalla(puno, no).
medalla(tallo, no).
medalla(lluvia, no).
medalla(relampago, no).
medalla(volcan, no).
medalla(arcoiris, no).

dinero(100).

%PC 


%Ciudades

ciudadPokemon(1, paleta).
ciudadPokemon(2, verde).
ciudadPokemon(3, azulona).
ciudadPokemon(4, plateada).
ciudadPokemon(5, celeste).
ciudadPokemon(6, lavanda).
ciudadPokemon(7, carmin).

ciudadAnterior(1, paleta).
ciudadSiguiente(X, Y):- 
    ciudadAnterior(X, _),
    X1 is X + 1, 
    ciudadPokemon(X1, Y).

distanciaCiudades(paleta, verde, 5).
distanciaCiudades(verde, azulona, 10).
distanciaCiudades(azulona, plateada, 8).
distanciaCiudades(plateada, celeste, 10).
distanciaCiudades(celeste, lavanda, 10).
distanciaCiudades(lavanda, carmin, 15).

%Poketienda

precio(pokeball, 10).
precio(superball, 15).
precio(ultraball, 25).
precio(masterball, 100).

mostrarMenu :-
    write("*** PokeMenu ***"), nl,
    write("1.- Pokemons"), nl,
    write("2.- Pokemochila"), nl,
    write("3.- Ficha de Entrenador"), nl,
    write("4.- Info"), nl,
    write("5.- Salir"), nl,
    write("¿Que deseas hacer? (Numero)"), nl,
    read(X), menuPrincipalController(X).

menuInfo :-
    write("--- Informacion ---"), nl,
    write("1.- Pokebolas"), nl,
    write("2.- Evoluciones"), nl,
    write("3.- Huevos"), nl,
    write("4.- Ciudades"), nl,
    write("5.- Salir"), nl,
    write("Obtener info sobre: "),
    read(X), menuInfoController(X).

menuItemPokebolas :-
    write("----- Pokebolas -----"), nl,
    write(" Tipo         Precio"), nl,
    forall(precio(Nombre, Precio),
    (write("-"), write(Nombre), write("\t"), write(Precio), write(" pokes"), nl)).
    

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
        menuPokemochila;
    (X = 3) ->
        menuFichaEntrenador;
    (X = 4) ->
        menuInfo;
    (X = 5) ->
        write("Salir del menu principal");
    ((X < 1); (X > 5)) ->
        vuelveAIntentar(Y),
        menuPrincipalController(Y)
).

menuPokemochila :-
    write("----- Pokemochila -----"), nl,
    write("Tengo:"), nl,
    forall(pokebola(P, C),
    ( C \= 0,
    write(" -"), write(C), write(" "), write(P), nl)).


menuFichaEntrenador :-
    dinero(D),
    write("----- Ficha de Entrenador -----"), nl,
    write("Dinero: "), write(D), write(" pokes"), nl,
    write("Medallas Obtenidas: "), nl,
    forall(medalla(X, si),
    (write("- "), write(X), nl)).

menuInfoController(X) :-
(   (X = 1) ->
        menuItemPokebolas;
    (X = 2) ->
        menuInfoEvo;
    (X = 3) ->
        menuItemHuevos;
    (X = 4) ->
        menuItemCiudades;
    ((X < 1); (X > 4)) ->
        vuelveAIntentar(Y),
        menuInfoController(Y)
).

menuInfoEvo :-
    write("----- Evoluciones Pokemon ------"), nl,
    forall(evolucion(P, Evo, Exp),
    (write(" - "), write(P), write(" evoluciona a "),
     write(Evo), write(" con "), write(Exp), write(" exp"), nl)).
    

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
    numPokemons(N),
    N < 6,
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

menuViaje :-
    write("Elige el numero de tu accion:"),nl,
    write("1. Viajar a la siguiente ciudad"), nl,
    write("2. Regresar"), nl,
    read(X), menuViajeController(X).

menuViajeController(X) :-
    (
        (X = 1) ->
            bienvenidaCiudad;
        (X = 2) ->
            menuCiudad;
        ((X < 1); (X > 2)) ->
            vuelveAIntentar(Y),
            menuViajeController(Y)
    ).

bienvenidaCiudad :-
    ciudadSiguiente(A, C),
    asserta(ciudadAnterior(A + 1, C)),
    retract(ciudadAnterior(A,_)),
    write("Has llegado a ciudad "), 
    write(C), nl,
    menuCiudad.

menuCiudad :-
    write("Elige el numero de tu accion:"),nl,
    write("1. Ir a la Enfermeria"), nl,
    write("2. Ir a la Tienda"), nl,
    write("3. Ir al Gimnasio"), nl,
    write("4. Abrir el menu"), nl,
    read(X), menuCiudadController(X).

menuCiudadController(X) :-
    (
        (X = 1) ->
            write("Enfermeria");
        (X = 2) ->
            menuTienda;
        (X = 3) ->
            write("Gimnasio");
        (X = 4) ->
            mostrarMenu;
        ((X < 1); (X > 4)) ->
            vuelveAIntentar(Y),
            menuCiudadController(Y)
    ).

inicio :-
    write("!Bienvenido al Mini Mundo Pokemon¡"), nl,
    write("Los desarrolladores Ricardo y Ulises esperamos que disfrutes de la aventura"), nl,
    write(" - El objetivo principal es obtener las 6 medallas de los gimnasios en cada ciudad"), nl,
    write(" - Tienes que viajar entre las ciudades para lograrlo, pero cuidado con los entrenadores y pokemons salvajes"), nl,
    write(" - El juego se termina si todos los pokemon de tu equipo se debilitan"), nl,
    write("!Mucha Suerte! Empecemos..."), nl, nl,
    primerPokemon.

menuTienda :-
    dinero(D),
    write(" --- Bienvenido a la Tienda Pokemon --- "), nl,
    write("Tienes: "), write(D), write(" pokes."), nl, nl,
    write("Elige el numero de tu accion:"),nl,
    write("1. Comprar pokeballs"), nl,
    write("2. Comprar superballs"), nl,
    write("3. Comprar ultraballs"), nl,
    write("4. Comprar masterball"), nl,
    write("5. Regresar"), nl,
    read(X), menuTiedaController(X).

menuTiedaController(X) :-
    (
        (X = 1) ->
            write("Cantidad "), read(C),
            comprar(C, 10);
        (X = 2) ->
            write("Cantidad "), read(C),
            comprar(C, 15);
        (X = 3) ->
            write("Cantidad "), read(C),
            comprar(C, 25);
        (X = 4) ->
            write("Cantidad "), read(C),
            comprar(C, 100);
        (X = 5) ->
            menuCiudad;
        ((X < 1); (X > 5)) ->
            vuelveAIntentar(Y),
            menuTiedaController(Y)
    ).

comprar(Cantidad, Precio) :-
    dinero(D),
    Total is Cantidad * Precio,
    D > Total,
    Resto is D - Total,
    asserta(dinero(Resto)),
    retract(dinero(D)),
    write("Te quedan: "), write(Resto), write(" pokes.").

menuEnfermeria :-
    write(" --- Bienvenido al Centro Pokemon --- "), nl,
    write("Elige el numero de tu accion:"),nl,
    write("1. Curar pokemons"), nl,
    write("2. Acceder al PC"), nl,
    write("3. Regresar"), nl,
    read(X), menuEnfermeriaController(X).

menuEnfermeriaController(X) :-
    (
        (X = 1) ->
            write("Todos tus pokemon han sido curados!");
        (X = 2) ->
            menuItemPC;
        (X = 3) ->
            write("Gimnasio");
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            menuCiudadController(Y)
    ).

menuItemPC(asd).
