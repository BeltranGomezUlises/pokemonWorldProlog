/* Mini Pokemon World */

%Base de Conocimiento

%Pokemons
% (Id, Nombre, Tipo, Fuerza)
%Huevo
pokemon(0, huevo, huevo, 0).
%fuego
pokemon(1, charmander, fuego, 10).
pokemon(2, charmeleon, fuego, 20).
pokemon(3, charizard, fuego, 30).
pokemon(4, vulpix, fuego, 10).
pokemon(5, nineTails, fuego, 25).
pokemon(6, growlithe, fuego, 10).
pokemon(7, arcanine, fuego, 25).

%agua
pokemon(8, squirtle, agua, 9).
pokemon(9, wartortle, agua, 19).
pokemon(10, blastoise, agua, 30).
pokemon(11, marril, agua, 7).
pokemon(12, azulMarril, agua, 16).
pokemon(13, magikarp, agua, 1).
pokemon(14, giaradous, agua, 28).

%planta
pokemon(15, bulbasaur, planta, 8).
pokemon(16, ivysaur, planta, 18).
pokemon(17, venasaur, planta, 29).
pokemon(18, bellsprout, planta, 9).
pokemon(19, weepinbell, planta, 18).
pokemon(20, victreebel, planta, 26).
pokemon(21, chikorita, planta, 10).
pokemon(22, bayleef, planta, 20).
pokemon(23, meganium, planta, 30).

%electrico
pokemon(24, pikachu, electrico, 13).
pokemon(25, raichu, electrico, 29).
pokemon(26, voltrob, electrico, 12).
pokemon(27, electrode, electrico, 27).
pokemon(28, electabuzz, electrico, 27).

%normal
pokemon(29, rattata, normal, 12).
pokemon(30, raticate, normal, 25).
pokemon(31, meowth, normal, 13).
pokemon(32, persian, normal, 28).
pokemon(33, snorlax, normal, 30).

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

%PC bill (nombrePokemon, experiencia)
:-dynamic pcBill/2.

pcBill(pokemonPrueba, 100).

sacarPcBill(Pokemon):-
  pcBill(Pokemon, Y), agregarPokemon(Pokemon, Y), retract(pcBill(Pokemon, Y)).

almacenarPcBill(Pokemon, Exp) :-
    assertz(pcBill(Pokemon, Exp)).

numPokemonsPC(N) :-
    findall(Pokemon, pcBill(Pokemon,_), L),
    len(L, N).



%Poketienda

precio(pokeball, 10).
precio(superball, 15).
precio(ultraball, 25).
precio(masterball, 100).

mostrarMenu(MenuAnterior) :-
    write("*** PokeMenu ***"), nl,
    write("1.- Pokemons"), nl,
    write("2.- Pokemochila"), nl,
    write("3.- Ficha de Entrenador"), nl,
    write("4.- Info"), nl,
    write("5.- Salir"), nl,
    write("¿Que deseas hacer? (Numero)"), nl,
    read(X), menuPrincipalController(X, MenuAnterior).

menuInfo(MenuAnterior) :-
    write("--- Informacion ---"), nl,
    write("1.- Pokebolas"), nl,
    write("2.- Evoluciones"), nl,
    write("3.- Huevos"), nl,
    write("4.- Ciudades"), nl,
    write("5.- Salir"), nl,
    write("Obtener info sobre: "),
    read(X), menuInfoController(X, MenuAnterior).

menuItemPokebolas :-
    write("----- Pokebolas -----"), nl,
    write(" Tipo         Precio"), nl,
    forall(precio(Nombre, Precio),
    (write("-"), write(Nombre), write("\t"), write(Precio), write(" pokes"), nl)), menuCaminar.

menuItemCiudades :-
    write("----- Ciudades ------"), nl,
    write("Ciudad       Siguiente       Distancia"), nl, nl,
    write("Paleta         Verde           5 Kms"), nl,
    write("Verde          Azulona       10 Kms"), nl,
    write("Azulona       Plateada      8 Kms"), nl,
    write("Plateada      Celeste       10 Kms"), nl,
    write("Celeste       Lavanda        10 Kms"), nl,
    write("Lavanda      Carmin          15 Kms"), nl, menuCaminar.

menuItemHuevos :-
    write("----- Huevos ------"), nl,
    write(" Tipo        Kms"), nl, nl,
    write("Normal       3"), nl,
    write("Planta         3"), nl,
    write("Agua          5"), nl,
    write("Fuego         7"), nl,
    write("Electrico     5"), menuCaminar.

menuPrincipalController(X, MenuAnterior):-
(   (X = 1) ->
        menuItemPokemons,
        mostrarMenu(MenuAnterior);
    (X = 2) ->
        menuPokemochila,
        mostrarMenu(MenuAnterior);
    (X = 3) ->
        menuFichaEntrenador,
        mostrarMenu(MenuAnterior);
    (X = 4) ->
        menuInfo(MenuAnterior);
    (X = 5) ->
        (
            (MenuAnterior = viaje) ->
                menuCaminar;
            (MenuAnterior = ciudad) ->
                menuCiudad
        );
    ((X < 1); (X > 5)) ->
        vuelveAIntentar(Y),
        menuPrincipalController(Y, MenuAnterior)
).

menuPokemochila :-
    write("----- Pokemochila -----"), nl,
    write("Tengo:"), nl,
    forall(pokebola(P, C),
    ( C \= 0,
    write(" -"), write(C), write(" "), write(P), nl)), menuCaminar.

menuFichaEntrenador :-
    dinero(D),
    write("----- Ficha de Entrenador -----"), nl,
    write("Dinero: "), write(D), write(" pokes"), nl,
    write("Medallas Obtenidas: "), nl,
    forall(medalla(X, si),
    (write("- "), write(X), nl)), menuCaminar.

menuInfoController(X, MenuAnterior) :-
(   (X = 1) ->
        menuItemPokebolas,
        mostrarMenu(MenuAnterior);
    (X = 2) ->
        menuInfoEvo,
        mostrarMenu(MenuAnterior);
    (X = 3) ->
        menuItemHuevos,
        mostrarMenu(MenuAnterior);
    (X = 4) ->
        menuItemCiudades,
        mostrarMenu(MenuAnterior);
    (X = 5) ->
        mostrarMenu(MenuAnterior);
    ((X < 1); (X > 5)) ->
        vuelveAIntentar(Y),
        menuInfoController(Y)
).

menuInfoEvo :-
    write("----- Evoluciones Pokemon ------"), nl,
    forall(evolucion(P, Evo, Exp),
    (write(" - "), write(P), write(" evoluciona a "),
     write(Evo), write(" con "), write(Exp), write(" exp"), nl)), menuCaminar.


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
            getElement(P, X, Pokemon),
            getElement(Pokemon, 1, Nombre),
            getElement(Pokemon, 3, Vida),
            getElement(Pokemon, 4, Exp),
            pokemon(_, Nombre, Tipo, _),
            write("-Nombre: "), write(Nombre), nl,
            write("-Tipo: "), write(Tipo), nl,
            write("-Vida: "), write(Vida), nl,
            write("-Exp: "), write(Exp);
        ((X < 1); (X > N)) ->
            vuelveAIntentar(Y),
            menuPokemonController(P, Y)
    ).

%Param(Lista, 0)

obtenerListaNumerada([], _).
obtenerListaNumerada([X | T], C):-
    C1 is C + 1,
    write(C1), write(".- "), write(X), nl,
    obtenerListaNumerada(T, C1).

%Mispokemon([nombre, estado, vida, experiencia])
misPokemon([]).
numPokemons(N) :-
    misPokemon(P),
    len(P, N).

agregarPokemon(Pokemon, EXP) :-
    misPokemon(P),
    numPokemons(N),
    (
        (N < 6) ->
            append(P,[[Pokemon, normal, 100, EXP]], L),
            asserta(misPokemon(L)),
            retract(misPokemon(P));
        (N > 5) ->
            almacenarPcBill(Pokemon, EXP)
    ).


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
            agregarPokemon(charmander, 0),
            mensajePrimerViaje;
        (X = 2) ->
            agregarPokemon(bulbasaur, 0),
            mensajePrimerViaje;
        (X = 3) ->
            agregarPokemon(squirtle, 0),
            mensajePrimerViaje;
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            primerPokemonController(Y)
    ).

mensajePrimerViaje :-
    write("Ahora que tienes tu primer pokemon puedes ir a ciudad "),
    ciudadSiguiente(_, C), write(C), nl,
    write(" - Has comenzado tu viaje..."), nl, menuCaminar.

menuBatallaEntrenador :-
    write("Elige el numero de tu accion:"),nl,
    write("1. Pelear"), nl,
    write("2. Huir"), nl,
    read(X), batallaController(X).

batallaController(X) :-
    (
        (X = 1) ->
            pelear;
        (X = 2) ->
            write("Has huido"), nl,
            menuCaminar;
        ((X < 1); (X > 2)) ->
            vuelveAIntentar(Y),
            batallaController(Y)
    ).

menuBatallaSalvaje :-
    write("Elige el numero de tu accion:"),nl,
    write("1. Pelear"), nl,
    write("2. Capturar"), nl,
    write("3. Huir"), nl,
    read(X), batallaSalvajeController(X).

batallaSalvajeController(X) :-
    (
        (X = 1) ->
            pelearSalvaje;
        (X = 2) ->
            write("Lo has capturado, enhorabuena!"), nl,
            pokemonSalvaje([Nombre | _]),
            agregarPokemon(Nombre, 0);
        (X = 3) ->
            write("Has huido"), nl,
            menuCaminar;
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            batallaSalvajeController(Y)
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
            menuCaminar;
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
            menuEnfermeria;
        (X = 2) ->
            menuTienda;
        (X = 3) ->
            write("Gimnasio");
        (X = 4) ->
            mostrarMenu(ciudad);
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
    precio(pokeball, P1), 
    precio(superball, P2),
    precio(ultraball, P3), 
    precio(masterball, P4), 
    write(" --- Bienvenido a la Tienda Pokemon --- "), nl,
    write("Tienes: "), write(D), write(" pokes."), nl, nl,
    write("Elige el numero de tu accion:"),nl,
    write("1. Comprar pokeballs a "), write(P1), write(" c/u"), nl,
    write("2. Comprar superballs a "), write(P2), write(" c/u"), nl,
    write("3. Comprar ultraballs a "), write(P3), write(" c/u"), nl,
    write("4. Comprar masterball a "), write(P4), write(" c/u"), nl,
    write("5. Regresar"), nl,
    read(X), menuTiedaController(X).

menuTiedaController(X) :-
    (
        (X = 1) ->
            write("Cantidad "), read(C),
            comprar(C, 10, R),
            (
                (R = 1) ->
                    pokebola(pokeball, Cant),
                    C1 is C + Cant,
                    asserta(pokebola(pokeball, C1)),
                    retract(pokebola(pokeball, Cant)),
                    menuCiudad;
                (R = 0) ->
                    menuTienda
            );
        (X = 2) ->
            write("Cantidad "), read(C),
            comprar(C, 15, R),
            (
                (R = 1) ->
                    pokebola(superball, Cant),
                    C1 is C + Cant,
                    asserta(pokebola(superball, C1)),
                    retract(pokebola(superball, Cant)),
                    menuCiudad;
                (R = 0) ->
                    menuTienda
            );
        (X = 3) ->
            write("Cantidad "), read(C),
            comprar(C, 25, R),
            (
                (R = 1) ->
                    pokebola(ultraball, Cant),
                    C1 is C + Cant,
                    asserta(pokebola(ultraball, C1)),
                    retract(pokebola(ultraball, Cant)),
                    menuCiudad;
                (R = 0) ->
                    menuTienda
            );
        (X = 4) ->
            write("Cantidad "), read(C),
            comprar(C, 100, R),
            (
                (R = 1) ->
                    pokebola(masterball, Cant),
                    C1 is C + Cant,
                    asserta(pokebola(masterball, C1)),
                    retract(pokebola(masterball, Cant)),
                    menuCiudad;
                (R = 0) ->
                    menuTienda
            );
        (X = 5) ->
            menuCiudad;
        ((X < 1); (X > 5)) ->
            vuelveAIntentar(Y),
            menuTiedaController(Y)
    ).

:- dynamic dinero/1.

comprar(Cantidad, Precio, R) :-
    dinero(D),
    Total is Cantidad * Precio,
    (
        ((D > Total); (D = Total)) ->
            Resto is D - Total,
            asserta(dinero(Resto)),
            retract(dinero(D)),
            write("Te quedan: "), write(Resto), write(" pokes."),
            R is 1;
        (D < Total) ->
            write("Dinero insuficiente, vuelve a intentar"), nl,
            R is 0
    ).

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
            misPokemon(P),
            curarPokemons(P),
            write("Todos tus pokemon han sido curados!");
        (X = 2) ->
            menuItemPC;
        (X = 3) ->
            menuCiudad;
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            menuEnfermeriaController(Y)
    ).

menuItemPC :-
    write("--- PC de Bill ---"), nl,
    write("Elige el numero de tu accion:"),nl,
    write("1. Guardar Pokemon"), nl,
    write("2. Sacar Pokemon"), nl,
    write("3. Regresar"), nl,
    read(X), menuItemPCController(X).

menuItemPCController(X) :-
    numPokemons(N),
    (
        (X = 1) ->
            (
                (N > 1) ->
                    menuGuardarPC;
                (N = 1) ->
                    write("Imposible, es necesario que tengas por lo menos un pokemon en tu equipo."), nl,
                    menuItemPC
            );
            
        (X = 2) ->
            (
                (N < 6) ->
                    menuSacarPC;
                (N > 5) ->
                    write("Equipo pokemon lleno"), nl,
                    menuItemPC
            );
        (X = 3) ->
            nl, write("Saliendo del PC..."), nl,
            menuEnfermeria;
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            menuEnfermeriaController(Y)
    ).

menuGuardarPC :-
    misPokemon(P),
    obtenerListaNumerada(P, 0),
    write("Numero de pokemon que desea almacenar en el PC: "),
    read(N),
    getElement(P, N, R),
    getElement(R, 1, Nombre),
    getElement(R, 4, Exp),
    almacenarPcBill(Nombre, Exp),
    removeElement(R, P, NL),
    asserta(misPokemon(NL)),
    retract(misPokemon(P)).

menuSacarPC :-
    write("--Pokemons en el PC: "), nl,
    forall(pcBill(Nombre, _),
    (write("- "), write(Nombre), nl)),
    write("Elige el pokemon que deseas sacar: "),
    read(X), menuSacarPCController(X).

menuSacarPCController(X) :-
    numPokemonsPC(N),
    (
        ((X > 0), (X < N + 1)) ->
            findall(Pokemon, pcBill(Pokemon, _), Lista),
            getElement(Lista, X, Pokemon),
            sacarPcBill(Pokemon);
        ((X < 1); (X > N)) ->
            vuelveAIntentar(Y),
            menuSacarPCController(Y)
    ).

/*crear un equipo de pokemones para el contrincante*/

:- dynamic batallaTerminada/1.

inicializarPeleador:-
  random(1, 33, X1), random(1, 33, X2),
  pokemon(X1, Pokemon1, _, _),
  pokemon(X2, Pokemon2, _, _),
  retractall(pcPokemones([_])),
  asserta(
    pcPokemones([
      [Pokemon1, normal, 100, 0],
      [Pokemon2, normal, 100, 0]
    ])
  ).

inicializarSalvaje :-
    random(1, 33, X1),
    pokemon(X1, Pokemon1, _, _),
    retractall(pokemonSalvaje(_)),
    asserta(pokemonSalvaje([Pokemon1, normal, 20, 0])).

printPokemonesPeleador:- write("Pokemones del pc: \n"), pcPokemones(X), obtenerListaNumerada(X, 0), nl.
printPokemonSalvaje:- write("Pokemon salvaje aparece!!! es: "), pokemonSalvaje([Nombre|_]), write(Nombre), nl.
nombrePokemon([Nombre|_], Nombre).
estadoPokemon([_,Estado|_], Estado).
vidaPokemon([_, _, Vida|_], Vida).
expPokemon([_, _, _, EXP|_], EXP).

pelear:- asserta(batallaTerminada(no)),
  retractall(miDanioGenerado(_)),
  retractall(miDanioObtenido(_)),
  asserta(miDanioGenerado(0)),
  asserta(miDanioObtenido(0)),
  inicializarPeleador,
  printPokemonesPeleador,
  pelea(0, 0).

pelearSalvaje:- 
  asserta(batallaTerminada(no)),    
  retractall(miDanioGenerado(_)),
  retractall(miDanioObtenido(_)),
  asserta(miDanioGenerado(0)),
  asserta(miDanioObtenido(0)),
  inicializarSalvaje,  
  printPokemonSalvaje,
  peleaSalvaje(0,0).

peleaSalvaje(DanioInicial, Turno):- Turno < 4,  batallaTerminada(no), peleoYo(DanioInicial, DanioGeneradoYo), peleaPokemonSalvaje(DanioGeneradoYo, DanioGeneradoPC), Turno1 is Turno + 1, peleaSalvaje(DanioGeneradoPC, Turno1).
peleaSalvaje(_, Turno):- write("batalla terminada en turno: "), write(Turno), ganador(Ganador),
  (
    ((Ganador = 0);(Ganador = 1)) -> menuCaminar
  ).

pelea(DanioInicial, Turno):- Turno < 4, batallaTerminada(no),
  peleoYo(DanioInicial, DanioGeneradoYo), peleaPC(DanioGeneradoYo, DanioGeneradoPC), Turno1 is Turno + 1, pelea(DanioGeneradoPC, Turno1).
pelea(_, Turno):- write("batalla terminada en turno: "), write(Turno), ganador(Ganador),
  (
    ((Ganador = 0);(Ganador = 1)) -> menuCaminar
  ).

peleaPC(DanioInicial, DanioGenerado):-
  %elegir el pokemon a pelear del pc
  batallaTerminada(no), write("pc recibe danio: "), write(DanioInicial),nl,
  write("\nTurno maquina!"),nl,
  pcPokemones(PcPokemones),
  indexOf(PcPokemones, [_, normal, _, _], IP), Indice is IP + 1,
  getElement(PcPokemones, Indice, Pokemon),
  %obtener datos
  nombrePokemon(Pokemon, NombrePokemon),
  vidaPokemon(Pokemon, VidaPokemon),
  NuevaVidaPokemon is VidaPokemon - DanioInicial,
  %actualizar en los pokemones del pc con la nueva vida
  (
    (NuevaVidaPokemon < 1) -> (
        actualizarPokemon([NombrePokemon, debilitado, 0, 0], PcPokemones, NuevosPokemones),
        retractall(pcPokemones(_)), asserta(pcPokemones(NuevosPokemones))
      );
    (NuevaVidaPokemon > 1) -> (
      actualizarPokemon([NombrePokemon, normal, NuevaVidaPokemon, 0], PcPokemones, NuevosPokemones),
      retractall(pcPokemones(_)), asserta(pcPokemones(NuevosPokemones))
    )
  ),
  %mostrar datos actualizados
  pcPokemones(PcPokemonesAct),
  indexOf(PcPokemonesAct, [_, normal, _, _], IPAct), IndiceAct is IPAct + 1,
  getElement(PcPokemonesAct, IndiceAct, PokemonAct),
  write("Pokemon del pc: "), write(PokemonAct),
  random(1, 4, X),
  elegirAtaque(X, NombrePokemon, Ataque),
  ataque(Ataque, DanioGenerado, _), write(" Pc lanza: "), write(Ataque),
  write(" realiza danio: "), write(DanioGenerado), nl, nl,
  miDanioObtenido(DanioDB),
  NuevoMiDanioObtenido is DanioDB + DanioGenerado,
  retractall(miDanioObtenido(_)), asserta(miDanioObtenido(NuevoMiDanioObtenido)).

peleaPokemonSalvaje(DanioInicial, DanioGenerado):-
    batallaTerminada(no), write("Pokemon salvaje recibe danio: "), write(DanioInicial),nl,
    pokemonSalvaje(Pokemon),
    write("\nTurno pokemon salvaje!"),nl,
    nombrePokemon(Pokemon, NombrePokemon),
    vidaPokemon(Pokemon, VidaPokemon),
    NuevaVidaPokemon is VidaPokemon - DanioInicial,
    %actualizar en los pokemon con la nueva vida
    (
        ((NuevaVidaPokemon < 1), retractall(pokemonSalvaje(_)), asserta(pokemonSalvaje([NombrePokemon, debilitado, 0, 0])));
        ((NuevaVidaPokemon > 1), retractall(pokemonSalvaje(_)), asserta(pcPokemones([NombrePokemon, normal, NuevaVidaPokemon, 0])))
    ),
    %mostrar datos actualizados
    write("Pokemon Salvaje: "), write(NombrePokemon),
    random(1, 4, X),
    elegirAtaque(X, NombrePokemon, Ataque),
    ataque(Ataque, DanioGenerado, _), write(" lanza: "), write(Ataque),
    write(" realiza danio: "), write(DanioGenerado), nl, nl,
    miDanioObtenido(DanioDB),
    NuevoMiDanioObtenido is DanioDB + DanioGenerado,
    retractall(miDanioObtenido(_)), asserta(miDanioObtenido(NuevoMiDanioObtenido)).



peleoYo(DanioInicial, DanioGenerado):- batallaTerminada(no),
  %elegir el pokemon peleador
  misPokemon(MisPokemones),
  indexOf(MisPokemones, [_, normal, _, _], IP), Indice is IP + 1,
  getElement(MisPokemones, Indice, Pokemon),
  retractall(pokemonActual(_)), asserta(pokemonActual(Pokemon)),
  %obtener las propiedades del pokemon
  vidaPokemon(Pokemon, VidaPokemon),
  nombrePokemon(Pokemon, NombrePokemon),
  expPokemon(Pokemon, ExpPokemon),
  %aplicar el daño recibido
  NuevaVidaPokemon is VidaPokemon - DanioInicial,
  %actualizar con el nuevo daño
  (
    (NuevaVidaPokemon < 1) -> (
        actualizarPokemon([NombrePokemon, debilitado, 0, ExpPokemon], MisPokemones, NuevosPokemones),
        retractall(misPokemon(_)), asserta(misPokemon(NuevosPokemones))
      );
    (NuevaVidaPokemon > 1) -> (
      actualizarPokemon([NombrePokemon, normal, NuevaVidaPokemon, ExpPokemon], MisPokemones, NuevosPokemones),
      retractall(misPokemon(_)), asserta(misPokemon(NuevosPokemones))
    )
  ),
  %seleccion de ataque
  write("tu pokemon es: "), write(NombrePokemon), nl,
  write("tiene de vida: "), write(NuevaVidaPokemon), write(", sus ataque son: \n"),
  pokemonAtaque(NombrePokemon, L), write("\n elige uno: \n"),
  obtenerListaNumerada(L, 0),
  read(X),
  elegirAtaque(X, NombrePokemon, Ataque),
  ataque(Ataque, DanioGenerado, _),
  write("elegiste: "), write(Ataque), nl,
  write("realizaste de danio: "), write(DanioGenerado), nl,
  miDanioGenerado(DanioDB),
  NuevoMiDanioGenerado is DanioDB + DanioGenerado,
  retractall(miDanioGenerado(_)), asserta(miDanioGenerado(NuevoMiDanioGenerado)).

%retorna 1 si gano la pc, retorna 0 si gano el usuario
ganador(Ganador):- (
  (misPokemon(L), indexOf(L, [_, normal, _, _], _),
    miDanioGenerado(DanioGenerado), miDanioObtenido(DanioObtenido),
    write("\nGeneraste de danio: "), write(DanioGenerado), write(", Recibiste de danio: "), write(DanioObtenido),
    (
      (DanioGenerado < DanioObtenido)-> Ganador is 1, write("\nGano la pc con un Danio de: "), write(DanioObtenido);
      ((DanioGenerado > DanioObtenido);(DanioGenerado = DanioObtenido)) -> (
          Ganador is 0,
          write("\nGanaste de dinero pokemon: "),
          DineroPoke is DanioGenerado * 3, write(DineroPoke),
          dinero(D),
          NuevoDinero is D + DineroPoke,
          retractall(dinero(_)), asserta(dinero(NuevoDinero)),
          write("\nGanaste de experiencia pokemon: "),
          ExperienciaPokemon is DanioGenerado * 2, write(ExperienciaPokemon),
          pokemonActual(PokemonActual),
          nombrePokemon(PokemonActual, Nom), vidaPokemon(PokemonActual, Vid), estadoPokemon(PokemonActual, Edo), expPokemon(PokemonActual, Exp),
          NuevaExperiencia is Exp + ExperienciaPokemon,
          misPokemon(L), actualizarPokemon([Nom, Edo, Vid, NuevaExperiencia], L, Result), retractall(misPokemones(_)), asserta(misPokemones(Result))
        )
    ));
    (
      misPokemon(X), not(indexOf(X, [_, normal, _, _], _)),
      write("\n Te mataron todos los pokemones loco! El juego termina aqui... suerte para la proxima!"),
      Ganador is 2
    )
).

elegirAtaque(Indice, NombrePokemon, AtaqueElegido):-
  (
    (Indice = 1) -> pokemonAtaque(NombrePokemon, [AtaqueElegido|_] );
    (Indice = 2) -> pokemonAtaque(NombrePokemon, [_, AtaqueElegido|_] );
    (Indice = 3) -> pokemonAtaque(NombrePokemon, [_, _, AtaqueElegido|_] );
    (Indice = 4) -> pokemonAtaque(NombrePokemon, [_, _, _, AtaqueElegido|_] )
  ).

menuCaminar :-
    write("\n- Estas en la ruta pokemon"), nl,
    write("Elige el numero de tu accion:"),nl,
    write("1. Menu principal"), nl,
    write("2. Avanzar"), nl,
    read(X), menuCaminarController(X).

menuCaminarController(X) :-(
    (X = 1) ->
        mostrarMenu(viaje);
    (X = 2) ->
        write("Caminando..."), nl, caminar;
    ((X < 1); (X > 2)) ->
        vuelveAIntentar(Y),
        menuCaminarController(Y)
).

%Caminar 1-Pokemon salvaje,
%2- entrenador,
%3- huevo,
%4- pokebola,
%5- llegar ciudad
caminar :-
    random(1, 6, X),
    (
        (X = 1) ->
            write("Un pokemon salvaje ha aparecido"), nl,
            menuBatallaSalvaje;
        (X = 2) ->
            write("Batalla Entrenador"), nl,
            menuBatallaEntrenador;
        (X = 3) ->
            write("Huevo encontrado"), nl,
            menuHuevo;
        (X = 4) ->
            write("Pokeball encontrada!"), nl,
            pokebola(pokeball, C),
            C1 is C + 1,
            asserta(pokebola(pokeball, C1)),
            retract(pokebola(pokeball, C)),
            menuCaminar;
        (X = 5) ->
            verificarHuevo,
            bienvenidaCiudad
    ).

replaceAll(_, _, [], []).
replaceAll(O, R, [O|T], [R|T2]) :- replaceAll(O, R, T, T2).
replaceAll(O, R, [H|T], [H|T2]) :- H \= O, replaceAll(O, R, T, T2).

actualizarPokemon(_, [],[]).
actualizarPokemon([PN, ES, DN, EN], [[P, PES, PDN, EXP]|T], [[P, PES, PDN, EXP]|T2]):- PN \= P, actualizarPokemon([PN,ES, DN, EN], T, T2).
actualizarPokemon([PN, ES, DN, EN], [[PN, _, _, _]|T], [[PN, ES, DN, EN]|T2]):- actualizarPokemon([PN,ES, DN, EN], T, T2).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),!,
  Index is Index1 + 1.

removeElement(X, [X|Xs], Xs).
removeElement(X, [Y|Ys], [Y|Zs]):- 
    removeElement(X, Ys, Zs).

menuHuevo :-
    write("Elige el numero de tu accion:"),nl,
    write("1. Quedarse con el"), nl,
    write("2. Tirar"), nl,
    read(X), menuHuevoController(X).

menuHuevoController(X) :-
    (
        (X = 1) ->
            agregarPokemon(huevo, 0),
            menuCaminar;
        (X = 2) ->
            write("Lo has tirado"), nl,
            menuCaminar;
        ((X < 1); (X > 2)) ->
            vuelveAIntentar(Y),
            menuHuevoController(Y)
    ).

verificarHuevo :-
    misPokemon(P),
    indexOf(P,[huevo,_,_,_], I),
    getElement(P, I + 1, X),
    removeElement(X, P, NL),
    asserta(misPokemon(NL)),
    retract(misPokemon(P)),
    random(1, 33, X1),
    pokemon(X1, Pokemon1, _, _),
    write("El huevo ha eclosionado!"), nl,
    write("- Es un "), write(Pokemon1), nl,
    agregarPokemon(Pokemon1, 0).

curarPokemons([]).
curarPokemons([X | T]):-
    misPokemon(P),
    nombrePokemon(X, Nombre),
    expPokemon(X, Exp),
    removeElement(X, P, NL),
    retract(misPokemon(P)),
    asserta(misPokemon(NL)),
    agregarPokemon(Nombre, Exp),
    curarPokemons(T).



contadorGimnasio(X) :-
    X1 is X + 1,
    X < 7,
    write("Batalla numero "), write(X), nl,
    pelear,
    contadorGimnasio(X1).

menuGimnasio :-
    ciudadAnterior(Id, _),
    (
        (Id = 1) ->
            write("");
        ((Id > 1); (Id < 7)) ->
            write("")
    ),
    contadorGimnasio(Id).
