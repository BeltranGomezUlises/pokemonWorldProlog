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
pokemon(29, rattata, normal, 12, normal, 0).
pokemon(30, raticate, normal, 25, normal, 90).
pokemon(31, meowth, normal, 13, normal, 0).
pokemon(32, persian, normal, 28, normal, 120).
pokemon(33, snorlax, normal, 30, normal, 0).

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

%PC bill (nombrePokemon, experiencia)
pcBill(pokemonPrueba, 100).

sacarPcBill(Pokemon):-
  pcBill(Pokemon, Y), agregarPokemon(Pokemon, Y), retract(pcBill(Pokemon, Y)).

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

menuPrincipalController(X, MenuAnterior):-
(   (X = 1) ->
        menuItemPokemons;
    (X = 2) ->
        menuPokemochila;
    (X = 3) ->
        menuFichaEntrenador;
    (X = 4) ->
        menuInfo;
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

%Mispokemon([nombre, estado, vida])
misPokemon([]).
numPokemons(N) :-
    misPokemon(P),
    len(P, N).

agregarPokemon(Pokemon, EXP) :-
    misPokemon(P),
    numPokemons(N),
    (
        (N < 6) ->
            append(P,[[Pokemon, normal, 100]], L),
            asserta(misPokemon(L)),
            retract(misPokemon(P));
        (N > 5) ->
            write("something")
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
            agregarPokemon(charmander),
            mensajePrimerViaje;
        (X = 2) ->
            agregarPokemon(bulbasaur),
            mensajePrimerViaje;
        (X = 3) ->
            agregarPokemon(squirtle),
            mensajePrimerViaje;
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            primerPokemonController(Y)
    ).

mensajePrimerViaje :-
    write("Ahora que tienes tu primer pokemon puedes ir a ciudad "),
    ciudadSiguiente(_, C), write(C), nl, 
    write(" - Has comenzado tu viaje..."), nl, menuCaminar.

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
            write("Enfermeria");
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
            write("Todos tus pokemon han sido curados!");
        (X = 2) ->
            menuItemPC;
        (X = 3) ->
            write("Gimnasio");
        ((X < 1); (X > 3)) ->
            vuelveAIntentar(Y),
            menuEnfermeriaController(Y)
    ).


/*crear un equipo de pokemones para el contrincante*/

:- dynamic batallaTerminada/1.

inicializarPeleador:-
  random(1, 33, X1), random(1, 33, X2), random(1, 33, X3), random(1, 33, X4),
  pokemon(X1, Pokemon1, _, _, _, _),
  pokemon(X2, Pokemon2, _, _, _, _),
  pokemon(X3, Pokemon3, _, _, _, _),
  pokemon(X4, Pokemon4, _, _, _, _),
  retractall(pcPokemones([_])),
  asserta(
    pcPokemones([
      [Pokemon1, normal, 100],
      [Pokemon2, normal, 100],
      [Pokemon3, normal, 100],
      [Pokemon4, normal, 100]
    ])
  ).

printPokemonesPeleador:- write("Pokemones del pc: \n"), pcPokemones(X), obtenerListaNumerada(X, 0), nl.

pelear:- asserta(batallaTerminada(no)), inicializarPeleador, printPokemonesPeleador, pelea(0).
pelea(DanioInicial):- batallaTerminada(no),
  peleoYo(DanioInicial, DanioGeneradoYo), peleaPC(DanioGeneradoYo, DanioGeneradoPC), pelea(DanioGeneradoPC).
pelea(_):- write("batalla terminada").

peleaPC(DanioInicial, DanioGenerado):-
  nl, write("Maquina recibe danio: "), write(DanioInicial),
  batallaTerminada(no), write("turno maquina"), DanioGenerado is 5.

nombrePokemon([Nombre|_], Nombre).
estadoPokemon([_,Estado|_], Estado).
vidaPokemon([_, _, Vida|_], Vida).

peleoYo(DanioInicial, DanioGenerado):- batallaTerminada(no),
          misPokemon(MisPokemones),
          indexOf(MisPokemones, [_, normal, _], IP), Indice is IP + 1,
          getElement(MisPokemones, Indice, Pokemon),
          vidaPokemon(Pokemon, VidaPokemon),
          nombrePokemon(Pokemon, NombrePokemon),
          estadoPokemon(Pokemon, EstadoPokemon),
          NuevaVidaPokemon is VidaPokemon - DanioInicial,
          actualizarPokemon([NombrePokemon, EstadoPokemon, NuevaVidaPokemon], MisPokemones, NuevosPokemones),
          retractall(misPokemon(_)), asserta(misPokemon(NuevosPokemones)),
          write("tu pokemon es: "), write(NombrePokemon), nl,
          write("tiene de vida: "), write(NuevaVidaPokemon), write(", sus ataque son: \n"),
          pokemonAtaque(NombrePokemon, L), write("\n elige uno: \n"),
          obtenerListaNumerada(L, 0),
          read(X),
          elegirAtaque(X, NombrePokemon, Ataque),
          ataque(Ataque, DanioGenerado, _),
          write("elegiste: "), write(Ataque), nl,
          write("realizaste de danio: "), write(DanioGenerado), nl.

elegirAtaque(Indice, NombrePokemon, AtaqueElegido):-
  (
    (Indice = 1) -> pokemonAtaque(NombrePokemon, [AtaqueElegido|_] );
    (Indice = 2) -> pokemonAtaque(NombrePokemon, [_, AtaqueElegido|_] );
    (Indice = 3) -> pokemonAtaque(NombrePokemon, [_, _, AtaqueElegido|_] );
    (Indice = 4) -> pokemonAtaque(NombrePokemon, [_, _, _, AtaqueElegido|_] ), retractall(batallaTerminada(no))
  ).

menuCaminar :-
    write(" - Estas en la ruta pokemon"), nl,
    write("Elige el numero de tu accion:"),nl,
    write("1. Menu principal"), nl,
    write("2. Avanzar"), nl,
    read(X), menuCaminarController(X).

menuCaminarController(X) :-
    (
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
            write("Batalla Pokemon Salvaje"), nl,
            menuCaminar;
        (X = 2) ->
            write("Batalla Entrenador"), nl,
            menuCaminar;
        (X = 3) ->
            write("Huevo encontrado"), nl,
            menuCaminar;
        (X = 4) ->
            write("Pokeball encontrada!"), nl,
            pokebola(pokeball, C),
            C1 is C + 1,
            asserta(pokebola(pokeball, C1)),
            retract(pokebola(pokeball, C)),
            menuCaminar;
        (X = 5) ->
            bienvenidaCiudad
    ).

replaceAll(_, _, [], []).
replaceAll(O, R, [O|T], [R|T2]) :- replaceAll(O, R, T, T2).
replaceAll(O, R, [H|T], [H|T2]) :- H \= O, replaceAll(O, R, T, T2).

actualizarPokemon(_, [],[]).
actualizarPokemon([PN, ES, DN], [[P, PES, PDN]|T], [[P, PES, PDN]|T2]):- PN \= P, actualizarPokemon([PN,ES, DN], T, T2).
actualizarPokemon([PN, ES, DN], [[PN, _, _]|T], [[PN, ES, DN]|T2]):- actualizarPokemon([PN,ES, DN], T, T2).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),!,
  Index is Index1+1.
