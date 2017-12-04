
% Diego Díaz Alvarellos
% ddiaz136


/* PRIMERA PARTE : obtener una lista de coordenadas del Skyline */
/* ------------------------------------------------------------ */

/* Función 1. Funcion principal */
% ----------------------------------------------------------------------------------------------------------

resuelveSkyline([ed(X,Y,H)], D) :- edificioAskyline(ed(X,Y,H), D), !. 

resuelveSkyline(Lista_Edificios, D) :- 
            divide(Lista_Edificios, A,B), 
            resuelveSkyline(A, Sub1), 
            resuelveSkyline(B, Sub2), 
            combina(Sub1,Sub2,D).


/* Función 2: El caso base */
% ----------------------------------------------------------------------------------------------------------

edificioAskyline(ed(X,Y,H), [c(X,H), c(Y,0)]).


/* Funcion 3: Divide en dos subsoluciones  */
% ----------------------------------------------------------------------------------------------------------

divide([],0,0) :- !.
divide(L, A, B) :- length(L, N), Half is N div 2, length(A, Half), length(B, _), append(A, B, L).

/* Funcion 4: Combina las subsoluciones */
% ----------------------------------------------------------------------------------------------------------
combina([],[],[]) :- !.
combina(L1,L2,Total) :- merge(L1,0,Total,L2,0).


% (lista, altura del último punto consumido de cada linea de horizonte)

merge([], _, [],[], _) :- !.
merge([], _, [c(Y, Yh)|Ys],[c(Y, Yh)|Ys], _).
merge([c(X, Xh)|Xs], _, [c(X, Xh)|Xs],[], _).


merge( [c(X, Xh)|Xs], Xh_p, R, [c(Y, Yh)|Ys], Yh_p ) :- 
        X>Y, !,
        merge([c(Y, Yh)|Ys], Yh_p, R, [c(X, Xh)|Xs], Xh_p).

merge( [c(X, Xh)|Xs], _, [SubResult|R], [c(Y, Yh)|Ys], _) :- 
        X=Y, !, 
        max(Xh,Yh,Maximo), SubResult = (c(X,Maximo)),
        merge(Xs, Xh, R, Ys, Yh).

merge( [c(X, Xh)|Xs], Xh_p, [SubResult|R], [c(Y, Yh)|Ys], Yh_p ) :-
        max(Xh_p, Yh_p, Maximo), max(Xh, Yh_p, Maximo2), 
        Maximo \= Maximo2, SubResult = (c(X,Maximo2)),  
        merge(Xs, Xh, R, [c(Y, Yh)|Ys], Yh_p).

merge( [c(_, Xh)|Xs], _, R, [c(Y, Yh)|Ys], Yh_p ) :- 
       merge(Xs, Xh, R, [c(Y, Yh)|Ys], Yh_p).


% Devuelve el máximo entre dos números
max(X,Y,Y) :- X =< Y, !. 
max(X,_,X).


% ----------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------------

/* SEGUNDA PARTE : Imprimir por pantalla el skyline */
/* ------------------------------------------------ */

/* Dibuja n lineas siendo el valor de n la altura máxima del skyline y el suelo */

dibujaSkyline([]).
dibujaSkyline(L) :- alturasSkyline(L,ListaH), altura(ListaH,Hmax), dibuja(ListaH,Hmax).

dibuja(L,0):- suelo(L).
dibuja(L,N) :- dibujaLinea(L,N), nl, N1 is N-1, dibuja(L,N1).

% ----------------------------------------------------------------------------------------------------------
% Segunda forma sin imprimir por pantalla (en una lista)
% Solamente cambiar dibujaSkyline y dibuja por las dos siguientes

%dibujaSkyline([],_).
%dibujaSkyline(L,D) :- alturasSkyline(L,ListaH), altura(ListaH,Hmax), dibuja(ListaH,Hmax,D).
 
%dibuja(L,0,[]):- suelo(L).
%dibuja(L,N,[Dibujo|Resto]) :- dibujaLinea2(L,N,Dibujo), N1 is N-1, dibuja(L,N1,Resto).
% ----------------------------------------------------------------------------------------------------------


/* 1.Transformamos la lista de coordenadas del skyline en una lista de alturas para cada coordenada x del horizonte. */
% --------------------------------------------------------------------------------------------------------------------

alturasSkyline([],[]).
alturasSkyline(L,Rev) :- juntarSolar(L,L2), flatten(L2,RFlat), reverse(RFlat,Rev), !.

/* Concatena las subsoluciones de forma recursiva, quita el último elemento y
toma las dos últimas coordenadas para obtener una subsolución. */

%concatena(Rest,[SubResult|R,B],Longitud) :- solarInicio([Rest],B), !. 
concatena([_],[]).
concatena(List,[SubResult|R]) :-
    last2_resto(List,X,Y,Rest),
    bloque(X,Y,SubResult),
    concatena(Rest,R).

juntarSolar([L1|L2],[L3|Sol]) :- solarInicio([L1],Sol), concatena([L1|L2],L3),!. 

/* Recoge los dos últimos elementos de la lista y devuelve la lista sin el utlimo elemento*/
last2_resto([X,Y],X,Y,[X]).
last2_resto([H|T],X,Y,[H|A]) :- last2_resto(T,X,Y,A).

/* Solar inicio */
solarInicio([c(X,_)], L) :- N is X, findall(0, between(1,N,_), L).

/* Repite N veces (distancia entre dos puntos) el valor de H */
bloque(c(X,H), c(Y,_), L) :- N is (Y-X), findall(H, between(1,N,_), L).


/* 2.Calculamos la altura máxima del skyline.  */
% --------------------------------------------------------------------------------------------------------------------

altura([H], H).
altura([H|T], M2) :- altura(T, M), M2 is max(H, M), !.


/* 3.Para generar cada línea del dibujo del skyline. */
% --------------------------------------------------------------------------------------------------------------------

dibujaLinea([],_).
dibujaLinea([S|Y],N) :- S>=N, write('*'), dibujaLinea(Y,N).
dibujaLinea([S|Y],N) :- N>S, write(' '), dibujaLinea(Y,N).

% Esta es la que se utiliza para imprimir el array en una lista

dibujaLinea2([], _, nl).
dibujaLinea2([S|Y],N, Linea) :- S>=N, dibujaLinea2(Y,N,S2), atom_concat(*,S2, Linea).
dibujaLinea2([S|Y],N, Linea) :- N>S,  dibujaLinea2(Y,N,S2), atom_concat(' ',S2, Linea).



/* 4. Simular el suelo. */
% --------------------------------------------------------------------------------------------------------------------

suelo(S) :- length(S,N), for(N,write('-')), !.

for(0,_).   
for(N,X) :- call(X), N1 is N-1, for(N1,X).


/* Menu de ayuda */
% --------------------------------------------------------------------------------------------------------------------

/* Limpia la pantalla del interprete SWI-Prolog */
clear :-write('\033[2J').

/* Pantalla de Ayuda */
ayuda :- nl,

         write('*************************************************************************'),nl,
         write('Autor: Diego Diaz| Skyline                                               '),nl,
         write('                                                                         '),nl,
         tab(7),
         write('----------- Parte coordenadas Skyline ------------                       '),nl,
         tab(7),
         write('resuelveSkyline(Lista_Edificios, D).                                     '),nl,
         tab(7),
         write('edificioAskyline(X,Y,Z).                                                 '),nl,
         tab(7),
         write('divide(Lista_Edificios).                                                 '),nl,
         tab(7),
         write('combina(Lista_1,Lista_2).                                                '),nl,
         tab(7),
         write(' ----------- Parte imprimir -----------------------                      '),nl,
         tab(7),                    
         write('dibujaSkyline(Lista_Coordenadas).                                        '),nl,
         tab(7), 
         write('alturasSkyline(Lista_Coordenadas,Res).                                   '),nl,
         tab(7), 
         write('altura(Lista_alturas, Res_Hmax).                                         '),nl,
         tab(7), 
         write('suelo(Lista_alturas).                                                    '),nl,
         tab(7),
         write('dibujaLinea(Lista_alturas, Num_Linea).                                   '),nl,
         tab(7), 
         write('----------- EXTRA --------------------------------                       '),nl,
         tab(7),                                      
         write('clear.               |  Limpia la pantalla                               '),nl,
         tab(7),
         write('ayuda.               |  Muestra este MENU                                '),nl,
         write('                                                                         '),nl,
         write('*************************************************************************'),nl,
         nl.


% Ejemplos de entrada.
edificio0([ed(3,6,5),ed(4,9,3),ed(8,11,2),ed(10,12,4)]).
skyline0([c(3,5),c(6,3),c(9,2),c(10,4),c(12,0)]).

edificio1([ed(1,11,5),ed(2,6,7),ed(3,13,9),ed(12,7,16),ed(14,3,25),ed(19,18,22),ed(23,13,29),ed(24,4,28)]).
skyline1([c(1,11),c(3,13),c(9,0),c(12,7),c(16,3),c(19,18),c(22,3),c(23,13),c(29,0)]).
