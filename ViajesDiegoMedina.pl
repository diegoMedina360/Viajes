camino(manizales,chinchina,29).
camino(chinchina,santarosa,10).
camino(santarosa,pereira,15).
camino(manizales,villamaria,10).
camino(villamaria,chinchina,25).
camino(manizales,neira,30).


%%Pruebas:
%viaje(manizales,pereira,[1,0],R,T,D,S).
%viaje(manizales,chinchina,[2,4]R,T,D,S).

%%Caminos:
camino2(chinchina,santarosa,10,20).
camino2(manizales,chinchina,29,45).
camino2(santarosa,pereira,15,45).
camino2(manizales,villamaria,10,15).
camino2(villamaria,chinchina,25,40).
camino2(manizales,neira,30,49).
camino2(pereira,ibague,40,60).
camino2(ibague,neiva,20,40).
camino2(neiva,bogota,35,50).


%%Optiene los tiempos y distancias de cada camino.
obtenerTiempo(C1,C2,T):- camino2(C1,C2,R,_),T is R. 
obtenerDistancia(C1,C2,D):- camino2(C1,C2,_,R),D is R. 


%% Calculo de Distancias de acuedo a cada medio.
avionD(D,R):- R is D.                  %%Tipo 1
busD(D,R):- R is D-(0.2*D).            %%Tipo 2
intermunicipalD(D,R):- R is D-(0.2*D). %%Tipo 3
taxiD(D,R):- R is D-(0.2*D).           %%Tipo 4
carroD(D,R):- R is D-(0.2*D).          %%Tipo 5
busTaxiD(D1,D2,R):- R is ((D1+(0.2*D1))). %%Tipo 2 y 4
avionOtroD(D1,D2,R):-R is D1-(0.2*D1). %%Tipo 1 y Otro.

%%Se encarga de evaluar que medio de tranporte es y me retorna el valor de la distancia.
%%T1:Tipo de trasporte 1.
%%T2:Tipo de trasporte 2.
%Dist1:Distancia normal o promedio que tiene por defecto cada camino.
%Dist2:Para fucturo uso.
%%DistR:Distancia que retorna de acuerdo al o los medios ingresados.
medioDistancia(T1,T2,Dist1,Dist2,DistR):- 
                    (T1==2),(T2==4),busTaxiD(Dist1,Dist2,A),DistR is A; 
                    (T1==1),(T2==0),avionD(Dist1,D),DistR is D; 
                    (T1==1),avionOtroD(Dist1,Dist1,D),DistR is D;
                    (T1==2),busD(Dist1,D),DistR is D;
                    (T1==3),intermunicipalD(Dist1,D),DistR is D;
                    (T1==4),taxiD(Dist1,D),DistR is D;
                    (T1==5),carroD(Dist1,D),DistR is D.

%Calculo el tiempo de acuedo a cada medio.
avionT(T,R):- R is T+(0.3*T).
busT(T,R):- R is T-(0.2*T).
intermunicipalT(T,R):- R is T-(0.2*T).
taxiT(T,R):- R is T-(0.2*T).
carroT(T,R):- R is T-(0.2*T).
busTaxiT(T1,T2,R):-R is ((T1+(0.2*T1))).
avionOtroT(T1,T2,R):-R is T1.

%%Da el sentido de ida o regreso, 1 o 2 respectivamente.
sentido(P,R):- P is 1,R is P;
               P is 2,R is P.

%%Se encarga de realizar una funcion igual a la distancia, pero esta para los tiempos.
medioTiempo(T1,T2,Tiem1,Tiem2,TiemR):- 
                    (T1==2),(T2==4),busTaxiT(Tiem1,Tiem2,A),TiemR is A; 
                    (T1==1),(T2==0),avionT(Tiem1,D),TiemR is D;
                    (T1==1),avionOtroT(Tiem1,Tiem2,D),TiemR is D;
                    (T1==2),busT(Tiem1,D),TiemR is D;
                    (T1==3),intermunicipalT(Tiem1,D),TiemR is D;
                    (T1==4),taxiT(Tiem1,D),TiemR is D;
                    (T1==5),carroT(Tiem1,D),TiemR is D.

%%Funcion principal, que se encarga de dar la ruta,el tiempo, la distancia,y el sentido de un viaje entre dos ciudades.
%%Emplea la funciones de viaje3, cuando no hay camino directo, y viajeIda y viajeRegreso caundo lo hay.
%%C1:Ciudad uno.
%%C2:Ciudad dos.
%%M:Medios de trasporte utilizados, configurado para que reciba solo dos, en caso de ser unico, el segundo parametro debera ser cero(en una lista).
%%R:Retorna la ruta utilizada en caso de no haber camino derecto entre las dos ciudades.
%%T:Retorna el tiempo que se emplea para el viaje.
%%D:Retorna la distancia que se emplea para el viaje.
%%S:Retorna el sentido del viaje.

viaje(C1,C2,M,R,T,D,S):- 
                     viajeIda(C1,C2,M,Tr,Dr),T is Tr,D is Dr,sentido(1,Is),S is Is;
                     viajeRegreso(C1,C2,M,Tr,Dr),sentido(2,Is),T is Tr,D is Dr,S is Is;
                     viaje3(C1,C2,M,R,Tr,Dr,Sr),T is Tr,D is Dr,S is Sr. %%Cuando no hay camino directo

viajeIda(C1,C2,[M1,M2],T,D) :- 
                   camino2(C1,C2,_,_),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
                   (medioTiempo(M1,M2,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
                   (medioDistancia(M1,M2,(Di11),(Di22),DistR)),D is DistR.

viajeRegreso(C1,C2,[M1,M2],T,D):- 
                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
                   (medioTiempo(M1,M2,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
                   (medioDistancia(M1,M2,(Di11),(Di22),DistR)),D is DistR.

viaje3(C1,C2,M,[],T,D,S) :- 
                   camino2(C1,C2,_,_),viajeIda(C1,C2,M,Ti,Di),T is Ti,D is Di,sentido(1,Is),S is Is;
                   camino2(C2,C1,_,_),viajeRegreso(C1,C2,M,Ti,Di),T is Ti,D is Di,sentido(2,Is),S is Is.

viaje3(C1,C2,M,[Int1,Int2],T,D,S) :-
                   camino2(C1,C2,_,_),viajeIda(C1,C2,[M,0],Ti,Di),T is Ti,D is Di,sentido(1,Is),S is Is;
                   camino2(C2,C1,_,_),viajeRegreso(C1,C2,[M,0],Ti,Di),T is Ti,D is Di,sentido(2,Is),S is Is;
                   camino2(C1,_,_,_),viaje3(C1,Int1,M,[],T1,D1,S),viaje3(Int1,C2,M,Int2,T2,D2,S),T is T1+T2,D is D1+D2;
                   camino2(_,C1,_,_),viaje3(C2,Int1,M,[],T1,D1,S),viaje3(Int1,C1,M,Int2,T2,D2,S),T is T1+T2,D is D1+D2.                     

%%---------------------------------------------------------------------------------------------------------------------------


viaje2(C1,C2,[]) :- camino2(C1,C2,_,_);camino2(C2,C1,_,_).
viaje2(C1,C2,[Int1,Int2]) :- 
                     camino2(C1,C2,_,_);
                     camino2(C2,C1,_,_);
                     viaje2(C1,Int1,[]),viaje2(Int1,C2,Int2).


%%No se utiliza.
getFirElem([X|_],X).
toList([],[]).
toList([P|[_,_]],[P]).
toList([P|[]],[P]).
toList([P|Y],[P|L]):- toList(A,L),getFirElem(Y,A).

%%-----------------------------------------------------------------------------------------------------------------------------

%%getRest([X|Y],Y).

%%es_vacia([]).

%%list_vacia([]):- false.
%%es_lista([_|_]).
%%viajeRegreso(C1,C2,T,D):- camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
%%                   (medioTiempo(1,0,(Ti11),(Ti22),TiemR)),T is TiemR,
%%                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
%%                   (medioTiempo(1,0,(Di11),(Di22),DistR)),D is DistR;
%%                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
%%                   (medioTiempo(2,0,(Ti11),(Ti22),TiemR)),T is TiemR,
%%                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
%%                   (medioTiempo(2,0,(Di11),(Di22),DistR)),D is DistR;
%%                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
%%                   (medioTiempo(3,0,(Ti11),(Ti22),TiemR)),T is TiemR,
%%                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
%%                   (medioTiempo(3,0,(Di11),(Di22),DistR)),D is DistR;
%%                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
%%                   (medioTiempo(4,0,(Ti11),(Ti22),TiemR)),T is TiemR,
%%                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
%%                   (medioTiempo(4,0,(Di11),(Di22),DistR)),D is DistR;
%%                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
%%                   (medioTiempo(5,0,(Ti11),(Ti22),TiemR)),T is TiemR,
%%                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
%%                   (medioTiempo(5,0,(Di11),(Di22),DistR)),D is DistR.


%%viaje3(C1,C2,[M1|MR],[Int1,Int2],T,D) :-
%%                   camino2(C1,C2,_,_),viajeIda(C1,C2,[M1,0],Ti,Di),T is Ti,D is Di;
%%                   camino2(C2,C1,_,_),viajeRegreso(C1,C2,[M1,0],Ti,Di),T is Ti,D is Di;
%%                   camino2(C1,_,_,_),viaje3(C1,Int1,MR,[],T1,D1),viaje3(Int1,C2,MR,Int2,T2,D2),T is T1+T2,D is D1+D2;
%%                   camino2(_,C1,_,_),viaje3(C2,Int1,MR,[],T1,D1),viaje3(Int1,C1,MR,Int2,T2,D2),T is T1+T2,D is D1+D2.

%%medioTiempo(T1,T2,Tiem1,Tiem2,TiemR):- 
%%                    %%(T1==2),(T2==4),busTaxiT(Tiem1,Tiem2,A),TiemR is A; 
%%                    (T1==1),avionT(Tiem1,D),TiemR is D;
%%                    (T1==2),busT(Tiem1,D),TiemR is D;
%%                    (T1==3),intermunicipalT(Tiem1,D),TiemR is D;
%%                    (T1==4),taxiT(Tiem1,D),TiemR is D;
%%                    (T1==5),carroT(Tiem1,D),TiemR is D.

%%viaje(C1,C2,T,D,[Int1,Int2],[Tra1,Tra2]).

%%al ser solo un viaje, necesita solo un medio, no se daria el caso bus taxi.
%%viaje1(C1,C2),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
%%(medioTiempo(2,4,(Ti11),(Ti22),TiemR)),T is TiemR,
%%obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
%%(medioTiempo(2,4,(Di11),(Di22),DistR)),D is DistR;