camino(manizales,chinchina,29).
camino(chinchina,santarosa,10).
camino(santarosa,pereira,15).
camino(manizales,villamaria,10).
camino(villamaria,chinchina,25).
camino(manizales,neira,30).

camino2(chinchina,santarosa,10,20).
camino2(manizales,chinchina,29,45).
camino2(santarosa,pereira,15,45).
camino2(manizales,villamaria,10,15).
camino2(villamaria,chinchina,25,40).
camino2(manizales,neira,30,49).

obtenerTiempo(C1,C2,T):- camino2(C1,C2,R,_),T is R. 
obtenerDistancia(C1,C2,D):- camino2(C1,C2,_,R),D is R. 


%%Distancias
avionD(T,R):- R is T+(0.3*T).
busD(D,R):- R is D-(0.2*D).
intermunicipalD(D,R):- R is D-(0.2*D).
taxiD(D,R):- R is D-(0.2*D).
carroD(D,R):- R is D-(0.2*D).
busTaxiD(D1,D2,R):-R is ((D1+(0.2*D1))+(D2+(0.2*D2))).

medioDistancia(T1,T2,Dist1,Dist2,DistR):- 
                    (T1==2),(T2==4),busTaxiD(Dist1,Dist2,A),DistR is A; 
                    (T1==1),avionD(Dist1,D),DistR is D;
                    (T1==2),busD(Dist1,D),DistR is D;
                    (T1==3),intermunicipalD(Dist1,D),DistR is D;
                    (T1==4),taxiD(Dist1,D),DistR is D;
                    (T1==5),carroD(Dist1,D),DistR is D.

%Tiempo
avionT(T,R):- R is T+(0.3*T).
busT(T,R):- R is T-(0.2*T).
intermunicipalT(T,R):- R is T-(0.2*T).
taxiT(T,R):- R is T-(0.2*T).
carroT(T,R):- R is T-(0.2*T).
busTaxiT(T1,T2,R):-R is ((T1+(0.2*T1))+(T2+(0.2*T2))).
sentido(P,R):- P is 1,R is P;
               P is 2,R is P.


medioTiempo(T1,T2,Tiem1,Tiem2,TiemR):- 
                    (T1==2),(T2==4),busTaxiT(Tiem1,Tiem2,A),TiemR is A; 
                    (T1==1),avionT(Tiem1,D),TiemR is D;
                    (T1==2),busT(Tiem1,D),TiemR is D;
                    (T1==3),intermunicipalT(Tiem1,D),TiemR is D;
                    (T1==4),taxiT(Tiem1,D),TiemR is D;
                    (T1==5),carroT(Tiem1,D),TiemR is D.


%%viaje(C1,C2,T,D,[Int1,Int2],[Tra1,Tra2]).

%%al ser solo un viaje, necesita solo un medio, no se daria el caso bus taxi.
%%viaje1(C1,C2),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
%%(medioTiempo(2,4,(Ti11),(Ti22),TiemR)),T is TiemR,
%%obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
%%(medioTiempo(2,4,(Di11),(Di22),DistR)),D is DistR;

%viaje(manizales,pereira,R,T,D,S).
%viaje(manizales,chinchina,R,T,D,S).
viaje(C1,C2,R,T,D,S):- viajeIda(C1,C2,Tr,Dr),T is Tr,D is Dr,sentido(1,Is),S is Is;
                     viajeRegreso(C1,C2,Tr,Dr),sentido(2,Is),T is Tr,D is Dr,S is Is;
                     viaje2(C1,C2,R).%%Cuando no hay camino directo

viajeIda(C1,C2,T,D) :- camino2(C1,C2,_,_),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
                   (medioTiempo(1,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
                   (medioTiempo(1,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C1,C2,_,_),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
                   (medioTiempo(2,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
                   (medioTiempo(2,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C1,C2,_,_),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
                   (medioTiempo(3,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
                   (medioTiempo(3,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C1,C2,_,_),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
                   (medioTiempo(4,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
                   (medioTiempo(4,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C1,C2,_,_),obtenerTiempo(C1,C2,Ti1),Ti11 is Ti1,obtenerTiempo(C1,C2,Ti2),Ti22 is Ti2,
                   (medioTiempo(5,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C1,C2,Di1),Di11 is Di1,obtenerTiempo(C1,C2,Di2),Di22 is Di2,
                   (medioTiempo(5,0,(Di11),(Di22),DistR)),D is DistR.

viajeRegreso(C1,C2,T,D):- camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
                   (medioTiempo(1,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
                   (medioTiempo(1,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
                   (medioTiempo(2,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
                   (medioTiempo(2,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
                   (medioTiempo(3,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
                   (medioTiempo(3,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
                   (medioTiempo(4,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
                   (medioTiempo(4,0,(Di11),(Di22),DistR)),D is DistR;
                   camino2(C2,C1,_,_),obtenerTiempo(C2,C1,Ti1),Ti11 is Ti1,obtenerTiempo(C2,C1,Ti2),Ti22 is Ti2,
                   (medioTiempo(5,0,(Ti11),(Ti22),TiemR)),T is TiemR,
                   obtenerDistancia(C2,C1,Di1),Di11 is Di1,obtenerTiempo(C2,C1,Di2),Di22 is Di2,
                   (medioTiempo(5,0,(Di11),(Di22),DistR)),D is DistR.

viaje2(C1,C2,[]) :- camino2(C1,C2,_,_);camino2(C2,C1,_,_).
viaje2(C1,C2,[Int1,Int2]) :- 
                     camino2(C1,C2,_,_);
                     camino2(C2,C1,_,_);
                     viaje2(C1,Int1,[]),viaje2(Int1,C2,Int2).

getFirElem([X|_],X).

toList([],[]).
toList([P|[_,_]],[P]).
toList([P|[]],[P]).
toList([P|Y],[P|L]):- toList(A,L),getFirElem(Y,A).

%%list_vacia([]):- false.
%%es_lista([_|_]).