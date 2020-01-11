:- module(arbiter,[t/1, ruut/3]).
:- use_module(library(pce)).
:- require([ between/3, call/2, atomic_list_concat/2, term_variables/2]).

% Mängimiseks tuleb muuta ainult ridu 11 ja 12.
% Need read määravad mängijad
% Ise mängimiseks kirjutada "human". Näiteks:
%valged(human)  % valgetega mängib inimine
%mustad(k1)     % mustadega mängib programm k1

%valged(ai).		% Valged
%mustad(human).		% Mustad

valged(human).		% Valged
mustad(ai).		% Mustad

% Seejärel laadida mällu see programm (graphicArbiter) ja võistlevad kabeprogrammid.
%                                                         _________________________

% Mängimiseks "t(h)", pidev mäng (kui mängus on inimene)
%             "t(s)", step mode - kui mõlemad mängijad on programmid
%                      (siis saab tühikut (või muud nuppu) vajutades käikhaaval mängu jälgida.
%                      (vajutades "c" tähte, siis pidev mäng lõpuni)
%                      (pole soovitatav kasutada, kui human on mängus)
%             
%             "t(t)", mäng, milles enne arvuti käiku 0.5 sekundit pausi
%             "t(0.5)", mäng, milles saab pausi pikkuse ise määrata (sekundites)
%                          (0.5 on pool sekundit, 2 on 2 sekundit jne)
%
% Mängu alustades avaneb automaatselt graafiline aken, kus võimalik mängu jälgida.
% Kui mängus on human, siis saab enda käigu ajal enda värvi nuppe liigutada
%     tõstes valitud nuppu hiirega edasi.
%
% Uue mängu alustamiseks ei pea graafilist akent sulgema.
% Kui sisestada uus käsk nt "t(0.2)", siis algab samas aknas uus mäng (algasendist).
%
% Tasub tähele panna, et reeglite järgi on sundlöömine, seega kui ise mängides
%       löömise võimalust tähele ei pane, oled automaatselt kaotanud.
% Programm löömise võimalust kuidagi ette ei ütle.
% 
% Kui saad ühe käiguga lüüa mitu vastase nuppu järjest, siis kõigepealt teha esimene löömine,
% siis annab arbiiter käigu tagasi ja on võimalik teha järgmine löömine
% Programm ka tekstiliselt ütleb, kui on kohustus mingi nupuga löömist jätkata.
%
% Reeglite järgi loetakse mäng viigiks, kui 15 käigu jooksul löömist ei toimu.
%
% Kui ise mängides kõik nupud endal otsa saavad, oled kaotanud - seda arvestab ka programm.
% Kui aga mõni nupp on olemas, kuid ei ole võimalik kuhugi astuda,
%     siis reeglite järgi oled kaotanud - seda programm automaatselt ei arvest, kaotamiseks tuleb teha lihtsalt suvaline vale käik.
% 



:- dynamic computed_image/4, b/1, human_move/4, step_time/1.



square_image(Piece, PieceColour, SquareColour, Image) :-
	computed_image(Piece, PieceColour, SquareColour, Image), !.
square_image(Piece, PieceColour, SquareColour, Image) :-
	image_name(Piece, _, ImageName),
	new(TotalImage, image(ImageName)),
	sub_area(PieceColour, SquareColour, Area), !,
	get(TotalImage, clip, Area, Image),
	send(Image, lock_object, @on),
	send(Image, attribute, attribute(piece, Piece)),
	send(Image, attribute, attribute(colour, PieceColour)),
	asserta(computed_image(Piece, PieceColour, SquareColour, Image)).

%	image_name(?PieceName, ?CheckersProgram Id, ?ImageName)

image_name(empty,	_, 'chesssquare.bm').
image_name(pawn,	p, 'pawn.bm').
image_name(rook,	r, 'rook.bm').

%	sub_area(+PieceColour, +SquareColour, -AreaTerm)

sub_area(white, white, area(32,  0, 32, 32)).
sub_area(white, black, area(0,   0, 32, 32)).
sub_area(black, white, area(32, 32, 32, 32)).
sub_area(black, black, area(0,  32, 32, 32)).




:- pce_global(@square_colour, new(get_method(colour, name, new(vector),
					    @receiver?image?colour))).
:- pce_global(@square_piece, new(get_method(piece, name, new(vector),
					   @receiver?image?piece))).

make_checkers_board(Board) :-
	new(Board, device),
	(   between(0, 7, X),
	    between(0, 7, Y),
		GX is X * 32,
		GY is (7-Y) * 32,
		square_colour(X, Y, Colour),
		square_image(empty, _, Colour, Image),
		send(Board, display,
		     new(Bitmap, bitmap(Image)),
		     point(GX, GY)),
		send(Bitmap, attribute, attribute(square_colour, Colour)),
		send(Bitmap, get_method, @square_piece),
		send(Bitmap, get_method, @square_colour),
		xy_where(X, Y, Where),
		send(Bitmap, name, Where),
	    fail ; true
	).


%	square_colour(+X, +Y, -Colour)

square_colour(X, Y, Colour) :-
	(X+Y) mod 2 =:= 0, !,
	Colour = black.
square_colour(_, _, white).


%	xy_where(?X, ?Y, ?Where).

xy_where(X, Y, Where) :-
	var(Where), !,
	CX is X + 0'a,
	CY is Y + 0'1,
	name(Where, [CX, CY]).
xy_where(X, Y, Where) :-
	name(Where, [CX, CY]),
	X is CX - 0'a,
	Y is CY - 0'1.



%	put_piece(+Board, +Piece, +Colour, +Where)

put_piece(Board, Piece, Colour, Where) :-
	get(Board, member, Where, Bitmap),
	get(Bitmap, square_colour, SquareColour),
	square_image(Piece, Colour, SquareColour, Image),
	send(Bitmap, image, Image),
	send(Bitmap, flush).


%	move(+Board, +From, +To).

move(Board, From, To) :-
	get(Board, member, From, FromBitmap),
	get(FromBitmap, piece, Piece),
	get(FromBitmap, colour, Colour),
	put_piece(Board, Piece, Colour, To),
	put_piece(Board, empty, _, From).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The predicate below resets the board to the initial position.  Test it
using:

	3 ?- initial_position(@cb).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initial_position(Board) :-
	retractall(b(_)),
	assert(b(Board)),
	between(0, 7, X),
	between(0, 7, Y),
	    initial_piece(X, Y, Piece, Colour),
	    xy_where(X, Y, Where),
	    put_piece(Board, Piece, Colour, Where),
	fail.
initial_position(_).

initial_piece(X, Y, pawn,   white) :- 
	Y < 3,
	Z is (X + Y) mod 2,
	Z is 0,
	!.
initial_piece(X, Y, pawn,   black) :- 
	Y > 4,
	Z is (X + Y) mod 2,
	Z = 0,
	!.
initial_piece(_, _, empty,  black).


:- pce_global(@move_piece_gesture, make_move_piece_gesture).

make_move_piece_gesture(G) :-
	new(G, move_outline_gesture(left)),
	send(G, send_method,
	     send_method(verify, vector(event),
			 message(@prolog, verify, @receiver, @arg1))),
	send(G, send_method,
	     send_method(terminate, vector(event),
			 message(@prolog, terminate, @receiver, @arg1))).



verify(Gesture, Event) :-
	get(Event, receiver, SquareBitmap),
	\+ get(SquareBitmap, piece, empty),
	send(Gesture, send_class, verify, Event).



terminate(Gesture, Event) :-
	get(Event, receiver, FromBitmap),
	get(FromBitmap, device, Board),		  % the board
	get(Gesture, outline, Box),
	get(Box, area, Area),			  % current area
	send(Box, device, @nil),		  % Undisplay the outline
	get(Board?graphicals, find_all,
	    message(@arg1?area, overlap, Area),
	    OverlappingSquares),
	send(OverlappingSquares, sort,
	     ?((?(@arg1?area, intersection, Area)) ? measure, compare,
	       (?(@arg2?area, intersection, Area)) ? measure)),
	get(OverlappingSquares, tail, Best),
	get(Best, name, ToLocation),
	get(FromBitmap, name, FromLocation),
	get(FromBitmap, piece, Piece),
	user_move(Board, Piece, FromLocation, ToLocation).



attach_recognisers(Board) :-
	send(Board?graphicals, for_all,
	     message(@arg1, recogniser, @move_piece_gesture)).



user_move(_, _, FromLocation, FromLocation):- !.

user_move(Board, _, FromLocation, ToLocation) :-
	xy_where(Y1M, X1M, FromLocation),
	xy_where(Y2M, X2M, ToLocation),
	X1 is X1M + 1, Y1 is Y1M + 1,
	X2 is X2M + 1, Y2 is Y2M + 1,
	S is (X2 + Y2) mod 2,
	S = 0,
	assert(human_move(X1, Y1, X2, Y2)),
	%listing(human_move/4),
	move(Board, FromLocation, ToLocation).
user_move(_,_,_,_).


checkers_move_name(_Piece, F, T, Move) :-
	checkers_coordinate(F, CF),
	checkers_coordinate(T, CT),
	atomic_list_concat([CF, CT], Move).


checkers_coordinate(Where, C) :-
	xy_where(X, Y, Where),
	CY is Y + 1,
	checkers_x(X, CX),
	atom_concat(CX, CY, C).

checkers_x(0, a).
checkers_x(1, b).
checkers_x(2, c).
checkers_x(3, d).
checkers_x(4, e).
checkers_x(5, f).
checkers_x(6, g).
checkers_x(7, h).


checkers:-
	new(Window, window('Kabe', size(8*32, 8*32))),
	make_checkers_board(Board),
	send(Window, display, Board),
	attach_recognisers(Board),
	initial_position(Board),
	send(Window, open),
	send(Window, done_message,
	     and(if(message(Board?process, kill)),
		 and(message(@prolog, close_b_window), message(Window, destroy)))).

close_b_window:-
	retractall(b(_)).
%:- module(arbiter,[turniir/0, ruut/3]).
%:- module(arbiter,[t/1, ruut/3]).
:- dynamic ruut/3,ruut_old/3,vottis/1,siire/3,siire_1/3,moves_abi/2,valekaik/1,move_count/1, modeCntr/1.

% qcompile('C:/Users/Jüri/Desktop/Mina/Minu Programmid/Kabe/arbiiter.pl').
%===================================== Turniir ====================================
%turniir:-
t(_):- % Enne igat mängu paneb arvuti ooteaja 0.1 sekundi peale.
	retractall(step_time(_)),
	assertz(step_time(0.1)),
	fail.
t(Seconds):-
	number(Seconds),
	retractall(step_time(_)),
	assertz(step_time(Seconds)),
	fail.
t(t):-
	retractall(step_time(_)),
	assertz(step_time(0.5)),
	fail.
t(Mode):-
	korista,
	prepare(Mode),
	(b(_); checkers),
	graph_board(1),
    repeat,
		modeCntr(Mode1),
		make_move(Color,ColorN),
		verify_turn(ColorN),			% kas käib lubatud nupuga ja kas käik korrektne?
		show_board0(ColorN,Mode1),
		graph_board(1),
		end_condition,
		announce_winner(Color),
	korista,				% kommenteeritud välja testimise ajaks!!!!!!!!
	!.
	


human(Color,_,_):-
	aggregate_all(count, (sama_varv(Color, C), ruut(_,_,C)), Count),
	Count = 0. % Kui inimisel ühtegi nuppu alles ei ole, siis inimene kaotab.
human(Color,X,Y):-
	retractall(human_move(_,_,_,_)),
	((Color = 1, write('Ootan kaiku (valged): ')); (Color = 2, write('Ootan kaiku (mustad): '))),
	((X = 0); (X \= 0, write('Jatka loomist nupuga: '), write(X), write(' '), write(Y), write(' '))),
	flush_output,
	repeat,
	sleep(0.5),
	write('.'),
	flush_output,
	human_move(X1, Y1, X2, Y2),
	ruut(X1, Y1, C),
	%listing(human_move/4),
	retract(human_move(X1, Y1, X2, Y2)),
	%retract(ruut(X1,Y1,_)),
	retract(ruut(X2,Y2,_)),
	%assert(ruut(X1,Y1,0)),
	%write(">>"), write(Y2), nl,
	human_tamm(X2, C, C2),
	assert(ruut(X2,Y2,C2)),
	retract_between(X1, Y1, X2, Y2),
	!.
human(_,_,_).

human_tamm(8, 1, 10).
human_tamm(1, 2, 20).
human_tamm(_, C, C).

retract_between(X1, Y1, X1, Y1).
retract_between(X1, Y1, X2, Y2):-
	retractall(ruut(X1, Y1, _)),
	assert(ruut(X1, Y1, 0)),
	%% Faster update:
	 graph_remove(X1, Y1),
	dir(X1, X2, DX),
	X3 is X1 + DX,
	dir(Y1, Y2, DY),
	Y3 is Y1 + DY,
	%write(X1),write(Y1),nl,
	retract_between(X3, Y3, X2, Y2), !.
retract_between(_,_,_,_).

graph_remove(X1, Y1):-
	X is X1 - 1,
	Y is Y1 - 1,
	xy_where(Y, X, Where),
	b(Board),
	put_piece(Board, empty, _, Where).

dir(A, B, 1):-
	A < B, !.
dir(_, _, -1).
	
	
%======================== KÄIGU ÕIGUSE ANDMINE JA SOORITUS ===============================
make_move(Color,ColorNN):-
	 retract(colorCC(Color)),
	 players_turn0(Color,ColorN,_),
	assert(colorCC(ColorN)),
	record_board_status,							% kopeeri laua seis enne käiku
	siire(_,Vottis,(X,Y)),
	retract(move_count(M)),
	(Vottis=[],X1=0,Y1=0,ColorNN=ColorN,		% Kui eelnevalt ei võtnud, saab mängija X=0, Y=0
		M1 is M+1,assert(move_count(M1));		% Võtmiseta käikude loendamine
		assert(move_count(0)),					% Võtmiseta käikude loenduri reset
	millega_kaia(ColorN,(X,Y),(X1,Y1),ColorNN)),	% Kui võttis, vaatab kas sama nupuga saab veel võtta
	retract(colorCC(_)),assert(colorCC(ColorNN)),
	correct_player(ColorNN,PlayerNN),
%	listing(ruut),
	((PlayerNN \= human, step_time(Seconds), sleep(Seconds)); (PlayerNN = human)),
	 Turn=..[PlayerNN,ColorNN,X1,Y1],			% Kui sellega saab veel võtta, annab nupu X ja Y ja Color
%	nl,write('KÄIB: '),write(Turn),nl,
%	get0(_),
	 Turn,						% Käigu sooritamine
	!.
%================= Vaatab kellele anda käimisõigus ===================
reset_color(10,1):- retract(colorCC(_)),assert(colorCC(1)),!.
reset_color(20,2):- retract(colorCC(_)),assert(colorCC(2)),!.
reset_color(Color,Color).

reset_color1(1,10).
reset_color1(2,20).
reset_color1(20,2).
reset_color1(10,1).
reset_color1(C,C).

correct_player(1,PlayerNN):-
	valged(PlayerNN),!.
correct_player(10,PlayerNN):-
	valged(PlayerNN),!.
correct_player(2,PlayerNN):-
	mustad(PlayerNN),!.
correct_player(20,PlayerNN):-
	mustad(PlayerNN).
%-------------------
players_turn0(Color,ColorN,Player):-
	võttis([]),
	players_turn(Color,ColorN,Player),!.	% pöördub algselt genereeritud fakti poole
players_turn0(Color,ColorN,Player):-		% Kui võttis ja saab veel võtta sama nupuga, jätkab sama mängija
	partner_color(Color,ColorP),
	players_turn(ColorP,_,Player),		% pöördub algselt genereeritud fakti poole
	siire(_,_,XY),
	kas_sai_tammiks(Color,XY,ColorN),!.
%----------------------
kas_sai_tammiks(1,(8,_),10):- !.		% valge sai tammiks
kas_sai_tammiks(2,(1,_),20):- !.		% must sai tammiks
kas_sai_tammiks(Color,_,Color).			% ei saanud tammiks
%----------------------
millega_kaia(Color,(X,Y),(0,0),ColorNN):-		% Kui ei saa edasi võtta, siis saab käiguõiguse vastane
	ruut_old(X,Y,Z),
	generate_moves((X,Y,Z),Moves),			% genereerime selle nupu legaalsed käigud
	findall(Võttis,(member([Võttis,_],Moves),	% kui leitud käikude hulgas võtmist ei ole?
	not(Võttis=[])),[]),
	partner_color(Color,ColorNN),!.
millega_kaia(Color,(X,Y),(X,Y),ColorNN):-
	reset_color(Color,ColorNN),!.			% peab sama nupuga edasi võtma, millega võttis eelmisel käigul

%===================VERIFY: Käigu reeglitele vastavuse kontroll=======================

verify_turn(Color):-						% Võrdle ruut_old/3 ja ruut/3 fakte ja
	kaiv_nupp(Color,(X,Y),Kuhu,Võttis),		% Leia mis nupp käis
	retract(siire_1(_,_,_)),
	retract(siire((X0,Y0),Võttis0,Kuhu0)),
	assert(siire_1((X0,Y0),Võttis0,Kuhu0)),	% Salvesta eelmine käik
	assert(siire((X,Y),Võttis,Kuhu)),		% Salvesta jooksev käik
	ruut_old(X,Y,Z),
	generate_all_legal_moves((X,Y,Z),Võttis),	% genereeri selle nupu legaalsed käigud
	moves_abi((X,Y,_),Moves),
	Kaik=[Võttis,KuhuList],
	member(Kaik,Moves),
	member(Kuhu,KuhuList),
	retractall(võttis(_)),
	retractall(moves_abi(_,_)),
	assert(võttis(Võttis)),!.
verify_turn(Color):-
	(Color=1,Who=' WHITEs '; Who=' BLACKs '),
	nl,write('Player with '), write(Who), write(' did not make or failed a move!'),nl,
	assert(valekaik(Color)),
	retractall(moves_abi(_,_)),!.

kaiv_nupp(Color,(X0,Y0),(X2,Y2),Võttis):-
	ruut_old(X0,Y0,Color),ruut(X0,Y0,0),		% leia käiv nupp
	ruut_old(X2,Y2,0), ruut(X2,Y2,ColorN),		% leia sihtruut
	kas_sai_tammiks(Color,(X2,Y2),ColorN),		% võtmisega võib saada tammiks
	partner_color(Color,ColorP),
	(sama_varv(ColorP,ColorPP),ruut_old(X1,Y1,ColorPP),ruut(X1,Y1,0),Võttis = (X1,Y1);Võttis=[]),!.
kaiv_nupp(Color,(X0,Y0),(X2,Y2),Võttis):-		% kui nupp on juba tamm
	Color10 is Color * 10,
	ruut_old(X0,Y0,Color10),ruut(X0,Y0,0),		% leia käiv nupp
	ruut_old(X2,Y2,0), ruut(X2,Y2,Color10),		% leia sihtruut
	partner_color(Color,ColorP),
	(sama_varv(ColorP,ColorPP),ruut_old(X1,Y1,ColorPP),ruut(X1,Y1,0),Võttis = (X1,Y1);Võttis=[]),!.
%---------------------------
sama_varv(1,10).
sama_varv(10,1).
sama_varv(2,20).
sama_varv(20,2).
sama_varv(C,C).

%------------------------------ Genereeri antud seisust kõik legaalsed käigud -----------------------------

generate_all_legal_moves((X,Y,_),Võttis):-		% Kui eelmisel käigul võttis, proovib kas saab samaga veel
	not(Võttis=[]),					% Võttis ei tohi olla tühi
	ruut_old(X,Y,ColorN),
	generate_moves((X,Y,ColorN),Moves),
	findall([Võttis,Kuhu],(member([Võttis,Kuhu],Moves),not(Võttis=[])),MovesXY),	% Leia võtmisega käigud
	not(MovesXY=[]),
	assert(moves_abi((X,Y,ColorN),Moves)),!.
generate_all_legal_moves((_,_,Color),_):-		% Kui saab suva nupuga võtta, siis salvesta ainult võtmiskäigud
	reset_color1(Color,ColorN),			% Proovi nii nupu kui tammi koodiga
	ruut_old(X,Y,ColorN),
	generate_moves((X,Y,ColorN),Moves),		% genereeri käigud ühele nupule
	findall([Võttis,Kuhu],(member([Võttis,Kuhu],Moves),not(Võttis=[])),MovesXY),	% Leia võtmisega käigud
	not(MovesXY=[]),
	assert(moves_abi((X,Y,ColorN),MovesXY)),
	fail.
generate_all_legal_moves((_,_,_),_):-			% Sai võtta ja rohkem ei otsi käike
	moves_abi(_,_),!.
generate_all_legal_moves((_,_,Color),_):-		% Kui ei saa üldse võtta, leia kõik muud käigud
	reset_color1(Color,ColorN),			% Proovi nii nupu kui tammi koodiga
	ruut_old(X,Y,ColorN),
	generate_moves((X,Y,ColorN),Moves),
	assert(moves_abi((X,Y,ColorN),Moves)),
	fail.
generate_all_legal_moves((_,_,_),_).
%------------------------------------------------------------
%	Kõigi käikude genereerimine nupule (X0,Y0)
%-----------------------------------------------------------
generate_moves((X0,Y0,1), Moves):-		% Kui tavaline nupp ja valge
	ruut_old(X0,Y0,1),			% Kas niisugune nupp on üldse olemas?
	nupuga(X0, Y0,1,1,Moves),!.
generate_moves((X0,Y0,2), Moves):-		% Kui tavaline nupp ja must
	ruut_old(X0,Y0,2),			% Kas niisugune nupp on üldse olemas?
	nupuga(X0, Y0,2,-1,Moves),!.
generate_moves((X0,Y0,Value), Moves):-
	Value > 2,				% Kas nupp on tamm?
	ruut_old(X0,Y0,Value),			% Kas niisugune nupp on olemas?
	tammiga(X0,Y0,L0), list_to_set(L0,Moves),!.
%	write((X0,Y0,Value)),write('   '),write(Moves),nl,nl.	% TESTIMISEKS!!!!
%===================== Tavalise nupu käigud antud positsioonilt====================================
nupuga(X0, Y0,Color,XSuund,Kaigud):-
	vota_nupuga(X0,Y0,Color,(1,1), Kaik1),					% Genereeri nupuga võimalikud võtmised suunas (1,1)
	vota_nupuga(X0,Y0,Color,(1,-1), Kaik2),	append(Kaik1,Kaik2,Kaik12),	% Genereeri nupuga võimalikud võtmised suunas (1,-1)
	vota_nupuga(X0,Y0,Color,(-1,1), Kaik3),	append(Kaik12,Kaik3,Kaik123),	% Genereeri nupuga võimalikud võtmised suunas (-1,1)
	vota_nupuga(X0,Y0,Color,(-1,-1), Kaik4),append(Kaik123,Kaik4,Kaik1234),	% Genereeri nupuga võimalikud võtmised suunas (-1,-1)
		(Kaik1234=[],						% Kui vähemalt ühe on nupuga võetud, siis lihtsaid käike enam ei vaadata
	kai_nupuga(X0,Y0,Color,(XSuund,1),Kaik5),
	kai_nupuga(X0,Y0,Color,(XSuund,-1),Kaik6),
	append(Kaik5,Kaik6,Kaigud);
	Kaigud=Kaik1234),!.
%------------------ nupuga võtmised-------------
vota_nupuga(X0,Y0,Color,(DX,DY), [[(X1,Y1),[(X2,Y2)]]]):-
	X1 is X0+DX, Y1 is Y0 + DY,      kas_laual(X1,Y1),
	X2 is X1 + DX, Y2 is Y1 + DY,    kas_laual(X2,Y2),
	partner_color(Color,Color1),
	ruut_old(X1,Y1,Color1),
	ruut_old(X2,Y2,0),!.
vota_nupuga(_,_,_,_, []).
%------------------ nupu tavalised käigud-------------
kai_nupuga(X0,Y0,_,(DX,DY),[[[],[(X1,Y1)]]]):-
	X1 is X0+DX, Y1 is Y0 + DY,      kas_laual(X1,Y1),
	ruut_old(X1,Y1,0),!.
kai_nupuga(_,_,_,_,[]).

%====================== Tammi käigud antud positsioonilt ==============================================
tammiga(X0, Y0,Kaigud):-
	findall((X,Y), (ruut(X,Y,_), tingimus(X0,Y0,X,Y)), L0), delete(L0,(X0,Y0), Diagonals0),		% Leida ruudu (X0,Y0) diagonaalruudud
	list_to_set(Diagonals0, Diagonals),
	vota_tammiga((X0,Y0), Diagonals, _, Post_posits),					% Kui saab võtta, tagastab võtmise järgsed positsioonid ja
	(Post_posits=[], findall((XX,YY), (member((XX,YY), Diagonals), ruut_old(XX,YY,0)), VabadRd),	% võetava nupu koordinaadid
	kai_tammiga([(X0,Y0)], VabadRd, Vabad0), delete(Vabad0, (X0,Y0), Vabad),
	append([[]],[Vabad],Kaigud0),Kaigud=[Kaigud0];							% kui ei, tagastab mis ruudud on nendest liikumiseks vabad
	Kaigud=Post_posits).
%------------------ Tammiga võtmised-------------
vota_tammiga((X0,Y0),Diagonals,VoetavadNupud,Post_posits):-						% Voetavad=[[(X,Y), Post_positions],...]
	findall((X,Y), (member((X,Y),Diagonals), voetav((X0,Y0),(X,Y))), VoetavadNupud),
	find_post_pos((X0,Y0),VoetavadNupud,Post_posits).
%------------------------------------------------
find_post_pos(_,[],[]).								% Leia võimalikud järelpositsioonid peale võtmist
find_post_pos((X0,Y0),[(X,Y)|VoetavadNupud], [[(X,Y),PostsXY]|Post_posits]):-
	Vx is sign(X-X0), Vy is sign(Y-Y0),
	X1 is X + Vx, Y1 is Y + Vy,
	find_post_pos1((X1,Y1),(Vx,Vy),PostsXY),
	find_post_pos((X0,Y0),VoetavadNupud,Post_posits).

find_post_pos1((X,Y),(Vx,Vy),[(X,Y)|PostsXY]):-
	ruut_old(X,Y,0),
	X1 is X + Vx, Y1 is Y + Vy,
	find_post_pos1((X1,Y1),(Vx,Vy),PostsXY).
find_post_pos1(_,_,[]).

voetav((X0,Y0),(X,Y)):-
	colorCC(Color),
	partner_color(Color,Vastase_varv),
	ruut_old(X,Y,Vastase_varv),
	Vx is sign(X-X0), Vy is sign(Y-Y0),
	X1 is X0 + Vx, Y1 is Y0 + Vy,
	kas_vahel_vabad((X1,Y1),(X,Y),(Vx,Vy)),
	X2 is X + Vx, Y2 is Y + Vy,	ruut_old(X2,Y2,0).			% kas järel vaba ruut?

kas_vahel_vabad((X,Y),(X,Y),_):- !.						% Kas võetava ja käidava nupu vahel on vabad ruudud?
kas_vahel_vabad((Xi,Yi),(X,Y),(Vx,Vy)):-
	ruut_old(Xi,Yi,0),
	Xi1 is Xi + Vx, Yi1 is Yi + Vy,
	kas_vahel_vabad((Xi1,Yi1),(X,Y),(Vx,Vy)).

%--------------- tingimused, mida tammi liikumisteed peavad rahuldama---------------------------
tingimus(X0,Y0,X,Y):-
	kas_laual(X0,Y0),
	X =:=Y+X0-Y0.
tingimus(X0,Y0,X,Y):-
	kas_laual(X0,Y0),
	X =:= -Y + (X0+Y0).

%--------------------------------Vabade ruutude leidmine, kuhu tamm saab käia-------------------------
% Front liigub pikki vabasid ruute nupust sammu kaupa  kaugemale ja tagastab vabade ruutude listi
%-----------------------------------------------------------------------------------------------------
kai_tammiga([],_,[]):- !.
kai_tammiga(_,[],[]):- !.
kai_tammiga(Front,Space,Eligible):-
	subtract(Space,Front,RemainingSpace),
	findall((X,Y),constraint((X,Y),RemainingSpace,Front),NewFront),
	kai_tammiga(NewFront,RemainingSpace,Eligible0),
	append(Front,Eligible0,Eligible).

constraint((X,Y),RemainingSpace,Front):-
	member((X,Y),RemainingSpace),
	neighbourIn((X,Y),Front).

neighbourIn((X,Y), [(X1,Y1)|_]):-		% kas (X,Y)-l leidub hulgas Front vahetu naaber
	abs(abs(X) - abs(X1)) =:=1,
	abs(abs(Y) - abs(Y1)) =:=1,!.
neighbourIn((X,Y), [_|Front]):-
	neighbourIn((X,Y), Front),!.
%========================== END MAKE_MOVE==========================

end_condition:-
	valekaik(_),!.
end_condition:-
	check_fp,
	reset_seis,
	fixpoint(yes).
end_condition:-	
	move_count(M), 
	M >= 20.

%================== Kontrolli püsipunkti===========================
check_fp:-
    abolish(fixpoint/1),
    assert(fixpoint(yes)),
    ruut(X,Y,Color),
    check_fp1(X,Y,Color),
    fail.
check_fp.

check_fp1(X,Y,Color):-
    retract(ruut_o(X,Y,Color)),!.	% Kas leidub samade parameetritega ruut
check_fp1(_,_,_):-
    retract(fixpoint(_)),
    assert(fixpoint(no)),!.

reset_seis:-
    abolish(ruut_o/3),
    ruut(X,Y,Color),
    assert(ruut_o(X,Y,Color)),
    fail.
reset_seis.

%==================================================================
%-------------------------------------------------------------------------------------------------------------------
prepare(Mode):-
	tee_ruut_faktid,
	assert(modeCntr(Mode)),
	assert(move_count(0)),
	mustad(Functor2),	assert(players_turn(1,2,Functor2)),
	valged(Functor1),	assert(players_turn(2,1,Functor1)),
	assert(siire_1((0,0),[],(0,0))),
	assert(siire((0,0),[],(0,0))),
	assert(arv(0)),
	abolish(colorCC/1),
	assert(colorCC(2)),	% Kui alustab VALGE, siis 2
	reset_seis,
	assert(võttis([])),!.
%--------------
korista:-
%	abolish(mustad/1), abolish(valged/1),
	retractall(ruut(_,_,_)),
	retractall(ruut_old(_,_,_)),
	retractall(ruut_o(_,_,_)),
	retractall(modeCntr(_)),
	retractall(move_count(_)),
	retractall(siire(_,_,_)),
	retractall(siire_1(_,_,_)),
	retractall(arv(_)),
	retractall(colorCC(_)),
	retractall(players_turn(_,_,_)),
	retractall(võttis(_)),
	retractall(moves_abi(_,_)),
	retractall(valekaik(_)),
  	retractall(fixpoint(_)),
	!.
%--------------
kas_laual(X,Y):-
	1=< X, X=<8,
	1=< Y, Y=<8,!.
%-------------------------
partner_color(1, 2).
partner_color(1, 20).
partner_color(10, 2).
partner_color(10, 20).
partner_color(2, 1).
partner_color(2, 10).
partner_color(20, 1).
partner_color(20, 10).
%-------------------------


%============================= Announce_winner =====================================
announce_winner(_):-	% Kui viik
		move_count(M), 
		M >= 20,
		nl, write('The game is a draw (no winner). '),
		arv(NNN),nl,write('Total number of moves is '), write(NNN).
announce_winner(Color):-
    players_turn(_,Color,Winner),
    nl, write('The winner is program \"'),write(Winner), write('\"'),write(' playing with color '), write(Color),
    arv(NNN),nl,write('Total number of moves is '), write(NNN).
%-------------
record_board_status:-
	abolish(ruut_old/3),
	ruut(X,Y,Z), kas_laual(X,Y),
	assert(ruut_old(X,Y,Z)),
	fail.
record_board_status.

% ======================== Mängulaua seisu kuvamine =======================
show_board0(_,_):-
	valekaik(_),!.
show_board0(Color,Mode):-
	retract(arv(NN)), NN1 is NN + 1,assert(arv(NN1)),
	nl,nl,write('Move no '),write(NN1),				%write('--  Player\'s  '), 
	(Color=1, Who=':   WHITE ';Who=':   BLACK '),write(Who),	%write(' turn: '),
	siire(Kust,Võttis,Kuhu), 
	translate(Kust,Kust1),
	write(Kust1),write('--'),
	translate(Võttis,Võttis1),
	write(Võttis1),
	write('-->'),
	translate(Kuhu,Kuhu1),
	write(Kuhu1),nl,
	write('   ------------------------------'),nl,
	show_board(8),
	mode(Mode),!.
show_board0(_).

translate((X,Y),(Y1,X)):-
	y_to_letter(Y,Y1),!.
translate([X,Y],[Y1,X]):-
	y_to_letter(Y,Y1),!.
translate([],[]).

y_to_letter(1,'A'). y_to_letter(2,'B'). y_to_letter(3,'C'). y_to_letter(4,'D').
y_to_letter(5,'E'). y_to_letter(6,'F'). y_to_letter(7,'G'). y_to_letter(8,'H').

mode(s):-
	write(' (Hit "c" to continue without breaks/ Hit any other key to continue in step mode)'),
	get_single_char(Char),(Char=99, retractall(modeCntr(_)),assert(modeCntr(c));true),!.
mode(_).


graph_board(9).
graph_board(X):-
	X < 9,
	graph_board(X, 1),
	X1 is X + 1,
	graph_board(X1) ,!.
graph_board(_).
graph_board(_, 9).
graph_board(X, Y):-
	ruut(X, Y, P),
	b(Board),
	X1 is X - 1,
	Y1 is Y - 1,
	xy_where(Y1, X1, Where),
	((P = 0, put_piece(Board, empty, _, Where), fail); (P = 1, C = white, N = pawn);
	 (P = 2, C = black, N = pawn); (P = 10, C = white, N = rook); (P = 20, C = black, N = rook)),
	put_piece(Board, N, C, Where),
	fail.
%graph_board(X, Y):-
%	\+ruut(X, Y, _),
%	b(Board),
%	X1 is X - 1,
%	Y1 is Y - 1,
%	xy_where(Y1, X1, Where),
%	put_piece(Board, empty, _, Where),
%	fail.
graph_board(X, Y):-
	Y1 is Y + 1,
	graph_board(X, Y1).
 
	

show_board(0):- 
	write('   ------------------------------'),nl,
	write('      A B C D E F G H'),nl,!.
show_board(X):-
    write(X),write(' | '), show_board1(X,1),   write(' |'),nl,
    X1 is X - 1,!,
    show_board(X1).

show_board1(_,9):- !.
show_board1(X,Y):-
    (ruut(X,Y,Color),  show_board2(Color); write('    ')),
    Y1 is Y + 1,!,
    show_board1(X,Y1).

show_board2(Color):-
	Color > 2, write(Color),!. % tammi puhul üks tühik vähem
show_board2(Color):-
	write(' '),write(Color).
%========================================= MÄNGU ALGSEIS ================================
tee_ruut_faktid:-
	abolish(ruut/3),
	% Valged
	assert(ruut(1,1,1)),
	assert(ruut(1,3,1)),
	assert(ruut(1,5,1)),
	assert(ruut(1,7,1)),
	assert(ruut(2,2,1)),
	assert(ruut(2,4,1)),
	assert(ruut(2,6,1)),
	assert(ruut(2,8,1)),
	assert(ruut(3,1,1)),
	assert(ruut(3,3,1)),
	assert(ruut(3,5,1)),
	assert(ruut(3,7,1)),
	% Tühjad ruudud
	assert(ruut(4,2,0)),
	assert(ruut(4,4,0)),
	assert(ruut(4,6,0)),
	assert(ruut(4,8,0)),
	assert(ruut(5,1,0)),
	assert(ruut(5,3,0)),
	assert(ruut(5,5,0)),
	assert(ruut(5,7,0)),
	% Mustad
	assert(ruut(6,2,2)),
	assert(ruut(6,4,2)),
	assert(ruut(6,6,2)),
	assert(ruut(6,8,2)),
	assert(ruut(7,1,2)),
	assert(ruut(7,3,2)),
	assert(ruut(7,5,2)),
	assert(ruut(7,7,2)),
	assert(ruut(8,2,2)),
	assert(ruut(8,4,2)),
	assert(ruut(8,6,2)),
	assert(ruut(8,8,2)).
/*
 ================================ Mängija programmi vormistamise reeglid: =======================
 1. Programm peab olema vormistatud mooduli kujul
	:- module(mooduli_nimi, mooduli_peapredikaat/3).
		kus peapredikaadi parameetriks on selle programmi nuppude värv, X ja Y koordinaat
 2. Moodulis ei tohi olla defineeritud staatilisi fakte ruut/3 ja  definitsiooni :- dynamic ruut/3.
 3. Mängija programmist ei tohi väljuda fail-ga. Seetõttu on soovitav kasutada mängija peapredikaadi järgmist struktuuri:

	main_pgm(Color,X,Y):-
		...., !.
	main_pgm(_,_,_).

 4. Arbiiter salvestab seisu (ruut_old), annab juhtimise mängija programmile, saab käigu tulemuse, vaatab mis nupuga käidi.
	kontrollib kas õige mängija nupp ja kas see nupp eksisteerib
	genereerib ise sama nupu lubatud käikude hulga ja kontrollib kas käik sisaldub selles hulgas.
	Kui käik on võtmisega ja sama nupuga saab veel võtta, siis arbiiter annab õiguse samale mängijale koos võtva nupu koordinaatidega.
	Kui sama nupuga ei saa rohkem võtta, ei pea ka sama nupuga käima.
	Kui ei olnud võtmist, on parameetrid X=Y=0 ja käia võib suvalise nupuga
5. Lõpetamistingimused:
	5.1 Kui käik rikub reegkeid, võidab vastane.
	5.2 Kui ei ole rohkem käike, võidab see, kes teeb viimase käigu.
	5.3 Kui mõlema blokeerumisel käikude arv ja nuppude arv võrdne, siis viik.
	5.4 Kui 10 käigu järel ei ole seis muutunud, siis viik.

===============Mängu ettevalmistamine====================
Käsurealt defineerida faktid:    assert(mustad(Predik_nimi1)), assert(valged(Prdik_nimi2)).
Testimisel: assert(mustad(main)), assert(valged(main)).
=================Mängu käivitamine============================
 Laadida esmalt mällu arbiter.pl ja seejärel mängijate programmide failid
 Kutsuda välja predikaat "turniir/0"
==========================================================
*/

%%%%%TESTid
t1:- retract(ruut(3,3,_)),retract(ruut(4,4,_)),assert(ruut(3,3,0)),assert(ruut(4,4,1)).
t2:- retract(ruut(6,2,_)),retract(ruut(5,3,_)), assert(ruut(6,2,0)),assert(ruut(5,3,2)).
t3x:- retract(ruut(2,2,_)),retract(ruut(3,3,_)), assert(ruut(2,2,0)),assert(ruut(3,3,1)).
t3:- retract(ruut(4,4,_)),retract(ruut(5,3,_)),retract(ruut(6,2,_)), assert(ruut(4,4,0)),assert(ruut(5,3,0)),assert(ruut(6,2,1)).
t5:- retract(ruut(6,8,_)),retract(ruut(5,7,_)),retract(ruut(4,6,_)), assert(ruut(6,8,0)),assert(ruut(5,7,0)),assert(ruut(4,6,10)).
