:- multifile draw/2.
:- multifile route/3.
:- multifile allen/3.
:- multifile distance/3.
:- multifile orientation/3.
:- dynamic buffer/1.
:- dynamic relation/1.

:-

  nl,nl,write('Don\'t forget to set the buffer distance, and to derive the adjacent relation.'),nl,nl.

%facts
qcn_valid_relation(before).
qcn_valid_relation(meets).
qcn_valid_relation(overlaps).
qcn_valid_relation(starts).
qcn_valid_relation(finishes).
qcn_valid_relation(during).

converse(before,after).
converse(meets,met_by).
converse(during,during_inv).
converse(starts,started_by).
converse(finishes,finished_by).
converse(overlaps,overlapped_by).
converse(equal,equal).

converse(after,before).
converse(met_by,meets).
converse(during_inv,during).
converse(started_by,starts).
converse(finished_by,finishes).
converse(overlapped_by,overlaps).
  
  
 
%----------rules----------- 
  
derive_relations :-
  nl,write('Deriving Buffering Distance'),nl,flush,
  %set_buffer_distance,
  nl,write('Scale and Transform the Metric Map'),nl,flush,
  transform_all,
  nl,write('Deriving Proximity'),nl,flush,
  derive_proximity_relations,
  nl,write('buffuer around streets of city-block'),nl,flush,
  %city_block_polyline_buffer,
  nl,write('Deriving buffer_streets'),nl,flush,
  derive_buffer_streets,
  nl,write('Deriving ordering relations between ALL objects '),nl,flush,
  %derive_ordering_relations,
  nl,write('Deriving ordering relations between only LANDMARKS with in CITY-BLOCK '),nl,flush,
  derive_ordering_relations_CB,
  nl,write('Deriving NONE relations'),nl,flush,
  derive_none_relations.


  
create_qualify_file(Fn) :-

  open(Fn, write, Stream),
 
  forall(
   (
	 relation(ordering(Rel,A,B)),
	 qcn_valid_relation(Rel)
	),
    record_relation(Stream, Rel,A,B)
   ),
  
  forall(
   (route(Rel,A,B),
    qcn_valid_relation(Rel)),
    record_relation(Stream, Rel,A,B)
  ),
  
  forall(
   (relation(general(Rel,A,B))),
   record_relation(Stream, Rel,A,B)
  ),
  close(Stream).

  

derive_proximity_relations :-
  retractall(relation(proximity(adjacent,_,_))),
  forall((
  adjacent(A,B)
  ),(
  assert(relation(proximity(adjacent,A,B))),write('.'),flush
  )).
 
%ordering_of_LANDMARKS_with_in_city_block_CB

derive_ordering_relations_CB :-
  retractall(relation(sm_ordering(_,_,_))),
  forall((
    object(A,_), object(B,_), 
	A \= B,
    %% ignore if already derived the converse
    sm_ordering(Rel,A,B,CB),
    not(relation(sm_ordering(_,A,B,CB))),
    not(relation(sm_ordering(_,B,A,CB)))
   ),(
    converse(Rel,ConvRel),
    assert(relation(sm_ordering(Rel,A,B,CB))),
	assert(relation(sm_ordering(ConvRel,B,A, CB))),
	write('.'),flush
  )). 
  

derive_ordering_relations :-
  retractall(relation(ordering(_,_,_))),
  forall((
    %object(A,_), object(B,_), 
	object(A,landmark), object(B,landmark),
	A \= B,
    %% ignore if already derived the converse
    ordering(Rel,A,B),
    not(relation(ordering(_,A,B))),
    not(relation(ordering(_,B,A)))
   ),(
    converse(Rel,ConvRel),
    assert(relation(ordering(Rel,A,B))),
	assert(relation(ordering(ConvRel,B,A))),
	write('.'),flush
  )).
  
derive_none_relations:-
  retractall(relation(general(none,_,_))),
  forall((
  object(A,_), object(B,_), 
  A \= B,
  %% remove converse NONE relations between objects
  has_no_relation(A,B),
	not(relation(general(_, A, B))),
    not(relation(general(_, B, A)))
  ),(
  assert(relation(general(none,A,B))),
  write('.'),flush
 )).
  
   
has_no_relation(id(A), id(B)):-
  object(id(A),_),
  object(id(B),_),
  not(A == B),
  not(relation(proximity(_ , id(A), id(B)))),
  not(relation(sm_ordering(_ , id(A), id(B)))),
  not(route(_ , id(A), id(B))),
  not(relation(general(_ , id(A), id(B)))).


route(before,S1,S2):-
  route(meets,S1,Si),
  (route(meets,Si,S2); route(before,Si,S2)).



%after_relation
route(after,S1,S2):- 
   route(before,S2,S1).

 

%meet_inverse
route(met_by,S1,S2):- 
  route(meets,S2,S1).


 %before_relation 
ordering(before,L1, L2) :-
  object(L1, type(landmark)),
  object(L2, type(landmark)),
  relation(proximity(adjacent,L1,S1)),
  not(relation(proximity(adjacent,L2,S1))),
  relation(proximity(adjacent,L2,S2)),
  (route(meets,S1,S2) ;  route(before,S1,S2)).


ordering(after, L1,L2):-
 ordering(before,L2,L1).


 %new_rule_for_street_X_landmarks
ordering(Rel, St,Lm):-
  object(St, type(street)),
  object(Lm,type(landmark)),
  relation(proximity(adjacent,Lm,St)),
  oriented_representation(St,segment(Pa,Pb)),
  representation(Lm,GLm),
  projection(PrjLm,GLm,line(Pa,Pb)),
  projection(PrjSt,segment(Pa,Pb),segment(Pa,Pb)),
  %allen(Rel, PrjSt,PrjLm).
  allen_coarse(Rel, PrjSt,PrjLm).

  
  %rule_for_adjecent_landmarks_X_landmarks

ordering(Rel,L1, L2) :-
  object(L1, type(landmark)),
  object(L2, type(landmark)),
  relation(proximity(adjacent,L1,S)),
  relation(proximity(adjacent,L2,S)),
  oriented_representation(S, SGeom),
  representation(L1, L1Geom),
  representation(L2, L2Geom),
  projection(Prj1, L1Geom, SGeom ),
  projection(Prj2, L2Geom, SGeom ),
  allen(Rel, Prj1, Prj2).
  %allen_coarse(Rel, Prj1, Prj2).
  
  
 
%rule_for_adjecent_landmarks_X_landmarks
%%modified_for_projection_of_intersected_landmarks_with_buffer_distance

sm_ordering(Rel,L1, L2,CB) :-
  object(L1, type(landmark)),
  object(L2, type(landmark)),
  relation(proximity(adjacent,L1,S)),
  relation(proximity(adjacent,L2,S)),
  oriented_representation(S, SGeom),
  representation(L1, L1Geom),
  representation(L2, L2Geom),
  %object(CB, type(city_block)),
  %representation(CB, CBGeom),
  %CBGeom=complex_polygon(_,_),
  representation(type(buffered_region),S, Sbuff),
  boolean(intersection([L1Geom,Sbuff]), L1Clipped),
  boolean(intersection([L2Geom,Sbuff]), L2Clipped),
  projection(values(Prj1), L1Clipped, SGeom ),
  projection(values(Prj2), L2Clipped, SGeom ),
  interval_convex_hull(Prj1,Sprj1),
  interval_convex_hull(Prj2,Sprj2),
  %allen(Rel, Sprj1, Sprj2).
  allen_coarse(Rel, Sprj1, Sprj2).  

  
 derive_buffer_streets :-
  forall(
	( object(S, type(street)),
		oriented_representation(S, SGeom)
	),
	(
		buffer(SGeom, value(9), Sbuff),
	assert(representation(type(buffered_region),S, Sbuff))
	)
  ).
  
  
 
%print_rule_to_write_relation_in_QCN_format

record_relation(Stream,Rel,id(Obj1),id(Obj2)) :-
  nl(Stream),write(Stream,'('),write(Stream,Obj1),write(Stream,' '),
  write(Stream,Rel),write(Stream,' '),write(Stream,Obj2),write(Stream,')').

  
   
%check_orienation_of_street_segments


oriented_representation(S1, segment(Pa1, Pa2)) :-
  object(S1, type(street)),
  representation(S1, segment(Pa1, Pa2)),
  object(S2, type(street)),
  representation(S2, segment(Pb1, Pb2)),
  (route(meets,S1,S2),
  (equal(Pa2,Pb1); equal(Pa2,Pb2));
  route(meets,S2,S1),
  (equal(Pb2,Pa1);equal(Pb1,Pa1))).



%again_for_oriented_represenation
oriented_representation(S1, segment(Pa2, Pa1)) :-
  object(S1, type(street)),
  representation(S1, segment(Pa1, Pa2)),
  object(S2, type(street)),
  representation(S2, segment(Pb1, Pb2)),
  (route(meets,S1,S2),
  (equal(Pa1,Pb1); equal(Pa1,Pb2))
   ;
  route(meets,S2,S1),
  (equal(Pb2,Pa2);equal(Pb1,Pa2))).

  

%distance_between_landmarks_and_adjecent_streetSegments
set_buffer_distance :-
  max_landmark_street_distance(value(D)),
  retractall(buffer(_)),
  assert(buffer(D)).



max_landmark_street_distance(value(D)) :-
  setof(Dist, L^nearest_street_distance(value(Dist), L),Dists),
  sort(Dists,SDists),
  reverse(SDists,[D|_]).

  
%nearest_street_distance
nearest_street_distance(value(MinD), L) :-
  object(L, type(landmark)),
  representation(L,LG),
  setof(D, S^SG^(
  object(S, type(street)),
  representation(S,SG),
  distance(value(D), LG, SG)
  ),Ds),
  sort(Ds,[MinD|_]),
  write('.'),flush.
 
  

 %have_to_replace_buffer_distance_with_max_shortest_distance  
 buffer(9).


 %MM1=9.
 %SM1=25.
 %SM3=135
 %SM4=130
 %SM5=20
 %SM6=35
 %SM7=32
 %SM8=32
 %SM9=15
 %SM10=30
 %location-I
 %MM=9
 %SM1=45
 %SM3=145
 %SM4=150
 %SM5=150
 %SM6=80
  %SM8=150
   %SM10=30


 
adjacent(Lm, St) :-

  object(Lm, type(landmark)),
  object(St, type(street)),
  %% todo - optimisation: first check bounding boxes
  %% now check exact buffer distance
  buffer(Buf),
  representation(St, StreetGeom),
  representation(Lm, LandmarkGeom),
  distance(value(Dist), LandmarkGeom, StreetGeom),
  Dist < Buf.

  
  
 %create_buffer_AROUND_the_CITYBLOCK_CB1_streets
city_block_polyline_buffer :-
   object(id(cb1), type(city_block)),
   representation(id(cb1), polyline(Pts)),
   buffer(polyline(Pts),value(9), BufCB),
   assert(representation(id(cb1),BufCB)).
   

   %list_of_intervals_CMI
 interval_convex_hull(CmI, interval(MinS, MaxE)) :-
  setof(S,
        E^(member(interval(S,E), CmI)),
        Ss),
  setof(E,
        S^(member(interval(S,E), CmI)),
        Es),
  sort(Ss, [MinS|_]),
  sort(Es, SortEs),
  reverse(SortEs, [MaxE|_]).


 
 %%translate_and_scale_all_object_in_Metric_Map

 transform_all :-
  forall(
    representation(Id, Geom),
    (transform(Geom, TGeom),
     record_transformed_rep(Id, TGeom)
    )
  ).
  

transform_all :-
  forall(
    (representation(Id, Geom),
     not(transformed_rep(Id))
    ),
    (transform(Geom, TGeom),
     record_transformed_rep(Id, TGeom)
    )
  ).

   
  
%transformation
transform(Geom, TGeom) :-
	translate(Geom, value(-7.5, -51.9), SGeom),
	scale(SGeom, value(30000), TGeom).
	
	 
  
%record_the_transformed_geometries
record_transformed_rep(Id, TGeom) :-
  retractall(representation(Id, _)),
  assert(representation(Id, TGeom)),
  assert(transformed_rep(Id)).  %% flag to avoid infinite loop
    
  
   
  %scale_and_translate_metric_Map
  scale(point(X,Y), value(V), point(Sx,Sy)) :-
  Sx is X * V,
  Sy is Y * V.

 scale(polyline([]), value(_), polyline([])).
 scale(polyline([P|R]), value(V), polyline([Sp|Sr])) :-
  scale(P,value(V),Sp),
  scale(polyline(R), value(V), polyline(Sr)).
  
  %for_simple_polygon
 scale(polygon([]), value(_), polygon([])).
 scale(polygon([P|R]), value(V), polygon([Sp|Sr])) :-
  scale(P,value(V),Sp),
  scale(polygon(R), value(V), polygon(Sr)).
  
 %for_street_Segments
 scale(segment(Pa, Pb), value(V), segment(Spa,Spb)) :-
	scale(Pa, value(V), Spa),
	scale(Pb,value(V),Spb).
	
 
  

 %translate_coordinate_of_metric_map
translate(point(X,Y), value(Dx,Dy), point(Sx,Sy)) :-
 Sx is X + Dx,
 Sy is Y + Dy.


translate(polyline([]), value(_,_), polyline([])).
translate(polyline([P|R]), value(Dx,Dy), polyline([Sp|Sr])) :-
  translate(P,value(Dx,Dy),Sp),
  translate(polyline(R), value(Dx,Dy), polyline(Sr)). 
  
  
translate(polygon([]), value(_,_), polygon([])).
translate(polygon([P|R]), value(Dx,Dy), polygon([Sp|Sr])) :-
  translate(P,value(Dx,Dy),Sp),
  translate(polygon(R), value(Dx,Dy), polygon(Sr)).

  
translate(segment(Pa, Pb), value(Dx,Dy), segment(Spa, Spb)) :-
  translate(Pa,value(Dx,Dy),Spa),
  translate(Pb,value(Dx,Dy),Spb).  
  