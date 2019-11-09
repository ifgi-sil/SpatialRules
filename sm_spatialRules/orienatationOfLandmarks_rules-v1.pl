:- multifile route/3.
:- multifile distance/3.
:- multifile orientation/3.
:- dynamic buffer/1.
:- dynamic relation/1.
:- multifile scale/3.
:- multifile translate/3.
:- dynamic relation/1.
:- dynamic relation/3.

:-

  nl,nl,write('Don\'t forget to set the buffer distance, and to derive the adjacent relation.'),nl,nl.

  
%-----------------------1.Orientation_relations----------------------------------
 
 %orientation_relations
qcn_valid_orientation_relation(right_of).
qcn_valid_orientation_relation(left_of).
qcn_valid_orientation_relation(crosses).
qcn_valid_orientation_relation(crossed_by).
qcn_valid_orientation_relation(front_of).
qcn_valid_orientation_relation(back_of).
qcn_valid_orientation_relation(equal).


%orientation_and_inverse_relations
converse_orientations(left_of,right_of).
converse_orientations(right_of,left_of).
converse_orientations(crosses,crossed_by).
converse_orientations(crossed_by,crosses).
converse_orientations(front_of,back_of). 
converse_orientations(back_of,front_of).
converse_orientations(equal,equal).
  
 
%----------rules----------- 
  
derive_relations :-
  nl,write('make_representation_valid'),nl,flush,
  make_representations_valid, 
  nl,write('Scale and Transform the Metric Map'),nl,flush,
  transform_all, 
  nl,write('Deriving Buffering Distance'),nl,flush,
  %set_buffer_distance,
  nl,write('Deriving Proximity'),nl,flush,
  derive_proximity_relations,
  nl,write('Deriving reoriented street segments around at junction'),nl,flush,
  %derive_oriented_street_segments,
  nl,write('Deriving Relative Orientation_of_Landmarks'),nl,flush,
  derive_relative_orientation_landmarks,
  nl,write('Deriving NONE relations'),nl,flush,
  derive_none_relations.


%--------------------supporting_rules--------------------------------  

 %derive_oriented_street_segments_at_connected_JUNCTION
  derive_oriented_street_segments :-
	forall(	(
			object(Jun, type(junction)),
			relation(connects, Jun, St1),
			object(St1, type(street)),
			relation(connects, Jun, St2),
			object(St2, type(street)),
			St1 \= St2,
			not((
				object(StInt, type(street_interval)),
				relation(part_of,St1, StInt),
				relation(part_of,St2, StInt)
				))
			),(
			derive_street_cyclic_interval(St1, St2,Jun),
			write('.'),flush
			)
			).




%%find_proximity_relation
derive_proximity_relations :-
  retractall(relation(proximity(adjacent,_,_))),
  forall((
	adjacent(A,B)
  ),(
  assert(relation(proximity(adjacent,A,B))),write('.'),flush
  )).

%orienation_relation  
derive_relative_orientation_landmarks :-
  retractall(relation(sm_orientation_landmarks,_,_)),
  forall((
    object(A,_), object(B,_), 
	A \= B,
    %% ignore if already derived the converse
    sm_orientation_landmarks(Rel,A,B),
	not(relation(sm_orientation_landmarks(_,A,B))),
    not(relation(sm_orientation_landmarks(_,B,A)))
   ),(
    converse_orientations(Rel,ConvRel),
    assert(relation(sm_orientation_landmarks(Rel,A,B))),
	assert(relation(sm_orientation_landmarks(ConvRel,B,A))),
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
  %not(relation(ordering(_ , id(A), id(B)))),
  %not(relation(route(_ , id(A), id(B)))),
  not(relation(sm_orientation_landmarks(_ , id(A), id(B)))),
  not(relation(general(_ , id(A), id(B)))).


/*
%-------------------------------ORIENTATION of Landmarks--------------------------------------------------
to capture the orientation information between ADJECENT LANDMARKS and STREETS
*/

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
		(
			route(meets,S1,S2) ; 
			route(before,S1,S2)
		).

ordering(after, L1,L2):-
 ordering(before,L2,L1).
 
 
sm_orientation_landmarks(Rel, Lm, St):-
  object(St, type(street)),
  object(Lm,type(landmark)),
  relation(proximity(adjacent,Lm,St)),
  oriented_representation(St,segment(StA,StB)),
  %representation(St,segment(StA, StB)),
  representation(Lm,GLm),
  orientation_lm(Rel, GLm, line(StA, StB)).

  
/**
------------------------Supporting_rules---------------------------------------
% the_below_rules are supporting rules for relative orientation of landmarks
**/
  
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
  assert(buffer(10)).


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
 buffer(10).
 % Location-I
 %MM1=15
 %SM1=34
 %SM2=320
 %SM3=230
 %SM4=120
 %SM5=150
 %SM5=150
 %SM6=130
 %SM7=90
 %SM8=200
 %SM9=200
 %SM9=60
 %Location-II
 %MM=10
 %SM1=30
  %SM1=30
  %SM3=80
  %SM4=220
  %SM4=70
  %SM6=40
  %SM7=60
  %SM8=20
  %SM9=50
  %SM10=20

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
		
	
	 
 %%translate_and_scale_all_object_in_Metric_Map

 transform_all :-
  forall(
    representation(Id, Geom),
    (transform(Geom, TGeom),
     record_transformed_rep(Id, TGeom),
	  write('.'),flush
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
