:- dynamic object/2, representation/2.
:- multifile route/3.
:- multifile distance/3.
:- multifile orientation/3.
:- dynamic buffer/1.
:- dynamic relation/1.
:- multifile scale/3.
:- multifile translate/3.
:- dynamic relation/3.
%:- dynamic derive_street_cyclic_interval/3.

:-

  nl,nl,write('Don\'t forget to set the buffer distance, and to derive the adjacent relation.'),nl,nl.


%facts_forLinearordering 


qcn_valid_relation(before).
qcn_valid_relation(meets).
qcn_valid_relation(overlaps).
qcn_valid_relation(starts).
qcn_valid_relation(finishes).
qcn_valid_relation(during).

%orientation_relations
%qcn_valid_orientation_relation(right_of).
%qcn_valid_orientation_relation(left_of).
qcn_valid_orientation_relation(crosses).
%qcn_valid_orientation_relation(crossed_by).
qcn_valid_orientation_relation(front_of).
%qcn_valid_orientation_relation(back_of).


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

%orientation
converse_orientations(left_of,right_of).
converse_orientations(right_of,left_of).
converse_orientations(crosses,crossed_by).
converse_orientations(crossed_by,crosses).
converse_orientations(front_of,back_of). 
converse_orientations(back_of,front_of).


%%cyclicORdering 
qcn_valid_cyclicOrdering_relation(disjoint).
qcn_valid_cyclicOrdering_relation(meets).
qcn_valid_cyclicOrdering_relation(overlaps).
qcn_valid_cyclicOrdering_relation(during).
qcn_valid_cyclicOrdering_relation(containsAndTuch).
qcn_valid_cyclicOrdering_relation(equal).

%%coverses_of_cyclic_ordering
converse_cyclicOrdering(disjoint,disjoint).
converse_cyclicOrdering(meets,met_by).
converse_cyclicOrdering(overlaps,overlapped_by).
converse_cyclicOrdering(during,during_inv).
converse_cyclicOrdering(containsAndTuch,containsAndTuch_by).
converse_cyclicOrdering(equal,equal).

converse_cyclicOrdering(disjoint,disjoint).
converse_cyclicOrdering(met_by,meets).
converse_cyclicOrdering(overlapped_by,overlaps).
converse_cyclicOrdering(during_inv,during).
converse_cyclicOrdering(containsAndTuch_by,containsAndTuch).
converse_cyclicOrdering(equal,equal).
  
 
%----------rules----------- 
  
derive_relations :-
  nl,write('make_representation_valid'),nl,flush,
  make_representations_valid, 
  nl,write('Scale and Transform the Metric Map'),nl,flush,
  %transform_all, 
  nl,write('Deriving street Interval around Junction'),nl,flush,
  derive_street_intervals,
  nl,write('Deriving Buffering Distance_around_STREET'),nl,flush,
  set_buffer_distance,
  nl,write('Deriving Proximity'),nl,flush,
  derive_proximity_relations,
  nl,write('Deriving Ordering'),nl,flush,
  %derive_ordering_relations,
  nl,write('Deriving Orientation_Landmarks'),nl,flush,
  %derive_orientation_relations,
  nl,write('Deriving Buffering Distance_around_JUNCTIONS'),nl,flush,
  %set_buffer_distance_around_junction,
  nl,write('Deriving CyclicOrdering Relations_BETWEEN_Landmkars'),nl,flush,
  derive_cyclicOrdering_relations_LM,
  nl,write('Deriving CyclicOrdering Relations_BETWEEN_Connected_Streets and landmarks around_Junction'),nl,flush,
  derive_cyclicOrdering_relations_StInts_X_landmarks,
  nl,write('Deriving CyclicOrdering Relations_BETWEEN_Street_Intervals around_Junction'),nl,flush,
  %derive_cyclicOrdering_relations_StInts_X_StInts,
  nl,write('Deriving NONE relations'),nl,flush,
  derive_none_relations.




derive_proximity_relations :-
  retractall(relation(proximity(adjacent,_,_))),
  forall((
  adjacent(A,B)
  ),(
  assert(relation(proximity(adjacent,A,B))),write('.'),flush
  )).
  

 %derive_street_interval_around_JUNCTION
  derive_street_intervals :-
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
  
  
%CyclicOrdering_relation_between_Landmarks_around_JUNCTIONS 

derive_cyclicOrdering_relations_LM :-
  retractall(relation(sm_cyclicOrdering,_,_,_)),
  forall((
    object(A,type(landmark)),
	object(B,type(landmark)), 
	object(Jun,type(junction)), 
	A \= B,
    %% ignore if already derived the converse
    sm_cyclicOrdering(Rel,A,B,Jun),
	not(relation(sm_cyclicOrdering(_, A, B, Jun))),
	not(relation(sm_cyclicOrdering(_, B, A, Jun)))
	),(
    converse_cyclicOrdering(Rel,ConvRel),
    assert(relation(sm_cyclicOrdering(Rel,A,B,Jun))),
	assert(relation(sm_cyclicOrdering(ConvRel,B,A, Jun))),
	write('.'),flush
	)
  ).
  
  
 %cyclic_ordering_of_streets_interval_X_landmarks_around_junction 
derive_cyclicOrdering_relations_StInts_X_landmarks :-
  retractall(relation(sm_cyclicOrdering,_,_,_,_)),
  forall((
    object(StInterval, type(street_interval)),
	object(Lm, type(landmark)),
	object(Jun, type(junction)),
	
	% ignore if already derived the converse
    sm_cyclicOrdering(Rel,StInterval,Lm,Jun),
	not(relation(sm_cyclicOrdering(_, StInterval, Lm, Jun))),
	not(relation(sm_cyclicOrdering(_, Lm, StInterval, Jun)))
	),(
    converse_cyclicOrdering(Rel,ConvRel),
    assert(relation(sm_cyclicOrdering(Rel,StInterval,Lm,Jun))),
	assert(relation(sm_cyclicOrdering(ConvRel,Lm,StInterval,Jun))),
	write('.'),flush
	)
  ).
  
  
   %cyclic_ordering_of_streets_interval_X_interval_around_junction 
derive_cyclicOrdering_relations_StInts_X_StInts :-
  retractall(relation(sm_cyclicOrdering,_,_,_,_)),
  forall((
	object(StIn1, type(street_interval)),
	object(StIn2, type(street_interval)),
	object(Jun, type(junction)),
	StIn1 \= StIn2,
	
	% ignore if already derived the converse
    sm_cyclicOrdering(Rel,StIn1,StIn2,Jun),
	not(relation(sm_cyclicOrdering(_, StIn1, StIn2, Jun))),
	not(relation(sm_cyclicOrdering(_, StIn2, StIn1, Jun)))),
	(
    converse_cyclicOrdering(Rel,ConvRel),
    assert(relation(sm_cyclicOrdering(Rel,StIn1,StIn2,Jun))),
	assert(relation(sm_cyclicOrdering(ConvRel,StIn2,StIn1,Jun))),
	write('.'),flush
	)
  ).
  
  

%orienation_relation  
derive_orientation_relations :-
  retractall(relation(sm_orientation,_,_)),
  forall((
     object(A,_), object(B,_), 
	A \= B,
    %% ignore if already derived the converse
    %orientation(Rel,A,B),
    sm_orientation(Rel,A,B),
	not(relation(sm_orientation(_,A,B))),
    not(relation(sm_orientation(_,B,A)))
   ),(
    converse_orientations(Rel,ConvRel),
    assert(relation(sm_orientation(Rel,A,B))),
	assert(relation(sm_orientation(ConvRel,B,A))),
	write('.'),flush
  )).
  
  
derive_ordering_relations :-
  retractall(relation(ordering,_,_)),
  forall((
     object(A,_), object(B,_), 
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
  %not(relation(ordering(_ , id(A), id(B)))),
  %not(relation(route(_ , id(A), id(B)))),
  %not(relation(sm_orientation(_ , id(A), id(B)))),
  not(relation(sm_cyclicOrdering(_ , id(A), id(B), _))),
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
 (route(meets,S1,S2) ; route(before,S1,S2)).

ordering(after, L1,L2):-
  ordering(before,L2,L1).
  

%rule_for_adjecent_landmarks_X_landmarks

ordering(Rel,L1, L2) :-
  object(L1, type(landmark)),
  object(L2, type(landmark)),
  relation(proximity(adjacent,L1,S)),
  relation(proximity(adjacent,L2,S)),
  %representation(S, SGeom),
  oriented_representation(S, SGeom),
  representation(L1, L1Geom),
  representation(L2, L2Geom),
  projection(Prj1, L1Geom, SGeom ),
  projection(Prj2, L2Geom, SGeom ),
  allen_coarse(Rel, Prj1, Prj2).
  %allen(Rel, Prj1, Prj2).
 
  

 
%new_rule_for_street_X_landmarks

ordering(Rel, St,Lm):-
  object(St, type(street)),
  object(Lm,type(landmark)),
  relation(proximity(adjacent,Lm,St)),
  oriented_representation(St,segment(Pa,Pb)),
  representation(Lm,GLm),
  projection(PrjLm,GLm,line(Pa,Pb)),
  projection(PrjSt,segment(Pa,Pb),segment(Pa,Pb)),
  %allen_coarse(Rel, PrjSt,PrjLm).
  allen(Rel, PrjSt,PrjLm).
 
 
  
 
/*
to capture the orientation information between ADJECENT LANDMARKS and STREETS
*/
 
sm_orientation(Rel, Lm, St):-
  object(St, type(street)),
  object(Lm,type(landmark)),
  relation(proximity(adjacent,Lm,St)),
  %oriented_representation(St,segment(StA,StB)),
  representation(St,segment(StA, StB)),
  representation(Lm,GLm),
  orientation_lm(Rel, GLm, line(StA, StB)).

  
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
  setof(Dist, 
          L^nearest_street_distance(value(Dist), L),Dists),
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
 %buffer(0.0004).


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

  

  

 %adjecency_landmarks_around_junction
 adjacent(Lm, Jn) :-

  object(Lm, type(landmark)),
  object(Jn, type(junction)),
  %% todo - optimisation: first check bounding boxes
  %% now check exact buffer distance
  %buffer(Buf),
  buffer_test(Buf),
  representation(Jn, JunctionGeom),
  representation(Lm, LandmarkGeom),
  C=circle(JunctionGeom, Buf),
  topology(overlaps, C, LandmarkGeom).
  
 
 
 %buffer_around_junction
  buffer_test(80).
  
  %location-ii
  %MM1=28
  %SM1=60
  %SM2=80
  
  
 

 %%-----------------------------------------------CIA----------------------------
 %%CYCLIC_ORDERING_OF_LANDMARKS%%
  
  
 %%set_Buffer_distance_between_landmarks_and_reference_JUNCTION 
 set_buffer_distance_around_junction :-
   max_landmark_junction_distance(value(D)),
   retractall(buffer(_)),
   assert(buffer(D)).
   


 %%Compute_Max_landmark_X_junction_distance  
 max_landmark_junction_distance(value(D)) :- 
   setof(Dist, 
        L^nearest_junction_distance(value(Dist), L),Dists), 
   sort(Dists,SDists),
   reverse(SDists,[D|_]).
   


  %%nearest_distance_between_reference_junction_AND_Landmarks
  nearest_junction_distance(value(MinD), L) :- 
    object(L, type(landmark)), representation(L,LG), 
	setof(D, S^SG^( 
	      object(S, type(junction)), 
		  representation(S,SG), 
		  distance(value(D), LG, SG)
    ),Ds), 
	sort(Ds,[MinD|_]),
	write('.'),flush.
  
  

%%Cyclic_Interval_Algebra_(CIA)

distance(radian(V),cyclic_interval(centre(Ca),_),
                   cyclic_interval(centre(Cb),_)) :-
										{Diff =:= Ca - Cb},
										clpqs_utils:abs_(Diff , AbsDiff),
										Pi is pi,
										{Diff2 =:= 2*Pi - AbsDiff},
										clpqs_utils:min_(AbsDiff, Diff2, V).



 

%%sm_Cyclic_Ordering_Of_landmarks_X_landmarks
 sm_cyclicOrdering(Rel, Lm1, Lm2, Jun) :-
     object(Jun, type(junction)),
	 object(Lm1, type(landmark)),
     object(Lm2, type(landmark)),
	 relation(proximity(adjacent,Lm1,Jun)),
	 relation(proximity(adjacent,Lm2,Jun)),
	 Lm1 \= Lm2,
     representation(Lm1,GLm1),
	 representation(Lm2,GLm2),
	 representation(Jun,GJun),
	 projection(PrjLm1, GLm1, GJun ), %projection of geomatry of landmark1 on junction
     projection(PrjLm2, GLm2, GJun ), %projection of geomatry of landmark2 on the same junction
	 cyclic_ordering_lm(Rel, PrjLm1,PrjLm2).
 



%%rule_for_cyclicOrdering_of_street_X_landmarks
sm_cyclicOrdering(Rel, StInts, Lm, Jun) :-
	 object(Lm, type(landmark)),
	 object(StInts, type(street_interval)), %get objects which have type street_interval
	 object(Jun, type(junction)),
     relation(proximity(adjacent,Lm,Jun)),  %landmarks are adjecent_if_they are within the buffer distance around junction
	 representation(Lm,GLm),
	 representation(StInts,GStInts),
	 representation(Jun,GJun),
	 projection(PrjLm, GLm, GJun ),  %projection of geomatry of landmark1 on junction
     cyclic_ordering_lm(Rel, PrjLm, GStInts).
	 

%%rule_for_cyclicOrdering_of_street_X_street
sm_cyclicOrdering(Rel, StIn1, StIn2, Jun) :-
	 object(Jun, type(junction)),
	 object(StIn1, type(street_interval)),
	 object(StIn2, type(street_interval)), %get objects which have type street_interval
	 representation(StIn1,GStIn1),
	 representation(StIn2,GStIn2),
	 %projection(PrjLm, GLm, GJun ),  %projection of geomatry of landmark1 on junction
     cyclic_ordering_lm(Rel, GStIn1, GStIn2).

	 
%deriving_C_intervals_between_connected_streets_around_junction 
derive_street_cyclic_interval(S1, S2, Jun)	:-
	 object(Jun, type(junction)),
	 relation(connects, Jun,S1),
	 relation(connects, Jun,S2),
	 S1 \=S2,
	 representation(S1, GS1),
	 representation(S2, GS2),
	 representation(Jun, GJun),
	 other_endPoint(GS1, GJun,P1),
	 other_endPoint(GS2, GJun,P2),
	 projection(PrjP1P2, segment(P1, P2), GJun),
	 create_new_id('sTIN_',Id),
	 assert(object(Id,type(street_interval))),
	 assert(representation(Id, PrjP1P2)),
	 assert(relation(ref_junction, Id, Jun)),
	 assert(relation(part_of, S1,Id)),
	 assert(relation(part_of, S2,Id)).
	
	
	 
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
