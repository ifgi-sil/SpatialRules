:- multifile route/3.
:- multifile distance/3.
:- multifile orientation/3.
:- dynamic buffer/1.
:- dynamic relation/1.
:- multifile scale/3.
:- multifile translate/3.
:- dynamic relation/1.
:- dynamic relation/3.
get_common_junction_at_connected_streets

:-

  nl,nl,write('Don\'t forget to set the buffer distance, and to derive the adjacent relation.'),nl,nl.

  
%-----------------------1.Orientation_relations----------------------------------
 
 %orientation_relations
qcn_valid_klippel_relation(front).
qcn_valid_klippel_relation(half_left).
qcn_valid_klippel_relation(left).
qcn_valid_klippel_relation(sharp_left).
%qcn_valid_klippel_relation(back).
%qcn_valid_klippel_relation(sharp_right).
%qcn_valid_klippel_relation(right).
%qcn_valid_klippel_relation(half_right).
qcn_valid_klippel_relation(eq).


%orientation_and_inverse_relations
converse_klippel_relations(front,back).
converse_klippel_relations(back,front).
converse_klippel_relations(half_left,half_right).
converse_klippel_relations(half_right,half_left).
converse_klippel_relations(left,right). 
converse_klippel_relations(right,left).
converse_klippel_relations(sharp_left,sharp_right).
converse_klippel_relations(sharp_right,sharp_left).
converse_klippel_relations(eq,eq).

 %converse(klippel(left), orientation(right)).   
 
%----------rules----------- 
  
derive_relations :-
  nl,write('make_representation_valid'),nl,flush,
  %make_representations_valid, 
  nl,write('Scale and Transform the Metric Map'),nl,flush,
  %transform_all, 
  nl,write('Deriving Orientation_of_Streets'),nl,flush,
  derive_orientation_streets,
  nl,write('Deriving NONE relations'),nl,flush,
  derive_none_relations.

/**
%--------------------supporting_rules-------------------------------- 
**/
   
%%derive_topology_of_landmarks_within_city-block(b1)  

derive_orientation_streets :-
  retractall(relation(sm_orientation_streets,_,_)),
  forall((
    object(A,type(street)), object(B,type(street)), 
	A \= B,
    %% ignore if already derived the converse
    sm_orientation_streets(RelAB, A, B),
	not(relation(sm_orientation_streets(_,A,B))),
    not(relation(sm_orientation_streets(_,B,A)))
   ),(
    converse_klippel_relations(RelAB,ConvRelBA),
    assert(relation(sm_orientation_streets(RelAB,A,B))),
	assert(relation(sm_orientation_streets(ConvRelBA,B,A))),
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
  not(relation(sm_orientation_streets(_ , id(A), id(B)))),
  not(relation(general(_ , id(A), id(B)))).


 %print_rule_to_write_relation_in_QCN_format
record_relation(Stream,Rel,id(Obj1),id(Obj2)) :-
  nl(Stream),write(Stream,'('),write(Stream,Obj1),write(Stream,' '),
  write(Stream,Rel),write(Stream,' '),write(Stream,Obj2),write(Stream,')').

/**
------------------------Orientation_Of_Sstreet_segments---------------------------------------
% the_below_rules define the orienation of street segments
% orientation of only connected streets 
**/
 
 
 sm_orientation_streets(Rel12, St1, St2):-
  object(St1, type(street)),
  object(St2, type(street)),
  St1 \=St2,
  get_common_junction_at_connected_streets(St1, St2, GJun),
  representation(St1, GSt1),
  representation(St2, GSt2),
  other_endPoint(GSt1, GJun, P1),
  other_endPoint(GSt2, GJun, P2),
  %klippel´s_cones%	
  
  %orientation_streets_klippel8_nav(Rel12, P1, GJun, P2).
  %orientation_streets_klippel8_eq(Rel12, P1, GJun, P2).
  
  %our_direction_cones%
  %orientation_streets_klippel8_sm_sectors(Rel12, P1, GJun, P2).
  orientation_streets_klippel8_sm_sectors_coarse(Rel12, P1, GJun, P2).
  
  
  
  
  
  %orientation_streets(klippel8_nav(Rel12), GJun, P1, P2.  
  %to_extract_reverse_relation
  %orientation_streets_klippel8_nav(Rel21, GJun, P2, P1).
 
  
/**
--------------Supporting rules to define transormation and scaling ---------------------------
%translate_and_scale_all_object_in_Metric_Map
**/	 

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
