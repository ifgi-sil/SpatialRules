%Qualify_files_Creator

create_orientation_klippel_streets_QCN(streets_orientation, allStreets, Fn) :-

  open(Fn, write, Stream),
  
  %orientation_of_streets
  forall(
   (
	object(A,type(street)),
	object(B,type(street)), 
	 relation(sm_orientation_streets(RelAB,A,B)),
	 qcn_valid_klippel_relation(RelAB)
	),
    record_relation(Stream, RelAB,A,B)
   ),
   
  forall(
   (
	object(A,type(street)),
	object(B,type(street)), 
	not(A==B),
	not(relation(sm_orientation_streets(RelAB,A,B))),
	relation(general(none,A,B))
   ),
   record_relation(Stream, none,A,B)
  ),
  close(Stream).

/**

%%QCN_for_objects_in_particular_city_block
create_topology_landmarks_QCN_MM(topologicalRelations, landmarks, city_block(CB1), Fn) :-

  open(Fn, write, Stream),
  
  %% export linear ordering relations
  forall(
   (
    object(A, type(landmark)),
	object_name_mm(A, NameA),   
    city_block(CB1, NameA),
	object(B,type(landmark)),
	object_name_mm(B,NameB),
	city_block(CB1, NameB),
	relation(sm_topology_landmarks(Rel,A,B)),
    qcn_valid_topological_relation(Rel) 
	),
    record_relation(Stream, Rel,A,B)
  ),
  
  %% export the none relation
  forall(
   (
    object(A,type(landmark)), 
   	object_name_mm(A, NameA),   
   	city_block(CB1, NameA),
	object(B,type(landmark)), 
	object_name_mm(B,NameB),
	city_block(CB1, NameB),
	not(A==B),
	not(relation(sm_topology_landmarks(Rel,A,B))),
	relation(general(none,A,B)) 
	),
    record_relation(Stream, none,A,B)
  ),
  close(Stream).


  
%QCN_for_each_city_block_SM
create_topology_landmarks_QCN_SM(leftright_orientation, landmarks, city_block(CB1), Fn) :-
  open(Fn, write, Stream),

  %% export linear ordering relations
  forall(
   (
    object(A, type(landmark)),
	object_name_sm(A, NameA),   
    %city_block(CB1, NameA),
	object(B,type(street)),
	object_name_sm(B,NameB),
	%city_block(CB1, NameB),
	relation(sm_orientation_landmarks(Rel,A,B)),
    qcn_valid_topological_relation(Rel) 
	),
    record_relation(Stream, Rel,A,B)
  ),
  
  %% export the none relation
  forall(
   (
    object(A,_), 
   	object_name_sm(A, NameA),   
   	%city_block(CB1, NameA),
	object(B,_), 
	object_name_sm(B,NameB),
	%city_block(CB1, NameB),
	not(A==B),
	not(relation(sm_orientation_landmarks(Rel,A,B))),
	relation(general(none,A,B)) 
	),
    record_relation(Stream, none,A,B)
  ),
  close(Stream). 

  
  
 **/
  

%%leftright_orientation_QCN

create_orientation_landmarks_QCN(leftright_orientation, streetAndLandmarks, Fn) :-

  open(Fn, write, Stream),
  
  %orientation_of_landmarks
  forall(
   (
	object(A,type(landmark)),
	object(B,type(street)), 
	 relation(sm_orientation_landmarks(Rel,A,B)),
	 qcn_valid_orientation_relation(Rel)
	),
    record_relation(Stream, Rel,A,B)
   ),
   
  forall(
   (
	object(A,_), object(B,_), 
	not(A==B),
	not(relation(sm_orientation_landmarks(Rel,A,B))),
	relation(general(none,A,B))
   ),
   record_relation(Stream, none,A,B)
  ),
  close(Stream).
  
  
  
 %create_Facts_file_for_orientation_of_landmarks
 
 
 create_orientation_landmarks_facts(leftright_fact, allobjectss, Fn) :-
 
  open(Fn,write,Stream),
  nl(Stream),
  forall(
  call(relation(X)),
  (write_term(Stream, relation(X), [quoted(true)]), write(Stream,'.'), nl(Stream))
  ),
%forall(
 % call(route(Rel,A,B)),
  %(write_term(Stream, route(Rel,A,B), [quoted(true)]), write(Stream,'.'), nl(Stream))
  %),
(buffer(Buf),
 write_term(Stream, buffer(Buf), [quoted(true)]), write(Stream,'.'), nl(Stream)),
 close(Stream).
  