
  %%QCN_between_LANDMARKS_X_LANDMARKS_ONLY_within_CITY_BLOCK
 %%projection_of_intersecting_landmarks_only_on_street_segmetns

create_qualify_file_Landmarks_IN_CB(linear_ordering, intersecting_landmarks, Fn) :-

  open(Fn, write, Stream),
  
  %% export linear ordering relations
  forall(
   (object(A,type(landmark)),
    object(B,type(landmark)),
	not(A==B),
	relation(sm_ordering(Rel,A,B,_)),
    qcn_valid_relation(Rel) ),
    record_relation(Stream, Rel,A,B)
  ),
  
  %% export the none relation
  forall(
   (object(A,type(landmark)),
    object(B,type(landmark)),
	not(A==B),
	relation(general(none,A,B)) ),
    record_relation(Stream, none,A,B)
  ),
  close(Stream). 




%Qualify_files_Creator_for_each_city_block_MM

create_qualify_file_mm(linear_ordering, landmarks, block(Block), Fn) :-

  open(Fn, write, Stream),
  
  %% export linear ordering relations
  forall(
   (
    block(Block, NameA),
    name_mm(A, NameA),
    object(A,type(landmark)),
    	
	block(Block, NameB),
    name_mm(B, NameB),
    object(B,type(landmark)),
	not(A==B),
	relation(sm_ordering(Rel,A,B,_)),
    qcn_valid_relation(Rel) 
	),
    record_relation(Stream, Rel,A,B)
  ),
  
  %% export the none relation
  forall(
   (
	block(Block, NameA),
    name_mm(A, NameA),
       	
	block(Block, NameB),
    name_mm(B, NameB),
    	
    object(A,type(landmark)),
    object(B,type(landmark)),
	not(A==B),
	not(relation(sm_ordering(Rel,A,B,_))),
	relation(general(none,A,B)) 
	),
    record_relation(Stream, none,A,B)
  ),
  
  close(Stream). 
  
  
%QCN_for_each_CITY_BLOCK_SM

create_qualify_file_sm(linear_ordering, landmarks, block(Block), Fn) :-

  open(Fn, write, Stream),
  
  %% export linear ordering relations
  forall(
   (
    object(A,type(landmark)),
	name_sm(A, NameA),   
    match(NameA,CorrNameB,correct),
	block(Block, CorrNameB),
	
	object(B,type(landmark)),
	name_sm(B,NameB),
	match(NameB, CorrNameBB, correct),
	block(Block, CorrNameBB),
	
	not(A==B),
	relation(sm_ordering(Rel,A,B,_)),
    qcn_valid_relation(Rel) 
	),
    record_relation(Stream, Rel,A,B)
  ),
  
  %% export the none relation
  forall(
   (
    object(A,type(landmark)),
  	name_sm(A, NameA),   
    match(NameA,CorrNameB,correct),
	block(Block, CorrNameB),
	
	object(B,type(landmark)),
	name_sm(B,NameB),
	match(NameB, CorrNameBB, correct),
	block(Block, CorrNameBB),
	
	not(A==B),
	not(relation(sm_ordering(Rel,A,B,_))),
	relation(general(none,A,B)) 
	),
    record_relation(Stream, none,A,B)
  ),
 
  close(Stream). 


 
  

%%QCN_between_LANDMARKS_X_LANDMARKS_ONLY


create_qualify_file(linear_ordering, landmarks, Fn) :-

  open(Fn, write, Stream),
  
  %% export linear ordering relations
  forall(
   (object(A,type(landmark)),
    object(B,type(landmark)),
	not(A==B),
	relation(ordering(Rel,A,B)),
    qcn_valid_relation(Rel) ),
    record_relation(Stream, Rel,A,B)
  ),
  
  %% export the none relation
  forall(
   (object(A,type(landmark)),
    object(B,type(landmark)),
	not(A==B),
	relation(general(none,A,B)) ),
    record_relation(Stream, none,A,B)
  ),
  close(Stream). 
  

  %QCN_between_STREET_X_STREET_ONLY

create_qualify_file(linear_ordering, streets, Fn) :-
 
 open(Fn, write, Stream),

 %% export linear ordering relations

 forall(
   (
    object(A,type(street)),
    object(B,type(street)),
	not(A==B),
	route(Rel,A,B),
    qcn_valid_relation(Rel)
   ),
   (
    record_relation(Stream, Rel,A,B)
   )
   ),
  %% export the none relation
  forall(
   (object(A,type(street)),
    object(B,type(street)),
	not(A==B),
	relation(general(none,A,B)) 
   ),
    record_relation(Stream, none,A,B)
  ),
 close(Stream). 
 
 
 
 
 
   %QCN_between_STREET_X_landmark_ONLY
    
 create_qualify_file(linear_ordering, streets_and_landmarks, Fn) :-
 open(Fn, write, Stream),
 
  forall(
   (
	not(A==B),
	 relation(ordering(Rel,A,B)),
	 qcn_valid_relation(Rel)
	),
    record_relation(Stream, Rel,A,B)
   ),
  
  forall(
   (
    not(A==B),
    route(Rel,A,B),
    qcn_valid_relation(Rel)),
    record_relation(Stream, Rel,A,B)
  ),
  
  forall(
   (relation(general(none,A,B))
   ),
   record_relation(Stream, none,A,B)
  ),
  close(Stream).
  
  
 
%create_Facts_file_for_linear_Ordering
 create_facts_file(linear_ordering_facts, allobjects, Fn) :-
  open(Fn,write,Stream),
  nl(Stream),
  forall(
  call(relation(X)),
  (write_term(Stream, relation(X), [quoted(true)]), write(Stream,'.'), nl(Stream))
  ),
forall(
  call(route(Rel,A,B)),
  (write_term(Stream, route(Rel,A,B), [quoted(true)]), write(Stream,'.'), nl(Stream))
  ),
(buffer(Buf),
 write_term(Stream, buffer(Buf), [quoted(true)]), write(Stream,'.'), nl(Stream)),
 close(Stream).

  
  
