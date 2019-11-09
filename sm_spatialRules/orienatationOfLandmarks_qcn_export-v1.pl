%Qualify_files_Creator

%%leftright_orientation_QCN

create_qualify_file_QCN(leftright_orientation, streetAndLandmarks, Fn) :-

  open(Fn, write, Stream),
  
  %orientation_of_landmarks
  forall(
   (
	 relation(sm_orientation(Rel,A,B)),
	 qcn_valid_orientation_relation(Rel)
	),
    record_relation(Stream, Rel,A,B)
   ),
   
  /*%ordering_of_landmarks_streets
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
  
   */
  forall(
   (relation(general(Rel,A,B))),
   record_relation(Stream, Rel,A,B)
  ),
  close(Stream).
  
  
  
 %create_Facts_file_for_orientation_of_landmarks
 
 
 create_facts_file_QCN(leftright_fact, allobjectss, Fn) :-
 
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
