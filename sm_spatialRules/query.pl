__________________________________

////towrite file of relation as qcnS

Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\linearOrdering\\MM1.AI.qcn',
open(Filename, write, Stream),

forall((
  object(id(S),type(street)),
  object(id(L), type(landmark)),
  ordering_AI(Rel,id(S),id(L))
 ),(
  nl(Stream),write(Stream,'('),write(Stream,S),write(Stream,' '),write(Stream,Rel),write(Stream,' '),write(Stream,L),write(Stream,')')
 )),
 nl(Stream),
 close(Stream).

==============================================

////towrite file of relation as qcnS

Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\linearOrdering\\results\\SM1.AI.qcn',
open(Filename, write, Stream),

forall(
  (ordering(Rel,id(Obj1),id(Obj2))),
  (nl(Stream),write(Stream,'('),write(Stream,Obj1),write(Stream,' '),write(Stream,Rel),write(Stream,' '),write(Stream,Obj2),write(Stream,')'))
 ),
 forall(
  (route(Rel,id(Obj1),id(Obj2))),
  (nl(Stream),write(Stream,'('),write(Stream,Obj1),write(Stream,' '),write(Stream,Rel),write(Stream,' '),write(Stream,Obj2),write(Stream,')'))
  ),
  nl(Stream),
  write('qcn is finished'),
  close(Stream).
 
 
 ======================================

%to_write_QCNs_final%
%query_removes_the_repeated_relations%

Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\linearOrdering\\results\\MM1.AItest.qcn',
open(Filename, write, Stream),
forall(
	(relation(ordering(Rel,A,B))),
	record_relation(Stream, Rel,A,B)
),
forall(
	(route(Rel,A,B)),
	record_relation(Stream, Rel,A,B)
),
forall(
	(relation(proximity(Rel1,A,B))),
	record_relation(Stream, Rel1,A,B)
),
nl(Stream),
write('qcn is finished'),
close(Stream).
  
========================================================== 
  %final query to extract qcn
  
  
  1.set_buffer_distance.
  2. ?-derive_proximity_relations.
  3. derive_ordering_relations.
  4. derive_relations.
  5. Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\2.orientationOfLandmarks\\orientationOfLandmarks_QCN_location-I\\SM10.leftright.qcn',
    create_qualify_file(Filename).
 

 %landmarksAndLandmaqrks
 Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\1.linearOrdering\\linearOrderingResults_location-II\\MM10.landmarks.allen.qcn',
  create_qualify_file(linear_ordering, landmarks, Filename).
  
  
%streetsAndStreets
  Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\1.linearOrdering\\linearOrderingResults_location-II\\MM10.streets.allen.qcn',
  create_qualify_file(linear_ordering, streets, Filename).

%streetAndLandmarks
  Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\1.linearOrdering\\linearOrderingResults_location-II\\MM10.streets_and_landmarks.allen.qcn',
  create_qualify_file(linear_ordering, streets_and_landmarks, Filename).
  
  %create_fact_file
 Filename ='E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\1.linearOrdering\\linearOrderingResults_location-II\\MM10.facts_linearOrdering.allen.pl',
 create_facts_file(linear_ordering_facts, allobjects, Filename).
  
  

 %------------------------------OrientationOfLandmarks---------------------------------------- 
 %for_MM
Filename ='E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\2.orientationOfLandmarks\\orientationOfLandmarks_location-II\\MM9.leftright.qcn',
 create_orientation_landmarks_QCN_MM(leftright_orientation, streetAndLandmarks, scenario(s1), Filename).
 
 %for_SM
 Filename ='E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\2.orientationOfLandmarks\\orientationOfLandmarks_location-II\\SM9.leftright.qcn',
 create_orientation_landmarks_QCN_SM(leftright_orientation, streetAndLandmarks, scenario(s1), Filename).
  
 
     %create_left_right_orientation_relation_as_QCNs
 Filename ='E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\2.orientationOfLandmarks\\orientationOfLandmarks_location-II\\SM7-test.leftright.qcn',
 create_orientation_landmarks_QCN(leftright_orientation, streetAndLandmarks, Filename).
 
 
    %create_fact_file_leftright_orientation
 Filename ='E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\2.orientationOfLandmarks\\orientationOfLandmarks_QCN_location-II\\SM7.leftright_fact.pl',
 create_orientation_landmarks_facts(leftright_fact, allobjects, Filename).
 
 
 
  
  
  
 %--------------cyclic-ordering------------------------- 
 
  %cyclicOrderingBetween_all_objects
 Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\3.cyclicOrdering\\cyclicOrderingOfLandmarks_QCN_location-I\\SM10.CIA.qcn',
 create_qualify_file_cyclicOrdering_ALL_QCN(cyclic_ordering, allObjects, Filename). 
 
 
 %landmarksAndLandmaqrks
 Filename = 'E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\3.cyclicOrdering\\cyclicOrderingOfLandmarks_QCN_location-II\\SM2.CIA_test.qcn',
 create_qualify_file_cyclicOrdering_QCN(cyclic_ordering, landmarks, Filename). 
  
  
  ----

HOW TO SAVE A FILE

-----

BASIC STRUCTURE OF QUERY / RULE TO SAVE A FILE

open(test,write,Stream),
nl(Stream),

forall(
  call(Pred),
  (write_term(Stream, Pred, [quoted(true)]), write(Stream,'.'), nl(Stream))
),

close(Stream).

-----

CONCRETE EXAMPLE TO SAVE ROUTE, ORDERING, PROXIMITY RELATIONS


Fn ='E:\\04 Research_Work\\SketchMapia Documents\\Qualtiative-Relations QCNs\\spatialRules_prolog\\2.orientationOfLandmarks\\orientationOfLandmarks_QCN_location-I\\MM1.leftright.qcn',
create_facts_file(Fn).
open(Fn,write,Stream),
nl(Stream),

forall(
  call(relation(X)),
  (write_term(Stream, relation(X), [quoted(true)]), write(Stream,'.'), nl(Stream))
),

%forall(
 % call(route(Rel,A,B)),
  %(write_term(Stream, route(Rel,A,B), [quoted(true)]), write(Stream,'.'), nl(%Stream))
%),


(buffer(Buf),
 write_term(Stream, buffer(Buf), [quoted(true)]), write(Stream,'.'), nl(Stream)),


close(Stream).




cyclic-ordering query
================================
Ca is pi/2,
Cb is pi/4, % 45 degree 
A=cyclic_interval(centre(Ca),0.05),
B=cyclic_interval(centre(Cb),0.05),
ordering(Rel, A,B).



%%facts to remove converses



retractall(relation(ordering(equal,_,_)))


relation(ordering(equal,X,Y))

forall((
   relation(ordering(R1,A,B)),
   relation(ordering(R2,B,A))
  ),(
   retract(relation(ordering(R2,B,A)))
)).




 relation(proximity(adjacent, id(r45), id(l18l0)))
 sm_orientation(Rel,id(l17l0),id(r38))
 
 
 match(group(5), id(l1283792813), id(r187263781)).
 match(group(5), id(l1283792813), id(r187263781)).
 match(group(5), id(l1283792813), id(r187263781)).
 match(group(5), id(l1283792813), id(r187263781)).
 match(group(5), id(l1283792813), id(r187263781)).
 
 
 match(group(6), id(l1283792813), id(r187263781)).
 match(group(6), id(l1283792813), id(r187263781)).
 match(group(6), id(l1283792813), id(r187263781)).
 match(group(6), id(l1283792813), id(r187263781)).


 to count matches
 
 
G=41,
aggregate_all(count, match(group(G),_,_), Count)




object(A,_),
object(B,_),
not(A == B),
not(relation(sm_orientation(_,A,B))),
not(relation(general(_ , A, B))),
not(relation(route(_ , A, B))),
not(relation(ordering(_ , A, B))).
