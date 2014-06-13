what's new:

* combine modfail, modedg and add kcheckprecrack into modfnelm: edgcnc, kcheckprecrack, edgstatus, kplyfail and subcnc are all targetted at the parent fn element, 
  no need to separate them.
  - in the future, create an object (type) fnelm, with variables fstat, pstat, subcnc

* a clean & clear structure of the code is needed, a module does not modify global arrays! update to global arrays is only done in the main program through interfaces
  in this regard, modfnelm needs to be changed, remove all modifications to the fnode array! do it in the main uel program systematically.

* memory saving: create element object, newelement subroutine creates a new object, or empty an existing one for reuse (save memory)

* regarding output:
  global arrays: ndelm (in), fnode (inout), elmsub (inout) can output the mesh at every time 
  global arrays: stress, strain, damage, sdv
 
