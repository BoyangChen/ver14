c	Abaqus user-element subroutine to implement the floating node method
c 	-- codes written by Bo-Yang Chen
c	Department of Mechanical Engineering, National University of Singapore and 
c	Department of Aeronautics, Imperial College London
c
c
!------ include useful modules--------------------------
       include 'modparam.f'
       include 'modmtools.f'
       include 'modfnode.f'
       include 'modndelm.f'
       include 'modpatch.f'
       include 'modprecrack.f'
       include 'modfnelm.f'
       include 'modelmsub.f'
       include 'modoutput.f'
!------------------------------------------------------
c
! c
! c
! c
        subroutine uexternaldb(lop,lrestart,time,dtime,kstep,kinc)
        use modparam
        use modfnode
        use modndelm
        use modelmsub
        use modoutput
        use modprecrack, only: nprc,prctype,prctip,assignprc

         implicit none
 
         real(kind=dp),intent(in) :: time(2)
         real(kind=dp),intent(in) :: dtime
         integer,intent(in) :: lop,lrestart,kstep,kinc
         ! local variables
         integer :: ndim ! dimension of the analysis
         integer :: nsub, mxnd
         
!
!        initialize local variables
         ndim=0;nsub=0;mxnd=0
         
!------------------------------------
!------------------------------------
!enter here the dimension of analysis
         ndim=2
!------------------------------------
!------------------------------------
         select case (ndim)
            case(1)
                nsub=3; mxnd=2 ! max 3 sub elm, each with max 2 nodes
            case(2)
                nsub=5; mxnd=4 ! max 5 sub elm, each with max 4 nodes
            case(3)
                nsub=5; mxnd=8 ! max 5 sub elm, each with max 8 nodes
            case default
                write(6,*)'wrong ndim value in uexternaldb!'
                call xit
         end select


!
       	    if (lop .eq. 0) then
!           start of the analysis
                call initndelm()
                call initfnode()
                call initelmsub(nsub,mxnd)
                call assignprc()               
                ! get output directory
                lenoutdir=0
                outdir=''
                call getoutdir(outdir, lenoutdir)

!            else if (lop .eq. 1) then
!           start of the current increment
            
            else if (lop .eq. 2) then
!	        end of the increment 
                call output(kinc)
            
C             else if (lop .eq. 3) then
C !	        end of the analysis 
C                 write(6,*)'------- check floating node status -------'
C                 do i=1,size(fnode,2)
C                     if(fnode(1,i).ne.zero) then
C                     write(6,*)'--- floating node:', i, '---'
C                     write(6,*)'status:', fnode(1,i)
C                     write(6,*)'coordinates:', fnode(2,i), fnode(3,i)
C                     end if
C                 end do
!
            end if
!
!
        return
        end subroutine uexternaldb
c
c
c      
c
c
c
c
c
c********************************************************************************************
c********************* subroutine uel for floating node method ******************************
c********************************************************************************************
      subroutine uel(rhs,amatrx,svars,energy,ndofel,nrhs,nsvars,
     &       props,nprops,coords,mcrd,nnode,u,du,v,a,jtype,time,dtime,
     &       kstep,kinc,jelem,params,ndload,jdltyp,adlmag,predef,npredf,
     &       lflags,mlvarx,ddlmag,mdload,pnewdt,jprops,njprop,period)
c
      include 'aba_param.inc'
c
c
c
c
      dimension rhs(mlvarx,*),amatrx(ndofel,ndofel),props(*)
      dimension svars(*),energy(8),coords(mcrd,nnode),u(ndofel)
      dimension du(mlvarx,*),v(ndofel),a(ndofel),time(2),params(*)
      dimension jdltyp(mdload,*),adlmag(mdload,*),ddlmag(mdload,*)
      dimension predef(2,npredf,nnode),lflags(*),jprops(*)
c     variables to be updated:
c     rhs         residual vector
c     amatrx      element k (stiffness) matrix or jacobian
c	  svars		  solution dependent variables (sdvs)
c
c     variables passed in:
c     coords      nodal coordinate matrix: coord(j,n) is jth coord of nth node
c     u           element displacement vector, (u_i^1, u_i^2,.. u_i^mcrd), i=1,...,nnode
c     du          element incremental displacement vector
c     nnode       no. of nodes in the element
c     ndofel      no. of dof in the element
c     nprops      no. of real-valued user-defined properties
c     mcrd        max value of coordinates parameter      
c
c
c==========================================================================	
c----- define internal variables ------------------------------------------ 
c==========================================================================
c     2d condition (plain stress/strain)
      integer j2d
c     no. of real nodes and no. of floating nodes
      integer nndr,nndfl,ndofr,ndoffl
c     no. of strains and integration points in parent element 
      integer nst,nig0
c     no. of cohesive cracks; no. of sub-elements; max no. of nodes in an elm
      integer ncoh,nsub,mxnd
c     parent element type 
      character(len=10):: parent
c
c==========================================================================	   
c --- check element type from jtype: --------------------------------------
c==========================================================================	
c   jtype: 4-digit number
c   digit1: dimension
c   digit2: 0:plane stress; 1:plane strain (for 2d only)
c   digit3: no. of vertices of the element domain
c   digit4: additional info (reserved for future development)
c
c-- example: 2d floating node element -------------------------------------
c   jtype=2030: plane-stress tri3 elem. 3 real and 6 floating (2 per edge)
c   jtype=2040: plane-stress quad4 elem. 4 real and 8 floating (2 per edge)
c--------------------------------------------------------------------------
c
c
c
c
c
      if(jtype .eq. 2040) then !- quad4 elem.
c       extract element information from jtype
        j2d=0 ! 0:plane stress; 1:plane strain
        nndr=4 ! no. of real nodes = 4 (quadrilateral parent element)
        parent='quad4'
        nndfl=8 ! no. of floating nodes = 4 (sufficient for one coh. crack)
        ndofr=8 ! no. of real dof
        ndoffl=16 ! no. of fl. dof
        ncoh=1 ! max no. of coh. crack = 1 in this example
        nsub=5 ! max no. of sub-elms (4 tri3 and 1 coh2d4 in this example)
        nst=3 ! 3 strains in 2d bulk element
        mxnd=4 ! max 4 nodes in 2d element
        nig0=4 ! 4 integration points in quad4 element
c       check user element inputs 
        if (nnode .ne. 12) then
         write(6,*) 'no. of nodes inconsistent!'
         call xit
        end if
        if (mcrd .ne. 2) then
         write(6,*) 'no. of coordinates inconsistent!'
         call xit
        end if
c
        call kfnm2d(rhs,amatrx,svars,energy,ndofel,nrhs,nsvars,
     &       props,nprops,coords,mcrd,nnode,u,du,v,a,jtype,time,dtime,
     &       kstep,kinc,jelem,params,ndload,jdltyp,adlmag,predef,npredf,
     &       lflags,mlvarx,ddlmag,mdload,pnewdt,jprops,njprop,period,
     &       j2d,nndr,nndfl,ndofr,ndoffl,ncoh,nsub,parent,nst,nig0,mxnd) 
      else if(jtype .eq. 2030) then !- tri3 elem.
c       extract element information from jtype
        j2d=0 ! 0:plane stress; 1:plane strain
        nndr=3 ! no. of real nodes = 3 (triangular parent element)
        parent='tri3'
        nndfl=6 ! no. of floating nodes = 4 (sufficient for one coh. crack)
        ndofr=6 ! no. of real dof
        ndoffl=12 ! no. of fl. dof
        ncoh=1 ! max no. of coh. crack = 1
        nsub=4 ! max no. of sub-elms (3 tri3 and 1 coh2d4 in this example)
        nst=3 ! 3 strains in 2d bulk element
        mxnd=4 ! max 4 nodes in 2d element
        nig0=1 ! 1 integration points in tri3 element
c       check user element inputs 
        if (nnode .ne. 9) then
         write(6,*) 'no. of nodes inconsistent!'
         call xit
        end if
        if (mcrd .ne. 2) then
         write(6,*) 'no. of coordinates inconsistent!'
         call xit
        end if
c
        call kfnm2d(rhs,amatrx,svars,energy,ndofel,nrhs,nsvars,
     &       props,nprops,coords,mcrd,nnode,u,du,v,a,jtype,time,dtime,
     &       kstep,kinc,jelem,params,ndload,jdltyp,adlmag,predef,npredf,
     &       lflags,mlvarx,ddlmag,mdload,pnewdt,jprops,njprop,period,
     &       j2d,nndr,nndfl,ndofr,ndoffl,ncoh,nsub,parent,nst,nig0,mxnd)
c    
      else
        write(6,*) 'element type undefined!'
        call xit
      end if
c
      return
      end subroutine uel
c
c
c
c
c
c
c********************************************************************************************
c********************* subroutine kfnm2d for 2d floating node method ************************
c********************************************************************************************
      subroutine kfnm2d(rhs,amatrx,svars,energy,ndofel,nrhs,nsvars,
     &       props,nprops,coords,mcrd,nnode,u,du,v,a,jtype,time,dtime,
     &       kstep,kinc,jelem,params,ndload,jdltyp,adlmag,predef,npredf,
     &       lflags,mlvarx,ddlmag,mdload,pnewdt,jprops,njprop,period,
     &       j2d,nndr,nndfl,ndofr,ndoffl,ncoh,nsub,parent,nst,nig0,mxnd)
      use modparam
      use modfnode
      use modndelm
      use modelmsub
      use modprecrack
      use modfnelm
      use modpatch ! comment it if not needed
c
      include 'aba_param.inc'
c
      dimension rhs(mlvarx,*),amatrx(ndofel,ndofel),props(*)
      dimension svars(*),energy(8),coords(mcrd,nnode),u(ndofel)
      dimension du(mlvarx,*),v(ndofel),a(ndofel),time(2),params(*)
      dimension jdltyp(mdload,*),adlmag(mdload,*),ddlmag(mdload,*)
      dimension predef(2,npredf,nnode),lflags(*),jprops(*)
c     parent element type 
      character(len=10):: parent      
c
c==========================================================================	
c----- define internal variables ------------------------------------------ 
c==========================================================================

! -------------------------------------------------------------------------
! -- type (object class) fnelm --------------------------------------------
! -------------------------------------------------------------------------
c -- parent element arrays 
      dimension xr(mcrd,nndr) !-coordinates of real nodes
      dimension ar(ndofr,ndofr),fr(ndofr),ur(ndofr) !-a*u=f
      dimension sigr(nst,nig0),epsr(nst,nig0)!-stresses&strains at gauss points
      integer cncr(ndofr) !-dof connectivity matrix
c -- floating nodes arrays
      dimension xfl(mcrd,nndfl),ufl(ndoffl) !-coords&dof of fl. nodes; initially all zero
      integer cncfl(ndoffl)
c -- coords of all nodes after failure (fl. nodes coords are crack tip coords)
      dimension coordsf(mcrd,nnode)
c -- sub-element connectivities
      integer cncsub(mxnd+1,nsub) !-max nsub sub-elements with max. mxnd nodes per elm
      
      
! -------------------------------------------------------------------------
! -- type (object class) stndelm ------------------------------------------
! -------------------------------------------------------------------------
c -- 2d standard sub-elm relevant arrays
c    3 node triangular sub-element: tri3
      dimension xtri3(2,3) !-coordinates of 3 nodes
      dimension atri3(6,6),ftri3(6),utri3(6) !-a*u=f
      dimension sigtri3(3,1),epstri3(3,1) !-3 stresses&strains at 1 gauss point
      integer cnctri3(6) !-dof connectivity matrix (tri elm)
c    4 node quadrilateral sub-element: quad4
      dimension xquad4(2,4) !-coordinates of 4 nodes
      dimension aquad4(8,8),fquad4(8),uquad4(8) !-a*u=f
      dimension sigquad4(3,4),epsquad4(3,4)!-3 stresses&strains at 4 gauss points
      integer cncquad4(8) !-dof connectivity matrix (quad elm) 


! -------------------------------------------------------------------------
! -- type (object class) cohelm -------------------------------------------
! -------------------------------------------------------------------------      
c    4 node cohesive sub-element: coh2d4
      dimension xcoh4(2,4) !-coordinates of 4 nodes
      dimension xigcoh4(2,2) !-coordinates of 2 integration points
      dimension acoh4(8,8),fcoh4(8),ucoh4(8) !-a*u=f
      dimension	sigcoh4(2,2) !-2 stresses at 2 integration points
      integer cnccoh4(8) !-dof connectivity matrix (coh elm)      
c    damage & failure of all coh elms (stored in svars)
      dimension fstcoh4(2,ncoh) !-failure status variables(at 2 intg pnts of all coh elms)
      dimension dcoh4(2,ncoh),decoh4(2,ncoh) !-damage variables
      dimension	u0coh4(2,ncoh),ufcoh4(2,ncoh) !-initial&final failure displacements
      
      
      
      
c    increment counter, failure step and failure increment
      integer iinc,istep0,iinc0
      integer i,ic,j,l,m,kntr !counters; ic: index of coh crack
      integer ksdvpr,ksdvfl,ksdvsb,ksdvcoh !no. sdvs prnt elm, fl nds, sub-elm, coh-elms
      integer cntsdv,cntsdv0,cntsdv1,cntsdv2 ! counts of sdvs used
      integer ksub,isb ! no. of sub-elms
c
c -- character variable: sub-element type	
      character(len=10):: element
c
c -- define parameters
c      parameter (zero=0._dp,one=1._dp,two=2._dp,three=3._dp,four=4._dp)
c
c
c
c==========================================================================	
c----- initialize input and stored properties ----------------------------- 
c==========================================================================
c
c	   initialize local matrices and vectors of the parent element
        call kinitial(xr,mcrd,nndr)
        call kinitial(ar,ndofr,ndofr)
        call kinitial(fr,ndofr,1)
        call kinitial(ur,ndofr,1)
        call kinitial(sigr,nst,nig0)
        call kinitial(epsr,nst,nig0)
        call kinitial(xfl,mcrd,nndfl)
        call kinitial(ufl,ndoffl,1)
        call kinitial(coordsf,mcrd,nnode)
        call kinitial(fstcoh4,2,ncoh)
        call kinitial(dcoh4,2,ncoh)
        call kinitial(decoh4,2,ncoh)
        call kinitial(u0coh4,2,ncoh)
        call kinitial(ufcoh4,2,ncoh)
c       initialize integer arrays of parent element
        call kinitial_n(cncr,ndofr,1)
        call kinitial_n(cncfl,ndoffl,1)
        call kinitial_n(cncsub,mxnd+1,nsub)
c
c	    - read material properties from uel property line in input file
        if (nprops .lt. 15) then
         write(6,*) 'no. of properties insufficient!'
         call xit
        end if
      	e1   = props(1) !-young's modulus
      	e2   = props(2)
      	e3   = props(3)
      	g12  = props(4) !-shear modulus
      	g13  = props(5)
      	g23  = props(6)
      	v12  = props(7) !-poisson's ratio
      	v13  = props(8)
      	v23  = props(9)
        theta = props(10) !- material orientation w.r.t global x direction
      	yt 	= props(11) !-transverese tensile strength
      	s 	= props(12) !-shear strength
      	gnc = props(13) !-mode i fracture toughness
      	gsc = props(14) !-mode ii fracture toughness
      	bketa = props(15) !- eta for bk formula of mixed-mode gc
c       - cohesive element penalty stiffness
        cohk=100._dp*max(e2,e3)
c       - initialize coordsf matrix (coords of all nodes, incl. crack tip coords of fl. nodes)
        do i = 1,nnode
            do j = 1,mcrd
            coordsf(j,i)= fnode(j+1,ndelm(i,jelem))
            end do
        end do
c       - initialize cncsub matrix
        do i=1,nsub
         do j=1,mxnd+1
          cncsub(j,i)=elmsub(j,i,jelem)
         end do
        end do
c       - initialize cnc and u of real nodes
        do i = 1,ndofr
            cncr(i)=i !-connectivity matrix
            ur(i)=u(cncr(i))
        end do
c       - initialize cnc and u of floating nodes
        do i = 1,ndoffl
            cncfl(i)=ndofr+i !-connectivity matrix
            ufl(i)=u(cncfl(i))
        end do
c	    - initialize svars array
     	if(kstep .eq. 1 .and. kinc .eq. 1) then
     		do i=1,nsvars
     			svars(i)=zero
     		end do
     	end if		
c 	    - read stored variables from svars
c		- about iinc & kinc, iinc0 & istep0:
c		- kinc: the increment number (of a step) of the current iteration given by abaqus
c		- iinc: the increment number (of a step) of the previous iteration stored in svars
c		- if kinc = iinc: the increment hasn't converged (still running iterations)
c		- else: the previous increment has converged and new increment has started
        fstat=svars(1) !-failure status of the element
        pstat=svars(2) !-partition status of the element (no. of sub elems)
     	iinc=svars(3) !- increment number of last iteration
     	istep0=svars(4) !- failure step number
     	iinc0=svars(5) !- failure increment number
        ksdvpr=5
c		(4 svars are used to store failure variables of parent element)
c       - update floating nodes coordinates
        kntr=0
        ksdvfl=kntr !-ksdvfl=mcrd*nndfl
c       - update sub-element connectivities
        kntr=0
        ksdvsb=kntr !-ksdvsb = nsub*mxnd
c       - read damage variables of all cohesive cracks
        cntsdv1=ksdvpr+ksdvfl+ksdvsb !-counter to count the number of svars used
        kntr=0
        do ic=1,ncoh
c		 cohesive element ic stored failure information
         do j=1,2 ! 2 intg pnts for coh2d4 element
     	  fstcoh4(j,ic)=svars(cntsdv1+kntr+1) !-failure status of coh elm gauss point j
     	  dcoh4(j,ic)=svars(cntsdv1+kntr+2) !- damage of coh elm gauss point j
     	  decoh4(j,ic)=svars(cntsdv1+kntr+3) !- danage of prev, equilibrium at gauss point j
     	  u0coh4(j,ic)=svars(cntsdv1+kntr+4) !- effect. disp. at fail. init. of gauss point j
     	  ufcoh4(j,ic)=svars(cntsdv1+kntr+5) !- effect. disp. at total fail of gauss point j
          kntr=kntr+5
         end do
        end do
        ksdvcoh=kntr !-ksdvcoh=ncoh*10
        cntsdv2=cntsdv1+ksdvcoh !-counter to count the number of svars used
c		(10 svars per cohesive element are used to store variables wrt failure)
c
c==========================================================================	
c----- postprocessing with nodal coords output in .dat file ---------------
c==========================================================================
c       svars are output in .dat file as sdvs   
c		abaqus .dat file format: 9 svars per line;
c       1 coh. crack may create n sub-elms, with max 4 nodes and 8 coords per elm,
c       one line in .dat file can output the nodal coords of one bulk sub-elm;
c       one line can output the avg stresses of one bulk sub-elm (3 stresses in 2D, 6 in 3D)
c       one line can output the coords & damage of intg pnts in one coh sub-elm.
c       thus, the proposed rules for svars output and postprocessing:
c           - initially, the lines output the nodal coords of all the bulk sub-elms;
c           - the first svars in the line is the no. of nodes in the bulk sub-elm, followed
c           - by the nodal coords; the next line shows the stresses of this element
c           - afterwards, the lines output coords&damage of intg pnts in all coh sub-elms;
c           - followed by the stresses at two intg points
c           - if the first svars = -1, it indicates the line is for a coh sub-elm.
c
c		initialize svars counter for nodal coords output in .dat file; 
        if(mod(cntsdv2,9).ne.0) then
         cntsdv0=(cntsdv2/9+1)*9 !- svars output for nodal coords starts from next line
        else
         cntsdv0=cntsdv2
        end if
c       in Quad4 parent elem, 1 coh crack may partition the parent elm into 4 bulk sub-elms
c       and 1 coh sub-elm (nsub=5); no. of svars needed is 5*9;
c       in Tri3 parent elem, 1 coh crack may partition the parent elm into 3 bulk sub-elms
c       and 1 coh sub-elm (nsub=5); no. of svars needed is 4*9;
        if (nsvars .lt. (cntsdv0+nsub*9*2)) then
         write(6,*) 'no. of svars insufficient!'
         write(6,*) 'no. of svars should be = ', cntsdv0+nsub*9*2
         call xit
        end if
c
c
c
     	if(kstep .eq. 1 .and. kinc .eq. 1) then
c==========================================================================
c----- check for precrack in this element during 1st increment ------------
c==========================================================================
            if(nprc.gt.0) then
              call kcheckprecrack(fstat,pstat,jelem,parent,mcrd,coordsf,
     &        nnode,cncsub,mxnd)
              write(6,*) 'element',jelem,'fstat =',fstat,'pstat=',pstat
              if(fstat.gt.zero) then
     		    istep0=kstep !- failure step number
     		    iinc0=kinc !- failure increment number 
c      		    - update failure info in svars
                svars(1)=fstat
                svars(2)=pstat
                svars(4)=istep0
                svars(5)=iinc0
c               - update sub-element connectivities to the global arrays
                !kntr=0
                do i=1,nsub
                    do j=1,mxnd+1
                    !kntr=kntr+1
                    elmsub(j,i,jelem)=cncsub(j,i)
                    end do
                end do
                
              else
c==========================================================================
c----- check for fnode status in this element -----------------------------
c==========================================================================  
                call edgstatus(fstat,pstat,jelem,parent,mcrd,coordsf,
     &          nnode,cncsub,mxnd,theta)
            write(6,*) 'element',jelem,'fstat =',fstat,'pstat=',pstat
                if(fstat.gt.zero) then
                    istep0=kstep !- failure step number
                    iinc0=kinc !- failure increment number                
          		    !- update failure info in svars
                    svars(1)=fstat
                    svars(2)=pstat
                    svars(4)=istep0
                    svars(5)=iinc0
                    !- update sub-element connectivities to the global arrays
                    !kntr=0
                    do i=1,nsub
                        do j=1,mxnd+1
                        !kntr=kntr+1
                        elmsub(j,i,jelem)=cncsub(j,i)
                        end do
                    end do
                end if
               
              end if
            end if
            
        else if((kstep .eq. 1 .and. kinc .gt. 1).or.(kstep.gt.1)) then
c==========================================================================
c----- check for fnode status in this element -----------------------------
c==========================================================================     
            if(fstat.eq.five) then 
                ! go straight to sub-element calculations
            else
                fstat0=fstat
                call edgstatus(fstat,pstat,jelem,parent,mcrd,coordsf,
     &          nnode,cncsub,mxnd,theta)
            write(6,*) 'element',jelem,'fstat =',fstat,'pstat=',pstat
                if(fstat.gt.fstat0) then
                    istep0=kstep !- failure step number
                    iinc0=kinc !- failure increment number                
          		    !- update failure info in svars
                    svars(1)=fstat
                    svars(2)=pstat
                    svars(4)=istep0
                    svars(5)=iinc0
                    !- update sub-element connectivities to the global arrays
                    !kntr=0
                    do i=1,nsub
                        do j=1,mxnd+1
                        !kntr=kntr+1
                        elmsub(j,i,jelem)=cncsub(j,i)
                        end do
                    end do
                end if                
                
                
            end if    
           
       end if

c
c
c    
c
c
c
c============================================================================
c----------------------------------------------------------------------------       
c----- when the element is intact : parent element --------------------------
c----------------------------------------------------------------------------
c============================================================================
10     if (fstat .eq. zero) then !-parent element intact
c
c		- initialize arrays
        call kinitial(amatrx,ndofel,ndofel)
        call kinitial(rhs,ndofel,1)
c		- set initial counter value for svars		
        cntsdv=cntsdv0
c
c****** parent element calculation *********************************	
c 		- initialize coords matrix of parent elm
      	do i=1,mcrd
        	do j=1,nndr
          	xr(i,j)=coordsf(i,j)
        	end do
      	end do
c
c
        ! check ply angle 
        if(biply.eq.1) then
            call checkply(theta,theta1,theta2,xr,mcrd,nndr)
        end if
c
c		- get a matrix, f vector, stress and strain
        if (parent .eq. 'quad4') then
            call kquad4_elm(e1,e2,e3,g12,g13,g23,v12,v13,v23,theta,
     1	nndr,mcrd,j2d,nst,nig0,ndofr,xr,ar,fr,ur,epsr,sigr)
        else if (parent .eq. 'tri3') then
            call ktri3_elm(e1,e2,e3,g12,g13,g23,v12,v13,v23,theta,
     1	nndr,mcrd,j2d,nst,nig0,ndofr,xr,ar,fr,ur,epsr,sigr)
        else
            write(6,*) 'parent element type not supported!'
            call xit
        end if
c*******************************************************************
c
c****** check for failure initiation in parent element *************
c       -failure criterion to calculate failure status and crack tip coords
     	call kplyfail(nst,nig0,sigr,yt,s,fstat,pstat,parent,
     & mcrd,coordsf,nnode,cncsub,mxnd,theta,jelem)
c           	
     	if (fstat .eq. zero) then !- no failure, export amatrx & rhs
c		- assemble to global amatrx and rhs
     		do i = 1,ndofr
     		do j = 1,ndofr
            amatrx(cncr(i),cncr(j))=amatrx(cncr(i),cncr(j))+ar(i,j)
     		end do
     		rhs(cncr(i),1)=rhs(cncr(i),1)-fr(i)
     		end do
c       - update nodal positions to svars for postprocessing
            cntsdv=cntsdv+1 ! 1st output of a new line in .dat file
            svars(cntsdv)=nndr+100 !100(indic. parent elm)+no. nodes
            kntr=0
            do i=1,nndr
      		do j=1,mcrd !-2d
      		cntsdv=cntsdv+1
      		kntr=kntr+1
      		svars(cntsdv)=xr(j,i)+ur(kntr)
      		end do
            end do
            cntsdv=cntsdv+(8-kntr) !-fill up the line
c           output stress
             if(theta.eq.zero) then
                 svars(cntsdv+1)=sum(sigr(1,:))/nig0
                 svars(cntsdv+2)=sum(sigr(2,:))/nig0
                 svars(cntsdv+3)=sum(sigr(3,:))/nig0
             else if(theta.eq.ninety) then
                 svars(cntsdv+1)=sum(sigr(2,:))/nig0
                 svars(cntsdv+2)=sum(sigr(1,:))/nig0
                 svars(cntsdv+3)=sum(sigr(3,:))/nig0
             end if
             cntsdv=cntsdv+9
            goto 20 !-go straight to the end of the subroutine
       	else !- failure initiated, fstat>1
     		istep0=kstep !- failure step number
     		iinc0=kinc !- failure increment number 
c      		- update failure info in svars
            svars(1)=fstat
            svars(2)=pstat
            svars(4)=istep0
            svars(5)=iinc0
c		    - update fl. nodes coords (crack tip coords) in svars
C             kntr=0
C             do i=1,nndfl
C                 do j=1,mcrd
C                 kntr=kntr+1
C                 svars(ksdvpr+kntr)=coordsf(j,i+nndr)
C                 end do
C             end do
c           - update sub-element connectivities to the global arrays
            !kntr=0
            do i=1,nsub
                do j=1,mxnd+1
                !kntr=kntr+1
                elmsub(j,i,jelem)=cncsub(j,i)
                end do
            end do
            goto 10 !-go straight to calculate sub-elements
       	end if
c
c
c
c
c============================================================================
c----------------------------------------------------------------------------       
c----- when the element is failed (partitions): -----------------------------
c----------------------------------------------------------------------------
c============================================================================
c              
      else if (fstat .gt. zero) then !- parent element failed
c
c- store current fstat value
c- fstat will be passed in failure criterion to be updated
c- after the calculations of sub-elements are completed
        !fstat2=fstat 
        !pstat2=pstat
c
c
c-------------------------------------------------------		       		
c******* reading fstat *********************************
c- dif. codes for dif. types of partitions -------------
c-------------------------------------------------------
        ksub=int(pstat/10) !- no. of sub-elements
        ic=0 !-cohesive sub-elm index
c-------------------------------------------------------		       		
c******* end reading fstat *****************************
c-------------------------------------------------------
c
c
c
c-------------------------------------------------------		       		
c******* pre-conditioning of arrays ********************     
c- same codes for all types of partitions --------------
c-------------------------------------------------------
c		- initialize arrays
        call kinitial(amatrx,ndofel,ndofel)
        call kinitial(rhs,ndofel,1)
c	
        cntsdv=cntsdv0
c-------------------------------------------------------		       		
c******* end array pre-conditioning ********************
c-------------------------------------------------------
c
c
c       
c-------------------------------------------------------		       		
c******* sub-elm calculation ***************************
c- same codes for all types of partitions --------------
c-------------------------------------------------------
        do isb=1,ksub ! isb: index of sub-element
         if(cncsub(1,isb).gt.0) then !-bulk sub-elm
            nnds=cncsub(1,isb) ! no. of nodes in sub-elm
            if(nnds .eq. 4) then
             element='quad4'
             nig=4 !-no. of intg pnts
             ndofs=8 !-no. of dof in sub-elm
c		     - initialize arrays for quad4 sub-element
             call kinitial(xquad4,mcrd,nnds)
             call kinitial(aquad4,ndofs,ndofs)
             call kinitial(fquad4,ndofs,1)
             call kinitial(uquad4,ndofs,1)
             call kinitial(sigquad4,nst,nig)
             call kinitial(epsquad4,nst,nig)
             call kinitial_n(cncquad4,ndofs,1)
c            - obtain element coords, connec, u vector 
             l=0 !-counter
             do i=1,nnds
              do j=1,mcrd
              xquad4(j,i)=coordsf(j,cncsub(1+i,isb))
              l=l+1
              cncquad4(l)=(cncsub(1+i,isb)-1)*mcrd+j
              uquad4(l)=u(cncquad4(l))
              end do
             end do

            ! check ply angle 
            if(biply.eq.1) then
                call checkply(theta,theta1,theta2,xquad4,mcrd,nnds)
c                write(6,*)'verify theta:',theta
            end if

c           - calculate a matrix and f vector
             call kquad4_elm(e1,e2,e3,g12,g13,g23,v12,v13,v23,theta,
     1	    nnds,mcrd,j2d,nst,nig,ndofs,xquad4,aquad4,fquad4,uquad4,
     1      epsquad4,sigquad4)
     
c           -failure criterion to calculate failure status and crack tip coords
            if(fstat.lt.five) then ! not yet fully cracked
                call kplyfail(nst,nig,sigquad4,yt,s,fstat,pstat,parent,
     &          mcrd,coordsf,nnode,cncsub,mxnd,theta,jelem)
                if(fstat.eq.five) goto 10 ! repeat the calculation with new partition
            end if
c
c		    - assemble to global amatrx and rhs
     		 do i = 1,ndofs
                do j = 1,ndofs
                amatrx(cncquad4(i),cncquad4(j))=
     1          amatrx(cncquad4(i),cncquad4(j))+aquad4(i,j)
                end do
                rhs(cncquad4(i),1)=rhs(cncquad4(i),1)-fquad4(i)
     		 end do
c           - update nodal positions to svars for postprocessing
             cntsdv=cntsdv+1 ! 1st output of a new line in .dat file
             svars(cntsdv)=nnds ! no. of nodes in this element
             kntr=0
             do i=1,nnds
      		  do j=1,mcrd !-2d
      		   cntsdv=cntsdv+1
      		   kntr=kntr+1
      		   svars(cntsdv)=xquad4(j,i)+uquad4(kntr)
      		  end do
             end do
             cntsdv=cntsdv+(8-kntr) !-fill up the line
c            output stress
             if(theta.eq.zero) then
                 svars(cntsdv+1)=sum(sigquad4(1,:))/nig
                 svars(cntsdv+2)=sum(sigquad4(2,:))/nig
                 svars(cntsdv+3)=sum(sigquad4(3,:))/nig
             else if(theta.eq.ninety) then
                 svars(cntsdv+1)=sum(sigquad4(2,:))/nig
                 svars(cntsdv+2)=sum(sigquad4(1,:))/nig
                 svars(cntsdv+3)=sum(sigquad4(3,:))/nig
             end if
             cntsdv=cntsdv+9
c
            else if(nnds .eq. 3) then
             element='tri3'
             nig=1 !-no. of intg pnts
             ndofs=6 !-no. of dof in sub-elm
c		     - initialize arrays for quad4 sub-element
             call kinitial(xtri3,mcrd,nnds)
             call kinitial(atri3,ndofs,ndofs)
             call kinitial(ftri3,ndofs,1)
             call kinitial(utri3,ndofs,1)
             call kinitial(sigtri3,nst,nig)
             call kinitial(epstri3,nst,nig)
             call kinitial_n(cnctri3,ndofs,1)
c            - obtain element coords, connec, u vector 
             l=0 !-counter
             do i=1,nnds
              do j=1,mcrd
              xtri3(j,i)=coordsf(j,cncsub(1+i,isb))
              l=l+1
              cnctri3(l)=(cncsub(1+i,isb)-1)*mcrd+j
              utri3(l)=u(cnctri3(l))
              end do
             end do

            ! check ply angle 
            if(biply.eq.1) then
                call checkply(theta,theta1,theta2,xtri3,mcrd,nnds)
c                write(6,*)'verify theta:',theta
            end if
            
c           - calculate a matrix and f vector
             call ktri3_elm(e1,e2,e3,g12,g13,g23,v12,v13,v23,theta,
     1	    nnds,mcrd,j2d,nst,nig,ndofs,xtri3,atri3,ftri3,utri3,
     1      epstri3,sigtri3)

c           -failure criterion to calculate failure status and crack tip coords
            if(fstat.lt.five) then ! not yet fully cracked
                call kplyfail(nst,nig,sigtri3,yt,s,fstat,pstat,parent,
     &          mcrd,coordsf,nnode,cncsub,mxnd,theta,jelem)
                if(fstat.eq.five) goto 10 ! repeat the calculation with new partition
            end if
 
c		    - assemble to global amatrx and rhs
     		 do i = 1,ndofs
                do j = 1,ndofs
                amatrx(cnctri3(i),cnctri3(j))=
     1          amatrx(cnctri3(i),cnctri3(j))+atri3(i,j)
                end do
                rhs(cnctri3(i),1)=rhs(cnctri3(i),1)-ftri3(i)
     		 end do
c           - update nodal positions to svars for postprocessing
             cntsdv=cntsdv+1 ! 1st output of a new line in .dat file
             svars(cntsdv)=nnds ! no. of nodes in this element
             kntr=0
             do i=1,nnds
      		  do j=1,mcrd !-2d
      		   cntsdv=cntsdv+1
      		   kntr=kntr+1
      		   svars(cntsdv)=xtri3(j,i)+utri3(kntr)
      		  end do
             end do
             cntsdv=cntsdv+(8-kntr) !-fill up the line
c            output stress
             if(theta.eq.zero) then
                 svars(cntsdv+1)=sum(sigtri3(1,:))/nig
                 svars(cntsdv+2)=sum(sigtri3(2,:))/nig
                 svars(cntsdv+3)=sum(sigtri3(3,:))/nig
             else if(theta.eq.ninety) then
                 svars(cntsdv+1)=sum(sigtri3(2,:))/nig
                 svars(cntsdv+2)=sum(sigtri3(1,:))/nig
                 svars(cntsdv+3)=sum(sigtri3(3,:))/nig
             end if
             cntsdv=cntsdv+9
c          
            else ! no. nodes not 4 or 3
             write(6,*) 'no. of nodes in bulk sub-elm is not supported!'
             call xit
            end if
c            
         else if (cncsub(1,isb).lt.0) then !-coh sub-elm
            ic=ic+1 !-update cohesive sub-elm index
            nnds=-cncsub(1,isb) ! no. of nodes in sub-elm
            if(nnds .eq. 4) then
             element='coh2d4'
             nig=2 !-no. of intg pnts
             ndofs=8 !-no. of dof in sub-elm
c		     - initialize arrays for quad4 sub-element
             call kinitial(xcoh4,mcrd,nnds)
             call kinitial(xigcoh4,mcrd,nig)
             call kinitial(acoh4,ndofs,ndofs)
             call kinitial(fcoh4,ndofs,1)
             call kinitial(ucoh4,ndofs,1)
             call kinitial(sigcoh4,mcrd,nig)            
             call kinitial_n(cnccoh4,ndofs,1)
c            - obtain element coords, connec, u vector 
             l=0
             do i=1,nnds
              do j=1,mcrd
              xcoh4(j,i)=coordsf(j,cncsub(1+i,isb))
              l=l+1
              cnccoh4(l)=(cncsub(1+i,isb)-1)*mcrd+j
              ucoh4(l)=u(cnccoh4(l))
              end do
             end do
c           calculate a matrix and f vector
            call kcoh2d4_elm(cohk,yt,s,gnc,gsc,bketa,ucoh4,fcoh4,acoh4,
     & 	xcoh4,xigcoh4,sigcoh4,fstcoh4,dcoh4,decoh4,u0coh4,ufcoh4,
     &  istep0,iinc0,kstep,kinc,iinc,dtime,ndofs,nnds,nig,mcrd,ic)

c           -failure criterion to calculate failure status and crack tip coords
            if(fstat.lt.five) then ! not yet fully cracked
                call kplyfail(mcrd,nig,sigcoh4,yt,s,fstat,pstat,parent,
     &          mcrd,coordsf,nnode,cncsub,mxnd,theta,jelem)
                if(fstat.eq.five) goto 10 ! repeat the calculation with new partition
            end if

c		    assemble to global amatrx and rhs
     		 do i = 1,ndofs
                do j = 1,ndofs
                amatrx(cnccoh4(i),cnccoh4(j))=
     1          amatrx(cnccoh4(i),cnccoh4(j))+acoh4(i,j)
                end do
                rhs(cnccoh4(i),1)=rhs(cnccoh4(i),1)-fcoh4(i)
     		 end do
c           - update damage variables to svars for postprocessing
             cntsdv=cntsdv+1
             svars(cntsdv)=-nig ! neg. value for coh subelm; nig: no. of intg pnts
             kntr=0
             do i=1,nig
      		  do j=1,mcrd !-2d
      		   cntsdv=cntsdv+1
      		   kntr=kntr+1
      		   svars(cntsdv)=xigcoh4(j,i)
      		  end do
             end do
             do i=1,nig
              cntsdv=cntsdv+1
              kntr=kntr+1
              svars(cntsdv)=decoh4(i,ic)
             end do
             cntsdv=cntsdv+(8-kntr) !-fill up the line
c           
            else
              write(6,*) 'no. of nodes in coh sub-elm is not supported!'
              call xit
            end if 
c
         else
            write(6,*) 'no. of nodes in sub-elm is zero!',isb
            call xit
         end if
c
        end do ! looping over all sub-elms    
c-------------------------------------------------------		       		
c******* end sub-elm calculation ***********************
c-------------------------------------------------------        
c
c
c
c
c
c------------------------------------------------------- 
c* update damage variables of all cohesive cracks ******
c- same codes for all types of partitions --------------
c------------------------------------------------------- 
        kntr=0
        do ic=1,ncoh !-looping over all cohesive cracks
c		 cohesive element ic failure information
         do j=1,2 ! 2 intg pnts for coh2d4 element
     	  svars(cntsdv1+kntr+1)=fstcoh4(j,ic) !-failure status of coh elm gauss point j
     	  svars(cntsdv1+kntr+2)=dcoh4(j,ic) !- damage of coh elm gauss point j
     	  svars(cntsdv1+kntr+3)=decoh4(j,ic) !- danage of prev, equilibrium at gauss point j
     	  svars(cntsdv1+kntr+4)=u0coh4(j,ic) !- effect. disp. at fail init. of gauss point j
     	  svars(cntsdv1+kntr+5)=ufcoh4(j,ic) !- effect. disp. at total fail of gauss point j
          kntr=kntr+5
         end do
        end do
c------------------------------------------------------- 		       		
c******* end update damage variables *******************
c-------------------------------------------------------  
c
c
c
       else
        write(6,*) 'fstat value undefined!'
        call xit
       end if !-fstat>0
c       
20     svars(3)=kinc !- update increment number of last iteration
c
       return
       end subroutine kfnm2d
c********************************************************************************************
c********************************************************************************************
c
c
c
c
c
c
c
c
c
c********************************************************************************************
c******************* subroutine ktri3_elm ***************************************************
c**** subroutine to calculate k-matrix and f-vector of a linear triangular element **********
c********************************************************************************************
      subroutine ktri3_elm(e1,e2,e3,g12,g13,g23,v12,v13,v23,theta,
     1	nnode,ndim,j2d,nst,nig,ndofel,coords,amatrx,au,u,strain,stress)
      use modparam
c     
      include 'aba_param.inc'
c
c	passed-in variables
      dimension amatrx(ndofel,ndofel),au(ndofel),u(ndofel) ! elm system matrices
      dimension coords(ndim,nnode) ! coordinates of the element nodes
      dimension stress(nst,nig),strain(nst,nig) !stress&strain vectors at intg pnts
c
c	local variables
      dimension w(nig),pq(nig,ndim) !integration pnts weight & location
      dimension fn(nnode),dn(nnode,ndim) ! shape functions & their deriv. physical space
      dimension ajac(ndim,ndim),gn(nnode,ndim) ! jacobian & shape func. deriv. natural space
      dimension bee(nst,ndofel),beet(ndofel,nst) ! b matrix and its transpose
      dimension dee(nst,nst),deeg(nst,nst) ! d matrix in local & global coord. syst.
      dimension btd(ndofel,nst),btdb(ndofel,ndofel) ! b'*d & b'*d*b
      dimension stress2(nst),strain2(nst)
      integer i,j,ig
c
c
c	   initialize local matrices and vectors
       call kinitial(w,nig,1)
       call kinitial(pq,nig,ndim)
       call kinitial(fn,nnode,1)
       call kinitial(dn,nnode,ndim)
       call kinitial(ajac,ndim,ndim)
       call kinitial(gn,nnode,ndim)
       call kinitial(bee,nst,ndofel)
       call kinitial(beet,ndofel,nst)
       call kinitial(dee,nst,nst)
       call kinitial(deeg,nst,nst)
       call kinitial(btd,ndofel,nst)
       call kinitial(btdb,ndofel,ndofel)
       call kinitial(stress2,nst,1)
       call kinitial(strain2,nst,1)
c
c
c----- calculate material local stiffness matrix dee ----------------------------------------
       call kdeemat(e1,e2,e3,g12,g13,g23,v12,v13,v23,ndim,j2d,nst,dee)
c
c-------calculate material stiffness matrix in global coordinates deeg-----------------------
       call kdeemat_global(nst,dee,deeg,theta)
c		
c------ set up integration points and weights -----------------------------------------------
       call ktri3_intg(pq,w,nig,ndim)
c
c------ calculate amatrx --------------------------------------------------------------------
       	do ig=1,nig !-calculate at each int point
c
     		call ktri3_shape(pq,fn,dn,nig,ig,nnode,ndim) !-to get n and dn
     		call kmatrix_mul(coords,dn,ajac,ndim,nnode,ndim) !-ajac=coords*dn
     		call kdeterminant(ajac,detj,ndim) !-get det(ajac)
     		call kjac_inv(ajac,detj,ndim) ! invert ajac onto itself
     		call kmatrix_mul(dn,ajac,gn,nnode,ndim,ndim) !-gn=dn*inv(ajac)
     		call kbeemat(bee,gn,nnode,nst,ndofel) !-calculate b matrix nst*ndofel
     		call kmatrix_transpose(bee,beet,nst,ndofel) !-beet = transpose(bee)
     		call kmatrix_mul(beet,deeg,btd,ndofel,nst,nst) !-btd = beet * deeg
     		call kmatrix_mul(btd,bee,btdb,ndofel,nst,ndofel)!-btdb=beet*deeg*bee
c
       		do i=1,ndofel
          		do j=1,ndofel
              		amatrx(i,j) = amatrx(i,j)+btdb(i,j)*detj*w(ig) !-gauss integration
          		end do
        	end do
c			       	
c-------- calculate strain in global coords: epsilon = b * u --------------------------------
            call kinitial(strain2,nst,1) ! reinitialize
        	do i=1,nst
                do j=1,ndofel
                    strain2(i) = strain2(i)+bee(i,j)*u(j)
                end do
        	end do
c-------- transform strain into local coords ------------------------------------------------
        	call ktransfer_strain(nst,strain2,theta) !-strain2 now in local coords
c					
c-------- calculate stress in local coords --------------------------------------------------
        	call kmatrix_mul(dee,strain2,stress2,nst,nst,1)
c			
c-------- store in stress and strain for output ---------------------------------------------
        	do i=1,nst
        		strain(i,ig)=strain2(i)
        		stress(i,ig)=stress2(i)
        	end do
c     	
       	end do !-looped over all int points. ig=nig
c
c-------- calculate a*u ---------------------------------------------------------------------
        call kmatrix_mul(amatrx,u,au,ndofel,ndofel,1) !- a*u = amatrx*u = -rhs
c
       return
       end subroutine ktri3_elm
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c*********** shape functions and derivatives for tri3 element *******************************
c********************************************************************************************
      subroutine ktri3_shape(pq,f,df,nig,ig,nnode,ndim)
      use modparam
c
      include 'aba_param.inc'
c      
      dimension f(nnode),df(nnode,ndim),pq(nig,ndim)
c
c
        if (ndim .eq. 2) then
            xi=pq(ig,1)
            eta=pq(ig,2)
            if (nnode .eq. 3) then
                f(1)=one-xi-eta
                f(2)=xi
                f(3)=eta
                df(1,1)=-one
                df(2,1)=one
                df(3,1)=zero
                df(1,2)=-one
                df(2,2)=zero
                df(3,2)=one
            else
                write(6,*) 'no. of nodes incorrect for ktri3_shape!'
                call xit
            end if
        else
            write(6,*) 'dimension incorrect for ktri3_shape!'
            call xit
        end if
c
c
       return
       end subroutine ktri3_shape
c
c
c
c********************************************************************************************
c******************* integration points for tri3 element ************************************
c********************************************************************************************
      subroutine ktri3_intg(pq,wt,nig,ndim)
      use modparam
c	this subroutine updates pq(*,*), coords of intg pnts, and wt(*), weights
      include 'aba_param.inc'
c
      dimension pq(nig,ndim),wt(nig)
c	
      if (ndim .eq. 2) then
        if (nig .eq. 1) then
            pq(1,1)= one_third
            pq(1,2)= one_third
            wt(1) = half
        else
            write(6,*) 'no. of intg pnts incorrect for tri3_intg!'
            call xit
        end if
      else
        write(6,*) 'dimension incorrect for tri3_intg!'
        call xit
      end if
c
      return
      end subroutine ktri3_intg
c********************************************************************************************
c********************************************************************************************     
c
c
c
c********************************************************************************************
c******************* subroutine kquad4_elm **************************************************
c**** subroutine to calculate k-matrix and f-vector of a linear quadrilateral element *******
c********************************************************************************************
      subroutine kquad4_elm(e1,e2,e3,g12,g13,g23,v12,v13,v23,theta,
     1	nnode,ndim,j2d,nst,nig,ndofel,coords,amatrx,au,u,strain,stress)
      use modparam
c     
      include 'aba_param.inc'
c
c	passed-in variables
      dimension amatrx(ndofel,ndofel),au(ndofel),u(ndofel) ! elm system matrices
      dimension coords(ndim,nnode) ! coordinates of the element nodes
      dimension stress(nst,nig),strain(nst,nig) !stress&strain vectors at intg pnts
c
c	local variables
      dimension w(nig),pq(nig,ndim) !integration pnts weight & location
      dimension fn(nnode),dn(nnode,ndim) ! shape functions & their deriv. physical space
      dimension ajac(ndim,ndim),gn(nnode,ndim) ! jacobian & shape func. deriv. natural space
      dimension bee(nst,ndofel),beet(ndofel,nst) ! b matrix and its transpose
      dimension dee(nst,nst),deeg(nst,nst) ! d matrix in local & global coord. syst.
      dimension btd(ndofel,nst),btdb(ndofel,ndofel) ! b'*d & b'*d*b
      dimension stress2(nst),strain2(nst)
      integer i,j,ig
c
c
c	   initialize local matrices and vectors
       call kinitial(w,nig,1)
       call kinitial(pq,nig,ndim)
       call kinitial(fn,nnode,1)
       call kinitial(dn,nnode,ndim)
       call kinitial(ajac,ndim,ndim)
       call kinitial(gn,nnode,ndim)
       call kinitial(bee,nst,ndofel)
       call kinitial(beet,ndofel,nst)
       call kinitial(dee,nst,nst)
       call kinitial(deeg,nst,nst)
       call kinitial(btd,ndofel,nst)
       call kinitial(btdb,ndofel,ndofel)
       call kinitial(stress2,nst,1)
       call kinitial(strain2,nst,1)
c
c
c----- calculate material local stiffness matrix dee ----------------------------------------
       call kdeemat(e1,e2,e3,g12,g13,g23,v12,v13,v23,ndim,j2d,nst,dee)
c
c-------calculate material stiffness matrix in global coordinates deeg-----------------------
       call kdeemat_global(nst,dee,deeg,theta)
c		
c------ set up integration points and weights -----------------------------------------------
       call kquad4_intg(pq,w,nig,ndim)
c
c------ calculate amatrx --------------------------------------------------------------------
       	do ig=1,nig !-calculate at each int point
c
     		call kquad4_shape(pq,fn,dn,nig,ig,nnode,ndim) !-to get n and dn
     		call kmatrix_mul(coords,dn,ajac,ndim,nnode,ndim) !-ajac=coords*dn
     		call kdeterminant(ajac,detj,ndim) !-get det(ajac)
     		call kjac_inv(ajac,detj,ndim) ! invert ajac onto itself
     		call kmatrix_mul(dn,ajac,gn,nnode,ndim,ndim) !-gn=dn*inv(ajac)
     		call kbeemat(bee,gn,nnode,nst,ndofel) !-calculate b matrix nst*ndofel
     		call kmatrix_transpose(bee,beet,nst,ndofel) !-beet = transpose(bee)
     		call kmatrix_mul(beet,deeg,btd,ndofel,nst,nst) !-btd = beet * deeg
     		call kmatrix_mul(btd,bee,btdb,ndofel,nst,ndofel)!-btdb=beet*deeg*bee
c
       		do i=1,ndofel
          		do j=1,ndofel
              		amatrx(i,j) = amatrx(i,j)+btdb(i,j)*detj*w(ig) !-gauss integration
          		end do
        	end do
c			       	
c-------- calculate strain in global coords: epsilon = b * u --------------------------------
            call kinitial(strain2,nst,1) ! reinitialize
        	do i=1,nst
                do j=1,ndofel
                    strain2(i) = strain2(i)+bee(i,j)*u(j)
                end do
        	end do
c-------- transform strain into local coords ------------------------------------------------
        	call ktransfer_strain(nst,strain2,theta) !-strain2 now in local coords
c					
c-------- calculate stress in local coords --------------------------------------------------
        	call kmatrix_mul(dee,strain2,stress2,nst,nst,1)
c			
c-------- store in stress and strain for output ---------------------------------------------
        	do i=1,nst
        		strain(i,ig)=strain2(i)
        		stress(i,ig)=stress2(i)
        	end do
c     	
       	end do !-looped over all int points. ig=nig
c
c-------- calculate a*u ---------------------------------------------------------------------
        call kmatrix_mul(amatrx,u,au,ndofel,ndofel,1) !- a*u = amatrx*u = -rhs
c
       return
       end subroutine kquad4_elm
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c*********** shape functions and derivatives for quad4 element ******************************
c********************************************************************************************
      subroutine kquad4_shape(pq,f,df,nig,ig,nnode,ndim)
      use modparam
      include 'aba_param.inc'
c      
      dimension f(nnode),df(nnode,ndim),pq(nig,ndim)
c
c
        if (ndim .eq. 2) then
            xi=pq(ig,1)
            eta=pq(ig,2)
            if (nnode .eq. 4) then
                f(1)=quarter*(one-xi)*(one-eta)
                f(2)=quarter*(one+xi)*(one-eta)
                f(3)=quarter*(one+xi)*(one+eta)
                f(4)=quarter*(one-xi)*(one+eta)
                df(1,1) = -quarter*(one-eta)
                df(2,1) =  quarter*(one-eta)
                df(3,1) =  quarter*(one+eta)
                df(4,1) = -quarter*(one+eta)
                df(1,2) = -quarter*(one-xi)
                df(2,2) = -quarter*(one+xi)
                df(3,2) =  quarter*(one+xi)
                df(4,2) =  quarter*(one-xi)
            else
                write(6,*) 'no. of nodes incorrect for kquad4_shape!'
                call xit
            end if
        else
            write(6,*) 'dimension incorrect for kquad4_shape!'
            call xit
        end if
c
c
       return
       end subroutine kquad4_shape
c
c
c
c********************************************************************************************
c******************* integration points for quad4 element ***********************************
c********************************************************************************************
      subroutine kquad4_intg(pq,wt,nig,ndim)
      use modparam
c	this subroutine updates pq(*,*), coords of intg pnts, and wt(*), weights
      include 'aba_param.inc'
c
      dimension pq(nig,ndim),wt(nig)
c
      root3= one/sqrt(three)
c	
      if (ndim .eq. 2) then
        if (nig .eq. 4) then
            pq(1,1)= -root3
            pq(1,2)= -root3
            pq(2,1)=  root3
            pq(2,2)= -root3
            pq(3,1)= -root3
            pq(3,2)=  root3
            pq(4,1)=  root3
            pq(4,2)=  root3
            do i=1,4
                wt(i) = one
            end do
        else
            write(6,*) 'no. of intg pnts incorrect for kquad4_intg!'
            call xit
        end if
      else
        write(6,*) 'dimension incorrect for kquad4_intg!'
        call xit
      end if
c
      return
      end subroutine kquad4_intg
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c************************ subroutine kdeemat ************************************************
c***** returns the elastic constitutive matrix of the material ******************************
c********************************************************************************************
      subroutine kdeemat(e1,e2,e3,g12,g13,g23,v12,v13,v23,
     & ndim,j2d,nst,dee)
      use modparam
c
      include 'aba_param.inc'
c
      dimension dee(nst,nst)      
c
        v21= e2*v12/e1
        v31= e3*v13/e1
        v32= e3*v23/e2
c
        del= one-v12*v21-v13*v31-v23*v32-two*v21*v32*v13
        if (nst .eq. 3) then
          if (j2d .eq. 0) then
            dee(1,1)= e1/(one-v12*v21)
            dee(1,2)= e1*v21/(one-v12*v21)
            dee(2,2)= e2/(one-v12*v21)
            dee(2,1)= dee(1,2)
            dee(3,3)= g12
          else if (j2d .eq. 1) then
            dee(1,1)= e1*(one-v23*v32)/del
            dee(1,2)= e1*(v21+v23*v31)/del
            dee(2,2)= e2*(one-v13*v31)/del
            dee(2,1)= dee(1,2)
            dee(3,3)= g12
          else
            write(6,*) 'j2d value not supported for kdeemat!'
            call xit
          end if
        else
         write(6,*) 'no. of strains not supported for kdeemat!'
         call xit
        end if
c
c
      return
      end subroutine kdeemat
c
c
c
c********************************************************************************************
c******************** subroutine deemat_global **********************************************
c**** to transform d matrix from local to global coordinates ********************************
c********************************************************************************************
      subroutine kdeemat_global(nst,dee,deeg,theta)
      use modparam
c
      include 'aba_param.inc'
c     
      dimension dee(nst,nst), deeg(nst,nst)
c
      dimension dee2(nst,nst)
      integer i,j
c     initialize local array
      call kinitial(dee2,nst,nst)
c
      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
c     d matrix in global coords stored first in local array dee2
      if (nst .eq. 3) then
        dee2(1,1) = c*c*c*c*dee(1,1) + two*c*c*s*s*(dee(1,2)
     &            + two*dee(3,3)) + s*s*s*s*dee(2,2)
        dee2(1,2) = s*s*c*c*(dee(1,1) + dee(2,2) - four*dee(3,3))
     &            + (s*s*s*s+c*c*c*c)*dee(1,2)
        dee2(2,1) = dee2(1,2)
        dee2(2,2) = s*s*s*s*dee(1,1) + two*c*c*s*s*(dee(1,2)
     &            + two*dee(3,3)) + c*c*c*c*dee(2,2)
        dee2(1,3) = s*c*(c*c*(dee(1,1) - dee(1,2) - two*dee(3,3))
     &            + s*s*(dee(1,2) - dee(2,2) + two*dee(3,3)))
        dee2(3,1) = dee2(1,3)
        dee2(2,3) = s*c*(s*s*(dee(1,1) - dee(1,2) - two*dee(3,3))
     &            + c*c*(dee(1,2) - dee(2,2) + two*dee(3,3)))
        dee2(3,2) = dee2(2,3)
        dee2(3,3) = c*c*s*s*(dee(1,1)+dee(2,2)-2*dee(1,2))
     &			  +(c*c-s*s)**2*dee(3,3)
      else
       write(6,*) 'no. of strains not supported for kdeemat_global!'
       call xit
      end if
c     pass values to deeg
      do i=1,nst
        do j=1,nst
            deeg(i,j)=dee2(i,j)
        end do
      end do
c
      return
      end subroutine kdeemat_global
c********************************************************************************************
c********************************************************************************************    
c
c
c
c********************************************************************************************
c********************* subroutine kmatrix_mul ***********************************************
c********************* matrix multiplication ************************************************
c********************************************************************************************
      subroutine kmatrix_mul(a,b,c,l,m,n)
      use modparam
c
      include 'aba_param.inc'
c
      dimension a(l,m), b(m,n), c(l,n)
c
      dimension c2(l,n)
      integer i,j,k	  
c
      do i=1,l
        do j=1,n
          c2(i,j) = zero !-initialize
          do k=1,m
            c2(i,j) = c2(i,j) + a(i,k)*b(k,j)
          end do
        end do
      end do
c     update c; c could be the same as a or b;
      do i=1,l
        do j=1,n   
          c(i,j)=c2(i,j)
        end do
      end do        
c
      return
      end subroutine kmatrix_mul
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c********************* subroutine kdeterminant **********************************************
c********* returns the determinant of a jacobian matrix	*************************************
c********************************************************************************************
      subroutine kdeterminant(ajacob,detj,ndim)
c
      include 'aba_param.inc'
c
      dimension ajacob(ndim,ndim)
c
      if (ndim .eq. 2) then
        detj= ajacob(1,1)*ajacob(2,2) - ajacob(1,2)*ajacob(2,1)
      else
        write(6,*) 'dimension not supported for kdeterminant!'
        call xit
      end if
c
      return
      end subroutine kdeterminant
c********************************************************************************************
c********************************************************************************************      
c
c
c
c********************************************************************************************    
c********************* subroutine kbeemat ***************************************************
c******* strain-displacement matrix for infinitesimal deformation ***************************
c********************************************************************************************
c
      subroutine kbeemat(bee,gn,nnode,nst,ndofel)
      use modparam
c
      include 'aba_param.inc'
c
      dimension gn(nnode,*),bee(nst,ndofel)
c
      integer i,j,k,l,m
c
      if (nst .eq. 3) then
        do m=1,nnode
          k= 2*m
          l=k-1
          x=gn(m,1)
          y=gn(m,2)
          bee(1,l)= x
          bee(3,k)= x
          bee(2,k)= y
          bee(3,l)= y
        end do
      else
       write(6,*) 'no. of strains not supported for kbeemat!'
       call xit        
      end if
c
      return
      end subroutine kbeemat
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c********************* subroutine kjac_inv **************************************************
c************** inverts a jacobian matrix onto itself ***************************************
c********************************************************************************************
      subroutine kjac_inv(ajacob,detj,ndim)
      use modparam
c
      include 'aba_param.inc'
c
      dimension ajacob(ndim,ndim)
c
      dimension ajac(ndim,ndim)
      integer i,j
c
c	  initialize local matrices and vectors
      call kinitial(ajac,ndim,ndim)
c
      do i=1,ndim
        do j=1,ndim
          ajac(i,j)=ajacob(i,j)
        end do
      end do
c
      if(ndim .eq. 2) then
        ajacob(1,1)=ajac(2,2)
        ajacob(2,1)=-ajac(2,1)
        ajacob(1,2)=-ajac(1,2)
        ajacob(2,2)=ajac(1,1)
      else
       write(6,*) 'dimension not supported for kjac_inv!'
       call xit
      end if
c
      if(detj .gt. zero) then
        do i=1,ndim
            do j=1,ndim
                ajacob(i,j)=ajacob(i,j)/detj
            end do
        end do
      else
       write(6,*) 'zero or negative detj in kjac_inv!'
       call xit
      end if
c
      return
      end subroutine kjac_inv
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c********************* subroutine kmatrix_transpose *****************************************
c********************* calculate matrix transpose *******************************************
c********************************************************************************************
      subroutine kmatrix_transpose(a,at,m,n)
c
      include 'aba_param.inc'
      dimension a(m,n), at(n,m)
c
      dimension at2(n,m)
      integer i,j
c     initialize local array
      call kinitial(at2,n,m)
c
      do i=1,m
        do j=1,n
          at2(j,i) = a(i,j)
        end do
      end do
c     update at; at can be a itself
      do i=1,m
        do j=1,n
          at(j,i)=at2(j,i)
        end do
      end do
c
      return
      end subroutine kmatrix_transpose
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c******************* subroutine ktransfer_strain ********************************************
c********** transfer strains from global to material coordinate systems *********************
c********************************************************************************************
      subroutine ktransfer_strain(nst,strain,theta)
      use modparam
c
      include 'aba_param.inc'
c
      dimension strain(nst)
c
      dimension strn(nst),t(nst,nst)
      integer i,j	  
c
c	  initialize local matrices and vectors
      call kinitial(strn,nst,1)
      call kinitial(t,nst,nst)
c
      c=cos(pi*theta/halfcirc)
      s=sin(pi*theta/halfcirc)
c
      if (nst .eq. 3) then
        t(1,1)=c*c
        t(1,2)=s*s
        t(1,3)=c*s
        t(2,1)=s*s
        t(2,2)=c*c
        t(2,3)=-c*s
        t(3,1)=-two*c*s
        t(3,2)=two*c*s
        t(3,3)=c*c-s*s
      else
       write(6,*) 'no. of strains not supported for ktransfer_strain!'
       call xit
      end if
      call kmatrix_mul(t,strain,strn,nst,nst,1)
c
      do i=1,nst
        strain(i) = strn(i)
      end do
c
      return
      end subroutine ktransfer_strain
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c******************* subroutine interface ***************************************************
c********* subroutine for the calculation of a 2d four node linear cohesive element *********
c********************************************************************************************
      subroutine kcoh2d4_elm(dcoh,yt,s,gnc,gsc,bketa,u,au,amatrx,
     & 	coords,xigp,tract,cfstat,dm,dmeq,u0_eff,uf_eff,istep0,iinc0,
     &	kstep,kinc,iinc,dtime,ndof,nnode,nig,ndim,icoh)
      use modparam
c
      include 'aba_param.inc'
c
      dimension au(ndof),amatrx(ndof,ndof),u(ndof)
      dimension coords(ndim,nnode),xigp(ndim,nig),tract(ndim,nig)
      dimension cfstat(nig,*),dm(nig,*),dmeq(nig,*)
      dimension u0_eff(nig,*),uf_eff(nig,*)
c       dcoh        penalty stiffness
c	    nig		    no. integration points
c       icoh        cohesive crack index
c	    pq(nig)	    local coordinates of integration points
c	    w(nig)	    weights of integration points
c	    f(nnode)	shape function
c	    df(nnode)	derivative of shape function w.r.t local coordinate
c
      dimension w(nig),pq(nig,ndim-1)
      dimension f(nnode),df(nnode,ndim-1),ccoords(ndim,nnode)       
      dimension rnm(ndim),tangent(ndim)
      dimension qmatrx(ndim,ndim)
      dimension ujump(ndim),fmatrx(ndim,ndof)
      dimension delta(ndim),dmatrx(ndim,ndim)
      dimension qf(ndim,ndof),dqf(ndim,ndof)
      dimension ftqt(ndof,ndim),ftqtdqf(ndof,ndof)
      dimension ftqttau(ndof),tract2(ndim)
      integer isnewton,kntr,i,j,n
c
      parameter (dfail=one)
c	    initialize local matrices and vectors
        call kinitial(w,nig,1)
        call kinitial(pq,nig,ndim-1)
        call kinitial(f,nnode,1)
        call kinitial(df,nnode,ndim-1)
        call kinitial(ccoords,ndim,nnode)
        call kinitial(rnm,ndim,1)
        call kinitial(tangent,ndim,1)
        call kinitial(qmatrx,ndim,ndim)
        call kinitial(ujump,ndim,1)
        call kinitial(fmatrx,ndim,ndof)
        call kinitial(delta,ndim,1)
        call kinitial(dmatrx,ndim,ndim)
        call kinitial(qf,ndim,ndof)
        call kinitial(dqf,ndim,ndof)
        call kinitial(ftqt,ndof,ndim)
        call kinitial(ftqtdqf,ndof,ndof)
        call kinitial(ftqttau,ndof,1)
        call kinitial(tract2,ndim,1)
c       check convergence of last iteration
        isnewton=0
        if(iinc .eq. kinc) then
            isnewton=1 !-current increment still doing newton iterations
        else !-previous increment has converged
            do i=1,nig !-update dmeq
            dmeq(i,icoh)=dm(i,icoh) !-equilibrium dm
            end do
        end if
c       calculate current coordinates of nodes
      	kntr=0
      	do i=1,nnode
      		do j=1,ndim
      		kntr=kntr+1
      		ccoords(j,i)=coords(j,i)+u(kntr)
      		end do
      	end do
c       set up integration points and weights
        call kcoh2d4_intg(pq,w,nig,ndim)
c
        do ig=1,nig !-looping over all integration points
c
c           shape functions and derivatives
            call kcoh2d4_shape(pq,ig,f,df,nnode,ndim,nig)
c
c		    current coordinates of the integration point
            xigp(1,ig)=half*(f(1)*ccoords(1,1)+f(2)*ccoords(1,2)+
     &				f(3)*ccoords(1,3)+f(4)*ccoords(1,4))
            xigp(2,ig)=half*(f(1)*ccoords(2,1)+f(2)*ccoords(2,2)+
     &				f(3)*ccoords(2,3)+f(4)*ccoords(2,4))
c
c           compute tangent of the interface: node 2 coords - node 1 coords
            tangent(1)=coords(1,2)-coords(1,1)
            tangent(2)=coords(2,2)-coords(2,1)
c           normalize tangent vector
            call kunitv(tangent,det,ndim) !-tangent vector normalized
            det=half*det !-ref. line starts from -1 to 1, length=2
c
c           compute normal of the interface
            rnm(1)=-tangent(2)
            rnm(2)=tangent(1)
c
c		    compute q matrix: rotate global coords to local coords
c		    q = transpose[rnm,tangent]
            do j=1,2
                qmatrx(1,j)=rnm(j)
                qmatrx(2,j)=tangent(j)
            end do
c
c           ujump:displacement jump of the two crack surface, in global coords
c		    fmatrx: ujump (at each int pnt) = fmatrx*u
            do n = 1,ndim
                do m = 1,nnode/2
                fmatrx(n,n+(m-1)*ndim)= -f(m)
                fmatrx(n,n+(nnode-m)*ndim)=f(m)
                end do
            end do
c	   	    calculate ujump
            call kmatrix_mul(fmatrx,u,ujump,ndim,ndof,1)
c           calculate separation delta in local coords: delta=qmatrx*ujump
            call kmatrix_mul(qmatrx,ujump,delta,ndim,ndim,1)
c		
c       -- cohesive law at integration pnt ig ----------------------	
        	if(cfstat(ig,icoh) .eq. zero) then !-intact
                dnn=dcoh
                dss=dcoh
                do k=1,ndim
                tract(k,ig)=dcoh*delta(k)
                end do
c 		-- check for failure
        	fchck=(max(zero,tract(1,ig))/yt)**2+(tract(2,ig)/s)**2
        		if(fchck .ge. one) then
                cfstat(ig,icoh)=one !-failure initiation
                ratio=one/sqrt(fchck) !-fchck may >> 1
c 			-- effective displacement at failure			
                u_eff=sqrt(max(zero,delta(1))**2+delta(2)**2)*ratio		
c           -- effective stress at failure
                t_eff=dcoh*u_eff
c		    -- work of normal and shear stress at failure
                gn=half*max(zero,tract(1,ig))*max(zero,delta(1))
                gs=half*tract(2,ig)*delta(2)		
c		    -- mixed-mode ratio (b-k) at failure
                b=gs/(gn+gs)
c		    -- mixed-mode fracture energy defined at failure
                gmc=gnc+(gsc-gnc)*b**bketa
                u0_eff(ig,icoh)=u_eff
                uf_eff(ig,icoh)=two*gmc/(t_eff)
                end if
            end if
c
            if(cfstat(ig,icoh) .eq. one) then !-damaged
c	
            	if(dmeq(ig,icoh) .lt. dfail) then
c               -- degradation: linear
                    dm2=zero
                    if(uf_eff(ig,icoh) .le. u0_eff(ig,icoh)) then
                    dm2=dfail !-brittle failure
                    else
c 					-- effective displacement			
						u_eff=sqrt(max(zero,delta(1))**2+delta(2)**2)				
						if(u_eff .ne. zero) then
c                       - linear softening law
                        dm2=uf_eff(ig,icoh)*(u_eff-u0_eff(ig,icoh))/
     1                  (u_eff*(uf_eff(ig,icoh)-u0_eff(ig,icoh)))
						dm2=min(dfail,dm2)
						dm2=max(zero,dm2)
     					end if
                    end if
c
                    if(dm2 .ge. dmeq(ig,icoh)) then !-loading
						dm(ig,icoh)=dm2
c						update stresses				
						if(delta(1) .gt. zero) then
						dnn = dcoh*(one-dm(ig,icoh))
						else
						dnn = dcoh !-damage doesn't show in compression
						end if
                        dss = dcoh*(one-dm(ig,icoh))
						tract(1,ig)=dnn*delta(1)
						tract(2,ig)=dss*delta(2)
                    else !-unloading
						dm(ig,icoh)=dmeq(ig,icoh) !-cannot < existing damage level
						if(delta(1) .gt. zero) then
						dnn = dcoh*(one-dm(ig,icoh))
						else
						dnn = dcoh !-damage doesn't show in compression
						end if
						dss = dcoh*(one-dm(ig,icoh))
						tract(1,ig)=dnn*delta(1)
						tract(2,ig)=dss*delta(2)
                    end if !-loading/unloading
c		
                else ! dmeq=dfail, element already failed
                    dm(ig,icoh)=dfail
                    if(delta(1) .gt. zero) then
						dnn = dcoh*(one-dfail)
                    else
						dnn = dcoh !-damage doesn't show in compression
                    end if
                    dss = dcoh*(one-dfail)
                    tract(1,ig)=dnn*delta(1)
                    tract(2,ig)=dss*delta(2)
                end if
c				
            end if !-damaged or not
c
c-------------------------------------------------------------------------
c
c -- calculate a matrix and rhs contribution of integration pnt ig --------
c a(ig) = (fmatrx'*q'*d*q*fmatrx)*w(ig), f(ig)=a*u=(fmatrx'*q'*tract)*w(ig) 
            dmatrx(1,1)=dnn
            dmatrx(2,2)=dss
c
            call kmatrix_mul(qmatrx,fmatrx,qf,ndim,ndim,ndof)
            call kmatrix_transpose(qf,ftqt,ndim,ndof)
            call kmatrix_mul(dmatrx,qf,dqf,ndim,ndim,ndof)
            call kmatrix_mul(ftqt,dqf,ftqtdqf,ndof,ndim,ndof)
c		
            do j=1,ndof
          	do k=1,ndof
            	amatrx(j,k) = amatrx(j,k)+ftqtdqf(j,k)*det*w(ig)
          	end do
            end do		
c
            do j=1,ndim
                tract2(j)=tract(j,ig)
            end do
            call kmatrix_mul(ftqt,tract2,ftqttau,ndof,ndim,1)
c		
            do j=1,ndof
                au(j)=au(j)+ftqttau(j)*w(ig)*det
            end do
c
c
       end do !-looped over all integration pnts
c
c
       return
       end subroutine kcoh2d4_elm
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c***** shape functions and derivatives for coh2d4 element ***********************************
c********************************************************************************************
c
      subroutine kcoh2d4_shape(pq,ig,f,df,nnode,ndim,nig)
      use modparam
      include 'aba_param.inc'
c
      dimension f(nnode),df(nnode,ndim-1),pq(nig,ndim-1)
c
      integer k,j
c
        if(ndim .eq. 2) then
            xi=pq(ig,1)
            if(nnode .eq. 4) then
                f(1)=half*(one-xi)
                f(2)=half*(one+xi)
                f(3)=f(2)
                f(4)=f(1)
                df(1,1)=-half
                df(2,1)=half
                df(3,1)=df(2,1)
                df(4,1)=df(1,1)
            else
                write(6,*) 'wrong number of nodes for kcoh2d4_shape!'
                call xit
            end if
        else
            write(6,*) 'wrong dimension for kcoh2d4_shape!'
            call xit
        end if
c
c
       return
       end subroutine kcoh2d4_shape
c
c
c
c********************************************************************************************
c******************* integration points for coh2d4 element **********************************
c********************************************************************************************
c
      subroutine kcoh2d4_intg(pq,wt,nig,ndim)
      use modparam
c	this subroutine outputs pq(*,*), coords of all intg pnts, and wt(*), weight
      include 'aba_param.inc'
c       
      dimension pq(nig,ndim-1),wt(nig)
c
      integer i,j
c        
        if(ndim .eq. 2) then
            if(nig .eq. 2) then
c           cn=0.5773502691896260_dp !-gauss
            cn=one !-newton cotes
            pq(1,1)=-cn
            pq(2,1)=cn
            wt(1)=one
            wt(2)=one
            else
                write(6,*) 'wrong no. of intg pnts for kcoh2d4_intg!'
                call xit
            end if
        else
            write(6,*) 'wrong dimension for kcoh2d4_intg!'
            call xit
        end if
c
c
       return
       end subroutine kcoh2d4_intg
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c******************* subroutine kunitv ******************************************************
c********** normalize vector and return its magnitude  **************************************
c********************************************************************************************
       subroutine kunitv(a,amag,ndim)
       use modparam
c
       include 'aba_param.inc'
c       
       dimension a(ndim)
       integer i
c
        amag=zero
        do i=1,ndim
         amag=amag+a(i)*a(i)
        end do
        amag=dsqrt(amag)
        if(amag .ne. zero) then
         do i=1,ndim
            a(i)=a(i)/amag
         end do
        else
         write(6,*) 'zero vector for kunitv!'
c        do nothing
        end if
c
       return
       end subroutine kunitv
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c******************* subroutine kinitial ****************************************************
c******************* initialize a real*8 matrix *********************************************
c********************************************************************************************
       subroutine kinitial(amtrx,nrow,ncol)
       use modparam
c
       include 'aba_param.inc'
c       
       dimension amtrx(nrow,ncol)
       integer i,j
c
        do i=1,nrow
            do j=1,ncol
                amtrx(i,j)=zero
            end do
        end do
c
       return
       end subroutine kinitial
c********************************************************************************************
c********************************************************************************************
c
c
c
c********************************************************************************************
c******************* subroutine kinitial_n **************************************************
c******************* initialize an integer matrix *******************************************
c********************************************************************************************
       subroutine kinitial_n(amtrx,nrow,ncol)
c
       include 'aba_param.inc'
c       
       integer amtrx(nrow,ncol)
       integer i,j,izero
       parameter (izero=0)
c
        do i=1,nrow
            do j=1,ncol
                amtrx(i,j)=izero
            end do
        end do
c
       return
       end subroutine kinitial_n
c********************************************************************************************
c********************************************************************************************
