       module modfnelm ! define assoc. nodes of each edge accord. to elm type
        use modparam
        use modmtools
        use modndelm
        use modfnode
        use modelmsub
c
        implicit none

        ! type fnelm
        !  character(len=10)::element
        !  integer :: nndr,nndfl,nedg
        !  
        ! end type fnelm
        
        contains

        subroutine edgcnc(element,edg,nedg) ! takes in element, update edge & nedg

          character(len=10),intent(in):: element
          integer,intent(inout)::nedg
          integer,allocatable,intent(inout)::edg(:,:)

          if(allocated(edg)) then
              write(6,*)'edg array already allocated!'
              call xit
          end if

          if(element .eq. 'quad4') then
c           allocate edge connectivity
            nedg=4
            allocate(edg(4,nedg))
            edg(:,:)=0
c            write(6,*)'check edg init: ',edg
c            call kinitial_n(edg,4,nedg)
c           assign real nodes to edge
            edg(1,1)=1
            edg(2,1)=2
            edg(1,2)=2
            edg(2,2)=3
            edg(1,3)=3
            edg(2,3)=4
            edg(1,4)=4
            edg(2,4)=1
            ! fl. nodes to edge
            edg(3,1)=5
            edg(4,1)=6
            edg(3,2)=7
            edg(4,2)=8
            edg(3,3)=9
            edg(4,3)=10
            edg(3,4)=11
            edg(4,4)=12
          else if(element .eq. 'tri3') then
c           allocate edge connectivity
            nedg=3
            allocate(edg(4,nedg))
            edg(:,:)=0
c            call kinitial_n(edg,4,nedg)
c           assign real nodes to edge
            edg(1,1)=1
            edg(2,1)=2
            edg(1,2)=2
            edg(2,2)=3
            edg(1,3)=3
            edg(2,3)=1
            ! fl. nodes to edge
            edg(3,1)=5
            edg(4,1)=6
            edg(3,2)=7
            edg(4,2)=8
            edg(3,3)=9
            edg(4,3)=10
          end if
        end subroutine edgcnc
c
c
c
c
c
c
        subroutine kcheckprecrack(fstat,pstat,jelm,element,ndim,xelm,
     &  nnode,cncsub,mxnd)
        use modprecrack
c
C       passed in variables    
        character(len=10), intent(in) :: element  
        integer, intent(in) :: jelm,ndim,nnode,mxnd
        real(kind=dp),intent(inout) :: fstat,pstat,xelm(ndim,nnode) ! parent elm failure status, coords of all nodes
        integer,intent(inout) :: cncsub(mxnd+1,*)
c       local variables
        integer :: i,j,k,isprecrack,ispartition,iscross ! isprecrack: 0 if no cracked edge; n if n cracked edges
        integer,allocatable :: fedg(:) !fedg: status of edge, 0: intact, 1: broken
        integer,allocatable :: fedgprc(:) ! fedgprc(i): the index of prc which breaks edge j
        integer,allocatable :: fvertex(:) ! status of a vertex of the element
        real(kind=dp) :: xct,yct ! intersection pnt coords
        real(kind=dp) :: x1,x2,y1,y2 ! elm edge is (x1,y1) to (x2,y2)
        real(kind=dp) :: xp1,xp2,yp1,yp2 ! precrack is (xp1,yp1) to (xp2,yp2)
        integer :: nedg,kfedg, jfnd, jfnd2
        integer,allocatable :: edg(:,:) ! edg(:,j): fl. nodes on edge j 
c       initialize local variables (dont use data in subroutines)
        i=0;j=0;k=0;isprecrack=0;ispartition=0;iscross=0;kfedg=0
        jfnd=0;jfnd2=0
        xct=zero;yct=zero;x1=zero;x2=zero;y1=zero;y2=zero
        xp1=zero;xp2=zero;yp1=zero;yp2=zero
c-----------------
        call edgcnc(element,edg,nedg) ! find no. of edges and update nodes on edge according to element type
c-----------------        
        allocate(fedg(nedg))
        allocate(fedgprc(nedg))
        fedg(:)=0 !initialize fedg
        fedgprc(:)=0
c
c
        if(nprc.gt.0) then ! precracks exist
c            call assignprc(nprc,prctype,prctip)
c           ! verify precracks
C             write(6,*) '.... verify precracks ....'
C             write(6,*) '.... no. of precracks: ', nprc
C             do i=1,nprc
C                 write(6,*) '--- precrack no: ', i
C                 write(6,*) '    precrack type: ', prctype(i)
C                 write(6,*) '    precrack tip 1 coords: ', prctip(1,i),
C      &          prctip(2,i)
C                 write(6,*) '    precrack tip 2 coords: ', prctip(3,i),
C      &          prctip(4,i)
C             end do
c
            if(element .eq. 'quad4') then
            allocate(fvertex(4))
            fvertex(:)=0 !initialize all elms to 0
c           determine intersections with precrack lines
            do j=1,nprc !-all precracks
                ! tip coords of precrack j
                xp1=prctip(1,j)
                yp1=prctip(2,j)
                xp2=prctip(3,j)
                yp2=prctip(4,j)
                isprecrack=0 ! set counter to zero
                do i=1,nedg ! 4 edges
                if (fedg(i).eq.prctype(j)) cycle ! edge broken already,go to next edge
                ! tip corods of edge i
                x1=xelm(1,edg(1,i))
                y1=xelm(2,edg(1,i))
                x2=xelm(1,edg(2,i))
                y2=xelm(2,edg(2,i))
c               write(6,*) 'check nodal coords: ',x1,y1,x2,y2
                iscross=0
                xct=zero
                yct=zero
                call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &          iscross,xct,yct)
              !write(6,*) 'check iscross: ',iscross,'edg',i,'precrack',j
                if (iscross.eq.1) then
                    fedg(i)=prctype(j) ! edge i cracked
                    fedgprc(i)=j ! edge i cracked by precrack j
                    isprecrack=isprecrack+1
                    kfedg=kfedg+1
                    xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                    xelm(2,edg(3,i))=yct
                    xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                    xelm(2,edg(4,i))=yct
                    jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                    fnode(1,jfnd)=prctype(j) ! update its status
                    fnode(2,jfnd)=xct
                    fnode(3,jfnd)=yct
                else if(iscross.eq.11) then ! crack cross node 1
                    if(fvertex(edg(1,i)).eq.0) then ! if this node as not been crossed by a crack
                        fvertex(edg(1,i))=1 ! update node status
                        fedg(i)=prctype(j) ! break edge i
                        fedgprc(i)=j ! edge i cracked by precrack j
                        isprecrack=isprecrack+1
                        kfedg=kfedg+1
                        xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                        xelm(2,edg(3,i))=yct
                        xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                        xelm(2,edg(4,i))=yct 
                        jfnd=min(ndelm(edg(3,i),jelm),
     &                   ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                        fnode(1,jfnd)=prctype(j) ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct
                    end if
                else if(iscross.eq.12) then ! crack cross node 2
                    if(fvertex(edg(2,i)).eq.0) then ! if this node as not been crossed by a crack
                        fvertex(edg(2,i))=1 ! update node status
                        fedg(i)=prctype(j) ! break edge i
                        fedgprc(i)=j ! edge i cracked by precrack j
                        isprecrack=isprecrack+1
                        kfedg=kfedg+1
                        xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                        xelm(2,edg(3,i))=yct
                        xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                        xelm(2,edg(4,i))=yct 
                        jfnd=min(ndelm(edg(3,i),jelm),
     &                   ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                        fnode(1,jfnd)=prctype(j) ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct                        
                    end if
                end if
                if (isprecrack.eq.2) exit !-one precrack breaks max two edges
c               end if ! if edge intact
                end do
                if (kfedg.eq.nedg) exit ! all edges broken already
            end do
c
            !write(6,*) 'check isprecrack: ',isprecrack
     

! ---------- update fstat value -----------------------------------------------------
            if(kfedg.eq.0) then
                ! fstat=zero ! no change
     
            else if(kfedg.eq.1) then 
              fstat=four !wake elem
!           *** if only one edge is broken, then a wake element is present ***
!           this part of program finds the precrack which breaks the edge,
!           and find the other edge which is crossed by the extension line
!           of the precrack, and add a weak discont.(a node) on the crossing pnt
              do i=1,nedg
                  if(fedg(i).gt.0) exit
              end do
              if(i.eq.nedg .and. fedg(i).eq.0) then
                  write(6,*)'inconsistency between kfedg and fedg!'
                  call xit
              end if
              j=fedgprc(i) ! prc which breaks edg i  
              xp1=prctip(1,j)
              yp1=prctip(2,j)
              xp2=prctip(3,j)
              yp2=prctip(4,j)
              isprecrack=0 ! set counter to zero
              do i=1,nedg
                if (fedg(i).eq.prctype(j)) cycle ! edge broken already,go to next edge
                ! tip corods of edge i
                x1=xelm(1,edg(1,i))
                y1=xelm(2,edg(1,i))
                x2=xelm(1,edg(2,i))
                y2=xelm(2,edg(2,i))
c               write(6,*) 'check nodal coords: ',x1,y1,x2,y2
                iscross=0
                xct=zero
                yct=zero
                call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &          iscross,xct,yct)
              !write(6,*) 'check iscross: ',iscross,'edg',i,'precrack',j
                if (iscross.eq.2) then
                    fedg(i)=3 ! edge i contains a crack tip (weak discont.)
                    fedgprc(i)=j ! edge i crossed by precrack j extension line
                    isprecrack=isprecrack+1
                    kfedg=kfedg+1
                    xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                    xelm(2,edg(3,i))=yct
                    xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                    xelm(2,edg(4,i))=yct
                    jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                    fnode(1,jfnd)=3 ! update its status
                    fnode(2,jfnd)=xct
                    fnode(3,jfnd)=yct
                else if(iscross.eq.21) then ! crack cross node 1
                    if(fvertex(edg(1,i)).eq.0) then ! if this node as not been crossed by a crack
                        fvertex(edg(1,i))=1 ! update node status
                        fedg(i)=3 ! edge i contains a crack tip (weak discont.)
                        fedgprc(i)=j ! edge i cracked by precrack j
                        isprecrack=isprecrack+1
                        kfedg=kfedg+1
                        xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                        xelm(2,edg(3,i))=yct
                        xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                        xelm(2,edg(4,i))=yct 
                        jfnd=min(ndelm(edg(3,i),jelm),
     &                   ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                        fnode(1,jfnd)=3 ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct
                    end if
                else if(iscross.eq.22) then ! crack cross node 2
                    if(fvertex(edg(2,i)).eq.0) then ! if this node as not been crossed by a crack
                        fvertex(edg(2,i))=1 ! update node status
                        fedg(i)=3 ! edge i contains a crack tip (weak discont.)
                        fedgprc(i)=j ! edge i cracked by precrack j
                        isprecrack=isprecrack+1
                        kfedg=kfedg+1
                        xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                        xelm(2,edg(3,i))=yct
                        xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                        xelm(2,edg(4,i))=yct 
                        jfnd=min(ndelm(edg(3,i),jelm),
     &                   ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                        fnode(1,jfnd)=3 ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct                        
                    end if
                end if
                if (isprecrack.eq.1) exit !-once the crossing edge is found, no need to proceed
c               end if ! if edge intact
              end do    

            else if(kfedg.eq.2) then
              fstat=five !cracked element
            else if(kfedg.gt.2) then
              write(6,*)'more than 2 broken edges not yet supported!'
              kfedg=2
              fstat=five
            else
              write(6,*)'unknown no. of kfedg in modprecrack!'
              call xit
            end if
                
            
!----------- update pstat and sub-element cnc matrices ----------------------------------------
            if(fstat.gt.zero) then
            
                do i=1,nedg
                if(fedg(i).gt.3) then ! a strong/cohesive crack on this edge
                jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                jfnd2=max(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))
                do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                end do
                end if
                end do
            
                call subcnc(jelm,element,edg,nedg,fedg,kfedg,
     &          pstat,cncsub,mxnd)
            end if
c
            else
                write(6,*) 'unsupported element for failure!'
                call xit
            end if
c
        endif ! if nprc .gt. 0
c
        deallocate(fedg)
        deallocate(edg)
        deallocate(fvertex)
        return  
        end subroutine kcheckprecrack
c
c
c
c
c
c
        subroutine edgstatus(fstat,pstat,jelm,element,ndim,xelm,
     &  nnode,cncsub,mxnd,theta)
       
c
C       passed in variables    
        character(len=10), intent(in) :: element  
        integer, intent(in) :: jelm,ndim,nnode,mxnd
        real(kind=dp),intent(in) :: theta
        real(kind=dp),intent(inout) :: fstat,pstat,xelm(ndim,nnode) ! parent elm failure status, coords of all nodes
        integer,intent(inout) :: cncsub(mxnd+1,*)
c       local variables
        integer :: i,j,k,isprecrack,ispartition,iscross ! isprecrack: 0 if no cracked edge; n if n cracked edges
        integer,allocatable :: fedg(:) !fedg: status of edge, 0: intact, 1: broken
        integer,allocatable :: ifedg(:)
c        integer,allocatable :: fvertex(:) ! status of a vertex of the element
        real(kind=dp) :: xct,yct ! intersection pnt coords
        real(kind=dp) :: x1,x2,y1,y2 ! elm edge is (x1,y1) to (x2,y2)
        real(kind=dp) :: xp1,xp2,yp1,yp2 ! precrack is (xp1,yp1) to (xp2,yp2)
        real(kind=dp) :: fstat2 ! local fstat variable
        integer :: nedg, kfedg, jfnd, jfnd2, cntr
        integer,allocatable :: edg(:,:) ! edg(:,j): fl. nodes on edge j 
        
c------- initialize local variables (dont use data in subroutines)
        i=0;j=0;k=0;ispartition=0;iscross=0
        nedg=0;kfedg=0;jfnd=0;jfnd2=0;cntr=0
        xct=zero;yct=zero;x1=zero;x2=zero;y1=zero;y2=zero
        xp1=zero;xp2=zero;yp1=zero;yp2=zero;fstat2=zero
c-----------------
        call edgcnc(element,edg,nedg) ! find no. of edges and update nodes on edge according to element type
c-----------------        
        allocate(fedg(nedg),ifedg(nedg))
        fedg(:)=0 !initialize fedg
        ifedg(:)=0
c
c------- find no. of broken edges --------------------------------------------------------------------
        do i=1,nedg
            jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
            if(fnode(1,jfnd).gt.zero) then
                fedg(i)=int(fnode(1,jfnd))
                kfedg=kfedg+1
                ifedg(kfedg)=i              
            end if
        end do
     

! ---------- calculate fstat value --------------------------------------------------------------------
10      if(kfedg.eq.0) then
            ! fstat2=zero ! no change 
            
        else if(kfedg.eq.1) then ! could be 1st time wake elm, tip elm, ref elm and trans elm
        
          if(fedg(ifedg(1)).eq.1) then ! refinement end, trans elem start
            fstat2=one
          else
            ! find the other edge to be partitioned
            ! first, find the coords of the existing end of the crack line
            j=ifedg(1)
            jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
            xp1=fnode(2,jfnd)           
            yp1=fnode(3,jfnd)
            !write(6,*)'xp1,yp1',xp1,yp1
            ! from theta, calculate the other end of the crack line (could be any pnt along the line)
            xp2=xp1+cos(theta/halfcirc*pi)
            yp2=yp1+sin(theta/halfcirc*pi)
            !write(6,*)'xp2,yp2',xp2,yp2
            ! next, find the edge crossed by the crack line
            do i=1,nedg
                if (i.eq.j) cycle ! the already cracked edge,go to next edge
                ! tip corods of edge i
                x1=xelm(1,edg(1,i))
                y1=xelm(2,edg(1,i))
                x2=xelm(1,edg(2,i))
                y2=xelm(2,edg(2,i))
                iscross=0
                xct=zero
                yct=zero
                call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &          iscross,xct,yct)
                if (iscross.gt.0) exit ! found the edge, no need to proceed
            end do
            ! update kfedg & ifedg
            kfedg=2
            ifedg(2)=i
            jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
            ! update fnode array of the fnode on edge i
            if(fedg(ifedg(1)).eq.2) then ! tip elem end, refinement elem. start (1st time)
                fstat2=two               
                ! find the other edge to be partitioned; change its status to 1 (trans elem start)
                fedg(i)=1               
                fnode(1,jfnd)=one
                fnode(2,jfnd)=xct
                fnode(3,jfnd)=yct             
            elseif(fedg(ifedg(1)).eq.3) then ! wake elem end, tip elem start (1st time)
                fstat2=three
                ! find the other edge to be partitioned; change its status to 2 (refinement start)
                fedg(i)=2
                fnode(1,jfnd)=two
                fnode(2,jfnd)=xct
                fnode(3,jfnd)=yct               
            elseif(fedg(ifedg(1)).ge.4) then ! wake elem start (1st time)
                fstat2=four !wake elem               
                ! find the other edge to be partitioned; change its status to 3 (tip elem start)
                fedg(i)=3
                fnode(1,jfnd)=three
                fnode(2,jfnd)=xct
                fnode(3,jfnd)=yct               
            else ! unknown edge status
                write(6,*)'unknown edge status!'
                call xit
            endif        
          endif
            
        else if(kfedg.eq.2) then ! could be cracked, wake, tip, refinement elem
        
          if(fedg(ifedg(1)).le.2 .and. fedg(ifedg(2)).le.2) then ! refinement elem
            fstat2=two         
          elseif((fedg(ifedg(1)).le.3 .and. fedg(ifedg(2)).eq.3)
     &    .or.(fedg(ifedg(2)).le.3 .and. fedg(ifedg(1)).eq.3)) then ! tip elem
            fstat2=three
          elseif((fedg(ifedg(1)).lt.4 .and. fedg(ifedg(2)).ge.4)
     &    .or.(fedg(ifedg(2)).lt.4 .and. fedg(ifedg(1)).ge.4)) then ! wake elem, cohesive/stress-free crack
            fstat2=four
          elseif(fedg(ifedg(1)).ge.4 .and. fedg(ifedg(2)).ge.4)then ! cracked elem, cohesive/stress-free crack             
            fstat2=five        
          else ! unknown combination
            write(6,*)'unknown combination of 2 edge status!'
            call xit
          endif
          
        else if(kfedg.gt.2) then
        
          write(6,*)'more than 2 broken edges not yet supported!'
          kfedg=2
          goto 10
          
        else
        
          write(6,*)'unknown no. of kfedg!'
          call xit
          
        end if
                
            
!----------- update fstat,xelm,pstat and sub-element cnc matrices ----------------------------------------
        if(fstat2.gt.fstat) then
            fstat=fstat2
            
            do j=1,kfedg
                i=ifedg(j) ! find the local index of broken edges
                jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge              
                xct=fnode(2,jfnd)
                yct=fnode(3,jfnd)
                xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                xelm(2,edg(3,i))=yct
                xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                xelm(2,edg(4,i))=yct

                if(fedg(i).gt.3) then ! a strong/cohesive crack on this edge
                    jfnd2=max(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))
                    do k=1,3
                        fnode(k,jfnd2)=fnode(k,jfnd)
                    end do
                end if
            end do
                       
            call subcnc(jelm,element,edg,nedg,fedg,kfedg,
     &      pstat,cncsub,mxnd)
     
        end if
c
c
c
        deallocate(fedg)
        deallocate(ifedg)
        deallocate(edg)
c        deallocate(fvertex)
        return  
        end subroutine edgstatus
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
c******************* subroutine kplyfail ****************************************************
c*********** quadratic stress failure criteria and element partition criterion **************
c********************************************************************************************
      subroutine kplyfail(nst,nig,sig,yt,s,fstat,pstat,element,
     & ndim,xelm,nnode,cncsub,mxnd,theta,jelm)

c     ! passed in variables
      real(kind=dp),intent(in) :: sig(nst,nig),yt,s,theta
      integer,intent(in) :: jelm
      character(len=10),intent(in) :: element
      real(kind=dp),intent(inout) :: xelm(ndim,nnode),fstat,pstat
      integer,intent(inout) :: nst,nig,ndim,nnode,mxnd,cncsub(mxnd+1,*)     
      ! local variables
      real(kind=dp) :: ff,xo,yo,xct,yct
      real(kind=dp) :: xp1,xp2,yp1,yp2 ! precrack is (xp1,yp1) to (xp2,yp2)
      real(kind=dp) :: x1,x2,y1,y2 ! elm edge is (x1,y1) to (x2,y2)
      integer :: ig, cntr, i, j, k, iscross
      integer :: nedg, kfedg, jfnd, jfnd2
      integer,allocatable :: edg(:,:) ! edg(:,j): fl. nodes on edge j 
      integer,allocatable :: fedg(:) !fedg: status of edge, 0: intact, 1: broken
      integer,allocatable :: ifedg(:)
c
      ! initialize local variables
      ff=zero     
      xo=zero;yo=zero
      xct=zero;yct=zero
      xp1=zero;xp2=zero;yp1=zero;yp2=zero
      x1=zero;x2=zero;y1=zero;y2=zero
      ig=0;cntr=0;i=0;j=0;k=0;iscross=0
      nedg=0;kfedg=0;jfnd=0;jfnd2=0
c	  
c-----------------
        call edgcnc(element,edg,nedg) ! find no. of edges and update nodes on edge according to element type
c-----------------        
        allocate(fedg(nedg),ifedg(nedg))
        fedg(:)=0 !initialize fedg
        ifedg(:)=0
c
        
      if((yt .eq. zero) .or. (s .eq. zero)) then
        write(6,*) 'zero strength value!'
        ff=one
      else
        do ig=1,nig
            if(nst .eq. 3) then ! bulk element
            ff=(max(zero,sig(2,ig))/yt)**2+(sig(3,ig)/s)**2
            else if(nst .eq. 2) then ! cohesive element
            ff=(max(zero,sig(1,ig))/yt)**2+(sig(2,ig)/s)**2
            else
                write(6,*) 'unsupported no. of strains!'
                call xit
            end if	
        end do
      end if
c

            
      if(ff .ge. one) then
        if(element .eq. 'quad4') then
        
          ! find xp1, yp1
          if(fstat .eq. zero) then ! element originally intact
            ! find centroid
            xo=quarter*(xelm(1,1)+xelm(1,2)+xelm(1,3)+xelm(1,4))
            yo=quarter*(xelm(2,1)+xelm(2,2)+xelm(2,3)+xelm(2,4))
            xp1=xo
            yp1=yo
            ! find xp2, yp2
            xp2=xp1+cos(theta/halfcirc*pi)
            yp2=yp1+sin(theta/halfcirc*pi)
            do i=1,nedg 
                ! tip corods of edge i
                x1=xelm(1,edg(1,i))
                y1=xelm(2,edg(1,i))
                x2=xelm(1,edg(2,i))
                y2=xelm(2,edg(2,i))
                iscross=0
                xct=zero
                yct=zero
                call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &              iscross,xct,yct)
                if (iscross.gt.0) then
                    fedg(i)=4 ! edge i cracked
                    kfedg=kfedg+1
                    xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                    xelm(2,edg(3,i))=yct
                    xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                    xelm(2,edg(4,i))=yct
                    !jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                    jfnd=ndelm(edg(3,i),jelm)
                    fnode(1,jfnd)=four ! update its status
                    fnode(2,jfnd)=xct
                    fnode(3,jfnd)=yct
                    jfnd=ndelm(edg(4,i),jelm)
                    fnode(1,jfnd)=four ! update its status
                    fnode(2,jfnd)=xct
                    fnode(3,jfnd)=yct
                 endif
                 if(kfedg.eq.2) exit ! found 2 broken edges already
            end do
          
          else ! element already partitioned
            !---------- find no. of broken edges -------------------------
            do i=1,nedg
                jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                if(fnode(1,jfnd).gt.zero) then
                    !fedg(i)=int(fnode(1,jfnd))
                    kfedg=kfedg+1
                    ifedg(kfedg)=i              
                end if
            end do
            !-------------------------------------------------------------
            ! find (xp1,yp1), (xp2,yp2)
            if (fstat .eq. one) then ! transition elm, already has an edge partitioned
                if(kfedg.ne.1) then 
                 write(6,*)'inconsistency in kplyfail,fstat=1!'
                 call xit
                else !kfedg=1
                 j=ifedg(1)
                 jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 ! update fedg(j) & fnode(jfnd)
                 fedg(j)=4 ! cohesive crack
                 fnode(1,jfnd)=four
                 ! find xp1, yp1
                 xp1=fnode(2,jfnd)           
                 yp1=fnode(3,jfnd)
                 ! update fnode status of the other fnode on the edge
                 jfnd2=max(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                 end do                 
                 ! find xp2, yp2
                 xp2=xp1+cos(theta/halfcirc*pi)
                 yp2=yp1+sin(theta/halfcirc*pi)
                 ! next, find the edge crossed by the crack line
                 do i=1,nedg
                    if (i.eq.j) cycle ! the already cracked edge,go to next edge
                    ! tip corods of edge i
                    x1=xelm(1,edg(1,i))
                    y1=xelm(2,edg(1,i))
                    x2=xelm(1,edg(2,i))
                    y2=xelm(2,edg(2,i))
                    iscross=0
                    xct=zero
                    yct=zero
                    call klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     &              iscross,xct,yct)
                    if (iscross.gt.0) then
                        fedg(i)=4 ! edge i cracked
                        kfedg=kfedg+1
                        xelm(1,edg(3,i))=xct ! store c tip coords on edge i fl. nd 1
                        xelm(2,edg(3,i))=yct
                        xelm(1,edg(4,i))=xct ! store c tip coords on edge i fl. nd 2
                        xelm(2,edg(4,i))=yct
                        !jfnd=min(ndelm(edg(3,i),jelm),ndelm(edg(4,i),jelm))! find the smaller fnode on this edge
                        jfnd=ndelm(edg(3,i),jelm)
                        fnode(1,jfnd)=four ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct
                        jfnd=ndelm(edg(4,i),jelm)
                        fnode(1,jfnd)=four ! update its status
                        fnode(2,jfnd)=xct
                        fnode(3,jfnd)=yct
                     endif
                     if(kfedg.eq.2) exit ! found 2 broken edges already
                 end do
                endif
            elseif (fstat.gt.one .and. fstat.lt.five) then ! element already has two edges partitioned
                if(kfedg.lt.2) then 
                 write(6,*)'inconsistency in kplyfail,fstat>1!'
                 call xit
                else
                 kfedg=2 ! ignore more than 2 broken edges at the moment
                 ! update first edge status var & fl. node status var
                 j=ifedg(1)
                 jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 fedg(j)=4 ! cohesive crack
                 fnode(1,jfnd)=four
                 ! update fnode status of the other fnode on the edge
                 jfnd2=max(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                 end do
                 ! update second edge status var & fl. node status var
                 j=ifedg(2)
                 jfnd=min(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 fedg(j)=4 ! cohesive crack
                 fnode(1,jfnd)=four
                 ! update fnode status of the other fnode on the edge
                 jfnd2=max(ndelm(edg(3,j),jelm),ndelm(edg(4,j),jelm))
                 do k=1,3
                    fnode(k,jfnd2)=fnode(k,jfnd)
                 end do
                endif         
            else
                write(6,*) 'unsupported fstat value for failure!'
                call xit          
            endif
          
          end if
          
          ! update the fstat, pstat, cncsub
          fstat=five
          call subcnc(jelm,element,edg,nedg,fedg,kfedg,
     &    pstat,cncsub,mxnd)
          
          
            
        else
            write(6,*) 'unsupported element for failure!'
            call xit
        end if
        
      !else ! ff not yet one
        ! do nothing
      end if
c
      deallocate(fedg)
      deallocate(ifedg)
      deallocate(edg)
        
      return  
      end subroutine kplyfail
c********************************************************************************************
c********************************************************************************************
c
c
c
c
c
c
c       
        subroutine subcnc(jelm,element,edg,nedg,fedg,kfedg,
     &  pstat,cncsub,mxnd)
c       passed in variabls        
        character(len=10),intent(in) :: element
        integer,allocatable,intent(in) :: edg(:,:)
        integer,allocatable,intent(in) :: fedg(:)
        integer,intent(in) :: jelm,nedg,mxnd
        real(kind=dp),intent(inout) :: pstat
        integer,intent(inout) :: kfedg,cncsub(mxnd+1,*)
c       local variables
        integer:: i,ispartition,i1,i2,i3

        i=0;ispartition=0;i1=0;i2=0;i3=0
        
        if(.not.allocated(edg)) then
            write(6,*) 'edg cnc not yet allocated!'
            call xit
        end if

        if(.not.allocated(fedg)) then
            write(6,*) 'edg failure status not yet allocated!'
            call xit
        end if

        if(element.eq.'quad4') then
10      select case (kfedg)
         case (0) !- no precrack, do nothing
c            write(6,*) 'no precrack in this element!'
         case (1) !- one edge cracked, tip partition
          i=0
          ispartition=0
          do while (i.lt.nedg.and.ispartition.eq.0)
            i=i+1
            if(fedg(i).gt.0) then
              if(fedg(i).eq.1) then ! weak discon, refinement end, transition start
                ispartition=1
                pstat=31._dp ! 3 tri3 sub-elm
                cncsub(1,1)=3 ! no. of nodes in bulk sub-elm 1
                cncsub(1,2)=3 ! no. of nodes in bulk sub-elm 2                
                cncsub(1,3)=3 ! no. of nodes in bulk sub-elm 3
                select case(i)
                    case (1)
                        i1=2;i2=3;i3=4
                    case (2)
                        i1=3;i2=4;i3=1
                    case (3)
                        i1=4;i2=1;i3=2
                    case (4)
                        i1=1;i2=2;i3=3
                    case default
                write(6,*)'wrong broken edge in tip partition, subcnc'
                        call xit
                end select
                ! sub elm 1 connec
                cncsub(1+1,1)=edg(1,i1)
                cncsub(1+2,1)=edg(2,i1)
                cncsub(1+3,1)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                
                ! sub elm 2 connec
                cncsub(1+1,2)=edg(1,i2)
                cncsub(1+2,2)=edg(2,i2)
                cncsub(1+3,2)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)               
                
                ! sub elm 3 connec
                cncsub(1+1,3)=edg(1,i3)
                cncsub(1+2,3)=edg(2,i3)
                cncsub(1+3,3)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
              else
                write(6,*)'transition partition only accepts fedg=1!'
                call xit
              end if
                
            end if
          end do
         case (2) !- two edges cracked
          i=0
          ispartition=0
          do while (i.lt.nedg.and.ispartition.eq.0)
            i=i+1
            if((fedg(i).gt.0).and.
     &      (((i+2).le.nedg).and.(fedg(i+2).gt.0))) then ! opposing edges broken
                ispartition=1
                pstat=21._dp
                cncsub(1,1)=4 ! no. of nodes in bulk sub-elm 1
                cncsub(1,2)=4 ! no. of nodes in bulk sub-elm 2 
c               cncsub(1,3)=-4 ! no. of nodes in coh sub-elm 3 
c
                !-sub elm 1 connec
                if(i.eq.1) then
                cncsub(1+1,1)=edg(1,4)
                cncsub(1+2,1)=edg(2,4)
                else
                cncsub(1+1,1)=edg(1,i-1)
                cncsub(1+2,1)=edg(2,i-1)
                end if
                cncsub(1+3,1)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                cncsub(1+4,1)=actvnd(fedg(i+2),edg(4,i+2),edg(3,i+2),
     &          jelm)
c
                !-sub elm 2 connec
                cncsub(1+1,2)=edg(1,i+1)
                cncsub(1+2,2)=edg(2,i+1)
                cncsub(1+3,2)=actvnd(fedg(i+2),edg(3,i+2),edg(4,i+2),
     &           jelm)
                cncsub(1+4,2)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
c
                !-sub elm 3 connec (if exists)
                if(fedg(i).eq.4 .or. fedg(i+2).eq.4) then ! cohesive crack
                  pstat=31._dp
                  cncsub(1,3)=-4 ! cohesive sub-elm                 
                  cncsub(1+1,3)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                  cncsub(1+2,3)=actvnd(fedg(i+2),edg(3,i+2),edg(4,i+2)
     &             ,jelm)
                  cncsub(1+3,3)=actvnd(fedg(i+2),edg(4,i+2),edg(3,i+2)
     &             ,jelm)
                  cncsub(1+4,3)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                end if
             else if((fedg(i).gt.0).and.
     &      (((i+1).le.nedg).and.(fedg(i+1).gt.0))) then ! neighbouring edges broken
                ispartition=1
                pstat=41._dp
                cncsub(1,1)=3
                cncsub(1,2)=3
                cncsub(1,3)=3
                cncsub(1,4)=3
c
                !-sub elm 1 connec
                cncsub(1+1,1)=edg(1,i+1)
                cncsub(1+2,1)=actvnd(fedg(i+1),edg(3,i+1),edg(4,i+1),
     &          jelm)
                cncsub(1+3,1)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
c
                !-sub elm 2 connec
                cncsub(1+1,2)=actvnd(fedg(i+1),edg(4,i+1),edg(3,i+1),
     &          jelm)
                cncsub(1+2,2)=edg(2,i+1)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+3,2)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+3,2)=edg(2,nedg)
                end if
c
                !-sub elm 3 connec
                cncsub(1+1,3)=actvnd(fedg(i+1),edg(4,i+1),edg(3,i+1),
     &          jelm)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+2,3)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+2,3)=edg(2,nedg)
                end if
                cncsub(1+3,3)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
c
                !-sub elm 4 connec
                if(mod(i+3,nedg).ne.0) then
                    cncsub(1+1,4)=edg(1,mod(i+3,nedg)) 
                    cncsub(1+2,4)=edg(2,mod(i+3,nedg))
                else
                    cncsub(1+1,4)=edg(1,nedg) !-sub elm 4 connec
                    cncsub(1+2,4)=edg(2,nedg)
                end if
                cncsub(1+3,4)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                
                !-sub elm 5 connec (if exists)
                if(fedg(i).eq.4 .or. fedg(i+1).eq.4) then ! cohesive crack
                  pstat=51._dp
                  cncsub(1,5)=-4 ! cohesive sub-elm                 
                  cncsub(1+1,5)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                  cncsub(1+2,5)=actvnd(fedg(i+1),edg(3,i+1),edg(4,i+1)
     &             ,jelm)
                  cncsub(1+3,5)=actvnd(fedg(i+1),edg(4,i+1),edg(3,i+1)
     &             ,jelm)
                  cncsub(1+4,5)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                end if
                
             else if((fedg(i).gt.0).and.
     &      ((i.eq.nedg).and.(fedg(1).gt.0))) then ! neighbouring edges broken (last edge & 1st edge)
                ispartition=1
                pstat=41._dp
                cncsub(1,1)=3
                cncsub(1,2)=3
                cncsub(1,3)=3
                cncsub(1,4)=3
                !-sub elm 1 connec
                cncsub(1+1,1)=edg(1,1) 
                cncsub(1+2,1)=actvnd(fedg(1),edg(3,1),edg(4,1),jelm)
                cncsub(1+3,1)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
c
                !-sub elm 2 connec
                cncsub(1+1,2)=actvnd(fedg(1),edg(4,1),edg(3,1),jelm)
                cncsub(1+2,2)=edg(2,1)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+3,2)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+3,2)=edg(2,nedg)
                end if
c
                !-sub elm 3 connec
                cncsub(1+1,3)=actvnd(fedg(1),edg(4,1),edg(3,1),jelm)
                if(mod(i+2,nedg).ne.0) then 
                    cncsub(1+2,3)=edg(2,mod(i+2,nedg))
                else 
                    cncsub(1+2,3)=edg(2,nedg)
                end if
                cncsub(1+3,3)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
c
                !-sub elm 4 connec
                if(mod(i+3,nedg).ne.0) then
                    cncsub(1+1,4)=edg(1,mod(i+3,nedg)) 
                    cncsub(1+2,4)=edg(2,mod(i+3,nedg))
                else
                    cncsub(1+1,4)=edg(1,nedg)
                    cncsub(1+2,4)=edg(2,nedg)
                end if
                cncsub(1+3,4)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                
                !-sub elm 5 connec (if exists)
                if(fedg(i).eq.4 .or. fedg(1).eq.4) then ! cohesive crack
                  pstat=51._dp
                  cncsub(1,5)=-4 ! cohesive sub-elm                 
                  cncsub(1+1,5)=actvnd(fedg(i),edg(4,i),edg(3,i),jelm)
                  cncsub(1+2,5)=actvnd(fedg(1),edg(3,1),edg(4,1)
     &             ,jelm)
                  cncsub(1+3,5)=actvnd(fedg(1),edg(4,1),edg(3,1)
     &             ,jelm)
                  cncsub(1+4,5)=actvnd(fedg(i),edg(3,i),edg(4,i),jelm)
                end if
                
            end if            
          end do
C           write(6,*)'check cnc:',cncsub(2,1),cncsub(3,1),
C      &    cncsub(4,1),cncsub(5,1),cncsub(2,2),cncsub(3,2),
C      &    cncsub(4,2),cncsub(5,2),cncsub(2,3),cncsub(3,3),
C      &    cncsub(4,3),cncsub(5,3),cncsub(2,4),cncsub(3,4),
C      &    cncsub(4,4),cncsub(5,4)
         case(3) !- three edges crack
           write(6,*) 'three-edge precrack partition not yet supported!'
           kfedg=2
           goto 10
         case(4) !- four edges crack
           write(6,*) 'four-edge precrack partition not yet supported!'
           kfedg=2
           goto 10
         case default
           write(6,*) 'WARNING: precrack case selection default!'
        end select
c
        !- update sub-element connectivities to the global arrays
        do i=1,size(cncsub(1,:))
            do j=1,mxnd+1
            elmsub(j,i,jelem)=cncsub(j,i)
            end do
        end do
                    
        else
          write(6,*) 'unsupported element type for crack partition!'
        end if

        end subroutine subcnc

        

        integer function actvnd(ctype,nd1,nd2,jelm)

            integer,intent(in) :: ctype,nd1,nd2,jelm

            select case(ctype)
                case(5) 
                    actvnd=nd1 ! strong
                case(4) 
                    actvnd=nd1 ! cohesive 
                case(1,2,3)  ! weak discon. and tip, refinement, transition partition
                    if(ndelm(nd1,jelm).gt.ndelm(nd2,jelm)) then
                        actvnd=nd2 ! weak discont. only uses smaller fl node on the edge
                    else
                        actvnd=nd1
                    end if
                case default 
            write(6,*)'warning:wrong prctype input in function actvnd!'
                    call xit
            end select

        end function actvnd

       end module modfnelm
