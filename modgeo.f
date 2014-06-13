      module modgeo
      use modparam
!-------------------------------------------------------     
!---- module for geometrical quantities ----------------
!-------------------------------------------------------
      
      implicit none
      
      type point
        integer :: nmber
        real(kind=dp) :: x, y, z
      end type point
      
      type edge
        integer :: nmber
        type(point) :: pnt1, pnt2
        !integer :: dir ! dir=0 if edge vector is from pnt1 to pnt2; dir=1 if otherwise
      end type edge
      
      type tri
        type(point) :: pnt1, pnt2, pnt3
        type(edge) :: edg1, edg2, edg3
        !integer :: dir ! dir=0 if positive permutation; dir=1 if otherwise
      end type tri
      
      type quad
        type(point) :: pnt1, pnt2, pnt3, pnt4
        type(edge) :: edg1, edg2, edg3, edg4
        !integer :: dir ! dir=0 if positive permutation; dir=1 if otherwise        
      end type quad
      
      type tet
      
      end type tet
      
      type wedge
      
      end type wedge
      
      type hex
      
      end type hex
      
      contains
      
      
      end module modgeo