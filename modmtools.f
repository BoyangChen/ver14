      module modmtools
      use modparam
      implicit none

      contains

      subroutine klinecross(x1,y1,x2,y2,xp1,yp1,xp2,yp2,
     & iscross,xct,yct) !checks if two lines cross, and locate the crossing point

      
      ! feed in variables
      real(kind=dp),intent(in) :: x1,y1,x2,y2,xp1,yp1,xp2,yp2
      ! update variables
      real(kind=dp),intent(inout) :: xct,yct
      integer,intent(inout) :: iscross     
      ! local variables
      real(kind=dp) :: a1,b1,c1,a2,b2,c2,det
      real(kind=dp) :: xmin,ymin,xpmin,ypmin,xmax,ymax,xpmax,ypmax
      ! initialize local/update variables (don't use data in subroutines)     
      a1=zero;b1=zero;c1=zero
      a2=zero;b2=zero;c2=zero
      xmin=zero;ymin=zero;xmax=zero;ymax=zero
      xpmin=zero;ypmin=zero;xpmax=zero;ypmax=zero
c
c     **************** algorithm for the determination of precrack intersecting with element edges: *****************
c     equation of a line: a*x+b*y=c
c     the intersection of line 1 (an edge) and line 2 (a precrack) is to solve the system equation:
c     a1*x+b1*y=c1
c     a2*x+b2*y=c2
c     first, det=a1*b2-a2*b1; 
c     if det=0: lines are parallel, no intersection possible
c     else if det/=0, then the intersection point is:
c       xct=(b2*c1-b1*c2)/det
c       yct=(a1*c2-a2*c1)/det
c       check xct, yct with range of x,y of the edge and of the precrack
c       if (xmin<xct<xmax) or (ymin<yct<ymax): point on edge
c       if (xpmin<xct<xpmax) or (ypmin<yct<ypmax): point on precrack
c       if the point is both on edge and on precrack, then store the coords of this point
c           xp(edge no.)=xct
c           yp(edge no.)=yct
c           fedg(edge no.)=1 !edge crossed by precrack
c      
      ! line 1 equation
              a1=y2-y1
              b1=x1-x2
              c1=a1*x1+b1*y1
      ! line 2 equation
              a2=yp2-yp1
              b2=xp1-xp2
              c2=a2*xp1+b2*yp1
      ! range of line 1
              xmin=min(x1,x2)
              ymin=min(y1,y2)
              xmax=max(x1,x2)
              ymax=max(y1,y2)
      ! range of line 2
              xpmin=min(xp1,xp2)
              ypmin=min(yp1,yp2)
              xpmax=max(xp1,xp2)
              ypmax=max(yp1,yp2)
              ! check intersection
              det=a1*b2-a2*b1
              if(abs(det).eq.zero) then ! lines are parallel; no intersection possible
                ! do nothing
                
              else ! intersection possible
              
                xct=(b2*c1-b1*c2)/det !-find intersection point
                yct=(a1*c2-a2*c1)/det

                if ((xmin.gt.xct).or.(xmax.lt.xct) .or. 
     & (ymin.gt.yct).or.(ymax.lt.yct)) then ! intersection not on edge
                ! do nothing
                  !write(6,*)'intersection out of range of edge'
                else if (xct.eq.x1.and.yct.eq.y1) then
                    !write(6,*)'precrack line passes node 1'
                    xct=max(xct,xmin+tolerance*(xmax-xmin))
                    xct=min(xct,xmax-tolerance*(xmax-xmin))
                    yct=max(yct,ymin+tolerance*(ymax-ymin))
                    yct=min(yct,ymax-tolerance*(ymax-ymin))
                    if ((xpmin.gt.xct).or.(xpmax.lt.xct) .or. 
     &              (ypmin.gt.yct).or.(ypmax.lt.yct)) then ! intersection not on precrack                    
                        iscross=21
                    else
                        iscross=11
                    end if
                else if (xct.eq.x2.and.yct.eq.y2) then
                    !write(6,*)'precrack line passes node 2'
                    xct=max(xct,xmin+tolerance*(xmax-xmin))
                    xct=min(xct,xmax-tolerance*(xmax-xmin))
                    yct=max(yct,ymin+tolerance*(ymax-ymin))
                    yct=min(yct,ymax-tolerance*(ymax-ymin))
                    if ((xpmin.gt.xct).or.(xpmax.lt.xct) .or. 
     &              (ypmin.gt.yct).or.(ypmax.lt.yct)) then ! intersection not on precrack                    
                        iscross=22
                    else
                        iscross=12
                    end if
                else !-intersection on this edge
                    xct=max(xct,xmin+tolerance*(xmax-xmin))
                    xct=min(xct,xmax-tolerance*(xmax-xmin))
                    yct=max(yct,ymin+tolerance*(ymax-ymin))
                    yct=min(yct,ymax-tolerance*(ymax-ymin))
                    if ((xpmin.gt.xct).or.(xpmax.lt.xct) .or. 
     &              (ypmin.gt.yct).or.(ypmax.lt.yct)) then ! intersection not on precrack                    
                        iscross=2
                    else
                        iscross=1
                    end if
                end if
              end if              
      end subroutine klinecross    

      end module modmtools
