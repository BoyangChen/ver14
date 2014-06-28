       module parameter_module ! useful constants for all subroutines & all elements
        
        implicit none
        
        integer, parameter :: dp=selected_real_kind(14) !-equivalent to f77 double precision
        
        integer, parameter :: msg_file=6 ! I/O file index for message file
        
        integer, parameter :: matnamelength=10 ! character length of a material name
        
        real(kind=dp), parameter :: zero=0._dp,one=1._dp,two=2._dp, &
      three=3._dp,four=4._dp,five=5._dp,six=6._dp,seven=7._dp, &
      eight=8._dp,nine=9._dp,ten=10._dp, &
      half=0.5_dp,quarter=0.25_dp,one_third=one/three, &
      two_third=two/three,one_sixth=one/six, &
      halfcirc=180._dp,pi=3.14159265359_dp,ninety=90._dp &
      ,tolerance=0.1_dp
     
       end module parameter_module
