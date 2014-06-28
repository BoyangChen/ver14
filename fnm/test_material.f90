! fortran test program

! include useful modules

        include 'parameter_module.f90'
        include 'material_module.f90'

        program test
        use parameter_module
        use material_module

        implicit none

        type(material_type) :: mat
        type(isotropic_type) :: iso
        type(lamina_type) :: lamina
        type(interlayer_type) :: interlayer


        integer :: i

        i=0

        call empty(mat)

        call update(iso,modulus=isotropic_modulus(E=70._dp,nu=0.2_dp))
        call update(lamina,modulus=lamina_modulus(E1=one,E2=three,nu12=zero,nu23=zero,G12=five,G23=four))
        call update(interlayer,modulus=interlayer_modulus(Dnn=one,Dtt=two,Dll=two))
        call update(mat,isotropic=iso,lamina=lamina,interlayer=interlayer)
        print*,'material modulus:', mat%isotropic(1)%modulus(1)
        print*,'material modulus:', mat%lamina(1)%modulus(1)
        print*,'material modulus:', mat%interlayer(1)%modulus(1)


        end program test
