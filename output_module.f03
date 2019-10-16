!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

MODULE output_module
    IMPLICIT NONE
    SAVE
    CONTAINS

! print the gamefield in console
    SUBROUTINE printField(field)
        IMPLICIT NONE
        CHARACTER, DIMENSION(3,3), INTENT(IN) :: field

        PRINT*, ""
        PRINT*," ",field(1,1)," | ",field(1,2)," | ",field(1,3)," "
        PRINT*,"-----------"
        PRINT*," ",field(2,1)," | ",field(2,2)," | ",field(2,3)," "
        PRINT*,"-----------"
        PRINT*," ",field(3,1)," | ",field(3,2)," | ",field(3,3)," "
        PRINT*, ""

    END SUBROUTINE printField
END MODULE output_module
