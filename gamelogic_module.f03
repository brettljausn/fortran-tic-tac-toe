!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

MODULE gamelogic_module
    USE output_module
    IMPLICIT NONE
    SAVE
    CONTAINS
    
    SUBROUTINE checkField(field)
        IMPLICIT NONE
        CHARACTER, DIMENSION(3,3), INTENT(IN) :: field
        CHARACTER, DIMENSION(3) :: toCheck
        INTEGER :: i



        DO i = 1,3
            toCheck = field(i,:)

            IF (.NOT. ANY(toCheck == " ")) THEN
                IF (ALL(toCheck == toCheck(1))) THEN
                    CALL printField(field)
                    PRINT*, ""
                    PRINT*, "! --- ", toCheck(1), " is the Winner! --- !"
                    CALL EXIT()
                END IF
            END IF
        END DO

        DO i = 1,3
            toCheck = field(:,i)

            IF (.NOT. ANY(toCheck == " ")) THEN
                IF (ALL(toCheck == toCheck(1))) THEN
                    CALL printField(field)
                    PRINT*, ""
                    PRINT*, "! --- ", toCheck(1), " is the Winner! --- !"
                    CALL EXIT()
                END IF
            END IF
        END DO

        toCheck(1) = field(1,1)
        toCheck(2) = field(2,2)
        toCheck(3) = field(3,3)

        IF (.NOT. ANY(toCheck == " ")) THEN
            IF (ALL(toCheck == toCheck(1))) THEN
                CALL printField(field)
                PRINT*, ""
                PRINT*, "! --- ", toCheck(1), " is the Winner! --- !"
                CALL EXIT()
            END IF
        END IF

        toCheck(1) = field(1,3)
        toCheck(3) = field(3,1)

        IF (.NOT. ANY(toCheck == " ")) THEN
            IF (ALL(toCheck == toCheck(1))) THEN
                CALL printField(field)
                PRINT*, ""
                PRINT*, "! --- ", toCheck(1), " is the Winner! --- !"
                CALL EXIT()
            END IF
        END IF

            IF (.NOT. ANY( field==" " )) THEN
            CALL printField(field)
            PRINT*, "! --- DRAW! --- !"
            CALL EXIT()
        END IF
    END SUBROUTINE checkField

! calculate which move the computer opponent should make
    SUBROUTINE calculateMove(xOpponent, yOpponent, field)
        IMPLICIT NONE
        REAL, INTENT(INOUT) :: xOpponent, yOpponent
        CHARACTER, DIMENSION(3,3), INTENT(IN) :: field
        CHARACTER, DIMENSION(3) :: toCheck
        INTEGER :: i, j

        ! check for victory conditions in rows
        DO i = 1,3
            toCheck = field(i,:)
            IF((COUNT(toCheck == "o") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
                DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        xOpponent = i
                        yOpponent = j
                        RETURN
                    END IF
                END DO
            END IF
        END DO

        ! check for victory conditions in columns
        DO i = 1,3
            toCheck = field(:,i)
            IF((COUNT(toCheck == "o") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
                DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        xOpponent = j
                        yOpponent = i
                        RETURN
                    END IF
                END DO
            END IF
        END DO

        ! check for victory conditions in first diagonal
        toCheck(1) = field(1,1)
        toCheck(2) = field(2,2)
        toCheck(3) = field(3,3)

        IF((COUNT(toCheck == "o") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
            DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        xOpponent = j
                        yOpponent = j
                        RETURN
                    END IF
                END DO
        END IF

        ! check for victory conditions in second diagonal
        toCheck(1) = field(1,3)
        toCheck(3) = field(3,1)

        IF((COUNT(toCheck == "o") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
            DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        IF (j == 1) THEN
                            xOpponent = 1
                            yOpponent = 3
                        ELSE IF (j == 3) THEN
                            xOpponent = 3
                            yOpponent = 1
                        ELSE
                            xOpponent = j
                            yOpponent = j
                        END IF
                        RETURN
                    END IF
                END DO
        END IF

        ! block players victory in row
        DO i = 1,3
            toCheck = field(i,:)
            IF((COUNT(toCheck == "x") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
                DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        xOpponent = i
                        yOpponent = j
                        RETURN
                    END IF
                END DO
            END IF
        END DO

        ! block players victory in column
        DO i = 1,3
            toCheck = field(:,i)
            IF((COUNT(toCheck == "x") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
                DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        xOpponent = j
                        yOpponent = i
                        RETURN
                    END IF
                END DO
            END IF
        END DO

        ! block players victory in first diagonal
        toCheck(1) = field(1,1)
        toCheck(2) = field(2,2)
        toCheck(3) = field(3,3)

        IF((COUNT(toCheck == "x") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
            DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        xOpponent = j
                        yOpponent = j
                        RETURN
                    END IF
                END DO
        END IF

        ! block players victory in second diagonal
        toCheck(1) = field(1,3)
        toCheck(3) = field(3,1)

        IF((COUNT(toCheck == "x") == 2) .AND. (COUNT(toCheck == " ") == 1)) THEN
            DO j = 1,3
                    IF (toCheck(j) == " ") THEN
                        IF (j == 1) THEN
                            xOpponent = 1
                            yOpponent = 3
                        ELSE IF (j == 3) THEN
                            xOpponent = 3
                            yOpponent = 1
                        ELSE
                            xOpponent = j
                            yOpponent = j
                        END IF
                        RETURN
                    END IF
                END DO
        END IF

        ! make random move
        DO
            CALL RANDOM_NUMBER(xOpponent)
            CALL RANDOM_NUMBER(yOpponent)
            xOpponent = 1 + FLOOR(3*xOpponent)
            yOpponent = 1 + FLOOR(3*yOpponent)

            IF (field(INT(xOpponent), INT(yOpponent)) == " ") THEN
                EXIT
            END IF

        END DO

    END SUBROUTINE calculateMove


    
END MODULE gamelogic_module