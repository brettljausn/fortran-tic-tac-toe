
!     
! File:   tictactoe.f03
! Author: felix
!
! Created on 13. November 2018, 11:48
!



PROGRAM tictactoe
    USE output_module
    USE gamelogic_module
    IMPLICIT NONE
    CHARACTER, DIMENSION(3,3) :: field
    INTEGER :: x,y, gameMode, inputStatus
    REAL :: xOpponent, yOpponent


    ! make empty 3x3 field
    field = " "

    ! select game mode
    DO
        PRINT*, "1 player or 2 players? (1/2):"
        READ(*,*, iostat = inputStatus) gameMode
        IF (inputStatus == 0) THEN
            IF ((gameMode == 1) .OR. (gameMode == 2)) EXIT
        END IF
    END DO

    ! display empty field
    CALL printField(field)

    ! main game loop
    DO
        ! get valid player input
        DO
            PRINT*, "Player x - Enter coordinates (row, column):"
            READ(*,*, iostat = inputStatus) x, y

            IF (inputStatus == 0) THEN
                IF (field(x, y) == " ") THEN
                    EXIT
                END IF
            END IF
        END DO

        field(x,y) = "x"
        CALL checkField(field) ! check if x has won

        ! get valid move from player o (human or computer)
        IF (gameMode == 1) THEN
            CALL calculateMove(xOpponent, yOpponent, field)
        ELSE
            CALL printField(field)
            DO
                PRINT*, "Player o - Enter coordinates (row, column):"
                READ(*,*, iostat = inputStatus) xOpponent, yOpponent

                IF (inputStatus == 0) THEN
                    IF (field(INT(xOpponent), INT(yOpponent)) == " ") THEN
                        EXIT
                    END IF
                END IF
            END DO
        END IF

        field(INT(xOpponent), INT(yOpponent)) = "o"

        CALL checkField(field) ! check for victory
        CALL printField(field) ! display field


    END DO

END PROGRAM tictactoe

