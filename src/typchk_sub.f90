MODULE TYPCHK_SUB
CONTAINS
SUBROUTINE TYPCHK ( FAIL, ERRMSG )
!
! VERSION
!   19FEB20 AD Original.
!
! DESCRIPTION
!   Check HITRAN file consistent with RFM user options
!   Called by DRVHIT.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE QALCOM_DAT ! Band/isotope selection qualifiers
    USE FLGCOM_DAT, ONLY: MIXFLG, NTEFLG ! Flags for line mixing and non-LTE
!
! SUBROUTINES
!
  IMPLICIT NONE
!
! ARGUMENTS
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL VARIABLES
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Certain options rely on encoded global/local quantum numbers created by
! HITBIN program so not available in original HITRAN
  FAIL = .TRUE.
  IF ( MIXFLG ) THEN
    IF ( PARHFL .AND. .NOT. BINHFL ) THEN
      ERRMSG = 'F-TYPCHK: MIX flag not compatible with HITRAN .par file'
      RETURN
    END IF
  ELSE IF ( NTEFLG ) THEN
    IF ( .NOT. ( PARHFL .AND. BINHFL ) ) THEN
      ERRMSG = 'F-TYPCHK: NTE flag requires binary HITRAN .par file'
      RETURN
    END IF
  ELSE IF ( NQAL .GT. 0 ) THEN
    IF ( ANY ( QAL%ILS .NE. 0 ) .OR. ANY ( QAL%IUS .NE. 0 ) ) THEN
      IF ( .NOT. ( PARHFL .AND. BINHFL ) ) THEN
        ERRMSG = 'F-TYPCHK: Vib level selection requires binary HITRAN .par file'
        RETURN
      END IF
    END IF
  END IF
!
  FAIL = .FALSE.
!
END SUBROUTINE TYPCHK
END MODULE TYPCHK_SUB
