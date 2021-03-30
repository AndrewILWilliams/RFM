MODULE DRVHIT_SUB
CONTAINS
SUBROUTINE DRVHIT ( LUNDRV, FAIL, ERRMSG )
!
! VERSION
!   29JAN20 AD Use HITTYP, TYPCHK to identify type of HITRAN file
!   01MAY17 AD F90 conversion. Tested.
!
! DESCRIPTION
!   Read RFM driver table *HIT section
!   Called by RFMDRV once if *HIT section found in driver table.
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE LENREC_DAT ! Max length of input text record
!
! SUBROUTINES
    USE ENDCHK_SUB ! Check end of Driver Table section has been reached
    USE HITCHK_SUB ! Check HITRAN binary line data file contents
    USE HITTYP_SUB ! Identify type of HITRAN data file
    USE NXTFLD_SUB ! Load next field from section of driver file
    USE OPNHIT_SUB ! Open HITRAN binary line data file and read header
    USE TYPCHK_SUB ! Check HITRAN file consistent with RFM user options
!
  IMPLICIT NONE
!
! ARGUMENTS
    INTEGER(I4),   INTENT(IN)  :: LUNDRV ! LUN for Driver File
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)       :: LENGTH ! Length of field in Driver Table
    CHARACTER(LENREC) :: NAMHIT ! Name of HITRAN file 
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Read name of HITRAN binary file
  CALL NXTFLD ( LUNDRV, NAMHIT, LENGTH, FAIL, ERRMSG )
  IF ( FAIL ) RETURN
  IF ( LENGTH .EQ. 0 ) THEN
    FAIL = .TRUE.
    ERRMSG = 'F-DRVHIT: No filename supplied in *HIT section'
    RETURN 
  END IF
!
! Establish type of HITRAN data - set in HFLCOM
  CALL HITTYP ( NAMHIT(1:LENGTH), FAIL, ERRMSG )
  IF ( FAIL ) RETURN
!
! Check HITRAN data type consistent with user options
  CALL TYPCHK ( FAIL, ERRMSG )
  IF ( FAIL ) RETURN

! Open HITRAN file
  CALL OPNHIT ( NAMHIT(1:LENGTH), FAIL, ERRMSG ) 
  IF ( FAIL ) RETURN
!
! Check spectral range and contents of HITRAN file (closes file if not required)
  CALL HITCHK
!
! Check no more fields in this section (only one allowed, ie filename)
  CALL ENDCHK ( LUNDRV, '*HIT', FAIL, ERRMSG )
!
END SUBROUTINE DRVHIT
END MODULE DRVHIT_SUB
