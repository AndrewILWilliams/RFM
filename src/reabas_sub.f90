MODULE REABAS_SUB
CONTAINS
SUBROUTINE REABAS ( LUNHIT, FAIL, ERRMSG ) 
!
! VERSION
!   29JAN20 AD Original.
!
! DESCRIPTION
!   Read record from HITRAN RFM-basic line data file
!   Called by HITREC
!   Slightly more complicated treatment required to handle missing data strings
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE HITCOM_DAT ! HITRAN line data
    USE PHYCON_DAT, ONLY: AVOG   ! Avogradro's number [kmol/cm2]
!
  IMPLICIT NONE
!
! ARGUMENTS      
    INTEGER(I4),   INTENT(IN)  :: LUNHIT ! Logical unit number of HITRAN file
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    INTEGER(I4)    :: IOSVAL  ! Saved value of IOSTAT for error messages
    REAL(R8)       :: DSTR    ! Line strength allowing for < 1.0E-38
    CHARACTER(123) :: RECINP  ! Input record read as text
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Read record as a text string
    READ ( LUNHIT, '(A)', IOSTAT=IOSVAL, ERR=900 ) RECINP
    READ ( RECINP(1:12), '(F12.6)', IOSTAT=IOSVAL, ERR=900 ) HIT%WNO
    READ ( RECINP(14:15), '(I2)', IOSTAT=IOSVAL, ERR=900 )   HIT%IDM
    READ ( RECINP(17:18), '(I2)', IOSTAT=IOSVAL, ERR=900 )   HIT%IDI
! CO2 Isotope#10 returned as 0, although Iso#11 and 12 OK
    IF ( HIT%IDI .EQ. 0 ) HIT%IDI = 10
! To avoid underflow problems, STR read as double precision and scaled by
! Avogadro's number before converting to single precision
    READ ( RECINP(21:29), '(E10.3)', IOSTAT=IOSVAL, ERR=900 ) DSTR
    HIT%STR = SNGL ( DSTR * AVOG )
    READ ( RECINP(31:40), '(F10.4)', IOSTAT=IOSVAL, ERR=900 ) HIT%ELS
    READ ( RECINP(42:47), '(F6.4)', IOSTAT=IOSVAL, ERR=900 )  HIT%HWA
    IF ( RECINP(49:53) .EQ. '#####' ) THEN
      HIT%HWS = 0.0
    ELSE
      READ ( RECINP(49:53), '(F5.3)', IOSTAT=IOSVAL, ERR=900 )  HIT%HWS
    END IF
    READ ( RECINP(55:61), '(F7.4)', IOSTAT=IOSVAL, ERR=900 )  HIT%TCA
    IF ( RECINP(63:69) .EQ. '#######' ) THEN
      HIT%TCS = 0.0
    ELSE
      READ ( RECINP(63:69), '(F7.4)', IOSTAT=IOSVAL, ERR=900 ) HIT%TCS
    END IF
    READ ( RECINP(71:79), '(F9.6)', IOSTAT=IOSVAL, ERR=900 ) HIT%PSA
    IF ( RECINP(81:89) .EQ. '#########' ) THEN
      HIT%PSS = 0.0
    ELSE
      READ ( RECINP(81:89), '(F9.6)', IOSTAT=IOSVAL, ERR=900 ) HIT%PSS
    END IF
    IF ( RECINP(91:100) .EQ. '##########' ) THEN
      HIT%LMA = 0.0
    ELSE
      READ ( RECINP(91:100), '(E10.3)', IOSTAT=IOSVAL, ERR=900 ) HIT%LMA
    END IF
    IF ( RECINP(102:111) .EQ. '##########' ) THEN
      HIT%LMS = 0.0
    ELSE
      READ ( RECINP(102:111), '(E10.3)', IOSTAT=IOSVAL, ERR=900 ) HIT%LMS
    END IF
!
900 CONTINUE
  FAIL = IOSVAL .NE. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-REABAS: Failed to read HITRAN File. IOSTAT=', IOSVAL
  if ( fail ) write (*,*) recinp
!
END SUBROUTINE REABAS
END MODULE REABAS_SUB

