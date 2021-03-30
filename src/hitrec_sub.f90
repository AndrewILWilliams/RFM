MODULE HITREC_SUB
CONTAINS
SUBROUTINE HITREC ( EOF, FAIL, ERRMSG ) 
!
! VERSION
!   29JAN20 AD Original.
!
! DESCRIPTION
!   Read record from HITRAN line data file
!   Called by INIHFL and REAHIT
!   For binary files, only expect this to be called to read records containing
!   one of the required molecules
!   For ASCII files this reads through successive records until a record with 
!   one of the required molecules is found
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE HITCOM_DAT ! HITRAN line data
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN file
    USE PHYCON_DAT, ONLY: AVOG   ! Avogradro's number [kmol/cm2]
!
! SUBROUTINES
    USE C11INT_FNC ! Write integer as left-adjusted string
    USE REABAS_SUB ! Read record from HITRAN RFM-basic line data file
!    
  IMPLICIT NONE
!
! ARGUMENTS      
    LOGICAL,       INTENT(OUT) :: EOF    ! Set TRUE if end-of-file reached
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL CONSTANTS
    INTEGER(I4), PARAMETER :: FIXIDI(0:15) = &
              (/ 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16 /)
!
! LOCAL VARIABLES
    INTEGER(I4)  :: IDI    ! Isotope ID read from .par file
    INTEGER(I4)  :: IOS    ! Saved value of IOSTAT for error messages
    INTEGER(I4)  :: LID    ! Transition identification number
    INTEGER(I4)  :: LSTAT  ! Status of transition information.
    REAL(R4)     :: TPROB  ! Transition probability [Debyes2].
    REAL(R8)     :: DSTR   ! Original HITRAN linestrength
    CHARACTER(9) :: SPARE9 ! Spare bytes in binary file record
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  IOS = 0
!
  IF ( BINHFL ) THEN
! Find next useful record in file. 
    DO 
      EOF = IRCHFL .EQ. IR2HFL
      IF ( EOF ) EXIT
!
      READ ( LUNHIT, REC=IRCHFL, IOSTAT=IOS, ERR=900 ) LSTAT
      IF ( LSTAT .EQ. LSTLIN ) THEN
        IF ( PARHFL ) THEN     ! Binary derived from HITRAN .par record
          READ ( LUNHIT, REC=IRCHFL, IOSTAT=IOS, ERR=900 ) &     ! Load /HITCOM/
            LSTAT, HIT%IDM, HIT%IDI, HIT%WNO, HIT%STR,  TPROB, & 
            HIT%HWA, HIT%HWS, HIT%ELS, HIT%TCA, HIT%PSA, HIT%IUS, HIT%ILS, &
            HIT%ULQ, HIT%BLQ, SPARE9, IFWDPT
        ELSE                   ! Binary form of RFM basic record
          READ ( LUNHIT, REC=IRCHFL, IOSTAT=IOS, ERR=900 ) &     ! Load /HITCOM/
            LSTAT, HIT%WNO, HIT%IDM, HIT%IDI, HIT%STR, HIT%ELS, &
            HIT%HWA, HIT%HWS, HIT%TCA, HIT%TCS, HIT%PSA, HIT%PSS, &
            HIT%LMA, HIT%LMS, LID, IFWDPT
        END IF
! Set forward pointer for the next call
        IF ( USEIDG(HIT%IDM) ) THEN
          IFPHFL(HIT%IDM) = IRCHFL + IFWDPT  
          EXIT
        END IF
        STOP 'F-HITREC: Logical error'
!
      ELSE IF ( LSTAT .EQ. LSTFWD ) THEN ! Skip forward pointer block
        STOP 'F-HITREC: Logical error#2'
        IRCHFL = IRCHFL + 1
        CYCLE
      END IF
    END DO
  ELSE
    DO         ! Continue reading until a record with a required molecule found
      IRCHFL = IRCHFL + 1
      EOF = IRCHFL .GT. IR2HFL
      IF ( EOF ) EXIT
      IF ( PARHFL ) THEN
        READ ( LUNHIT, 1001, IOSTAT=IOS, ERR=900 ) HIT%IDM, IDI, &
          HIT%WNO, DSTR, TPROB, HIT%HWA, HIT%HWS, HIT%ELS, HIT%TCA, HIT%PSA
1001 FORMAT ( I2, Z1, F12.6, F10.3, E10.3, F5.2, F5.2, F10.4, F4.1, F8.5 )
        HIT%STR = SNGL ( DSTR * AVOG ) 
        HIT%IDI = FIXIDI(IDI)    ! Convert to correct numbering
      ELSE IF ( BASHFL ) THEN
        CALL REABAS ( LUNHIT, FAIL, ERRMSG ) 
        IF ( FAIL .OR. EOF ) RETURN
      ELSE
        STOP 'F-HITREC: Logical error'
      END IF
      IF ( USEIDG(HIT%IDM) ) EXIT   ! found record with a required molecule
    END DO
  END IF
!
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) 'F-HITREC: Failed to read Rec#' // &
    TRIM(C11INT(IRCHFL)) // ' in HITRAN File. IOSTAT=', IOS
!
END SUBROUTINE HITREC
END MODULE HITREC_SUB
