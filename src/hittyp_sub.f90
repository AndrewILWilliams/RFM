MODULE HITTYP_SUB
CONTAINS
SUBROUTINE HITTYP ( NAMHIT, FAIL, ERRMSG )
!
! VERSION
!   25APR20 AD Correction to reading data from .par file
!   29JAN20 AD Original.
!
! DESCRIPTION
!   Identify type of HITRAN data file
!   Called by DRVHIT
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE RFMLUN_DAT, ONLY: LUNTMP ! LUN for HITRAN file
!
! SUBROUTINES
    USE LEXIST_FNC ! Check if file exists
    USE WRTLOG_SUB ! Write text message to log file
!
  IMPLICIT NONE
!
! ARGUMENTS
    CHARACTER(*),  INTENT(IN)  :: NAMHIT ! Name of HITRAN file
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message if FAIL is TRUE
!
! LOCAL CONSTANTS
    INTEGER(I4),  PARAMETER :: I2DUM(2) = 0 ! Dummy array to find RECL
    CHARACTER(*), PARAMETER :: HDRBAS   = & ! RFM-basic header record
           'nu molec_id local_iso_id sw elower gamma_air gamma_self n_air ' &
        // 'n_self delta_air delta_self y_air y_self trans_id'
!
! LOCAL VARIABLES
    INTEGER(I4)    :: IFMT   ! Binary file format identifier
    INTEGER(I4)    :: IOS    ! Saved value of IOSTAT for error messages
    INTEGER(I4)    :: ISO    ! Isotope ID 
    INTEGER(I4)    :: LSTAT  ! Used to identify HITRAN Fwd.Pointer block
    INTEGER(I4)    :: MOL    ! Molecule ID
    INTEGER(I4)    :: RECLEN ! RECL parameter for opening file (2 or 8)
    REAL(R8)       :: WNO    ! Wavenumber [cm-1]
    CHARACTER(160) :: REC160 ! Record from ASCII file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Send message to LOG file saying which file is about to be opened
  CALL WRTLOG ( 'I-HITTYP: Opening HITRAN File: '//NAMHIT )
!
  IF ( .NOT. LEXIST ( NAMHIT ) ) THEN 
    FAIL = .TRUE.
    ERRMSG = 'F-HITTYP: file not found'
    RETURN
  END IF
! 
! Initially just require first two I4 integers from first record
  INQUIRE ( IOLENGTH=RECLEN ) I2DUM
!
! Start by assuming it is a direct-access (binary) file
  OPEN ( UNIT=LUNTMP, FILE=NAMHIT, STATUS='OLD', ACTION='READ', &
         ACCESS='DIRECT', RECL=RECLEN, IOSTAT=IOS )
  IF ( IOS .NE. 0 ) THEN
    FAIL = .TRUE.
    WRITE ( ERRMSG, * ) 'F-HITTYP: Open failure on HITRAN file. IOSTAT=', IOS
    RETURN
  END IF
!
! Read header and extract first/last record#
  READ ( LUNTMP, REC=1, IOSTAT=IOS, ERR=900 ) LSTAT, IFMT
! 
! old version of HITBIN had IFMT=56  
  IF ( IFMT .EQ. 1 .OR. IFMT .EQ. 56 ) THEN 
    PARHFL = .TRUE.
    BINHFL = .TRUE.
    NFPHFL = 14
  ELSE IF ( IFMT .EQ. 1001 ) THEN
    BASHFL = .TRUE.
    BINHFL = .TRUE.
    NFPHFL = 14
!  ELSE IF ( IFMT .EQ. 2001 ) THEN
!    HTPHFL = .TRUE.
!    BINHFL = .TRUE.
  END IF
!
  CLOSE ( LUNTMP ) 
  IF ( BINHFL ) THEN   ! Identified as a binary file
    IF ( PARHFL ) &
      CALL WRTLOG ( 'I-HITTYP: Identified as binary HITRAN .par file' )
    IF ( BASHFL ) &
      CALL WRTLOG ( 'I-HITTYP: Identified as binary RFM-basic file' )
    FAIL = .FALSE.
    RETURN
  END IF
!
! Next try opening as ASCII file
  OPEN ( UNIT=LUNTMP, FILE=NAMHIT, STATUS='OLD', ACTION='READ', IOSTAT=IOS )
  IF ( IOS .NE. 0 ) THEN
    FAIL = .TRUE.
    WRITE ( ERRMSG, * ) 'F-HITTYP: Open failure on HITRAN file. IOSTAT=', IOS
    RETURN
  END IF
! 
  READ ( LUNTMP, '(A)', IOSTAT=IOS, ERR=900 ) REC160  
  CLOSE ( LUNTMP ) 

  IF ( REC160 .EQ. HDRBAS ) THEN
    BASHFL = .TRUE.
    CALL WRTLOG ( 'I-HITTYP: Identified as text RFM-basic file' )
    FAIL = .FALSE.
    RETURN
  END IF
!
  READ ( REC160, '(I2,Z1,F12.6)', IOSTAT=IOS, ERR=900 ) MOL, ISO, WNO
  IF ( MOL .GT. 0 .AND. WNO .GT. 0.0D0 ) THEN
    PARHFL = .TRUE.
    CALL WRTLOG ( 'I-HITTYP: Identified as HITRAN 160-character .par file' )
    FAIL = .FALSE.
    RETURN
  END IF
!
  FAIL = .TRUE.
  ERRMSG = 'F-HITTYP: Unable to identify type of HITRAN file'
  RETURN
! 
900 CONTINUE
  FAIL = IOS .NE. 0 
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-HITTYP: Read failure on HITRAN file. IOSTAT=', IOS
!
END SUBROUTINE HITTYP
END MODULE HITTYP_SUB
