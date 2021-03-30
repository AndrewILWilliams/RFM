MODULE OPNHIT_SUB
CONTAINS
SUBROUTINE OPNHIT ( NAMHIT, FAIL, ERRMSG )
!
! VERSION
!   29JAN20 AD Open all forms of HITRAN data file
!   12JUN17 AD Allow for ASCII files. Checked.
!   O1MAY17 AD F90 conversion. 
!
! DESCRIPTION
!   Open HITRAN line data file and read header
!   Called once by DRVHIT
!
! VARIABLE KINDS
    USE KIND_DAT
!
! GLOBAL DATA
    USE GASCOM_DAT ! Molecule and isotope data
    USE HFLCOM_DAT ! HITRAN file data
    USE RFMCON_DAT, ONLY: FWIND  ! Window [cm-1] for widemesh calc
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN file
    USE SPCCOM_DAT, ONLY: WMXSPC ! Upper Wavenumber reqd for any range
!
! SUBROUTINES
    USE C9REAL_GEN ! Write real number as C*9 string
    USE C11INT_FNC ! Write integer as left-adjusted string
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
    INTEGER(I4), PARAMETER :: BASLEN(17) = 0 ! Dummy array to find RECL
    INTEGER(I4), PARAMETER :: PARLEN(22) = 0 ! Dummy array to find RECL
!
! LOCAL VARIABLES
    INTEGER(I4)   :: IDUM   ! Dummy integer(i4) for read
    INTEGER(I4)   :: IDXISO ! HITRAN index of isotope
    INTEGER(I4)   :: IDXMOL ! HITRAN index of molecule
    INTEGER(I4)   :: IFMT   ! Binary file format identifier
    INTEGER(I4)   :: IGAS   ! Counter for required molecules
    INTEGER(I4)   :: IOS    ! Saved value of IOSTAT for error messages
    INTEGER(I4)   :: IPTOFF = 0 ! Offset for reading forward pointers
    INTEGER(I4)   :: IPTR   ! Pointer counter 
    INTEGER(I4)   :: IREC   ! Record counter
    INTEGER(I4)   :: LSTAT  ! Used to identify HITRAN Fwd.Pointer block
    INTEGER(I4)   :: RECLEN ! RECL parameter for opening file (22 or 88)
    REAL(R8)      :: DDUM   ! Dummy R*8
    CHARACTER(1)  :: CDUM   ! Dummy character
    CHARACTER(48) :: LABEL  ! Label read from HITRAN file
    CHARACTER(80) :: LOGMSG ! Message sent to log file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
! Send message to LOG file saying which file is about to be opened
  CALL WRTLOG ( 'I-OPNHIT: Opening HITRAN File: '//NAMHIT )
!
! Open file
  IF ( BINHFL ) THEN
    IF ( PARHFL ) INQUIRE ( IOLENGTH=RECLEN ) PARLEN
    IF ( BASHFL ) INQUIRE ( IOLENGTH=RECLEN ) BASLEN
! 
    LOGMSG = 'I-OPNHIT: Opening as binary file using RECL=' // C11INT(RECLEN)
    CALL WRTLOG ( LOGMSG )
!
    OPEN ( UNIT=LUNHIT, FILE=NAMHIT, STATUS='OLD', ACTION='READ', &
           ACCESS='DIRECT', RECL=RECLEN, IOSTAT=IOS )
  ELSE 
    OPEN ( UNIT=LUNHIT, FILE=NAMHIT, STATUS='OLD', ACTION='READ', IOSTAT=IOS )
  END IF
!
  IF ( IOS .NE. 0 ) THEN
    FAIL = .TRUE.
    WRITE ( ERRMSG, * ) 'F-OPNHIT: Open failure on HITRAN file. IOSTAT=', IOS
    RETURN
  END IF
  OPNHFL = .TRUE.
!
  IF ( BINHFL ) THEN
! Read header and extract first/last record#
    READ ( LUNHIT, REC=1, IOSTAT=IOS, ERR=900 ) &
      LSTAT, IFMT, IR1HFL, IR2HFL, LABEL
    CALL WRTLOG ( 'I-OPNHIT: HITRAN File Label='//LABEL )
!
! Find wavenumber range by reading first,last records
    IF ( PARHFL ) THEN 
      READ ( LUNHIT, REC=IR1HFL, IOSTAT=IOS, ERR=900 ) LSTAT, IDUM, IDUM, WNLHFL
      READ ( LUNHIT, REC=IR2HFL, IOSTAT=IOS, ERR=900 ) LSTAT, IDUM, IDUM, WNUHFL
    ELSE IF ( BASHFL ) THEN
      READ ( LUNHIT, REC=IR1HFL, IOSTAT=IOS, ERR=900 ) LSTAT, WNLHFL
      READ ( LUNHIT, REC=IR2HFL, IOSTAT=IOS, ERR=900 ) LSTAT, WNUHFL
    ELSE
      STOP 'F-OPNHIT: Logical error'
    END IF
!
! Initialise pointers for all different line species to point to last
! record in file
    IFPHFL = 0
!
! Read Forward Pointers from 1st n records from start of file
    IREC = IR1HFL
    READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) LSTAT
    IF ( LSTAT .NE. LSTFWD ) THEN                 ! Check this is FP record
      ERRMSG = 'F-OPNHIT: Expected Fwd Ptr (LSTAT=-7) for Rec#' // &
               TRIM ( C11INT(IREC) ) // ', got LSTAT=' // C11INT(LSTAT)
      FAIL = .TRUE.
      RETURN
    END IF
!
    IPTOFF = 0
    DO 
      IF ( IPTOFF+NFPHFL .GT. MAXPTR ) THEN  ! expect NFPHFL=14 for now
        FAIL = .TRUE.
        ERRMSG = 'F-OPNHIT: No.forward ptrs in HITRAN file >MAXPTR=' // &
                 TRIM ( C11INT(MAXPTR) ) // ' in hflcom.f90' 
        RETURN
      END IF
      IF ( PARHFL ) THEN
        READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) &
          LSTAT, IDUM, IDUM, DDUM, ( IFPHFL(IPTR), IPTR=IPTOFF+1, IPTOFF+NFPHFL)
      ELSE IF ( BASHFL ) THEN
        READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) &
          LSTAT, DDUM, ( IFPHFL(IPTR), IPTR=IPTOFF+1, IPTOFF+NFPHFL)
      END IF
! Binary format has rel.forward pointer set to last rec in file for no data,
! but here use IFPHFL=0 as flag indicating no molec.data within file
      DO IPTR = IPTOFF+1, IPTOFF+NFPHFL
        IF ( IREC + IFPHFL(IPTR) .EQ. IR2HFL ) IFPHFL(IPTR) = 0
      END DO
      IPTOFF = IPTOFF + NFPHFL
      IREC = IREC + 1
      READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) LSTAT
      IF ( LSTAT .NE. LSTFWD ) EXIT
    END DO
!
  ELSE 
! ASCII file - no forward pointers
    IFPHFL = 0
! Flag all required molecules as -1 until first line found in HITRAN file
    DO IGAS = 1, NGAS
      IDXMOL = GAS(IGAS)%IDM
      IF ( IDXMOL .LE. MAXPTR ) IFPHFL(IDXMOL) = -1
    END DO
!
! Read wavenumber of first record
    IF ( BASHFL ) THEN
      READ ( LUNHIT, '(A)', IOSTAT=IOS, ERR=900 ) CDUM  ! Skip header record 
      READ ( LUNHIT, '(F12.6)', ERR=900, IOSTAT=IOS ) WNLHFL
      IR1HFL = 2
    ELSE 
      READ ( LUNHIT, '(3X,F12.6)', ERR=900, IOSTAT=IOS ) WNLHFL
      IR1HFL = 1
    END IF
    BACKSPACE ( LUNHIT, ERR=900, IOSTAT=IOS ) 
!
    WNUHFL = WNLHFL
    IRCHFL = IR1HFL
! Don't need to read to the end of the file, just to max required wavenumber
! However, keep going if a required molecule has not yet been found in file
    DO WHILE ( WNUHFL .LT. WMXSPC + FWIND .OR. ANY ( IFPHFL .EQ. -1 ) ) 
      IF ( BASHFL ) THEN
        READ ( LUNHIT, '(F12.6,2I3)', IOSTAT=IOS, END=800, ERR=900 ) &
          WNUHFL, IDXMOL, IDXISO
      ELSE  
        READ ( LUNHIT, '(I2,Z1,F12.6)', IOSTAT=IOS, END=800, ERR=900 ) &
          IDXMOL, IDXISO, WNUHFL
      END IF
      IF ( IDXMOL .LE. MAXPTR ) IFPHFL(IDXMOL) = IRCHFL
      IR2HFL = IRCHFL
      IRCHFL = IRCHFL + 1
    END DO
800 CONTINUE
! If EOF reached, then IR2HFL will still point to last data record in file
    CALL WRTLOG ( 'I-OPNHIT: HITRAN file has wavenumber range ' & 
                  // TRIM ( C9REAL(WNLHFL) ) // 'to >=' &
                  // TRIM ( C9REAL(WNUHFL) ) // ' cm-1' ) 
  END IF
! 
900 CONTINUE
  FAIL = IOS .GT. 0 
  IF ( FAIL ) &
    ERRMSG = 'F-OPNHIT: Read failure on HITRAN file. IOSTAT=' // C11INT(IOS)
!
END SUBROUTINE OPNHIT
END MODULE OPNHIT_SUB
