MODULE INIHFL_SUB
CONTAINS
SUBROUTINE INIHFL ( WNOREQ, FAIL, ERRMSG )
!
! VERSION
!   29JAN20 AD Major revision.
!   13JUN17 AD Add INIPAR
!   01MAY17 AD F90 conversion. Checked.
!
! DESCRIPTION    
!   Initialise the HITRAN line data file
!   Called by SPCWID and SPCFIN once for each spectral range.
!   Set the forward pointers for each required gas
!   and set current record to required wavenumber
!
! VARIABLE KINDS
    USE KIND_DAT  
!
! GLOBAL DATA
    USE HFLCOM_DAT ! HITRAN file data
    USE HITCOM_DAT ! HITRAN line data
    USE RFMLUN_DAT, ONLY: LUNHIT ! LUN for HITRAN binary file
!
! SUBROUTINES
    USE HITREC_SUB
!
  IMPLICIT NONE
!
! ARGUMENTS      
    REAL(R8),      INTENT(IN)  :: WNOREQ ! Initial wavenumber
    LOGICAL,       INTENT(OUT) :: FAIL   ! Set TRUE if a fatal error is detected
    CHARACTER(80), INTENT(OUT) :: ERRMSG ! Error message written if FAIL is TRUE
!
! LOCAL VARIABLES
    LOGICAL      :: EOF        ! T=End-of-file reache
    LOGICAL      :: LBLOCK     ! T=within block of forward pointer records
    INTEGER(I4)  :: I          ! Dummy read
    INTEGER(I4)  :: IDXMOL     ! HITRAN gas ID
    INTEGER(I4)  :: IOS        ! Value of IOSTAT for error messages
    INTEGER(I4)  :: IPTOFF     ! Offset for blocks of forward pointers
    INTEGER(I4)  :: IPTR       ! Counter for forward pointers
    INTEGER(I4)  :: IRCSTA     ! Record# with Wno at low end of required range
    INTEGER(I4)  :: IREC       ! Record#
    INTEGER(I4)  :: K,L        ! Record#
    INTEGER(I4)  :: LSTAT      ! Status of transition information.
    REAL(R8)     :: DDUMMY     ! Dummy variable for read
    REAL(R8)     :: WNUM       ! Wavenumber of HITRAN record
    CHARACTER(1) :: CDUMMY     ! Dummy character
    INTEGER(I4)  :: FPTEMP(MAXPTR) = 0 ! Forward pointers from HITRAN file
!
! EXECUTABLE CODE -------------------------------------------------------------
!
  FAIL = .FALSE.
  WNOHFL = 0.0D0         ! Set when actual data is read in by REACYC

! For ASCII files just rewind and advance until first record is found
  IF ( .NOT. BINHFL ) THEN
    REWIND ( LUNHIT, IOSTAT=IOS, ERR=900 ) 
    IRCHFL = 1           ! Point to Rec# about to be read
! RFM-basic file has a header record
    IF ( BASHFL ) THEN
      READ ( LUNHIT, '(A)', IOSTAT=IOS, ERR=900 ) CDUMMY  ! skip header
      IRCHFL = IRCHFL + 1
    END IF
    DO      
      IF ( PARHFL ) THEN
        READ ( LUNHIT, '(3X,F12.6)', IOSTAT=IOS, ERR=900 ) WNUM
      ELSE IF ( BASHFL ) THEN
        READ ( LUNHIT, '(F12.6)', IOSTAT=IOS, ERR=900 ) WNUM
      END IF 
      IRCHFL = IRCHFL + 1
      WNOHFL = WNUM
      IF ( WNUM .GT. WNOREQ ) EXIT
    END DO
    BACKSPACE ( LUNHIT, IOSTAT=IOS, ERR=900 ) 
    IRCHFL = IRCHFL-1
    RETURN
  END IF
!       
! Binary search for 1st record: follows code in the IBM SLUP routine:
! K is first record, L is last. Only need to read WNUM for search
  K = IR1HFL
  L = IR2HFL
  DO WHILE ( K + 1 .LT. L )       
    IREC = (K + L) / 2
    IF ( PARHFL ) THEN
      READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) I, I, I, WNUM
    ELSE
      READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) I, WNUM
    END IF     
    IF ( WNUM .LT. WNOREQ ) THEN   ! Reqd WN higher, try half way up to L
      K = IREC 
    ELSE                     ! Reqd WN lower or =, try half way down to K
      L = IREC 
    ENDIF
  END DO
!
! If K & L differ by 1, have found required location where the K .LT. DWNLOW 
! and L .GE. DWNLOW, Choose record L (note K<L). If there is more than 1 
! record at exactly DWNLOW, will finish pointing to first. 
! Forward pointer block are labelled with wavenumber of next line to exploit 
! this. 
!
  IRCHFL = L
  IRCSTA = IRCHFL   ! save starting record
!
! Now set the initial forward pointers for each gas
! First determine which HITRAN Gas IDs are required from this file
! (IDG < MAXPTR for all reqd line molecules checked earlier in HITFIL)
  IFPHFL = IR2HFL ! initialise all fwd pointers to end-of-file
!
! Step back to start of previous forward pointer block
  IREC = IRCHFL
  LBLOCK = .FALSE.
  DO
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) LSTAT
    IF ( LBLOCK .AND. LSTAT .NE. LSTFWD ) EXIT ! gone past start of block
    LBLOCK = LSTAT .EQ. LSTFWD                 ! inside fwd ptr block
    IREC = IREC - 1
  END DO
  IREC = IREC + 1
! Now positioned at start of forward pointer block
  IPTOFF = 0
  DO 
    READ ( LUNHIT, REC=IREC, IOSTAT=IOS, ERR=900 ) LSTAT
    IF ( LSTAT .NE. LSTFWD ) EXIT   ! Reached end of fwd ptr block
    IF ( PARHFL ) THEN
      READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) &
        LSTAT, I, I, DDUMMY, ( FPTEMP(IPTR), IPTR = IPTOFF+1, IPTOFF+NFPHFL)
    ELSE
      READ ( LUNHIT, REC=IREC, ERR=900, IOSTAT=IOS ) &
        LSTAT, DDUMMY, ( FPTEMP(IPTR), IPTR = IPTOFF+1, IPTOFF+NFPHFL )
    END IF
! Set forward pointers only for required molecules (others set=last rec in file)
    DO IDXMOL = IPTOFF+1, IPTOFF+NFPHFL
      IF ( USEIDG(IDXMOL) ) IFPHFL(IDXMOL) = IREC + FPTEMP(IDXMOL)
    END DO
    IPTOFF = IPTOFF + NFPHFL
    IREC = IREC + 1
  END DO
  IRCHFL = IREC
! Now advance through file back to original starting point using HITREC to
! automatically update fwd ptrs for the required molecules
  DO 
    IRCHFL = MINVAL ( IFPHFL )
    IF ( IRCHFL .GE. IRCSTA ) EXIT 
    CALL HITREC ( EOF, FAIL, ERRMSG ) 
    IF ( FAIL ) RETURN
    IF ( EOF ) STOP 'F-INIHFL: Logical error'
  END DO  
!
! Copy information on input file format which shows which HIT variables are set
  PARHIT = PARHFL
  BASHIT = BASHFL
!
! Ensure 'optional' HITRAN parameters are defined 
  HIT%PSA = 0.0
  HIT%PSS = 0.0
  HIT%TCS = 0.0
  HIT%LMA = 0.0
  HIT%LMS = 0.0
!
! Also use this opportunity to initialise numbers which will then remain zero
! if LTE calculation being used
  HIT%IUV = 0
  HIT%ILV = 0
!
900 CONTINUE
  FAIL = IOS .NE. 0
  IF ( FAIL ) WRITE ( ERRMSG, * ) &
    'F-INIHFL: Failed to read HITRAN file, rec#:', IREC, '. IOSTAT=', IOS
!
END SUBROUTINE INIHFL
END MODULE INIHFL_SUB
