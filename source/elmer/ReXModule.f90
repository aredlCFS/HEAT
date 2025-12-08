!-------------------------------------------------------------------------------
!> File: ReXModule.f90
!> Engineer: TL
!> Date: 20240201
!> 
!> Uses the JMAK equation to calculate the recrystallization fraction given
!> the temperature history.  We adapt the JMAK equation so that the time
!> variable is an effective time, related back to an empirical oven test,
!> rather than time itself.  This enables us to have a time varying temperature
!> history, and still use the Avrami coefficients derived at a reference
!> temperature.  
!>
!> Reads in the Temperature, and calculates the ReX fraction at each mesh node, n
!> requires the Avrami coefficient, k0, the material activation energy, E [eV/atom],
!> the Avrami exponent, n, and the reference temperature at which these coefficients
!> were derived, Tref.  The user should supply these values in the Material section
!> of the SIF, and the ReX calculation will be called once per timestep.  Example
!> SIF:
!>  Material 3
!>  Name = "W"
!>    ...other variables...
!>    ReX = Variable Temperature
!>    Real Procedure "ReXLibrary" "rexonnodes" End
!>    avrami_n = Real 1.0
!>    avrami_k0 = Real 4446601450.0
!>    avrami_E = Real 3.0
!>    avrami_Tref = Real 20.0
!>  End
!>
!>
!> Compile like this:  elmerf90 -o ReXLibrary.so ReXModule.f90
!> Compile like this for a single HEAT shared object:  
!>               elmerf90 -o HEATLibrary.so ReXModule.f90 HFReaderModule.f90
!> Build grid like this:  ElmerGrid 8 2 <name_of_part> -autoclean -relh 1.0
!> run like this: ElmerSolver case.sif
!-------------------------------------------------------------------------------
MODULE ReXModule
    USE Types
    USE DefUtils
    USE, INTRINSIC :: ieee_arithmetic
    IMPLICIT NONE

    PUBLIC :: ReXOnNodes
    PUBLIC :: WriteReX

    ! Global storage for recrystallization result
    REAL(KIND=8), SAVE, ALLOCATABLE :: ReXArray(:)

    !Interface required to pass function handle through to Elmer
    INTERFACE
        FUNCTION ReXOnNodes(Model, n, Temp) RESULT(ReX)
            USE DefUtils
            IMPLICIT NONE
            TYPE(Model_t) :: Model
            TYPE(Solver_t) :: Solver
            TYPE(Variable_t), POINTER :: TimeVar
            Character(LEN=100) :: nodalReXPrefix
            REAL(KIND=dp) :: Temp, kB, ReX, test
            REAL(KIND=dp) :: avrami_n, avrami_k0, avrami_E
            REAL(KIND=dp) :: c, dt, Time
            TYPE(ValueList_t), POINTER :: mat
            Logical :: GotIt
            Character(LEN=255) :: f, ff
            REAL(KIND=dp), ALLOCATABLE :: dataIntegral(:)
            REAL(KIND=dp), ALLOCATABLE :: dataTprev(:)
            REAL(KIND=dp), ALLOCATABLE :: dataTimeN(:)
            REAL(KIND=dp), ALLOCATABLE :: dataAvrami(:)
            INTEGER :: TotalNodes, n
            INTEGER :: numLines
            REAL, ALLOCATABLE :: data(:,:)

        END FUNCTION ReXOnNodes
        
        SUBROUTINE ReadCSV_ReX(filename, data, numLines)
            CHARACTER(LEN=255) :: filename
            REAL, ALLOCATABLE, INTENT(OUT) :: data(:,:)
            INTEGER, INTENT(OUT) :: numLines
            CHARACTER(LEN=200) :: line
            CHARACTER(LEN=20), DIMENSION(2) :: splitLine
            INTEGER, DIMENSION(2) :: splitPos
            INTEGER :: ioStat, fileUnit, i
            CHARACTER(len=255) :: cwd

        END SUBROUTINE ReadCSV_ReX

        SUBROUTINE SplitString_ReX(str, splitStr)
            CHARACTER(LEN=*), INTENT(IN) :: str
            CHARACTER(LEN=20), DIMENSION(2), INTENT(OUT) :: splitStr
            INTEGER :: endPos

        END SUBROUTINE SplitString_ReX

		SUBROUTINE WriteReX(Model, Solver, dt)
			USE DefUtils
			IMPLICIT NONE
			TYPE(Model_t), INTENT(IN) :: Model
			TYPE(Solver_t), INTENT(IN) :: Solver
			REAL(KIND=8), INTENT(IN) :: dt
			LOGICAL :: GotIt
			CHARACTER(LEN=255) :: filename_new
			INTEGER :: i, fu
		END SUBROUTINE

    END INTERFACE
END MODULE ReXModule

! Outside any "scope" the Functions Declared 
! in the Interface above must be implemented
FUNCTION ReXOnNodes(Model, n, Temp) RESULT(ReX)
    USE DefUtils
    USE ReXModule , except_this_one => ReXOnNodes
    USE, INTRINSIC :: ieee_arithmetic
    IMPLICIT NONE
    TYPE(Model_t) :: Model
    TYPE(Solver_t) :: Solver
    TYPE(Variable_t), POINTER :: TimeVar
    Character(LEN=100) :: nodalReXPrefix
    REAL(KIND=dp) :: Temp, kB, ReX, test
    REAL(KIND=dp) :: c, dt, Time
    REAL(KIND=dp) :: avrami_n, avrami_k0, avrami_E
    TYPE(ValueList_t), POINTER :: mat
    Logical :: GotIt
    Character(LEN=255) :: f, ff
    REAL(KIND=dp), SAVE, ALLOCATABLE :: dataIntegral(:)
    REAL(KIND=dp), SAVE, ALLOCATABLE :: dataTprev(:)
    REAL(KIND=dp), SAVE, ALLOCATABLE :: dataTimeN(:)
    REAL(KIND=dp), SAVE, ALLOCATABLE :: dataAvrami(:)
    INTEGER :: TotalNodes, n
    INTEGER :: numLines
    REAL, ALLOCATABLE :: data(:,:)
    REAL(KIND=dp), SAVE :: tRead = -1.0
	INTEGER, SAVE :: nodeCounter = 0

    !physics constants
    kB = 8.617333e-5 ! [eV/K]

    !Load the material and avrami values from SIF
    IF (.NOT. ALLOCATED(dataAvrami)) THEN
        mat => GetMaterial()
        !get avrami exponent
        avrami_n = getConstReal(mat, 'avrami_n', GotIt)
        IF(.NOT. GotIt) CALL Fatal('ReXOnNodes', 'could not read avrami parameters from SIF')
        IF ( .NOT. ASSOCIATED(mat) ) THEN
            CALL FATAL('ReXNodes','No ReX Solver found')
        END IF
        !get avrami coefficient
        avrami_k0 = getConstReal(mat, 'avrami_k0', GotIt)
        IF(.NOT. GotIt) CALL Fatal('ReXOnNodes', 'could not read avrami parameters from SIF')
        IF ( .NOT. ASSOCIATED(mat) ) THEN
            CALL FATAL('ReXNodes','No ReX Solver found')
        END IF
        !get avrami activation energy [eV/atom]
        avrami_E = getConstReal(mat, 'avrami_E', GotIt)
        IF(.NOT. GotIt) CALL Fatal('ReXOnNodes', 'could not read avrami parameters from SIF')
        IF ( .NOT. ASSOCIATED(mat) ) THEN
            CALL FATAL('ReXNodes','No ReX Solver found')
        END IF

        !JMAK equation coefficients
        ALLOCATE(dataAvrami(3))
        dataAvrami(1) = avrami_n
        dataAvrami(2) = avrami_k0
        dataAvrami(3) = avrami_E

    ELSE
        avrami_n = dataAvrami(1)
        avrami_k0 = dataAvrami(2)
        avrami_E = dataAvrami(3)
        
    END IF

    ! Allocate the ReX array
    IF (.NOT. ALLOCATED(dataIntegral)) THEN
        TotalNodes = Model % Mesh % NumberOfNodes
        print *, "Number of mesh nodes allocated for ReX:", TotalNodes
        ALLOCATE(dataIntegral(TotalNodes))
	END IF

	IF (.NOT. ALLOCATED(ReXArray)) THEN
        TotalNodes = Model % Mesh % NumberOfNodes
		ALLOCATE(ReXArray(TotalNodes))
        !get ReX init file name
        mat => GetMaterial()
        nodalReXPrefix = getString(mat, 'nodalReXprefix', GotIt)
    	f = TRIM(nodalReXPrefix) // '.dat'
    	IF (.NOT. GotIt) THEN
	    	print *, "ReX init value not found, set to 0."
         	ReXArray = 0.0
		ELSE
			CALL ReadCSV_ReX(f, data, numLines)
			ReXArray(:) = data(:,2)
		END IF    
		    dataIntegral = 1 / avrami_k0 * (- log(1 - ReXArray))**(1 / avrami_n)
	END IF

    !Allocate the time tracker
    IF (.NOT. ALLOCATED(dataTimeN)) THEN
        TotalNodes = Model % Mesh % NumberOfNodes
        print *, "Number of mesh nodes allocated for TimeN:", TotalNodes
        ALLOCATE(dataTimeN(TotalNodes))
        dataTimeN = 0.0
    END IF

    ! Allocate the Tprev array
    IF (.NOT. ALLOCATED(dataTprev)) THEN
        TotalNodes = Model % Mesh % NumberOfNodes
        print *, "Number of mesh nodes allocated for Tprev:", TotalNodes
        ALLOCATE(dataTprev(TotalNodes))
        dataTprev = Temp
    END IF

    !get current timestep
    TimeVar => VariableGet( Model % Variables, "Time" )
    Time = TimeVar % Values(1)
	
    IF (dataTimeN(n) .NE. TIME) THEN
        !get dt in hours
        dt = GetTimestepSize() / 3600.0
        !temperature history integral (using trapz integration for this timestep)
        c = dt * ( exp(-avrami_E / (kB * dataTprev(n))) + exp(-avrami_E / (kB * (Temp))) ) / 2.0
        !update integral
        dataIntegral(n) = dataIntegral(n) + c

        !For testing, you can print data for a specific node id here
        !IF (n .EQ. 8383) THEN
        !    print *, "True..."
        !    print *, ReXArray(n)
        !    print *, dataIntegral(n)
        !    print *, (1 - exp(-avrami_k0*c1 * (c2 * dataIntegral(n))**avrami_n ))
        !    print *, dataTprev(n) !represents 20.0 + 273.15 at first timestep
        !    print *, Temp 
        !END IF

        !save Temperature data for next step
        dataTprev(n) = Temp
        !Save timestep data for next timestep
        dataTimeN(n) = Time

    END IF   

    !JMAK equation using our effective time
    ReXArray(n) = (1 - exp(-(avrami_k0* dataIntegral(n))**avrami_n ))
    !These values can sometimes be huge or nan.  check it
    IF (ieee_is_nan(ReXArray(n)) .OR. .NOT. ieee_is_finite(ReXArray(n))) THEN
        print *, "NaN or Inf detected in ReX calc...assigning 0..."
        ReXArray(n) = 0.0
    END IF    
	ReX = ReXArray(n)

END FUNCTION ReXOnNodes

SUBROUTINE ReadCSV_ReX(filename, data, numLines)
    USE ReXModule, except_this_one => ReadCSV_ReX
    CHARACTER(LEN=255) :: filename
    REAL, ALLOCATABLE, INTENT(OUT) :: data(:,:)
    INTEGER, INTENT(OUT) :: numLines
    CHARACTER(LEN=200) :: line
    CHARACTER(LEN=20), DIMENSION(2) :: splitLine
    INTEGER, DIMENSION(2) :: splitPos
    INTEGER :: ioStat, fileUnit, i
    logical :: exists

    print *, "Reading ReX init CSV File..."

    ! First pass: Count the number of lines
    fileUnit = 10  ! Arbitrary choice, ensure this unit is not in use elsewhere
    OPEN(UNIT=fileUnit, FILE=filename, ACTION='READ')
    numLines = 0
    DO
        READ(fileUnit, '(A)', IOSTAT=ioStat) line
        IF (ioStat /= 0) EXIT
        numLines = numLines + 1
    END DO
    CLOSE(fileUnit)

    ! Allocate the array
    ALLOCATE(data(numLines, 2))

    ! Second pass: Read the data
    OPEN(UNIT=fileUnit, FILE=filename, ACTION='READ')
    DO i = 1, numLines
        READ(fileUnit, '(A)', IOSTAT=ioStat) line
        IF (ioStat /= 0) EXIT
        CALL SplitString_ReX(line, splitLine)
        IF (status /= 0) THEN
            PRINT *, 'Error: Line ', i, ' does not have the correct format.'
            STOP
        END IF        
        READ(splitLine(1), *) data(i, 1)
        READ(splitLine(2), *) data(i, 2)
    END DO
    CLOSE(fileUnit)
END SUBROUTINE ReadCSV_ReX

SUBROUTINE SplitString_ReX(str, splitStr)
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=20), DIMENSION(2), INTENT(OUT) :: splitStr
    INTEGER :: endPos

    endPos = INDEX(str, ',')
    splitStr(1) = str(1:endPos-1)
    splitStr(2) = str(endPos+1:)

END SUBROUTINE SplitString_ReX

SUBROUTINE WriteReX(Model, Solver, dt)
    USE DefUtils
    USE ReXModule , except_this_one => WriteReX
    IMPLICIT NONE
    TYPE(Model_t), INTENT(IN) :: Model
    TYPE(Solver_t), INTENT(IN) :: Solver
    REAL(KIND=8), INTENT(IN) :: dt
    LOGICAL :: GotIt

    CHARACTER(LEN=255) :: filename_new
    INTEGER :: i, fu

    IF (.NOT. ALLOCATED(ReXArray)) THEN
        PRINT *, "WriteReX: ReXArray not allocated -> nothing to write."
        RETURN
    END IF
    ! Important: For Elmer Solver, use Output File from SIF:
	filename_new = GetString( Solver % Values, 'Filename', GotIt )
	IF (.NOT. GotIt) THEN
		CALL Fatal('WriteReX', 'Missing "Filename" in solver section of SIF.')
	END IF
    PRINT *, "WriteReX: Writing file:", filename_new
    fu = 77
    OPEN(fu, FILE=filename_new, STATUS="REPLACE", ACTION="WRITE")
    DO i = 1, SIZE(ReXArray)
        WRITE(fu,'(E15.9,",",E15.9)') REAL(i), ReXArray(i)
    END DO

    CLOSE(fu)
END SUBROUTINE WriteReX
