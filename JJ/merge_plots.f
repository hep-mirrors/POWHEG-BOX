C ********************************************************************* C
C ********************************************************************* C
C ********************************************************************* C
C ***** A program to merge two Topdrawer files                    ***** C
C *****                                                           ***** C
C ********************************************************************* C
C ********************************************************************* C 
C ********************************************************************* C
      PROGRAM MERGE_PLOTS
C ********************************************************************* C
C ********************************************************************* C

      IMPLICIT NONE
C -   *************

C **************** C
C ARRAY DIMENSIONS C
C **************** C

C - --------------------------------------------------------------
C - A limit on the maximum number of bins to read in a histogram
C - and the number of lines of waffle to read before reading data.
C - Also a limit on the number of histograms per plot to read from
C - either file.
C - --------------------------------------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - --------------------------------------------------------------
C - Variables to hold onto the data as it's input output and 
C - getting manipulated
C - --------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! A  values in chi2
      DOUBLE PRECISION B_CHI2       ! B  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)

C - --------------------------------------------------------------
C - Input and output files and stream indices & status codes as 
C - well as the 'mode' for outputting plots:
C - 0 = main plot, Delta sigma / delta Delta sigma, Delta sigma / sigma 
C - 1 = main plot, Delta sigma / delta Delta sigma
C - 2 = main plot, Delta sigma / sigma 
C - 3 = main plot
C - --------------------------------------------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C - Colours for the histograms
C -----------------------------
      CHARACTER*12 COLOURS
      COMMON/THE_COLOURS/COLOURS(7)
      DATA COLOURS/
     $     'BLUE DOTS   ','RED DASH    ','GREEN       ',
     $     'CYAN        ','MAGENTA     ','YELLOW      ',
     $     'BLACK       '/

C - line_type for histograms
C -----------------------------
      CHARACTER*8 LINE_TYPES
      COMMON/THE_LINE_TYPE/LINE_TYPES(4)
      DATA LINE_TYPES/
     $     'SOLID   ','DOTS    ','DASH    ','DOT DASH'/

C - plot_type for histograms
C -----------------------------
      CHARACTER*8 PLOT_TYPES
      COMMON/THE_PLOT_TYPE/PLOT_TYPES(3)
      DATA PLOT_TYPES/
     $     'HIST    ','JOIN    ','PLOT    '/

C *************** C
C LOCAL VARIABLES C
C *************** C

C - ----------------------------------------
C - Do loop iterators and dummy variables
C ------------------------------------------
      INTEGER HXX,IXX,JXX,KXX,LXX,TEMPI1,TEMPI2,TEMPI3


C - --------------------------------------------------------------------
C - Place holders to monitor the reading of records in the files
C ----------------------------------------------------------------------
      CHARACTER*80 TMP_STRING(5)    ! Temporary string storage
      INTEGER A_LAST_LINE_READ      ! ... in file A
      INTEGER B_LAST_LINE_READ      ! ... in file B
      INTEGER A_PLACE_HOLDER        ! ... for file A
      INTEGER B_PLACE_HOLDER        ! ... for file B
      LOGICAL READING,NOT_READING,WRITING,NOT_WRITING
      INTEGER A_NHEADER_LINES       ! Holds the No. of header lines for A plot.
      INTEGER B_NHEADER_LINES       ! Holds the No. of header lines for B plot.
      INTEGER A_NDATA_LINES         ! Holds the No. of data lines for A plot.
      INTEGER B_NDATA_LINES         ! Holds the No. of data lines for B plot.

C - ---------------------------------------
C - Number of plots in files A and B
C -----------------------------------------
      INTEGER A_NPLOTS,B_NPLOTS,MIN_NPLOTS

C - ---------------------------------------
C - Some spare real numbers
C -----------------------------------------
      DOUBLE PRECISION RTMP1,RTMP2,RTMP3
      DOUBLE PRECISION YMIN,YMAX

      READING     = .TRUE.
      WRITING     = .TRUE.
      NOT_READING = .TRUE.
      NOT_WRITING = .TRUE.

      A_STREAM=11
      B_STREAM=12
      C_STREAM=13

      CALL GETARG(1,A_FILE)
      CALL GETARG(2,B_FILE)
      CALL GETARG(3,C_FILE)

      CALL GETARG(4,MODE  )
      IF(TRIM(MODE).NE.'0'.AND.TRIM(MODE).NE.'1'.AND.
     $   TRIM(MODE).NE.'2'.AND.TRIM(MODE).NE.'3') THEN
         WRITE(6,*) 'Invalid mode selction: ',MODE
         WRITE(6,*) 'Mode should be in range 0-3 ...'
         WRITE(6,*) 'Proceeding with default mode.'
      ENDIF

      OPEN(UNIT=A_STREAM,FILE=A_FILE,STATUS='OLD')
      OPEN(UNIT=B_STREAM,FILE=B_FILE,STATUS='OLD')
      OPEN(UNIT=C_STREAM,FILE=C_FILE,STATUS='NEW')

      WRITE(6,*) 'A_FILE = ',A_FILE
      WRITE(6,*) 'B_FILE = ',B_FILE
      WRITE(6,*) 'C_FILE = ',C_FILE

      CALL COUNT_PLOTS(A_STREAM,A_NPLOTS)
      CALL COUNT_PLOTS(B_STREAM,B_NPLOTS)
      MIN_NPLOTS=MIN(A_NPLOTS,B_NPLOTS)

      IF(A_NPLOTS.NE.B_NPLOTS) THEN
         WRITE(6,*) 'I found ',A_NPLOTS,' plots in the 1st file.'
         WRITE(6,*) 'I found ',B_NPLOTS,' plots in the 2nd file.'
         WRITE(6,*) 'I will assume that the first',MIN_NPLOTS,'are',
     $              'common and proceed neglecting the rest.'
      ENDIF

      A_PLACE_HOLDER=0
      B_PLACE_HOLDER=0
      A_LAST_LINE_READ=0
      B_LAST_LINE_READ=0


C - Loop over all the plots:
      DO HXX=1,MIN_NPLOTS

C - Update file record place holders:
         A_PLACE_HOLDER=A_LAST_LINE_READ
         B_PLACE_HOLDER=B_LAST_LINE_READ

C - Read the plot header info from both input files:
         CALL READ_PLOT_HEADER(A_STREAM,A_LAST_LINE_READ)
         CALL READ_PLOT_HEADER(B_STREAM,B_LAST_LINE_READ)
         A_NHEADER_LINES=A_LAST_LINE_READ-A_PLACE_HOLDER
         B_NHEADER_LINES=B_LAST_LINE_READ-B_PLACE_HOLDER

C - Update file record place holders:
         A_PLACE_HOLDER=A_LAST_LINE_READ
         B_PLACE_HOLDER=B_LAST_LINE_READ

C - Read the plot data (including footers) from both input files:
         CALL READ_PLOT_DATA(A_STREAM,A_LAST_LINE_READ)
         CALL READ_PLOT_DATA(B_STREAM,B_LAST_LINE_READ)
         A_NDATA_LINES=A_LAST_LINE_READ-A_PLACE_HOLDER
         B_NDATA_LINES=B_LAST_LINE_READ-B_PLACE_HOLDER

C - Compute chi^2 per bin
         CALL COMPUTE_CHI2()

C - Compute fractional difference per bin
         CALL COMPUTE_FRAC_DIFF()

C - Modifies lines in the header starting like SET WINDOW X / Y:
         CALL RESET_WINDOW_FRAMES(A_HEADER,A_NHEADER_LINES)
         CALL RESET_WINDOW_FRAMES(B_HEADER,B_NHEADER_LINES)

C - Edit the plot header to make the fonts nicer and also to comment
C - out a couple of commands on the bottom axis of the main plot, which
C - appear in the difference plots which follow beneath it.
         CALL MAKE_NICER_HEADER(A_HEADER,A_NHEADER_LINES)
         CALL MAKE_NICER_HEADER(B_HEADER,B_NHEADER_LINES)

C - Modifies lines starting with TITLE not followed by INT, ENT,
C - UFL, OFL, BOTTOM, LEFT, RIGHT, TOP i.e. it should fish out
C - TITLE #X #Y "Blah"
         CALL REPOSITION_TITLE(A_HEADER,A_NHEADER_LINES)
         CALL REPOSITION_TITLE(B_HEADER,B_NHEADER_LINES)

C - Comments out lines in the header containing "INT=","ENT=",
C - "UFL=","OFL=","JOIN " and also any lines starting with a number.
         CALL REMOVE_INT_BOX(A_HEADER,A_NHEADER_LINES)
         CALL REMOVE_INT_BOX(B_HEADER,B_NHEADER_LINES)

C - Find the minimum and maximum Y values.
         CALL FIND_Y_BOUNDS(A_X,A_Y,A_NHISTOGRAMS,YMIN ,YMAX )
         CALL FIND_Y_BOUNDS(B_X,B_Y,B_NHISTOGRAMS,RTMP1,RTMP2)
         IF(RTMP1.LT.YMIN) YMIN=RTMP1
         IF(RTMP2.GT.YMAX) YMAX=RTMP2

C - Reset the maximum and minumum Y values in the 
C - plot so all data is visible.
         CALL RESET_Y_BOUNDS(A_HEADER,A_NHEADER_LINES,YMIN,YMAX)
         CALL RESET_Y_BOUNDS(B_HEADER,B_NHEADER_LINES,YMIN,YMAX)

C - Looks in the histograms for a 'tag' indicating where the data
C - came from / what it corresponds to. If it doesn't find one it
C - assigns one on the basis of the name of the file. 
         CALL IDENTIFY_PLOTS(A_STREAM)
         CALL IDENTIFY_PLOTS(B_STREAM)

C - Write the header taken from file A or file B:
         DO IXX=1,A_NHEADER_LINES
            WRITE(C_STREAM,*) A_HEADER(IXX)
         ENDDO

C - Write the histogram data and footers taken from the plots:
         CALL WRITE_PLOT_DATA(A_STREAM)
         CALL WRITE_PLOT_DATA(B_STREAM)

         IF(TRIM(MODE).EQ.'0') THEN
C - Find new minimum and maximum Y values to feed to WRITE_DELTA_SIG_HEADER:
           CALL FIND_Y_BOUNDS(A_X,A_CHI2,A_NHISTOGRAMS,YMIN ,YMAX )
           CALL FIND_Y_BOUNDS(B_X,B_CHI2,B_NHISTOGRAMS,RTMP1,RTMP2)
           IF(RTMP1.LT.YMIN) YMIN=RTMP1
           IF(RTMP2.GT.YMAX) YMAX=RTMP2
            
C - Write the header for the Delta sigma / delta Delta sigma plots
C - based on what is in A_HEADER (or, if you want, B_HEADER):
           CALL WRITE_DELTA_SIG_HEADER(A_HEADER,YMIN,YMAX)

C - Write the Delta sigma / delta Delta sigma data and footers:
           CALL WRITE_DELTA_SIG_DATA(A_STREAM)
           CALL WRITE_DELTA_SIG_DATA(B_STREAM)

C - Find new minimum and maximum Y values to feed to WRITE_DELTA_SIG_HEADER:
           CALL FIND_Y_BOUNDS(A_X,A_FRAC_DIFF,A_NHISTOGRAMS,YMIN ,YMAX )
           CALL FIND_Y_BOUNDS(B_X,B_FRAC_DIFF,B_NHISTOGRAMS,RTMP1,RTMP2)
           IF(RTMP1.LT.YMIN) YMIN=RTMP1
           IF(RTMP2.GT.YMAX) YMAX=RTMP2
            
C - Write the header for the Delta sigma / delta Delta sigma plots
C - based on what is in A_HEADER (or, if you want, B_HEADER):
           CALL WRITE_FRAC_DIFF_HEADER(A_HEADER,YMIN,YMAX)

C - Write the Delta sigma / delta Delta sigma data and footers:
           CALL WRITE_FRAC_DIFF_DATA(A_STREAM)
           CALL WRITE_FRAC_DIFF_DATA(B_STREAM)
         ELSEIF(TRIM(MODE).EQ.'1') THEN
C - Find new minimum and maximum Y values to feed to WRITE_DELTA_SIG_HEADER:
           CALL FIND_Y_BOUNDS(A_X,A_CHI2,A_NHISTOGRAMS,YMIN ,YMAX )
           CALL FIND_Y_BOUNDS(B_X,B_CHI2,B_NHISTOGRAMS,RTMP1,RTMP2)
           IF(RTMP1.LT.YMIN) YMIN=RTMP1
           IF(RTMP2.GT.YMAX) YMAX=RTMP2
            
C - Write the header for the Delta sigma / delta Delta sigma plots
C - based on what is in A_HEADER (or, if you want, B_HEADER):
           CALL WRITE_DELTA_SIG_HEADER(A_HEADER,YMIN,YMAX)

C - Write the Delta sigma / delta Delta sigma data and footers:
           CALL WRITE_DELTA_SIG_DATA(A_STREAM)
           CALL WRITE_DELTA_SIG_DATA(B_STREAM)
         ELSEIF(TRIM(MODE).EQ.'2') THEN
C - Find new minimum and maximum Y values to feed to WRITE_DELTA_SIG_HEADER:
           CALL FIND_Y_BOUNDS(A_X,A_FRAC_DIFF,A_NHISTOGRAMS,YMIN ,YMAX )
           CALL FIND_Y_BOUNDS(B_X,B_FRAC_DIFF,B_NHISTOGRAMS,RTMP1,RTMP2)
           IF(RTMP1.LT.YMIN) YMIN=RTMP1
           IF(RTMP2.GT.YMAX) YMAX=RTMP2
            
C - Write the header for the Delta sigma / delta Delta sigma plots
C - based on what is in A_HEADER (or, if you want, B_HEADER):
           CALL WRITE_FRAC_DIFF_HEADER(A_HEADER,YMIN,YMAX)

C - Write the Delta sigma / delta Delta sigma data and footers:
           CALL WRITE_FRAC_DIFF_DATA(A_STREAM)
           CALL WRITE_FRAC_DIFF_DATA(B_STREAM)
         ENDIF

C - Write a command to start a new plot before going onto the next plot:
         WRITE(C_STREAM,*) 'NEW PLOT'

      ENDDO

C - Write some useful instructions for how to easily manipulate the
C - result file down to one containing only the main plot using sed
C - to act on the special 'tag lines'.
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*)
     $ "Applying the following sed commands to the results"
      WRITE(6,*)
     $ "file will remove the CHI2 subplot:"
      WRITE(6,*)
     $ "gsed -i -e '/.*( CHI2/d' x.top"
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*)
     $ "Applying the following sed commands to the results"
      WRITE(6,*) 
     $ "file will reinstate the usual lower title and axes"
      WRITE(6,*) 
     $ "if only the main plot is wanted:"
      WRITE(6,*)
     $ "gsed -i -e 's/\(.*\)\(( COMMENT\)/( \1 ( COMMENT/' x.top"
      WRITE(6,*)
     $ "gsed -i -e 's/( \(.*\)\(( UNCOMMENT\)/\1 ( UNCOMMENT/' x.top"
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*)

C - Close the file streams and exit:
      CLOSE(A_STREAM)
      CLOSE(B_STREAM) 
      CLOSE(C_STREAM)

      END

C *********************************************************************** C
      SUBROUTINE COUNT_PLOTS(THE_STREAM,NPLOT)
C     Counts the number of actual plots in a file assuming that for  
C     each different plot the string 'SET ORDER X ...' appears once
C     and once only.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************
      INTEGER THE_STREAM,STAT
      INTEGER NPLOT,NREAD              ! Number of plots and lines read
      CHARACTER*80 TMP_STRING1         ! Temporary string storage
      LOGICAL READING                  ! Boolean for DO-WHILE loop

      NREAD=0
      NPLOT=0
      READING=.TRUE.

      DO WHILE(READING)
         READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING1
         IF(STAT.EQ.-1) THEN 
            WRITE(6,*) 'OK, end of file reached, breaking out of loop.'
            READING=.FALSE.
            CYCLE
         ENDIF
         NREAD=NREAD+1
         TMP_STRING1=ADJUSTL(TMP_STRING1)
         TMP_STRING1=TRIM(TMP_STRING1)
         IF(TMP_STRING1(1:9).EQ.'SET ORDER'.OR.
     $      TMP_STRING1.EQ.'SET  ORDER'.OR.
     $      TMP_STRING1.EQ.'SET   ORDER') THEN
            NPLOT=NPLOT+1
         ENDIF
      ENDDO

      WRITE(*,*) 'Read ',NREAD,' lines and found ',NPLOT,' plots',
     $           ' in stream ',THE_STREAM

      RETURN
      END

C *********************************************************************** C
      SUBROUTINE READ_PLOT_HEADER(THE_STREAM,LAST_LINE_READ)
C     Reads a plot header from the file with stream THE_STREAM
C     returning the number of the last line it read in the file.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C **************** C
C ARRAY DIMENSIONS C
C **************** C

      INTEGER      NMAX_HEADER
      PARAMETER   (NMAX_HEADER=100)

C ************* C
C COMMON BLOCKS C
C ************* C

C - Plot common block to fill with the header being read
C -------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER)

C - IO stream common block
C -------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C *************** C
C LOCAL VARIABLES C
C *************** C

      INTEGER THE_STREAM               ! Doh!
      CHARACTER*80 TMP_STRING1         ! Temporary string storage.
      CHARACTER*80 TMP_STRING2         ! Temporary string storage.
      CHARACTER*80 TMP_STRING3         ! Temporary string storage.
      INTEGER LAST_LINE_READ           ! Doh!
      LOGICAL READING                  ! Boolean condition for while loop.
      CHARACTER*80 HEADER(NMAX_HEADER) ! The plot header
      INTEGER NREAD,IXX                ! Loop iterators

C - Initialise the READING boolean so we actually start reading.
      READING     = .TRUE.

C - Before starting make sure the relevant header arrays are junk free...
      DO IXX=1,NMAX_HEADER
         IF(THE_STREAM.EQ.A_STREAM) THEN
            A_HEADER(IXX)=''
         ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
            B_HEADER(IXX)=''
         ENDIF
         HEADER(IXX)=''
      ENDDO

C - First go to the line before the HEADER starts (LAST_LINE_READ):
      REWIND(THE_STREAM)
      DO NREAD=1,LAST_LINE_READ
         READ(THE_STREAM,'(A80)',IOSTAT=STAT)
      ENDDO

C - Now read the plot header:
      NREAD=0
      DO WHILE(READING)
         READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING1
         IF(STAT.EQ.-1) THEN 
            WRITE(6,*) 'OK, end of file reached, breaking out of loop.'
            READING=.FALSE.
            CYCLE
         ENDIF
         NREAD=NREAD+1
         TMP_STRING1=ADJUSTL(TMP_STRING1)
         TMP_STRING1=TRIM(TMP_STRING1)
         IF(TMP_STRING1.EQ.'') CYCLE
C - Basically if the line begins with anything from the alphabet we
C - assume it is the header and so we store it:
         IF(SCAN(TMP_STRING1,'ABCDFGHIJKLMNOPQRSTUVWXYZ(').NE.0) THEN
            HEADER(NREAD)=TMP_STRING1
         ELSE 
C - However, sometimes the header contains the lines for the box on 
C - the plot which contains the integral etc. To get around this we
C - assume that this box is always specified as two (x,y) values
C - followed by a line containing the string 'TEXT'.
C - 
C - So we read in the next three lines and put them in temporary storage...
            NREAD=NREAD+1
            READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING2
            NREAD=NREAD+1
            READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING3
C - If the third line contains the 'TEXT' string we keep going:
            IF(INDEX(TMP_STRING3,'TEXT').NE.0) THEN
               HEADER(NREAD-2)=TMP_STRING1
               HEADER(NREAD-1)=TMP_STRING2
               HEADER(NREAD  )=TMP_STRING3
C - Otherwise we rewind the file to the line before where we first read
C - in the numbers, adjusting NREAD, and the exit the DO WHILE loop:
            ELSE
               NREAD=NREAD-3
               BACKSPACE(THE_STREAM)
               BACKSPACE(THE_STREAM)
               BACKSPACE(THE_STREAM)
               READING=.FALSE.
            ENDIF
         ENDIF
      ENDDO

C - Warn if nothing got read!
      IF(NREAD.LE.0) THEN
         WRITE(6,*) 'READ_PLOT_HEADER: error!'
         WRITE(6,*) 'NREAD          = ',NREAD
         WRITE(6,*) 'LAST_LINE_READ = ',LAST_LINE_READ
      ENDIF

C - Copy the contents of HEADER to either A_HEADER or B_HEADER
      IF(THE_STREAM.EQ.A_STREAM) THEN
         DO IXX=1,NMAX_HEADER
            A_HEADER(IXX)=HEADER(IXX)
         ENDDO
      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         DO IXX=1,NMAX_HEADER
            B_HEADER(IXX)=HEADER(IXX)
         ENDDO
      ELSE
         WRITE(6,*) 'READ_PLOT_HEADER: invalid stream input!'
         WRITE(6,*) 'THE_STREAM = ',THE_STREAM
      ENDIF

C - Set the value of LAST_LINE_READ to be the last line read in the file.
      LAST_LINE_READ=LAST_LINE_READ+NREAD

      WRITE(6,*) 'Read ',NREAD,' lines of header to line',LAST_LINE_READ
      READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING1
      WRITE(6,*) 'Next line says: ',TMP_STRING1

      RETURN
      END


C *********************************************************************** C
      SUBROUTINE READ_PLOT_DATA(THE_STREAM,LAST_LINE_READ)
C     Reads data needed used to make the histograms in the current plot
C     in the file corresponding to stream THE_STREAM. Returning the number
C     of the last line it read in the file.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C **************** C
C ARRAY DIMENSIONS C
C **************** C

C - Dimensions of histogram arrays
C ---------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - Plot common block to fill with the data nd footers being read
C ----------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! X  values in chi2
      DOUBLE PRECISION B_CHI2       ! Y  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)

C - IO stream common block
C -------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C *************** C
C LOCAL VARIABLES C
C *************** C

      INTEGER THE_STREAM               ! Doh!
      CHARACTER*80 TMP_STRING1         ! Temporary string storage.
      CHARACTER*80 TMP_STRING2         ! Temporary string storage.
      INTEGER LAST_LINE_READ           ! Doh!
      LOGICAL READING                  ! Boolean condition for while loop.
      LOGICAL PREVIOUS_LINE_WAS_FOOTER ! Doh!
      CHARACTER*80 FOOTERS(NMAX_HEADER,NMAX_HISTS)     ! The footers
      DOUBLE PRECISION X_VALUES (NMAX_BINS,NMAX_HISTS) ! Bin x-values
      DOUBLE PRECISION Y_VALUES (NMAX_BINS,NMAX_HISTS) ! Bin y-values
      DOUBLE PRECISION DY_VALUES(NMAX_BINS,NMAX_HISTS) ! Bin dy-values
      INTEGER NREAD                    ! Lines read from LAST_LINE_READ
      INTEGER NHISTOGRAMS              ! Number of histograms found in plot
      INTEGER COLUMN_GAP               ! Holds position of x/y/dy separators
      INTEGER FOOTER_IDX               ! Index for footer entries for a histo
      INTEGER BIN_IDX                  ! Index of an individual histogram bin
      INTEGER MAX_BIN_IDX              ! Index of an individual histogram bin
      INTEGER IXX,JXX                  ! Loop iterators
      INTEGER TEMPI1,TEMPI2
C - Initialise the READING boolean so we actually start reading.
      READING = .TRUE.

      PREVIOUS_LINE_WAS_FOOTER = .FALSE.
      NHISTOGRAMS=0
      BIN_IDX=0
      FOOTER_IDX=0

C - Making sure the footer array doesn't get junk in it
      DO IXX=1,NMAX_HISTS
         DO JXX=1,NMAX_HEADER
            FOOTERS(JXX,IXX)=''
         ENDDO
      ENDDO

C - Making sure the data arrays don't contain any junk either
      DO IXX=1,NMAX_HISTS
         DO JXX=1,NMAX_BINS
            X_VALUES(JXX,IXX) =-9.99D+30
            Y_VALUES(JXX,IXX) =-9.99D+30
            DY_VALUES(JXX,IXX)=-9.99D+30
         ENDDO
      ENDDO

C - First go to the line before the DATA starts (LAST_LINE_READ):
      REWIND(THE_STREAM)
      DO NREAD=1,LAST_LINE_READ
         READ(THE_STREAM,'(A80)',IOSTAT=STAT)
      ENDDO

C - Now start reading the data:
      NREAD=0
      DO WHILE(READING)
         READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING1
         IF(STAT.EQ.-1) THEN 
            WRITE(6,*) 'OK, end of file reached, breaking out of loop.'
            READING=.FALSE.
            CYCLE
         ENDIF
         NREAD=NREAD+1
         TMP_STRING1=ADJUSTL(TMP_STRING1)
         TMP_STRING1=TRIM(TMP_STRING1)
C - If the line is blank just let the counter increase & go to
C - next record
         IF(TMP_STRING1.EQ.'') CYCLE
         IF(INDEX(TMP_STRING1,'CHI2').NE.0) CYCLE
         IF(INDEX(TMP_STRING1,'FDIFF').NE.0) CYCLE
C - If the line contains with any letters (except E or NAN or INF) we
C - assume it is either part of the plot header or a histogram footer.
         IF(SCAN(TMP_STRING1,'BCDFGHIJKLMOPQRSTUVWXYZ(').NE.0) THEN
            IF(((INDEX(TMP_STRING1,'PLOT').NE.0).AND.
     $          (INDEX(TMP_STRING1,'NEW' ).EQ.0)
     $         ).OR.
     $          (INDEX(TMP_STRING1,'HIST').NE.0).OR.
     $          (INDEX(TMP_STRING1,'JOIN').NE.0).OR.
     $          (INDEX(TMP_STRING1,'TAG ID').NE.0)
     $        ) THEN
C - It's part of a footer ...
               IF(PREVIOUS_LINE_WAS_FOOTER.EQV..FALSE.) THEN
                  NHISTOGRAMS=NHISTOGRAMS+1
                  BIN_IDX=0
                  FOOTER_IDX=1
               ENDIF
               PREVIOUS_LINE_WAS_FOOTER=.TRUE.
               FOOTER_IDX = FOOTER_IDX+1
               FOOTERS(FOOTER_IDX,NHISTOGRAMS) = TMP_STRING1
            ELSE
C - It's part of the plot header ... so we stop reading and go back one.
               PREVIOUS_LINE_WAS_FOOTER=.FALSE.
               READING=.FALSE.
               NREAD=NREAD-1
               BACKSPACE(THE_STREAM)
            ENDIF
         ELSE 
C - Otherwise we assume it's some important it of data we should be saving
            PREVIOUS_LINE_WAS_FOOTER=.FALSE.
            BIN_IDX=BIN_IDX+1
            IF(BIN_IDX.GT.MAX_BIN_IDX) MAX_BIN_IDX=BIN_IDX
            COLUMN_GAP=SCAN(TMP_STRING1,' ')
            TMP_STRING2=ADJUSTL(TMP_STRING1(1:COLUMN_GAP)) ! The x-value
            READ(TMP_STRING2,'(ES15.8)')
     $           X_VALUES(BIN_IDX,NHISTOGRAMS+1)
            TMP_STRING2=ADJUSTL(TMP_STRING1(COLUMN_GAP:))  ! The y & dy-values
            TMP_STRING1=TMP_STRING2                        ! The y & dy-values
            COLUMN_GAP=SCAN(TMP_STRING1,' ')
            TMP_STRING2=ADJUSTL(TMP_STRING1(1:COLUMN_GAP)) ! The y-values
            IF(TMP_STRING2(1:3).EQ.'NAN') THEN
               Y_VALUES(BIN_IDX,NHISTOGRAMS+1) = -9.99D+30
            ELSE
               READ(TMP_STRING2,'(ES15.8)')
     $              Y_VALUES(BIN_IDX,NHISTOGRAMS+1)
            ENDIF
            TMP_STRING2=ADJUSTL(TMP_STRING1(COLUMN_GAP:))  ! The dy-values
            IF(TMP_STRING2(1:3).EQ.'NAN') THEN
               DY_VALUES(BIN_IDX,NHISTOGRAMS+1) = -9.99D+30
            ELSE
               READ(TMP_STRING2,'(ES15.8)')
     $              DY_VALUES(BIN_IDX,NHISTOGRAMS+1)
            ENDIF
         ENDIF
      ENDDO

C - Warn if nothing got read!
      IF(NREAD.LE.0) THEN
         WRITE(6,*) 'READ_PLOT_DATA: error!'
         WRITE(6,*) 'NREAD          = ',NREAD
         WRITE(6,*) 'LAST_LINE_READ = ',LAST_LINE_READ
      ENDIF

C - Copy the contents of X_VALUES, Y_VALUES and DY_VALUES into either
C - the histogram A or histogram B bits of THE_PLOT common block
      IF(THE_STREAM.EQ.A_STREAM) THEN
         DO IXX=1,NMAX_HISTS
            DO JXX=1,NMAX_BINS
               A_X (JXX,IXX)=X_VALUES (JXX,IXX)
               A_Y (JXX,IXX)=Y_VALUES (JXX,IXX)
               A_DY(JXX,IXX)=DY_VALUES(JXX,IXX)
            ENDDO
         ENDDO
         DO IXX=1,NMAX_HISTS
            DO JXX=1,NMAX_HEADER
               A_FOOTERS(JXX,IXX)=FOOTERS(JXX,IXX)
            ENDDO
         ENDDO
         A_NHISTOGRAMS=NHISTOGRAMS
      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         DO IXX=1,NMAX_HISTS
            DO JXX=1,NMAX_BINS
               B_X (JXX,IXX)=X_VALUES (JXX,IXX)
               B_Y (JXX,IXX)=Y_VALUES (JXX,IXX)
               B_DY(JXX,IXX)=DY_VALUES(JXX,IXX)
            ENDDO
         ENDDO
         DO IXX=1,NMAX_HISTS
            DO JXX=1,NMAX_HEADER
               B_FOOTERS(JXX,IXX)=FOOTERS(JXX,IXX)
            ENDDO
         ENDDO
         B_NHISTOGRAMS=NHISTOGRAMS
      ELSE
         WRITE(6,*) 'READ_PLOT_HEADER: invalid stream input!'
         WRITE(6,*) 'THE_STREAM = ',THE_STREAM
      ENDIF

C - Set the value of LAST_LINE_READ to be the last line read in the file.
      LAST_LINE_READ=LAST_LINE_READ+NREAD

      WRITE(6,*) 'Read ',NREAD,' lines of data to line ',LAST_LINE_READ
      READ(THE_STREAM,'(A80)',IOSTAT=STAT) TMP_STRING1
      WRITE(6,*) 'Next line says: ',TMP_STRING1

      RETURN
      END


C *********************************************************************** C
      SUBROUTINE RESET_WINDOW_FRAMES(THE_HEADER,NHEADER_LINES)
C     Resets the SET WINDOW X and SET WINDOW Y values in a plot. See
C     below for more details of what things get reset TO.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - IO stream common block
C -------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C - Dimensions of header array
C ---------------------------------
      INTEGER NMAX_HEADER
      PARAMETER(NMAX_HEADER=100)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

      INTEGER NHEADER_LINES   ! Number of lines found for this pot header
      CHARACTER*80 TMP_STRING  ! Yes, its a temporary string
      INTEGER IXX,JXX,KXX      ! Loop counters
      LOGICAL READING          ! Doh!

C - Look for strings containing SETWINDOWX/Y when all white space is
C - removed and replace them with the new window parameters
      DO IXX=1,NHEADER_LINES
        TMP_STRING=""
        CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETWINDOWY').NE.0) THEN
            IF(MODE.EQ.'0') THEN
               THE_HEADER(IXX)="SET WINDOW Y 4.0 TO 9."
            ELSE
               THE_HEADER(IXX)="SET WINDOW Y 2.8 TO 9."
            ENDIF
         ELSEIF(INDEX(TMP_STRING,'SETWINDOWX').NE.0) THEN
            THE_HEADER(IXX)="SET WINDOW X 2.5 TO 10."
         ENDIF
      ENDDO
      
      RETURN
      END


C *********************************************************************** C
      SUBROUTINE REMOVE_INT_BOX(THE_HEADER,NHEADER_LINES)
C     Removes the upper left corner text box containing INT,OFL,UFL
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - Dimensions of header array
C ---------------------------------
      INTEGER NMAX_HEADER
      PARAMETER(NMAX_HEADER=100)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

      INTEGER NHEADER_LINES   ! Number of lines found for this pot header
      CHARACTER*80 TMP_STRING  ! Yes, its a temporary string
      INTEGER IXX,JXX,KXX      ! Loop counters
      LOGICAL READING          ! Doh!

C - Look for INT=,OFL=,UFL=,ENT= and JOIN  in the header lines. 
      DO IXX=1,NHEADER_LINES
         READING=.TRUE.
         JXX=0
         KXX=0
         TMP_STRING=""
         IF((INDEX(THE_HEADER(IXX),"INT=").NE.0).OR.
     $      (INDEX(THE_HEADER(IXX),"ENT=").NE.0).OR.
     $      (INDEX(THE_HEADER(IXX),"UFL=").NE.0).OR.
     $      (INDEX(THE_HEADER(IXX),"OFL=").NE.0).OR.
     $      (INDEX(THE_HEADER(IXX),"JOIN ").NE.0)
     $     ) THEN
            TMP_STRING=ADJUSTL(TRIM(THE_HEADER(IXX)))
C - If the found string is not already commented out comment it now:
            IF(TMP_STRING(1:1).NE.'(') THEN
               TMP_STRING='(  '//THE_HEADER(IXX)
            ENDIF
            THE_HEADER(IXX)=TMP_STRING
         ENDIF
         IF(SCAN(ADJUSTL(THE_HEADER(IXX)),'123456789').EQ.1) THEN
            TMP_STRING='(  '//THE_HEADER(IXX)
            THE_HEADER(IXX)=TMP_STRING
         ENDIF
      ENDDO
      
      RETURN
      END

C *********************************************************************** C
      SUBROUTINE MAKE_NICER_HEADER(THE_HEADER,NHEADER_LINES)
C     Repositions the titles to account for the reset window frames
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - Dimensions of header array
C ---------------------------------
      INTEGER NMAX_HEADER
      PARAMETER(NMAX_HEADER=100)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

      INTEGER NHEADER_LINES    ! Number of lines found for this pot header
      CHARACTER*80 TMP_STRING  ! Yes, its a temporary string
      CHARACTER*80 TMP_STRING2 ! Yes, its a temporary string
      CHARACTER*80 BLANK       ! An empty string
      INTEGER IXX,KXX,LXX      ! IXX is a loop counters, KXX & LXX are holders
      LOGICAL SET_OPTION       ! Goes true when an option has been reset


      BLANK=''

C - Add a y-axis label template on the left
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,"TITLE").NE.0.AND.
     $      INDEX(TMP_STRING,'"').NE.0.AND.
     $      INDEX(TMP_STRING,'""').EQ.0) THEN
            KXX=SCAN(THE_HEADER(IXX),'"')+1
            LXX=SCAN(THE_HEADER(IXX),'"',.TRUE.)-1
            TMP_STRING2=ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))
         ENDIF
      ENDDO
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,"TITLELEFT").NE.0.AND.
     $      INDEX(TMP_STRING,"(TITLELEFT").EQ.0.AND.
     $      INDEX(TMP_STRING,'""').EQ.0) THEN
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-2
         DO WHILE(IXX.GE.1.AND.TRIM(THE_HEADER(IXX)).EQ.'') 
            CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
            IXX=IXX-1
         ENDDO
         THE_HEADER(IXX+1)=
     $        'TITLE LEFT "dS/d('//TRIM(TMP_STRING2)//') (?b/bin)"'
         THE_HEADER(IXX+2)=
     $        'CASE       " G  '//
     $         BLANK(1:LEN(TRIM(TMP_STRING2)))//
     $        '   S      S'
         NHEADER_LINES=NHEADER_LINES+2
      ENDIF

C - Switch off any 'NEW PLOT' commands which may already be there
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF((INDEX(TMP_STRING,'NEWPLOT'  ).NE.0).AND.
     $      (INDEX(TMP_STRING,'(NEWPLOT' ).EQ.0)) THEN
            THE_HEADER(IXX)='( '//TRIM(THE_HEADER(IXX))
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO

C - Switch off the title on the bottom of the main plot
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'TITLEBOTTOM' ).NE.0) THEN
            KXX=SCAN(THE_HEADER(IXX),'"')+1
            LXX=SCAN(THE_HEADER(IXX),'"',.TRUE.)-1
            THE_HEADER(IXX)=
     $          'TITLE BOTTOM "'//
     $           ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))//
     $          '"'
         ENDIF
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF((INDEX(TMP_STRING,'TITLEBOTTOM' ).NE.0).AND.
     $      (INDEX(TMP_STRING,'(TITLEBOTTOM').EQ.0)) THEN
            THE_HEADER(IXX)='( '//TRIM(THE_HEADER(IXX))//' ( UNCOMMENT'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO

C - Switch off the axes on the bottom of the main plot
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF((INDEX(TMP_STRING,'SETAXESBOTTOM'  ).NE.0).AND.
     $      (INDEX(TMP_STRING,'(SETAXESBOTTOM' ).EQ.0)) THEN
            THE_HEADER(IXX)=TRIM(THE_HEADER(IXX))//' ( COMMENT'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-1
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+1)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET AXES BOTTOM OFF  ( COMMENT'
         NHEADER_LINES=NHEADER_LINES+1
      ENDIF

C - Set the size of the axes labels
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETLABELSSIZE').NE.0.OR.
     $      INDEX(TMP_STRING,'SETLABELSIZE' ).NE.0.) THEN
            THE_HEADER(IXX)='SET LABEL SIZE 2.4'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-1
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+1)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET LABELS SIZE 2.4'
         NHEADER_LINES=NHEADER_LINES+1
      ENDIF

C - Not sure what this one is doing...
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETLABELCHARACTERS').NE.0) THEN
            THE_HEADER(IXX)='SET LABEL CHARACTERS 5'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-1
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+1)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET LABEL CHARACTERS 5'
         NHEADER_LINES=NHEADER_LINES+1
      ENDIF

C - Set the scale of the top title to be a little bigger than the rest
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETTITLETOPSCALE').NE.0) THEN
            THE_HEADER(IXX)='SET TITLE TOP SCALE 1.2'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-1
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+1)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET TITLE TOP SCALE 1.2'
         NHEADER_LINES=NHEADER_LINES+1
      ENDIF

C - Set the size of the title:
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETTITLESIZE').NE.0) THEN
            THE_HEADER(IXX)='SET TITLE SIZE 2.5'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-1
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+1)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET TITLE SIZE 2.5'
         NHEADER_LINES=NHEADER_LINES+1
      ENDIF

C - Set the scale of the title:
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETTITLEALLSCALE').NE.0) THEN
            THE_HEADER(IXX)='SET TITLE ALL SCALE 1.0'
            SET_OPTION=.TRUE.
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-1
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+1)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET TITLE ALL SCALE 1.0'
         NHEADER_LINES=NHEADER_LINES+1
      ENDIF

C - Make the font DUPLEX and set intensity 5:
      SET_OPTION=.FALSE.
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         IF(INDEX(TMP_STRING,'SETFONTDUPLEX').NE.0) THEN
            CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX+1),TMP_STRING)
            IF(INDEX(TMP_STRING,'INTENSITY').NE.0) THEN
               SET_OPTION=.TRUE.
            ENDIF
         ENDIF
      ENDDO
      IF(.NOT.SET_OPTION) THEN
         IXX=NHEADER_LINES-2
         DO WHILE(IXX.GE.1) 
            THE_HEADER(IXX+2)=THE_HEADER(IXX)
            IXX=IXX-1
         ENDDO
         THE_HEADER(1)='SET FONT DUPLEX'
         THE_HEADER(2)='SET INTENSITY 3'
         NHEADER_LINES=NHEADER_LINES+2
      ENDIF
      
      write(6,*) 'THE_HEADER(1)=',THE_HEADER(1)

      RETURN
      END

C *********************************************************************** C
      SUBROUTINE REPOSITION_TITLE(THE_HEADER,NHEADER_LINES)
C     Repositions the titles to account for the reset window frames
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - Dimensions of header array
C ---------------------------------
      INTEGER NMAX_HEADER
      PARAMETER(NMAX_HEADER=100)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

      INTEGER NHEADER_LINES    ! Number of lines found for this pot header
      CHARACTER*80 TMP_STRING  ! Yes, its a temporary string
      INTEGER IXX,KXX,LXX      ! IXX is a loop counters, KXX & LXX are holders
      LOGICAL READING          ! Doh!

C - Look for TITLE NOT followed by one of INT=, ENT=, UFL= etc etc
      DO IXX=1,NHEADER_LINES
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING)
         KXX=SCAN(THE_HEADER(IXX),'"')+1
         LXX=SCAN(THE_HEADER(IXX),'"',.TRUE.)-1
         IF(TMP_STRING(1:5).EQ.'TITLE'.AND.
     $     ((INDEX(TMP_STRING,'INT=').EQ.0).AND.
     $      (INDEX(TMP_STRING,'ENT=').EQ.0).AND.
     $      (INDEX(TMP_STRING,'UFL=').EQ.0).AND.
     $      (INDEX(TMP_STRING,'OFL=').EQ.0).AND.
     $      (INDEX(TMP_STRING,'TOP').EQ.0).AND.
     $      (INDEX(TMP_STRING,'BOTTOM').EQ.0).AND.
     $      (INDEX(TMP_STRING,'RIGHT').EQ.0).AND.
     $      (INDEX(TMP_STRING,'LEFT').EQ.0)
     $     ))
C - Assuming we found TITLE #X #Y "Blah Blah" and now replace it:
     $      THE_HEADER(IXX)=
     $         'TITLE TOP "'//
     $         ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))//'"'
         IF((TMP_STRING(1:5).EQ.'TITLE').AND.
     $      (INDEX(TMP_STRING,'TOP').NE.0)) 
     $      THE_HEADER(IXX)=
     $         'TITLE TOP "'//
     $         ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))//'"'
         IF((TMP_STRING(1:5).EQ.'TITLE').AND.
     $      (INDEX(TMP_STRING,'BOTTOM').NE.0))
     $      THE_HEADER(IXX)=
     $         'TITLE BOTTOM "'//
     $         ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))//'"'
         IF((TMP_STRING(1:5).EQ.'TITLE').AND.
     $      (INDEX(TMP_STRING,'LEFT').NE.0))
     $      THE_HEADER(IXX)=
     $         'TITLE LEFT "'//
     $         ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))//'"'
         IF((TMP_STRING(1:5).EQ.'TITLE').AND.
     $      (INDEX(TMP_STRING,'RIGHT').NE.0))
     $      THE_HEADER(IXX)=
     $         'TITLE RIGHT "'//
     $         ADJUSTL(TRIM(THE_HEADER(IXX)(KXX:LXX)))//'"'
      ENDDO
      
      RETURN
      END

C *********************************************************************** C
      SUBROUTINE REMOVE_WHITE_SPACES(IN_STRING,OUT_STRING)
C     Removes all white space in a string
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

      CHARACTER*80 IN_STRING,OUT_STRING ! input and output strings
      INTEGER JXX,KXX                    ! Loop counters
      LOGICAL READING                    ! Doh!

C - Goes through a IN_STRING and copies all non-blank characters 
C - into OUT_STRING
      READING=.TRUE.
      JXX=0
      KXX=0
      OUT_STRING=""
      DO WHILE(READING)
         JXX=JXX+1
         IF(IN_STRING(JXX:JXX).EQ." ") CYCLE
         KXX=KXX+1
         OUT_STRING(KXX:KXX)=IN_STRING(JXX:JXX)
         IF(JXX.GT.80) READING=.FALSE.
      ENDDO

      RETURN
      END


C *********************************************************************** C
      SUBROUTINE FIND_Y_BOUNDS(X_VALS,Y_VALS,NHISTOS,YMIN,YMAX)
C     Finds the maximum and minimum Y-values in a given plot.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C - Local variables
      DOUBLE PRECISION X_VALS(NMAX_BINS,NMAX_HISTS)
      DOUBLE PRECISION Y_VALS(NMAX_BINS,NMAX_HISTS)
      INTEGER IXX,JXX                      ! Loop counters
      DOUBLE PRECISION YMAX,YMIN,BINWIDTH ! Like the name says ...
      INTEGER NHISTOS

      YMAX=-9.99D+30               ! <--- Ugly. Minimum allowed by topdrawer.
      YMIN= 9.99D+30               ! <--- Ugly. Maximum allowed by topdrawer.
      BINWIDTH= 0.0
      DO IXX=1,NHISTOS
         BINWIDTH=X_VALS(2,IXX)-X_VALS(1,IXX)
         DO JXX=1,NMAX_BINS
            IF(
     $      (JXX.NE.1).AND.
     $      (X_VALS(JXX,IXX).EQ.-9.99D+30).AND.
     $      (ABS(X_VALS(JXX,IXX)-X_VALS(JXX-1,IXX)).GT.(1.1*BINWIDTH)))
     $           GOTO 200
            IF(Y_VALS(JXX,IXX).GT.YMAX) YMAX = Y_VALS(JXX,IXX)
            IF(Y_VALS(JXX,IXX).LT.YMIN.AND.Y_VALS(JXX,IXX).NE.-9.99D+30
     $                                .AND.Y_VALS(JXX,IXX).NE.-9.99D+2)
     $           YMIN = Y_VALS(JXX,IXX)
         ENDDO
 200     WRITE(6,*) ' '
      ENDDO
 220     WRITE(6,*) 'Found bin width is ',BINWIDTH
      WRITE(6,*) 'Found ',YMIN,'< Y < ',YMAX
      
      RETURN
      END

C *********************************************************************** C
      SUBROUTINE RESET_Y_BOUNDS(THE_HEADER,NHEADER_LINES,YMIN,YMAX)
C     Removes the upper left corner text box containing INT,OFL,UFL
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - Dimensions of header array
C ---------------------------------
      INTEGER NMAX_HEADER
      PARAMETER(NMAX_HEADER=100)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

      INTEGER NHEADER_LINES     ! Number of lines found for this pot header
      CHARACTER*80 TMP_STRING1   ! Yes, its a temporary string
      CHARACTER*80 TMP_STRING2   ! Yes, its a temporary string
      CHARACTER*80 TMP_STRING3   ! Yes, its a temporary string
      INTEGER IXX,JXX,KXX        ! Loop counters
      LOGICAL READING            ! Doh!
      DOUBLE PRECISION YMIN,YMAX ! Minimum and maxnimum Y values in plot

C - Make the minimum and maximum Y values a little bit lower and higher,
C - respectively, than those found while scanning the data for the plot:
      YMIN=0.8*YMIN
      YMAX=1.2*YMAX
C - Turn YMIN and YMAX into strings for writing in the file:
      WRITE(TMP_STRING2,*) YMIN
      WRITE(TMP_STRING3,*) YMAX
C - Look for SET LIMITS Y in the header and reset it when found:
      DO IXX=1,NHEADER_LINES
         READING=.TRUE.
         JXX=0
         KXX=0
         TMP_STRING1=""
         CALL REMOVE_WHITE_SPACES(THE_HEADER(IXX),TMP_STRING1)
         IF(TMP_STRING1(1:10).EQ.'SETLIMITSY') THEN
            THE_HEADER(IXX)=
     $           "SET LIMITS Y "//
     $           ADJUSTL(TRIM(TMP_STRING2))
     $           //" "//
     $           ADJUSTL(TRIM(TMP_STRING3))
            GOTO 200
         ENDIF
      ENDDO
      
 200  RETURN
      END


C *********************************************************************** C
      SUBROUTINE WRITE_PLOT_DATA(THE_STREAM)
C     Writes data needed used to make the histograms in the current plot
C     in the file corresponding to stream THE_STREAM. Returning the number
C     of the last line it read in the file.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C **************** C
C ARRAY DIMENSIONS C
C **************** C

C - Dimensions of histogram arrays
C ---------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - Plot common block to fill with the data and footers being read
C ----------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! X  values in chi2
      DOUBLE PRECISION B_CHI2       ! Y  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)

C - IO stream common block
C -------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C- Local variables
C ------------------
      INTEGER IXX,JXX
      INTEGER THE_STREAM
      LOGICAL WRITING
      
      WRITING=.TRUE.

      IF(THE_STREAM.EQ.A_STREAM) THEN
         DO IXX=1,A_NHISTOGRAMS
            JXX=1
            DO WHILE(A_X(JXX,IXX).NE.-9.99D+30)
               WRITE(C_STREAM,"(ES15.8,'  ',ES15.8,'  ',ES15.8)") 
     $              A_X(JXX,IXX),A_Y(JXX,IXX),A_DY(JXX,IXX)
               JXX=JXX+1
            ENDDO
            WRITING=.TRUE.
            JXX=1
            DO WHILE((WRITING.EQV..TRUE.).OR.A_FOOTERS(JXX,IXX).NE.'')
               IF(A_FOOTERS(JXX,IXX).NE.'') WRITING=.FALSE.
               WRITE(C_STREAM,*) A_FOOTERS(JXX,IXX)
               JXX=JXX+1
            ENDDO
         ENDDO
      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         DO IXX=1,B_NHISTOGRAMS
            JXX=1
            DO WHILE(B_X(JXX,IXX).NE.-9.99D+30)
               WRITE(C_STREAM,"(ES15.8,'  ',ES15.8,'  ',ES15.8)") 
     $              B_X(JXX,IXX),B_Y(JXX,IXX),B_DY(JXX,IXX)
               JXX=JXX+1
            ENDDO
            WRITING=.TRUE.
            JXX=1
            DO WHILE((WRITING.EQV..TRUE.).OR.B_FOOTERS(JXX,IXX).NE.'')
               IF(B_FOOTERS(JXX,IXX).NE.'') WRITING=.FALSE.
               WRITE(C_STREAM,*) B_FOOTERS(JXX,IXX)
               JXX=JXX+1
            ENDDO
         ENDDO
      ENDIF

      RETURN
      END

C *********************************************************************** C
      SUBROUTINE WRITE_DELTA_SIG_DATA(THE_STREAM)
C     Writes data needed used to make the histograms in the current plot
C     in the file corresponding to stream THE_STREAM. Returning the number
C     of the last line it read in the file.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C **************** C
C ARRAY DIMENSIONS C
C **************** C

C - Dimensions of histogram arrays
C ---------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - Plot common block to fill with the data and footers being read
C ----------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! X  values in chi2
      DOUBLE PRECISION B_CHI2       ! Y  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)

C - IO stream common block
C -------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C- Local variables
C ------------------
      INTEGER IXX,JXX
      INTEGER THE_STREAM
      LOGICAL WRITING,AT_BOTTOM
      CHARACTER*80 TMP_STRING
      
      WRITING=.TRUE.

      IF(THE_STREAM.EQ.A_STREAM) THEN
         DO IXX=1,A_NHISTOGRAMS
            JXX=1
            DO WHILE(A_X(JXX,IXX).NE.-9.99D+30)
               WRITE(C_STREAM,
     $              "(ES15.8,'  ',ES15.8,'  ',ES15.8,' ( CHI2')") 
     $              A_X(JXX,IXX),A_CHI2(JXX,IXX),0.0
               JXX=JXX+1
            ENDDO
            WRITING=.TRUE.
            JXX=1
            DO WHILE((WRITING.EQV..TRUE.).OR.A_FOOTERS(JXX,IXX).NE.'')
               IF(A_FOOTERS(JXX,IXX).NE.'') WRITING=.FALSE.
               TMP_STRING=''
               CALL REMOVE_WHITE_SPACES(A_FOOTERS(JXX,IXX),TMP_STRING)
               IF(INDEX(TMP_STRING,'(TAGID').EQ.0) THEN
                  WRITE(C_STREAM,*) TRIM(A_FOOTERS(JXX,IXX))//' ( CHI2'
               ENDIF
               JXX=JXX+1
            ENDDO
         ENDDO
      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         DO IXX=1,B_NHISTOGRAMS
            JXX=1
            DO WHILE(B_X(JXX,IXX).NE.-9.99D+30)
               WRITE(C_STREAM,
     $              "(ES15.8,'  ',ES15.8,'  ',ES15.8,' ( CHI2')") 
     $              B_X(JXX,IXX),B_CHI2(JXX,IXX),0.0
               JXX=JXX+1
            ENDDO
            WRITING=.TRUE.
            JXX=1
            DO WHILE((WRITING.EQV..TRUE.).OR.B_FOOTERS(JXX,IXX).NE.'')
               IF(B_FOOTERS(JXX,IXX).NE.'') WRITING=.FALSE.
               TMP_STRING=''
               CALL REMOVE_WHITE_SPACES(B_FOOTERS(JXX,IXX),TMP_STRING)
               IF(INDEX(TMP_STRING,'(TAGID').EQ.0) THEN
                  WRITE(C_STREAM,*) TRIM(B_FOOTERS(JXX,IXX))//' ( CHI2'
               ENDIF
               JXX=JXX+1
            ENDDO
         ENDDO
      ENDIF

      RETURN
      END


C *********************************************************************** C
      SUBROUTINE IDENTIFY_PLOTS(THE_STREAM)
C     Looks for a 'tag' indicating the origin of the data in each 
C     histogram and if it can't find one it assigns one on the basis
C     of the filename and the order of the histogram in the plot in
C     said file.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - --------------------------------------------------------------
C - A limit on the maximum number of bins to read in a histogram
C - and the number of lines of waffle to read before reading data.
C - Also a limit on the number of histograms per plot to read from
C - either file.
C - --------------------------------------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - --------------------------------------------------------------
C - Variables to hold onto the data as it's input output and 
C - getting manipulated
C - --------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! X  values in chi2
      DOUBLE PRECISION B_CHI2       ! Y  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)

C - --------------------------------------------------------------
C - Input and output stream indices & status codes
C - --------------------------------------------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C - Colours for the histograms
C -----------------------------
      CHARACTER*12 COLOURS
      COMMON/THE_COLOURS/COLOURS(7)

C - Local variables
C -------------------
      INTEGER IXX,JXX,ITMP,THE_STREAM
      CHARACTER*80 TMP_STRING1,TMP_STRING2
      CHARACTER*80 STR_XPOS,STR_YPOS
      CHARACTER*80 THE_TAG
      CHARACTER*12  THE_COLOUR
      LOGICAL NOT_FOUND_TAG
      REAL XPOS,YPOS

      IF(THE_STREAM.EQ.A_STREAM) THEN

         DO IXX=1,A_NHISTOGRAMS
            NOT_FOUND_TAG=.TRUE.
            JXX=1
            DO WHILE((JXX.LT.NMAX_HEADER).AND.NOT_FOUND_TAG)
               JXX=JXX+1
               TMP_STRING1=A_FOOTERS(JXX,IXX)
               CALL REMOVE_WHITE_SPACES(TMP_STRING1,TMP_STRING2)
               IF(INDEX(TMP_STRING2,"TAGID0").NE.0) THEN
                  THE_TAG=TMP_STRING1
                  NOT_FOUND_TAG=.FALSE.
               ENDIF
            ENDDO

C - Calculate position of label for histogram on plot:
            XPOS=8.00
            YPOS=8.90-IXX*0.2
            WRITE(STR_XPOS,'(F6.3)') XPOS
            WRITE(STR_YPOS,'(F6.3)') YPOS

C - Set the histogram colour
            THE_COLOUR=COLOURS(IXX)

C - If a tag was found then we reposition & recolour it but we keep the name
            IF((.NOT.NOT_FOUND_TAG)) THEN
               THE_TAG=ADJUSTL(A_FOOTERS(JXX+1,IXX))
               ITMP=SCAN(THE_TAG,'"')+1
               TMP_STRING1=ADJUSTL(TRIM(THE_TAG(ITMP:)))
               ITMP=SCAN(TMP_STRING1,'"')-1
               TMP_STRING1=ADJUSTL(TRIM(TMP_STRING1(1:ITMP)))
               A_FOOTERS(JXX+1,IXX) =
     $              'TITLE TEXT '//TRIM(STR_XPOS)//TRIM(STR_YPOS)
               A_FOOTERS(JXX+1,IXX) = 
     $              TRIM(A_FOOTERS(JXX+1,IXX))//' "'//TRIM(TMP_STRING1)
               A_FOOTERS(JXX+1,IXX) =
     $              TRIM(A_FOOTERS(JXX+1,IXX))//'" '//
     $              THE_COLOUR(1:SCAN(THE_COLOUR," "))
               A_FOOTERS(JXX+1,IXX) = 
     $              TRIM(A_FOOTERS(JXX+1,IXX))//' ( TAG ID 1'
            ENDIF

C - If a tag wasn't found then we invent one, position it and colour it:
            IF(NOT_FOUND_TAG) THEN
               ITMP=IXX
               TMP_STRING1=''
               JXX=NMAX_HEADER+1
               DO WHILE(JXX.GT.1.AND.TMP_STRING1.EQ.'') 
                  JXX=JXX-1
                  TMP_STRING1=A_FOOTERS(JXX,IXX)
               ENDDO
               JXX=JXX+1        ! Should be the 1st blank line after footer
               WRITE(TMP_STRING1,'(I2)') ITMP
               WRITE(TMP_STRING2,'(I2)') THE_STREAM
               A_FOOTERS(JXX,IXX)='SET TITLE SIZE 1.2 ( TAG ID 0'
               A_FOOTERS(JXX+1,IXX) =
     $              'TITLE TEXT '//TRIM(STR_XPOS)//TRIM(STR_YPOS)
               A_FOOTERS(JXX+1,IXX) = 
     $              TRIM(A_FOOTERS(JXX+1,IXX))//' "'//TRIM(A_FILE)
               A_FOOTERS(JXX+1,IXX) =
     $              TRIM(A_FOOTERS(JXX+1,IXX))//'" '//
     $              THE_COLOUR(1:SCAN(THE_COLOUR," "))
               A_FOOTERS(JXX+1,IXX) =
     $              TRIM(A_FOOTERS(JXX+1,IXX))//' ( TAG ID 1'
            ENDIF
C - Here we put a line, the same colour as the histogram, beside the tag
C - for the current histogram appearing in the plot
            XPOS=7.20
            YPOS=8.92-IXX*0.2
            WRITE(STR_XPOS,'(F6.3)') XPOS
            WRITE(STR_YPOS,'(F6.3)') YPOS
            A_FOOTERS(JXX+2,IXX) = 
     $           TRIM(STR_XPOS)//' '//TRIM(STR_YPOS)//' ( TAG ID 2'
            XPOS=7.80
            YPOS=8.92-IXX*0.2
            WRITE(STR_XPOS,'(F6.3)') XPOS
            WRITE(STR_YPOS,'(F6.3)') YPOS
            A_FOOTERS(JXX+3,IXX) = 
     $           TRIM(STR_XPOS)//' '//TRIM(STR_YPOS)//' ( TAG ID 3'
            A_FOOTERS(JXX+4,IXX) = 
     $           'JOIN TEXT '//TRIM(THE_COLOUR)//' ( TAG ID 4'
         ENDDO
      
      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         
         DO IXX=1,B_NHISTOGRAMS
            NOT_FOUND_TAG=.TRUE.
            JXX=1
            DO WHILE((JXX.LT.NMAX_HEADER).AND.NOT_FOUND_TAG)
               JXX=JXX+1
               TMP_STRING1=B_FOOTERS(JXX,IXX)
               CALL REMOVE_WHITE_SPACES(TMP_STRING1,TMP_STRING2)
               IF(INDEX(TMP_STRING2,"TAGID0").NE.0) THEN
                  THE_TAG=TMP_STRING1
                  NOT_FOUND_TAG=.FALSE.
               ENDIF
            ENDDO

C - Calculate position of label for histogram on plot:
            XPOS=8.00
            YPOS=8.90-(IXX+A_NHISTOGRAMS)*0.2
            WRITE(STR_XPOS,'(F6.3)') XPOS
            WRITE(STR_YPOS,'(F6.3)') YPOS

C - Set the histogram colour
            THE_COLOUR=COLOURS(IXX+A_NHISTOGRAMS)

C - If a tag was found then we reposition & recolour it but we keep the name:
            IF((.NOT.NOT_FOUND_TAG)) THEN
               THE_TAG=ADJUSTL(B_FOOTERS(JXX+1,IXX))
               ITMP=SCAN(THE_TAG,'"')+1
               TMP_STRING1=ADJUSTL(TRIM(THE_TAG(ITMP:)))
               ITMP=SCAN(TMP_STRING1,'"')-1
               TMP_STRING1=ADJUSTL(TRIM(TMP_STRING1(1:ITMP)))
               B_FOOTERS(JXX+1,IXX) =
     $              'TITLE TEXT '//TRIM(STR_XPOS)//TRIM(STR_YPOS)
               B_FOOTERS(JXX+1,IXX) = 
     $              TRIM(B_FOOTERS(JXX+1,IXX))//' "'//TRIM(TMP_STRING1)
               B_FOOTERS(JXX+1,IXX) =
     $              TRIM(B_FOOTERS(JXX+1,IXX))//'" '//
     $              THE_COLOUR(1:SCAN(THE_COLOUR," "))
               B_FOOTERS(JXX+1,IXX) = 
     $              TRIM(B_FOOTERS(JXX+1,IXX))//' ( TAG ID 1'
            ENDIF

C - If a tag wasn't found then we invent one, position it and colour it:
            IF(NOT_FOUND_TAG) THEN
               ITMP=IXX
               TMP_STRING1=''
               JXX=NMAX_HEADER+1
               DO WHILE(JXX.GT.1.AND.TMP_STRING1.EQ.'') 
                  JXX=JXX-1
                  TMP_STRING1=B_FOOTERS(JXX,IXX)
               ENDDO
               JXX=JXX+1        ! Should be the 1st blank line after footer
               WRITE(TMP_STRING1,'(I2)') ITMP
               WRITE(TMP_STRING2,'(I2)') THE_STREAM
               B_FOOTERS(JXX,IXX)='SET TITLE SIZE 1.2 ( TAG ID 0'
               B_FOOTERS(JXX+1,IXX) =
     $              'TITLE TEXT '//TRIM(STR_XPOS)//TRIM(STR_YPOS)
               B_FOOTERS(JXX+1,IXX) = 
     $              TRIM(B_FOOTERS(JXX+1,IXX))//' "'//TRIM(B_FILE)
               B_FOOTERS(JXX+1,IXX) =
     $              TRIM(B_FOOTERS(JXX+1,IXX))//'" '//
     $              THE_COLOUR(1:SCAN(THE_COLOUR," "))
               B_FOOTERS(JXX+1,IXX) =
     $              TRIM(B_FOOTERS(JXX+1,IXX))//' ( TAG ID 1'
            ENDIF
C - Here we put a line, the same colour as the histogram, beside the tag
C - for the current histogram appearing in the plot
            XPOS=7.20
            YPOS=8.92-(IXX+A_NHISTOGRAMS)*0.2
            WRITE(STR_XPOS,'(F6.3)') XPOS
            WRITE(STR_YPOS,'(F6.3)') YPOS
            B_FOOTERS(JXX+2,IXX) = 
     $           TRIM(STR_XPOS)//' '//TRIM(STR_YPOS)//' ( TAG ID 2'
            XPOS=7.80
            YPOS=8.92-(IXX+A_NHISTOGRAMS)*0.2
            WRITE(STR_XPOS,'(F6.3)') XPOS
            WRITE(STR_YPOS,'(F6.3)') YPOS
            B_FOOTERS(JXX+3,IXX) = 
     $           TRIM(STR_XPOS)//' '//TRIM(STR_YPOS)//' ( TAG ID 3'
            B_FOOTERS(JXX+4,IXX) = 
     $           'JOIN TEXT '//TRIM(THE_COLOUR)//' ( TAG ID 4'
         ENDDO

      ENDIF


C - Now change the histogram colours to match the corresponding tag colours:
      IF(THE_STREAM.EQ.A_STREAM) THEN

         DO IXX=1,A_NHISTOGRAMS
C - Set the histogram colour
            THE_COLOUR=COLOURS(IXX)
            JXX=1
            DO WHILE(JXX.LT.NMAX_HEADER)
               JXX=JXX+1
               TMP_STRING1=A_FOOTERS(JXX,IXX)
               TMP_STRING2=''
               CALL REMOVE_WHITE_SPACES(TMP_STRING1,TMP_STRING2)
               IF(INDEX(TMP_STRING2,"HIST").NE.0) THEN
                  A_FOOTERS(JXX,IXX)='HIST '//THE_COLOUR
               ELSEIF((INDEX(TMP_STRING2,"JOIN").NE.0).AND.
     $                (INDEX(TMP_STRING2,"TEXT").EQ.0)) THEN
                  A_FOOTERS(JXX,IXX)='JOIN '//THE_COLOUR
               ELSEIF((INDEX(TMP_STRING2,"PLOT").NE.0).AND.
     $                (INDEX(TMP_STRING2,"NEW").EQ.0)) THEN
                  A_FOOTERS(JXX,IXX)='PLOT '//THE_COLOUR
               ENDIF
            ENDDO   
         ENDDO

      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         
         DO IXX=1,B_NHISTOGRAMS
C - Set the histogram colour:
            THE_COLOUR=COLOURS(IXX+A_NHISTOGRAMS)
            JXX=1
            DO WHILE(JXX.LT.NMAX_HEADER)
               JXX=JXX+1
               TMP_STRING1=B_FOOTERS(JXX,IXX)
               TMP_STRING2=''
               CALL REMOVE_WHITE_SPACES(TMP_STRING1,TMP_STRING2)
               IF(INDEX(TMP_STRING2,"HIST").NE.0) THEN
                  B_FOOTERS(JXX,IXX)='HIST '//THE_COLOUR
               ELSEIF((INDEX(TMP_STRING2,"JOIN").NE.0).AND.
     $                (INDEX(TMP_STRING2,"TEXT").EQ.0)) THEN
                  B_FOOTERS(JXX,IXX)='JOIN '//THE_COLOUR
               ELSEIF((INDEX(TMP_STRING2,"PLOT").NE.0).AND.
     $                (INDEX(TMP_STRING2,"NEW").EQ.0)) THEN
                  B_FOOTERS(JXX,IXX)='PLOT '//THE_COLOUR
               ENDIF
            ENDDO
         ENDDO

      ENDIF
      
      RETURN
      END


C *********************************************************************** C
      SUBROUTINE COMPUTE_CHI2()
C     Computes the chi^2 per bin of each histogram using the first 
C     histogram in file A as the reference.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - --------------------------------------------------------------
C - A limit on the maximum number of bins to read in a histogram
C - and the number of lines of waffle to read before reading data.
C - Also a limit on the number of histograms per plot to read from
C - either file.
C - --------------------------------------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - --------------------------------------------------------------
C - Variables to hold onto the data as it's input output and 
C - getting manipulated
C - --------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! A  values in chi2
      DOUBLE PRECISION B_CHI2       ! B  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)


C - Local variables
C -------------------
      INTEGER IXX,JXX

      DO IXX=2,A_NHISTOGRAMS
         DO JXX=1,NMAX_BINS
            IF((A_DY(JXX,IXX).GT.0.0).OR.(A_DY(JXX,1).GT.0.0)) THEN
               A_CHI2(JXX,IXX)=(A_Y(JXX,IXX)-A_Y(JXX,1))/
     $                         SQRT(A_DY(JXX,IXX)**2+A_DY(JXX,1)**2)
            ELSE
               A_CHI2(JXX,IXX)=-9.99D+2
            ENDIF
         ENDDO
      ENDDO
      DO IXX=1,B_NHISTOGRAMS
         DO JXX=1,NMAX_BINS
            IF((B_DY(JXX,IXX).GT.0.0).OR.(B_DY(JXX,1).GT.0.0)) THEN
               B_CHI2(JXX,IXX)=(B_Y(JXX,IXX)-A_Y(JXX,1))/
     $                         SQRT(B_DY(JXX,IXX)**2+A_DY(JXX,1)**2)
            ELSE
               B_CHI2(JXX,IXX)=-9.99D+2
            ENDIF
         ENDDO
      ENDDO



      RETURN
      END


C *********************************************************************** C
      SUBROUTINE COMPUTE_FRAC_DIFF()
C     Computes the fractional difference per bin of each histogram using
C     the first histogram in file A as the reference.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - --------------------------------------------------------------
C - A limit on the maximum number of bins to read in a histogram
C - and the number of lines of waffle to read before reading data.
C - Also a limit on the number of histograms per plot to read from
C - either file.
C - --------------------------------------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - --------------------------------------------------------------
C - Variables to hold onto the data as it's input output and 
C - getting manipulated
C - --------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! A  values in chi2
      DOUBLE PRECISION B_CHI2       ! B  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)


C - Local variables
C -------------------
      INTEGER IXX,JXX

      DO IXX=2,A_NHISTOGRAMS
         DO JXX=1,NMAX_BINS
            IF(A_Y(JXX,1).NE.0.0) THEN
               A_FRAC_DIFF(JXX,IXX)=(A_Y(JXX,IXX)-A_Y(JXX,1))/A_Y(JXX,1)
            ELSE
               A_FRAC_DIFF(JXX,IXX)=-9.99D+2
            ENDIF
         ENDDO
      ENDDO
      DO IXX=1,B_NHISTOGRAMS
         DO JXX=1,NMAX_BINS
            IF(B_Y(JXX,IXX).GT.0.0) THEN
               B_FRAC_DIFF(JXX,IXX)=(B_Y(JXX,IXX)-A_Y(JXX,1))/A_Y(JXX,1)
            ELSE
               B_FRAC_DIFF(JXX,IXX)=-9.99D+2
            ENDIF
         ENDDO
      ENDDO



      RETURN
      END


C *********************************************************************** C
      SUBROUTINE WRITE_DELTA_SIG_HEADER(THE_HEADER,YMIN,YMAX)
C     Computes the fractional difference per bin of each histogram using
C     the first histogram in file A as the reference.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - --------------------------------------------------------------
C - A limit on the maximum number of bins to read in a histogram
C - and the number of lines of waffle to read before reading data.
C - Also a limit on the number of histograms per plot to read from
C - either file.
C - --------------------------------------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

C ************* C
C COMMON BLOCKS C
C ************* C

C - --------------------------------------------------------------
C - Variables to hold onto the data as it's input output and 
C - getting manipulated
C - --------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! A  values in chi2
      DOUBLE PRECISION B_CHI2       ! B  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)


C - --------------------------------------------------------------
C - Input and output stream indices & status codes
C - --------------------------------------------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE


C - Local variables
C -------------------
      INTEGER          JXX,KXX,LXX
      LOGICAL          READING,AT_BOTTOM
      CHARACTER*80     TMP_STRING0,TMP_STRING1,TMP_STRING2
      DOUBLE PRECISION YMIN,YMAX

      IF(MODE.EQ.'0') THEN
         WRITE(C_STREAM,*) 'SET WINDOW Y 1.6 TO  2.8 ( CHI2'
      ELSE
         WRITE(C_STREAM,*) 'SET WINDOW Y 1.5 TO  2.8 ( CHI2'
      ENDIF
      WRITE(C_STREAM,*) 'SET WINDOW X 2.5 TO 10.0 ( CHI2'
      WRITE(C_STREAM,*) 'SET FONT DUPLEX          ( CHI2'
      WRITE(C_STREAM,*) 'SET TITLE ALL SCALE 1.0  ( CHI2'
      WRITE(C_STREAM,*) 'SET TITLE SIZE 2.5       ( CHI2'
      WRITE(C_STREAM,*) 'SET TITLE TOP SCALE 1.2  ( CHI2'
      WRITE(C_STREAM,*) 'SET LABEL CHARACTERS 5   ( CHI2'
      WRITE(C_STREAM,*) 'SET LABELS SIZE 2.4      ( CHI2'
      WRITE(C_STREAM,*) 'SET AXES BOTTOM ON       ( CHI2'
      WRITE(C_STREAM,*) 'SET TICKS TOP ON         ( CHI2'
      WRITE(C_STREAM,*) 'TITLE LEFT ','"','DS/DDS','"    ( CHI2'
      WRITE(C_STREAM,*) 'CASE       ','"','FG GFG','"    ( CHI2'
      WRITE(C_STREAM,*) 'SET SCALE Y LIN          ( CHI2'
      WRITE(C_STREAM,*) 'SET SYMBOL SIZE 1.3      ( CHI2'
      WRITE(C_STREAM,*) '(SET TICKS TOP OFF)      ( CHI2'


      READING=.TRUE.
      JXX=0
      DO WHILE(READING.AND.(JXX.LT.NMAX_HEADER))
         JXX=JXX+1
         TMP_STRING0=''
         CALL REMOVE_WHITE_SPACES(THE_HEADER(JXX),TMP_STRING0)
         IF(INDEX(TMP_STRING0,"TITLEBOTTOM").NE.0) THEN
            READING=.FALSE.
         ENDIF
      ENDDO
      KXX=SCAN(THE_HEADER(JXX),'(')+1
      WRITE(C_STREAM,*) TRIM(THE_HEADER(JXX)(KXX:))//' ( CHI2'


      DO JXX=1,NMAX_HEADER
         TMP_STRING0=''
         CALL REMOVE_WHITE_SPACES(THE_HEADER(JXX),TMP_STRING0)
         IF(INDEX(TMP_STRING0,'SETLIMITSX').NE.0) THEN
            KXX=(INDEX(THE_HEADER(JXX)," X ")+3)
            TMP_STRING0=ADJUSTL(THE_HEADER(JXX)(KXX:))
            KXX=INDEX(TMP_STRING0," ")
            TMP_STRING1=ADJUSTL(TMP_STRING0(1:KXX))              
            TMP_STRING2=ADJUSTL(TMP_STRING0(KXX:))
         ENDIF
      ENDDO
      WRITE(C_STREAM,*) 'SET LIMITS X ',
     $     TRIM(TMP_STRING1),' ',TRIM(TMP_STRING2),
     $     ' ( CHI2'
      WRITE(C_STREAM,*) 'SET LIMITS Y ',1.2*YMIN,1.2*YMAX,' ( CHI2'
      WRITE(C_STREAM,*) 'SET ORDER X Y DY        ( CHI2'
      
      RETURN
      END

C *********************************************************************** C
      SUBROUTINE WRITE_FRAC_DIFF_HEADER(THE_HEADER,YMIN,YMAX)
C     Writes the header for the fractional difference plot.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C - --------------------------------------------------------------
C - A limit on the maximum number of bins to read in a histogram
C - and the number of lines of waffle to read before reading data.
C - Also a limit on the number of histograms per plot to read from
C - either file.
C - --------------------------------------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

      CHARACTER*80 THE_HEADER(NMAX_HEADER)

C ************* C
C COMMON BLOCKS C
C ************* C

C - --------------------------------------------------------------
C - Variables to hold onto the data as it's input output and 
C - getting manipulated
C - --------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! A  values in chi2
      DOUBLE PRECISION B_CHI2       ! B  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)


C - --------------------------------------------------------------
C - Input and output stream indices & status codes
C - --------------------------------------------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C - Local variables
C -------------------
      INTEGER          JXX,KXX,LXX
      LOGICAL          READING,AT_BOTTOM
      CHARACTER*80     TMP_STRING0,TMP_STRING1,TMP_STRING2
      DOUBLE PRECISION YMIN,YMAX

      IF(MODE.EQ.'0') THEN
         WRITE(C_STREAM,*) 'SET WINDOW Y 2.8 TO  4.0 ( FDIFF'
      ELSE
         WRITE(C_STREAM,*) 'SET WINDOW Y 1.5 TO  2.8 ( FDIFF'
      ENDIF
      WRITE(C_STREAM,*) 'SET WINDOW X 2.5 TO 10.0 ( FDIFF'
      WRITE(C_STREAM,*) 'SET FONT DUPLEX          ( FDIFF'
      WRITE(C_STREAM,*) 'SET TITLE ALL SCALE 1.0  ( FDIFF'
      WRITE(C_STREAM,*) 'SET TITLE SIZE 2.5       ( FDIFF'
      WRITE(C_STREAM,*) 'SET TITLE TOP SCALE 1.2  ( FDIFF'
      WRITE(C_STREAM,*) 'SET LABEL CHARACTERS 5   ( FDIFF'
      WRITE(C_STREAM,*) 'SET LABELS SIZE 2.4      ( FDIFF'
      IF(MODE.EQ.'0') THEN
         WRITE(C_STREAM,*) 'SET AXES BOTTOM OFF       ( FDIFF'
      ELSE
         WRITE(C_STREAM,*) 'SET AXES BOTTOM ON        ( FDIFF'
      ENDIF
      WRITE(C_STREAM,*) 'SET TICKS TOP ON         ( FDIFF'
      WRITE(C_STREAM,*) 'TITLE LEFT ','"','DS/S','"    ( FDIFF'
      WRITE(C_STREAM,*) 'CASE       ','"','FG G','"    ( FDIFF'
      WRITE(C_STREAM,*) 'SET SCALE Y LIN          ( FDIFF'
      WRITE(C_STREAM,*) 'SET SYMBOL SIZE 1.3      ( FDIFF'
      WRITE(C_STREAM,*) '(SET TICKS TOP OFF)      ( FDIFF'


      READING=.TRUE.
      JXX=0
      DO WHILE(READING.AND.(JXX.LT.NMAX_HEADER))
         JXX=JXX+1
         TMP_STRING0=''
         CALL REMOVE_WHITE_SPACES(THE_HEADER(JXX),TMP_STRING0)
         IF(INDEX(TMP_STRING0,"TITLEBOTTOM").NE.0) THEN
            READING=.FALSE.
         ENDIF
      ENDDO
      KXX=SCAN(THE_HEADER(JXX),'(')+1
      IF(MODE.NE.'0') THEN
         WRITE(C_STREAM,*) TRIM(THE_HEADER(JXX)(KXX:))//' ( FDIFF'
      ENDIF


      DO JXX=1,NMAX_HEADER
         TMP_STRING0=''
         CALL REMOVE_WHITE_SPACES(THE_HEADER(JXX),TMP_STRING0)
         IF(INDEX(TMP_STRING0,'SETLIMITSX').NE.0) THEN
            KXX=(INDEX(THE_HEADER(JXX)," X ")+3)
            TMP_STRING0=ADJUSTL(THE_HEADER(JXX)(KXX:))
            KXX=INDEX(TMP_STRING0," ")
            TMP_STRING1=ADJUSTL(TMP_STRING0(1:KXX))              
            TMP_STRING2=ADJUSTL(TMP_STRING0(KXX:))
         ENDIF
      ENDDO
      WRITE(C_STREAM,*) 'SET LIMITS X ',
     $     TRIM(TMP_STRING1),' ',TRIM(TMP_STRING2),
     $     ' ( FDIFF'
      WRITE(C_STREAM,*) 'SET LIMITS Y ',1.2*YMIN,1.2*YMAX,' ( FDIFF'
      WRITE(C_STREAM,*) 'SET ORDER X Y DY        ( FDIFF'
      
      RETURN
      END

C *********************************************************************** C
      SUBROUTINE WRITE_FRAC_DIFF_DATA(THE_STREAM)
C     Writes data for the fractional difference subplot.
C *********************************************************************** C

      IMPLICIT NONE
C -   *************

C **************** C
C ARRAY DIMENSIONS C
C **************** C

C - Dimensions of histogram arrays
C ---------------------------------
      INTEGER NMAX_BINS,NMAX_HEADER,NMAX_HISTS
      PARAMETER(NMAX_BINS=600)
      PARAMETER(NMAX_HEADER=100)
      PARAMETER(NMAX_HISTS=5)

C ************* C
C COMMON BLOCKS C
C ************* C

C - Plot common block to fill with the data and footers being read
C ----------------------------------------------------------------
      CHARACTER*80     A_HEADER
      CHARACTER*80     B_HEADER     ! Preamble  for A/B plot
      CHARACTER*80     A_FOOTERS
      CHARACTER*80     B_FOOTERS    ! Footers   for A/B plot
      INTEGER A_NHISTOGRAMS,B_NHISTOGRAMS ! Number of histograms in plot
      DOUBLE PRECISION A_X ,B_X     ! X  values in A/B histos
      DOUBLE PRECISION A_Y ,B_Y     ! Y  values in A/B histos
      DOUBLE PRECISION A_DY,B_DY    ! DY values in A/B histos
      DOUBLE PRECISION A_CHI2       ! X  values in chi2
      DOUBLE PRECISION B_CHI2       ! Y  values in chi2
      DOUBLE PRECISION A_FRAC_DIFF  ! A  fractional differences
      DOUBLE PRECISION B_FRAC_DIFF  ! B  fractional differences
      COMMON/THE_PLOT/
     $     A_HEADER(NMAX_HEADER),B_HEADER(NMAX_HEADER),
     $     A_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     B_FOOTERS(NMAX_HEADER,NMAX_HISTS),
     $     A_NHISTOGRAMS,B_NHISTOGRAMS,
     $     A_X(NMAX_BINS,NMAX_HISTS),A_Y(NMAX_BINS,NMAX_HISTS),
     $     A_DY(NMAX_BINS,NMAX_HISTS),
     $     B_X(NMAX_BINS,NMAX_HISTS),B_Y(NMAX_BINS,NMAX_HISTS),
     $     B_DY(NMAX_BINS,NMAX_HISTS),
     $     A_CHI2(NMAX_BINS,NMAX_HISTS),
     $     B_CHI2(NMAX_BINS,NMAX_HISTS),
     $     A_FRAC_DIFF(NMAX_BINS,NMAX_HISTS),
     $     B_FRAC_DIFF(NMAX_BINS,NMAX_HISTS)

C - IO stream common block
C -------------------------
      CHARACTER*80 A_FILE           ! Input file A
      CHARACTER*80 B_FILE           ! Input file B
      CHARACTER*80 C_FILE           ! Merged File A and File B
      CHARACTER*80 MODE             ! The mode (see above).
      INTEGER A_STREAM,B_STREAM,C_STREAM,STAT
      COMMON/THE_STREAMS/
     $     A_STREAM,B_STREAM,C_STREAM,MODE,STAT,
     $     A_FILE,B_FILE,C_FILE

C- Local variables
C ------------------
      INTEGER IXX,JXX
      INTEGER THE_STREAM
      LOGICAL WRITING,AT_BOTTOM
      CHARACTER*80 TMP_STRING
      
      WRITING=.TRUE.

      IF(THE_STREAM.EQ.A_STREAM) THEN
         DO IXX=1,A_NHISTOGRAMS
            JXX=1
            DO WHILE(A_X(JXX,IXX).NE.-9.99D+30)
               WRITE(C_STREAM,
     $              "(ES15.8,'  ',ES15.8,'  ',ES15.8,' ( FDIFF')") 
     $              A_X(JXX,IXX),A_FRAC_DIFF(JXX,IXX),0.0
               JXX=JXX+1
            ENDDO
            WRITING=.TRUE.
            JXX=1
            DO WHILE((WRITING.EQV..TRUE.).OR.A_FOOTERS(JXX,IXX).NE.'')
               IF(A_FOOTERS(JXX,IXX).NE.'') WRITING=.FALSE.
               TMP_STRING=''
               CALL REMOVE_WHITE_SPACES(A_FOOTERS(JXX,IXX),TMP_STRING)
               IF(INDEX(TMP_STRING,'(TAGID').EQ.0) THEN
                  WRITE(C_STREAM,*) TRIM(A_FOOTERS(JXX,IXX))//' ( FDIFF'
               ENDIF
               JXX=JXX+1
            ENDDO
         ENDDO
      ELSEIF(THE_STREAM.EQ.B_STREAM) THEN
         DO IXX=1,B_NHISTOGRAMS
            JXX=1
            DO WHILE(B_X(JXX,IXX).NE.-9.99D+30)
               WRITE(C_STREAM,
     $              "(ES15.8,'  ',ES15.8,'  ',ES15.8,' ( FDIFF')") 
     $              B_X(JXX,IXX),B_FRAC_DIFF(JXX,IXX),0.0
               JXX=JXX+1
            ENDDO
            WRITING=.TRUE.
            JXX=1
            DO WHILE((WRITING.EQV..TRUE.).OR.B_FOOTERS(JXX,IXX).NE.'')
               IF(B_FOOTERS(JXX,IXX).NE.'') WRITING=.FALSE.
               TMP_STRING=''
               CALL REMOVE_WHITE_SPACES(B_FOOTERS(JXX,IXX),TMP_STRING)
               IF(INDEX(TMP_STRING,'(TAGID').EQ.0) THEN
                  WRITE(C_STREAM,*) TRIM(B_FOOTERS(JXX,IXX))//' ( FDIFF'
               ENDIF
               JXX=JXX+1
            ENDDO
         ENDDO
      ENDIF

      RETURN
      END

