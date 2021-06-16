#include <qtforth.h>

#if ((ULONG_MAX) == (UINT_MAX))
# define ARCH32
#else
# define ARCH64
#endif

#define TIB_SIZE (256)

#ifndef FALSE
  #define FALSE (0)
#endif
#ifndef TRUE
  #define TRUE (1)
#endif

#define FFALSE (0)
#define FTRUE (-1)
#define BLANK (' ')
#define BACKSPACE  (8)

#define FLAG_PRECEDENCE (0x80)
#define FLAG_IMMEDIATE  (0x40)
#define FLAG_SMUDGE     (0x20)
#define MASK_NAME_SIZE  (0x1F)

/* Debug TRACE flags */
#define TRACE_INNER     (0x0002)
#define TRACE_COMPILE   (0x0004)
#define TRACE_SPECIAL   (0x0008)

/* Numeric types returned by NUMBER? */
#define NUM_TYPE_BAD    (0)
#define NUM_TYPE_SINGLE (1)
#define NUM_TYPE_DOUBLE (2)
#define NUM_TYPE_FLOAT  (3)

#define CREATE_BODY_OFFSET  (3*sizeof(int))

/* ANSI standard definitions needed by Forth */
#define THROW_ABORT            (-1)
#define THROW_ABORT_QUOTE      (-2)
#define THROW_STACK_OVERFLOW   (-3)
#define THROW_STACK_UNDERFLOW  (-4)
#define THROW_UNDEFINED_WORD  (-13)
#define THROW_EXECUTING       (-14)
#define THROW_PAIRS           (-22)
#define THROW_FLOAT_STACK_UNDERFLOW  ( -45)
#define THROW_QUIT            (-56)

/* THROW codes unique to nForth */
#define THROW_BYE            (-256) /* Exit program. */
#define THROW_SEMICOLON      (-257) /* Error detected at ; */
#define THROW_DEFERRED       (-258) /* Not a deferred word. Used in system.fth */

#define PF_ERR_INDEX_MASK         (0xFFFF)
#define PF_ERR_BASE               (0x80000000)
#define PF_ERR_NO_MEM             (PF_ERR_BASE |  0)
#define PF_ERR_TOO_BIG            (PF_ERR_BASE |  2)
#define PF_ERR_NUM_PARAMS         (PF_ERR_BASE |  3)
#define PF_ERR_OPEN_FILE          (PF_ERR_BASE |  4)
#define PF_ERR_WRONG_FILE         (PF_ERR_BASE |  5)
#define PF_ERR_BAD_FILE           (PF_ERR_BASE |  6)
#define PF_ERR_READ_FILE          (PF_ERR_BASE |  7)
#define PF_ERR_WRITE_FILE         (PF_ERR_BASE |  8)
#define PF_ERR_CORRUPT_DIC        (PF_ERR_BASE |  9)
#define PF_ERR_NOT_SUPPORTED      (PF_ERR_BASE | 10)
#define PF_ERR_VERSION_FUTURE     (PF_ERR_BASE | 11)
#define PF_ERR_VERSION_PAST       (PF_ERR_BASE | 12)
#define PF_ERR_COLON_STACK        (PF_ERR_BASE | 13)
#define PF_ERR_HEADER_ROOM        (PF_ERR_BASE | 14)
#define PF_ERR_CODE_ROOM          (PF_ERR_BASE | 15)
#define PF_ERR_NO_SHELL           (PF_ERR_BASE | 16)
#define PF_ERR_NO_NAMES           (PF_ERR_BASE | 17)
#define PF_ERR_OUT_OF_RANGE       (PF_ERR_BASE | 18)
#define PF_ERR_ENDIAN_CONFLICT    (PF_ERR_BASE | 19)
#define PF_ERR_FLOAT_CONFLICT     (PF_ERR_BASE | 20)
#define PF_ERR_CELL_SIZE_CONFLICT (PF_ERR_BASE | 21)
#define PF_ERR_NO_FUNCTION        (PF_ERR_BASE | 22)

#define PF_FAM_READ_ONLY   (0)
#define PF_FAM_READ_WRITE  (1)
#define PF_FAM_WRITE_ONLY  (2)
#define PF_FAM_BINARY_FLAG (8)

#define fp_acos   acos
#define fp_asin   asin
#define fp_atan   atan
#define fp_atan2  atan2
#define fp_cos    cos
#define fp_cosh   cosh
#define fp_fabs   fabs
#define fp_floor  floor
#define fp_log    log
#define fp_log10  log10
#define fp_pow    pow
#define fp_sin    sin
#define fp_sinh   sinh
#define fp_sqrt   sqrt
#define fp_tan    tan
#define fp_tanh   tanh

#define ABORT_RETURN_CODE   (10)
#define UINT32_MASK  ((sizeof(unsigned int)-1))

#define DEFAULT_RETURN_DEPTH (512)
#define DEFAULT_USER_DEPTH (512)
#define DEFAULT_HEADER_SIZE (120000)
#define DEFAULT_CODE_SIZE (300000)

#define DEPTH_AT_COLON_INVALID (-100)

#define WRITE_FLOAT_DIC(addr,data)  { *((double *)(addr)) = (double)(data); }
#define WRITE_CELL_DIC(addr,data)   { *((int *)(addr)) = (int)(data); }
#define WRITE_SHORT_DIC(addr,data)  { *((short *)(addr)) = (short)(data); }
#define READ_FLOAT_DIC(addr)        ( *((double *)(addr)) )
#define READ_CELL_DIC(addr)         ( *((const unsigned int *)(addr)) )
#define READ_SHORT_DIC(addr)        ( *((const unsigned short *)(addr)) )

#define HEADER_HERE     (Dictionary->dic_HeaderPtr.Cell)
#define CODE_HERE       (Dictionary->dic_CodePtr.Cell)
#define CODE_COMMA( N ) WRITE_CELL_DIC(CODE_HERE++,(N))
#define NAME_BASE       (Dictionary->dic_HeaderBase)
#define CODE_BASE       (Dictionary->dic_CodeBase  )
#define NAME_SIZE       (Dictionary->dic_HeaderLimit - Dictionary->dic_HeaderBase)
#define CODE_SIZE       (Dictionary->dic_CodeLimit   - Dictionary->dic_CodeBase  )

#define IN_CODE_DIC(addr) ( ( ((unsigned char *)(addr)) >= Dictionary->dic_CodeBase)   && ( ((unsigned char *)(addr)) < Dictionary->dic_CodeLimit) )

#define IN_NAME_DIC(addr) ( ( ((unsigned char *)(addr)) >= Dictionary->dic_HeaderBase) && ( ((unsigned char *)(addr)) < Dictionary->dic_HeaderLimit) )
#define IN_DICS(addr) (IN_CODE_DIC(addr) || IN_NAME_DIC(addr))

#ifdef ARCH32
#define AddressPointer unsigned int
#else
#define AddressPointer unsigned long long
#endif

/* Address conversion */
#define ABS_TO_NAMEREL( a ) ((int)  (((AddressPointer) a) - NAME_BASE ))
#define ABS_TO_CODEREL( a ) ((int)  (((AddressPointer) a) - CODE_BASE ))
#define NAMEREL_TO_ABS( a ) ((AddressPointer) (((AddressPointer) a) + NAME_BASE))
#define CODEREL_TO_ABS( a ) ((AddressPointer) (((AddressPointer) a) + CODE_BASE))

/* The check for >0 is only needed for CLONE testing. !!! */
#define IsTokenPrimitive(xt) ((xt<NumPrimitives) && (xt>=0))

#define DATA_STACK_DEPTH (TaskNow->td_StackBase - TaskNow->td_StackPtr)
#define DROP_DATA_STACK (TaskNow->td_StackPtr++)
#define POP_DATA_STACK (*(TaskNow->td_StackPtr++))
#define PUSH_DATA_STACK(x) {*(--(TaskNow->td_StackPtr)) = (int) x; }

/* Force Quad alignment. */
#define QUADUP(x) (((x)+3)&~3)

#ifndef TOUCH
  #define TOUCH(argument) ((void)argument)
#endif

/* Bits in sd_Flags */
#define SD_F_BIG_ENDIAN_DIC    (1<<0)

#ifndef MAKE_ID
#define MAKE_ID(a,b,c,d) ((((unsigned int)a)<<24)|(((unsigned int)b)<<16)|(((unsigned int)c)<<8)|((unsigned int)d))
#endif

#define ID_FORM MAKE_ID('F','O','R','M')
#define ID_P4TH MAKE_ID('P','4','T','H')
#define ID_P4DI MAKE_ID('P','4','D','I')
#define ID_P4NM MAKE_ID('P','4','N','M')
#define ID_P4CD MAKE_ID('P','4','C','D')
#define ID_BADF MAKE_ID('B','A','D','F')

#ifndef EVENUP
#define EVENUP(n) ((n+1)&(~1))
#endif

#define MAX_INCLUDE_DEPTH (16)
#define STACK_SAFETY  (8)

#define C_RETURNS_VOID (0)
#define C_RETURNS_VALUE (1)

#define DIC_SAFETY_MARGIN  (400)

#define SYSTEM_LOAD_FILE "system.fth"

/***************************************************************
** Macros for data stack access.
** TOS is cached in a register in Catch.
***************************************************************/

#define STKPTR   (DataStackPtr)
#define M_POP    (*(STKPTR++))
#define M_PUSH(n) {*(--(STKPTR)) = (int) (n);}
#define M_STACK(n) (STKPTR[n])

#define TOS      (TopOfStack)
#define PUSH_TOS M_PUSH(TOS)
#define M_DUP    PUSH_TOS;
#define M_DROP   { TOS = M_POP; }

/***************************************************************
** Macros for Floating Point stack access.
***************************************************************/
#define FP_STKPTR     (FloatStackPtr)
#define M_FP_SPZERO   (TaskNow->td_FloatStackBase)
#define M_FP_POP      (*(FP_STKPTR++))
#define M_FP_PUSH(n)  {*(--(FP_STKPTR)) = (double) (n);}
#define M_FP_STACK(n) (FP_STKPTR[n])

#define FP_TOS      (fpTopOfStack)
#define PUSH_FP_TOS M_FP_PUSH(FP_TOS)
#define M_FP_DUP    PUSH_FP_TOS;
#define M_FP_DROP   { FP_TOS = M_FP_POP; }

/***************************************************************
** Macros for return stack access.
***************************************************************/

#define TORPTR      (ReturnStackPtr)
#define M_R_DROP    {  TORPTR++;}
#define M_R_POP     (*(TORPTR++))
#define M_R_PICK(n) (  TORPTR[n])
#define M_R_PUSH(n) {*(--(TORPTR)) = (int) (n);}

/***************************************************************
** Misc Forth macros
***************************************************************/

#define M_BRANCH   { InsPtr = (int *) (((unsigned char *) InsPtr) + READ_CELL_DIC(InsPtr)); }

/* Cache top of data stack like in JForth. */
#define LOAD_REGISTERS                          \
    {                                           \
        STKPTR    = TaskNow->td_StackPtr      ; \
        TOS       = M_POP                     ; \
        FP_STKPTR = TaskNow->td_FloatStackPtr ; \
        FP_TOS    = M_FP_POP                  ; \
        TORPTR    = TaskNow->td_ReturnPtr     ; \
    }

#define SAVE_REGISTERS                          \
    {                                           \
        TaskNow->td_ReturnPtr     = TORPTR    ; \
        M_PUSH( TOS )                         ; \
        TaskNow->td_StackPtr      = STKPTR    ; \
        M_FP_PUSH( FP_TOS )                   ; \
        TaskNow->td_FloatStackPtr = FP_STKPTR ; \
    }

#define M_DOTS       \
    SAVE_REGISTERS ; \
    DotS ( )       ; \
    LOAD_REGISTERS ;

#define DO_VAR(varname) { PUSH_TOS; TOS = (int) &varname; }

#define M_THROW(err)                     \
    {                                    \
      ExceptionReturnCode = (int)(err) ; \
      TORPTR    = InitialReturnStack   ; \
      STKPTR    = InitialDataStack     ; \
      FP_STKPTR = InitialFloatStack    ; \
    }

#define BINARY_OP( op ) { TOS = M_POP op TOS; }

#define FP_DHI1  (((double)0x40000000)*4.0)

/* Assume 8-bit char and calculate cell width. */
#define NBITS ((sizeof(unsigned int)) * 8)

/* Define half the number of bits in a cell. */
#define HNBITS (NBITS / 2)

/* Assume two-complement arithmetic to calculate lower half. */
#define LOWER_HALF(n) ((n) & (((unsigned int)1 << HNBITS) - 1))
#define HIGH_BIT ((unsigned int)1 << (NBITS - 1))

#define LOCAL_CODEREL_TO_ABS( a ) ((int *) (((int) a) + CodeBase))

#define TRACENAMES                                                \
    if ( ( VarTraceLevel > Level ) )                              \
    { SAVE_REGISTERS; TraceNames( Token, Level ); LOAD_REGISTERS; }

///////////////////////////////////////////////////////////////////////////////

#define HEADERPTR  (0x00003D64)
#define RELCONTEXT  (0x00003D5C)
#define CODEPTR  (0x000093E8)
#define IF_LITTLE_ENDIAN  (0x00000001)

extern unsigned char MinDicNames [] ;
extern unsigned char MinDicCode  [] ;

///////////////////////////////////////////////////////////////////////////////

typedef struct pfTaskData_s
{
  int     * td_StackPtr        ; /* Primary data stack */
  int     * td_StackBase       ;
  int     * td_StackLimit      ;
  int     * td_ReturnPtr       ; /* Return stack */
  int     * td_ReturnBase      ;
  int     * td_ReturnLimit     ;
  double  * td_FloatStackPtr   ;
  double  * td_FloatStackBase  ;
  double  * td_FloatStackLimit ;
  int     * td_InsPtr          ; /* Instruction pointer, "PC" */
  char      td_TIB[TIB_SIZE]   ; /* Buffer for terminal input. */
  int       td_IN              ; /* Index into Source */
  int       td_SourceNum       ; /* #TIB after REFILL */
  char    * td_SourcePtr       ; /* Pointer to TIB or other source. */
  int       td_LineNumber      ; /* Incremented on every refill. */
  int       td_OUT             ; /* Current output column. */
  N::File * td_InputStream     ;
} pfTaskData_t                 ;

typedef struct pfNode
{
  struct pfNode * n_Next ;
  struct pfNode * n_Prev ;
} pfNode                 ;

/* Structure of header entry in dictionary. These will be stored in dictionary specific endian format*/
typedef struct cfNameLinks
{
  int          cfnl_PreviousName ; /* name relative address of previous */
  unsigned int cfnl_ExecToken    ; /* Execution token for word. */
  /* Followed by variable length name field. */
} cfNameLinks                    ;

#define PF_DICF_ALLOCATED_SEGMENTS  ( 0x0001)
typedef struct pfDictionary_s
{
  pfNode       dic_Node                ;
  unsigned int dic_Flags               ;
  /* Headers contain pointers to names and dictionary. */
  unsigned int dic_HeaderBaseUnaligned ;
  unsigned int dic_HeaderBase          ;
  unsigned int dic_HeaderPtr           ;
  unsigned int dic_HeaderLimit         ;
  /* Code segment contains tokenized code and data. */
  unsigned int dic_CodeBaseUnaligned   ;
  unsigned int dic_CodeBase            ;
  union                                {
    int           * Cell               ;
    unsigned char * Byte               ;
  } dic_CodePtr                        ;
  unsigned int dic_CodeLimit           ;
} pfDictionary_t                       ;

/* Save state of include when nesting files. */
typedef struct IncludeFrame        {
  N::File * inf_File               ;
  int       inf_LineNumber         ;
  int       inf_SourceNum          ;
  int       inf_IN                 ;
  char      inf_SaveTIB [TIB_SIZE] ;
} IncludeFrame                     ;

/* All fields are stored in BIG ENDIAN format for consistency in data files.
 * All fields must be the same size for easy endian conversion.
 * All fields must be 32 bit for file compatibility with older versions.
 */
typedef struct DictionaryInfoChunk
{
  int          sd_Version         ;
  int          sd_RelContext      ; /* relative ptr to Dictionary Context */
  int          sd_RelHeaderPtr    ; /* relative ptr to Dictionary Header Ptr */
  int          sd_RelCodePtr      ; /* relative ptr to Dictionary Header Ptr */
  int          sd_EntryPoint      ; /* relative ptr to entry point or NULL */
  int          sd_UserStackSize   ; /* in bytes */
  int          sd_ReturnStackSize ; /* in bytes */
  int          sd_NameSize        ; /* in bytes */
  int          sd_CodeSize        ; /* in bytes */
  int          sd_NumPrimitives   ; /* To distinguish between primitive and secondary. */
  unsigned int sd_Flags           ;
  int          sd_FloatSize       ; /* In bytes. Must match code. 0 means no floats. */
  int          sd_CellSize        ; /* In bytes. Must match code. */
} DictionaryInfoChunk             ;

///////////////////////////////////////////////////////////////////////////////

typedef int (*CFunc1)(int) ;
typedef int (*CFunc2)(int,int) ;
typedef int (*CFunc3)(int,int,int) ;
typedef int (*CFunc4)(int,int,int,int) ;
typedef int (*CFunc5)(int,int,int,int,int) ;
typedef int (*CFunc6)(int,int,int,int,int,int) ;
typedef int (*CFunc7)(int,int,int,int,int,int,int) ;
typedef int (*CFunc8)(int,int,int,int,int,int,int,int) ;
typedef int (*CFunc9)(int,int,int,int,int,int,int,int,int) ;

///////////////////////////////////////////////////////////////////////////////

#define IncludeStk ( (IncludeFrame   *)IncludeStack      )
#define TaskNow    ( (pfTaskData_t   *)CurrentTask       )
#define Dictionary ( (pfDictionary_t *)CurrentDictionary )

///////////////////////////////////////////////////////////////////////////////

N::Forth:: Forth             ( void  )
         : uuid              ( 0     )
         , Name              ( ""    )
         , Traditional       ( false )
         , Radix             ( 10    )
         , NumPrimitives     ( 0     )
         , LocalCompilerXT   ( 0     )
         , VarContext        ( 0     ) /* Points to last name field. */
         , VarState          ( 0     ) /* 1 if compiling. */
         , VarEcho           ( 1     ) /* Echo input. */
         , VarTraceLevel     ( 0     ) /* Trace Level for Inner Interpreter. */
         , VarTraceFlags     ( 0     ) /* Enable various internal debug messages. */
         , VarQuiet          ( 1     )
         , VarReturnCode     ( 0     ) /* Returned to caller of Forth, eg. UNIX shell. */
         , IncludeIndex      ( 0     )
         , CurrentFunctions  ( 0     )
         , VarTraceStack     ( 0     )
         , MaxFunctions      ( 8192  )
         , CurrentTask       ( NULL  )
         , CurrentDictionary ( NULL  )
         , IncludeStack      ( NULL  )
         , DepthAtColon      ( DEPTH_AT_COLON_INVALID )
{
  IncludeFrame * includeStack = NULL                 ;
  includeStack = new IncludeFrame[MAX_INCLUDE_DEPTH] ;
  IncludeStack = (void *)includeStack                ;
  Functions    = new ForthFunction[MaxFunctions]     ;
  memset(Functions,0,sizeof(ForthFunction))          ;
}

N::Forth::~Forth(void)
{
  IncludeFrame * includeStack = (IncludeFrame *)IncludeStack ;
  delete [] includeStack                                     ;
  IncludeStack = NULL                                        ;
}

void N::Forth::Error(const char * functionName,int errorCode)
{
  QString s = ""                                               ;
  Report ( QObject::tr( "Error in "  ) )                       ;
  Report ( QObject::tr( functionName ) )                       ;
  Report ( QObject::tr( " - "        ) )                       ;
  //////////////////////////////////////////////////////////////
  switch (errorCode & 0xFF)                                    {
    case PF_ERR_NO_MEM             & 0xFF                      :
      s = QObject::tr("insufficient memory"                  ) ;
    break                                                      ;
    case PF_ERR_TOO_BIG            & 0xFF                      :
      s = QObject::tr("data chunk too large"                 ) ;
    break                                                      ;
    case PF_ERR_NUM_PARAMS         & 0xFF                      :
      s = QObject::tr("incorrect number of parameters"       ) ;
    break                                                      ;
    case PF_ERR_OPEN_FILE          & 0xFF                      :
      s = QObject::tr("could not open file"                  ) ;
    break                                                      ;
    case PF_ERR_WRONG_FILE         & 0xFF                      :
      s = QObject::tr("wrong type of file format"            ) ;
    break                                                      ;
    case PF_ERR_BAD_FILE           & 0xFF                      :
      s = QObject::tr("badly formatted file"                 ) ;
    break                                                      ;
    case PF_ERR_READ_FILE          & 0xFF                      :
      s = QObject::tr("file read failed"                     ) ;
    break                                                      ;
    case PF_ERR_WRITE_FILE         & 0xFF                      :
      s = QObject::tr("file write failed"                    ) ;
    break                                                      ;
    case PF_ERR_CORRUPT_DIC        & 0xFF                      :
      s = QObject::tr("corrupted dictionary"                 ) ;
    break                                                      ;
    case PF_ERR_NOT_SUPPORTED      & 0xFF                      :
      s = QObject::tr("not supported in this version"        ) ;
    break                                                      ;
    case PF_ERR_VERSION_FUTURE     & 0xFF                      :
      s = QObject::tr("version from future"                  ) ;
    break                                                      ;
    case PF_ERR_VERSION_PAST       & 0xFF                      :
      s = QObject::tr("version is obsolete. Rebuild new one.") ;
    break                                                      ;
    case PF_ERR_COLON_STACK        & 0xFF                      :
      s = QObject::tr("stack depth changed between : and ; . Probably unbalanced conditional") ;
    break                                                      ;
    case PF_ERR_HEADER_ROOM        & 0xFF                      :
      s = QObject::tr("no room left in header space"         ) ;
    break                                                      ;
    case PF_ERR_CODE_ROOM          & 0xFF                      :
      s = QObject::tr("no room left in code space"           ) ;
    break                                                      ;
    case PF_ERR_NO_SHELL           & 0xFF                      :
      s = QObject::tr("attempt to use names in forth compiled with PF_NO_SHELL") ;
    break                                                      ;
    case PF_ERR_NO_NAMES           & 0xFF                      :
      s = QObject::tr("dictionary has no names"              ) ;
    break                                                      ;
    case PF_ERR_OUT_OF_RANGE       & 0xFF                      :
      s = QObject::tr("parameter out of range"               ) ;
    break                                                      ;
    case PF_ERR_ENDIAN_CONFLICT    & 0xFF                      :
      s = QObject::tr("endian-ness of dictionary does not match code") ;
    break                                                      ;
    case PF_ERR_FLOAT_CONFLICT     & 0xFF                      :
      s = QObject::tr("float support mismatch between .dic file and code") ;
    break                                                      ;
    case PF_ERR_CELL_SIZE_CONFLICT & 0xFF                      :
      s = QObject::tr("cell size mismatch between .dic file and code") ;
    break                                                      ;
    case PF_ERR_NO_FUNCTION & 0xFF                             :
      s = QObject::tr("No function call assigned"            ) ;
    break                                                      ;
    default                                                    :
      s = QObject::tr("unrecognized error code!"             ) ;
    break                                                      ;
  }                                                            ;
  //////////////////////////////////////////////////////////////
  Report ( s             )                                     ;
  Report ( QString("\n") )                                     ;
}

void N::Forth::Throw(int code)
{
  QString s = ""                                                 ;
  switch ( code )                                                {
    case THROW_ABORT                                             :
    case THROW_ABORT_QUOTE                                       :
      s = QObject::tr("ABORT"                                  ) ;
    break                                                        ;
    case THROW_STACK_OVERFLOW                                    :
      s = QObject::tr("Stack overflow!"                        ) ;
    break                                                        ;
    case THROW_STACK_UNDERFLOW                                   :
      s = QObject::tr("Stack underflow!"                       ) ;
    break                                                        ;
    case THROW_EXECUTING                                         :
      s = QObject::tr("Executing a compile-only word!"         ) ;
    break                                                        ;
    case THROW_FLOAT_STACK_UNDERFLOW                             :
      s = QObject::tr("Float Stack underflow!"                 ) ;
    break                                                        ;
    case THROW_UNDEFINED_WORD                                    :
      s = QObject::tr("Undefined word!"                        ) ;
    break                                                        ;
    case THROW_PAIRS                                             :
      s = QObject::tr("Conditional control structure mismatch!") ;
    break                                                        ;
    case THROW_BYE                                               :
    case THROW_QUIT                                              :
    break                                                        ;
    case THROW_SEMICOLON                                         :
      s = QObject::tr("Stack depth changed between : and ; . Probably unbalanced conditional!") ;
    break                                                        ;
    case THROW_DEFERRED                                          :
      s = QObject::tr("Not a DEFERred word!"                   ) ;
    break                                                        ;
    default                                                      :
      s = QObject::tr("Unrecognized throw code!"               ) ;
    break                                                        ;
  }                                                              ;
  nDropOut ( s.length() <= 0 )                                   ;
  Report ( QObject::tr("Throw code = %1\n").arg(code) )          ;
  Report ( QString("%1\n").arg(s)                     )          ;
}

/* PLB 19980522 - Expand PAD so "-1 binary .s" doesn't crash. */
#define CNTT_PAD_SIZE ((sizeof(int)*8)+2)

char * N::Forth::toString(int Num,int Base,bool IfSigned,int MinChars)
{
  int    IfNegative = 0                   ;
  int    i          = 0                   ;
  int    NewNum                           ;
  int    Rem                              ;
  int    uNum                             ;
  char   c                                ;
  char * p                                ;
  char   cnttPad [CNTT_PAD_SIZE]          ;
  /////////////////////////////////////////
  uNum = Num                              ;
  /* Convert to positive and keep sign.  */
  if ( IfSigned )                         {
    if ( Num < 0 )                        {
      IfNegative = TRUE                   ;
      uNum       = -Num                   ;
    }                                     ;
  }                                       ;
  /////////////////////////////////////////
  p      = cnttPad + CNTT_PAD_SIZE        ; /* Point past end of Pad */
  *(--p) = (char) 0                       ; /* NUL terminate */
  /////////////////////////////////////////
  while ( (i++<MinChars) || (uNum != 0) ) {
    NewNum = uNum / Base                  ;
    Rem    = uNum - ( NewNum * Base  )    ;
    c      = (char) (( Rem < 10      )    ?
                     ( Rem + '0'     )    :
                     ( Rem - 10 + 'A') )  ;
    *(--p) = c                            ;
    uNum   = NewNum                       ;
  }                                       ;
  /////////////////////////////////////////
  if ( IfSigned )                         {
    if( IfNegative ) *(--p) = '-'         ;
  }                                       ;
  /////////////////////////////////////////
  char * z = new char[strlen(p)+1]        ;
  strcpy(z,p)                             ;
  return z                                ;
}

void N::Forth::DotS(void)
{
  int * sp                                                          ;
  int   Depth                                                       ;
  ///////////////////////////////////////////////////////////////////
  Report (QObject::tr("Stack<%1> ").arg(toString(Radix,10,TRUE,1))) ;
  Depth = TaskNow->td_StackBase - TaskNow->td_StackPtr              ;
  sp    = TaskNow->td_StackBase                                     ;
  ///////////////////////////////////////////////////////////////////
  if ( Depth < 0 ) Report (QObject::tr("UNDERFLOW!")) ; else        {
    nFullLoop ( i , Depth )                                         {
      /* Print as unsigned if not base 10.                         */
      Report(QString("%1 "                                          )
             .arg( toString(*(--sp),Radix,(Radix==10),1)) )         ;
    }                                                               ;
  }                                                                 ;
  Report ( QString("\n") )                                          ;
}

int N::Forth::Quit(void)
{
  int exception                        ;
  int go        = 1                    ;
  while ( go )                         {
    exception = OuterInterpreterLoop() ;
    if ( exception == 0 )              {
      exception = OK ( )               ;
    }                                  ;
    ////////////////////////////////////
    switch ( exception )               {
      case 0                           :
      break                            ;
      case THROW_BYE                   :
        go = 0                         ;
      break                            ;
      case THROW_ABORT                 :
      default                          :
        DotS  (           )            ;
        Throw ( exception )            ;
        HandleIncludeError ( )         ;
        ResetTask ( )                  ;
      break                            ;
    }                                  ;
  }                                    ;
  return VarReturnCode                 ;
}

/***************************************************************
** Forth equivalent 'C' functions.
***************************************************************/

/* Convert a string to the corresponding number using BASE. */
int N::Forth::NumberQ(const char * FWord,int * Num)
{
  int          Len       ;
  int          n         ;
  int          Accum = 0 ;
  int          Sign  = 1 ;
  const char * s         ;
  ////////////////////////
  Len = *FWord           ; /* get count */
  FWord++                ;
  s   =  FWord           ; /* get string address */
  if ( *s == '-' )       { /* process initial minus sign */
    Sign = -1            ;
    s++                  ;
    Len--                ;
  }                      ;
  ////////////////////////
  nFullLoop ( i , Len )  {
    n = toNumber (*s++)  ;
    if ( (n < 0)        ||
         (n >= Radix)    )
    return NUM_TYPE_BAD  ;
    Accum *= Radix       ;
    Accum += n           ;
  }                      ;
  ////////////////////////
  *Num = Accum * Sign    ;
  return NUM_TYPE_SINGLE ;
}

/***************************************************************
** Compiler Support
***************************************************************/

/* ( char -- c-addr , parse word ) */
char * N::Forth::Word(char c)
{
  char * s1                                   ;
  char * s2                                   ;
  char * s3                                   ;
  int    n1                                   ;
  int    n2                                   ;
  int    n3                                   ;
  int    nc                                   ;
  /////////////////////////////////////////////
  s1 = TaskNow->td_SourcePtr + TaskNow->td_IN ;
  n1 = TaskNow->td_SourceNum - TaskNow->td_IN ;
  n2 = Skip ( s1, n1, c, &s2 )                ;
  n3 = Scan ( s2, n2, c, &s3 )                ;
  nc = n2 - n3                                ;
  /////////////////////////////////////////////
  if ( nc > 0 )                               {
    Scratch[0] = (char) nc                    ;
    nFullLoop ( i , nc )                      {
      Scratch [ i+1 ] = toUpper ( s2[i] )     ;
    }                                         ;
  } else Scratch[0] = 0                       ;
  TaskNow->td_IN += ( n1 - n3 ) + 1           ;
  return Scratch                              ;
}

/***************************************************************
** Task Management
***************************************************************/

void N::Forth::setCurrentTask (void * task)
{
  CurrentTask = task ;
}

/***************************************************************
** Used by Quit and other routines to restore system.
***************************************************************/

void N::Forth::ResetTask(void)
{
  TaskNow->td_InputStream   = NULL                       ; /* Go back to terminal input. */
  TaskNow->td_StackPtr      = TaskNow->td_StackBase      ; /* Reset stacks. */
  TaskNow->td_ReturnPtr     = TaskNow->td_ReturnBase     ;
  TaskNow->td_FloatStackPtr = TaskNow->td_FloatStackBase ;
  TaskNow->td_IN            = TaskNow->td_SourceNum      ; /* Advance >IN to end of input. */
  VarState                  = 0                          ;
}

void N::Forth::DeleteTask(void * task)
{
  pfTaskData_t * cftd = (pfTaskData_t *)task ;
  nDeleteArray ( cftd -> td_ReturnLimit )    ;
  nDeleteArray ( cftd -> td_StackLimit  )    ;
  delete         cftd                        ;
}

/* Allocate some extra cells to protect against mild stack underflows. */

void * N::Forth::CreateTask(int UserStackDepth,int ReturnStackDepth)
{
  pfTaskData_t * cftd = new pfTaskData_t()                             ;
  if (IsNull(cftd))                                                    {
    Report(QObject::tr("CreateTaskContext: insufficient memory.\n"))   ;
    Report(QObject::tr("Task can not allocate data.\n"            ))   ;
    return NULL                                                        ;
  }                                                                    ;
  //////////////////////////////////////////////////////////////////////
  memset(cftd,0,sizeof(pfTaskData_t))                                  ;
  /* Allocate User Stack                                              */
  cftd->td_StackLimit = new int [ UserStackDepth + STACK_SAFETY ]      ;
  cftd->td_StackBase  = &cftd->td_StackLimit[UserStackDepth]           ;
  cftd->td_StackPtr   =  cftd->td_StackBase                            ;
  //////////////////////////////////////////////////////////////////////
  /* Allocate Return Stack                                            */
  cftd->td_ReturnLimit = new int [ ReturnStackDepth ]                  ;
  cftd->td_ReturnBase  = &cftd->td_ReturnLimit[ReturnStackDepth]       ;
  cftd->td_ReturnPtr   =  cftd->td_ReturnBase                          ;
  //////////////////////////////////////////////////////////////////////
  /* Allocate room for as many Floats as we do regular data.          */
  cftd->td_FloatStackLimit = new double[UserStackDepth + STACK_SAFETY] ;
  cftd->td_FloatStackBase  = &cftd->td_FloatStackLimit[UserStackDepth] ;
  cftd->td_FloatStackPtr   =  cftd->td_FloatStackBase                  ;
  //////////////////////////////////////////////////////////////////////
  cftd->td_InputStream = NULL                                          ;
  cftd->td_SourcePtr   = cftd->td_TIB                                  ;
  cftd->td_SourceNum   = 0                                             ;
  return (void *) cftd                                                 ;
}

/***************************************************************
** Dictionary Management
***************************************************************/

int N::Forth::ExecIfDefined(const char * CString)
{
  int result = 0                 ;
  if ( NAME_BASE != (int)NULL)   {
    unsigned int XT              ;
    if ( FindC( CString, &XT ) ) {
      result = Catch( XT )       ;
    }                            ;
  }                              ;
  return result                  ;
}

/***************************************************************
** Create a complete dictionary.
** The dictionary consists of two parts, the header with the names,
** and the code portion.
** Delete using pfDeleteDictionary().
** Return pointer to dictionary management structure.
*/

void * N::Forth::CreateDictionary(int HeaderSize,int CodeSize)
{ /* Allocate memory for initial dictionary.  */
  pfDictionary_t * dic = new pfDictionary_t()                     ;
  if (IsNull(dic)) return NULL                                    ;
  /////////////////////////////////////////////////////////////////
  memset(dic,0,sizeof(pfDictionary_t))                            ;
  dic->dic_Flags |= PF_DICF_ALLOCATED_SEGMENTS                    ;
  /****************************************************************
   * Align dictionary segments to preserve alignment of floats across hosts.
   * Thank you Helmut Proelss for pointing out that this needs to be cast
   * to (ucell_t) on 16 bit systems.
   ***************************************************************/
  #define DIC_ALIGNMENT_SIZE  ((unsigned int)(0x10))
  #define DIC_ALIGN(addr) \
    ((((unsigned int)(addr))+DIC_ALIGNMENT_SIZE-1) & ~(DIC_ALIGNMENT_SIZE-1))
  /* Allocate memory for header.                                 */
  if ( HeaderSize > 0 )                                           {
    char * hbu = new char[HeaderSize+DIC_ALIGNMENT_SIZE]          ;
    dic->dic_HeaderBaseUnaligned = (unsigned int)hbu              ;
    /* Align header base.                                        */
    dic->dic_HeaderBase = DIC_ALIGN(dic->dic_HeaderBaseUnaligned) ;
    memset((void *)dic->dic_HeaderBase,0xA5,HeaderSize)           ;
    dic->dic_HeaderLimit = dic->dic_HeaderBase + HeaderSize       ;
    dic->dic_HeaderPtr   = dic->dic_HeaderBase                    ;
  } else dic->dic_HeaderBase = 0                                  ;
  /* Allocate memory for code.                                   */
  char * cbu = new char[CodeSize + DIC_ALIGNMENT_SIZE]            ;
  dic->dic_CodeBaseUnaligned = (unsigned int)cbu                  ;
  dic->dic_CodeBase = DIC_ALIGN(dic->dic_CodeBaseUnaligned)       ;
  memset((void *)dic->dic_CodeBase,0x5A,CodeSize)                 ;
  dic->dic_CodeLimit = dic->dic_CodeBase + CodeSize               ;
  dic->dic_CodePtr.Byte                                           =
  ((unsigned char *)(dic->dic_CodeBase + QUADUP(NUM_PRIMITIVES))) ;
  #undef  DIC_ALIGNMENT_SIZE
  #undef  DIC_ALIGN
  return (void *) dic                                             ;
}

/*****************************************************
** Delete a dictionary created by CreateDictionary() */
void N::Forth::DeleteDictionary(void * dictionary)
{
  nDropOut ( IsNull(dictionary) )                     ;
  pfDictionary_t *dic = (pfDictionary_t *)dictionary  ;
  /////////////////////////////////////////////////////
  if ( dic->dic_Flags & PF_DICF_ALLOCATED_SEGMENTS )  {
    char * hbu = (char *)dic->dic_HeaderBaseUnaligned ;
    char * cbu = (char *)dic->dic_CodeBaseUnaligned   ;
    dic->dic_HeaderBaseUnaligned = 0                  ;
    dic->dic_CodeBaseUnaligned   = 0                  ;
    delete [] hbu                                     ;
    delete [] cbu                                     ;
  }                                                   ;
  delete dic                                          ;
}

/***************************************************************
** Create an entry in the Dictionary for the given ExecutionToken.
** FName is name in Forth format.
*/

void N::Forth::CreateEntry(unsigned int XT,const char * fName,unsigned int Flags)
{
  cfNameLinks * cfnl = (cfNameLinks *)Dictionary->dic_HeaderPtr ;
  /* Set link to previous header, if any.                      */
  if ( VarContext != 0 )                                        {
    WRITE_CELL_DIC( &cfnl->cfnl_PreviousName                    ,
                    ABS_TO_NAMEREL( VarContext )              ) ;
  } else cfnl->cfnl_PreviousName = 0                            ;
  /* Put Execution token in header.                            */
  WRITE_CELL_DIC( &cfnl->cfnl_ExecToken, XT )                   ;
  /* Advance Header Dictionary Pointer                         */
  Dictionary->dic_HeaderPtr += sizeof(cfNameLinks)              ;
  /* Laydown name.                                             */
  int Len = (*fName)+1                                          ;
  VarContext = Dictionary->dic_HeaderPtr                        ;
  memcpy((unsigned char *)Dictionary->dic_HeaderPtr,fName,Len)  ;
  Dictionary->dic_HeaderPtr += Len                              ;
  /* Set flags.                                                */
  *(char*)VarContext |= (char) Flags                            ;
  /* Align to quad byte boundaries with zeroes.                */
  while ( Dictionary->dic_HeaderPtr & UINT32_MASK )             {
    *(char*)(Dictionary->dic_HeaderPtr++) = 0                   ;
  }                                                             ;
}

/* Convert name then create dictionary entry. */
void N::Forth::CreateEntryC(unsigned int XT,const char * cName,unsigned int Flags)
{
  QByteArray FName = toForth(QString(cName)) ;
  CreateEntry ( XT , FName.data() , Flags )  ;
}

int N::Forth::PopInt(void)
{
  return POP_DATA_STACK ;
}

int N::Forth::Call(int Index,int ReturnMode,int NumParams)
{
  int           P1,P2,P3,P4,P5,P6,P7,P8,P9                      ;
  int           Result = 0                                      ;
  ForthFunction CF                                              ;
  nKickOut ( Index >= CurrentFunctions , 0 )                    ;
  CF = Functions[Index]                                         ;
  if (IsNull(CF))                                               {
    Error ( "N::Forth::Call" , PF_ERR_NO_FUNCTION )               ;
    Exit  ( 1                                   )               ;
    Result = 0                                                  ;
    if (ReturnMode == C_RETURNS_VALUE) PUSH_DATA_STACK (Result) ;
    return Result                                               ;
  }                                                             ;
  ///////////////////////////////////////////////////////////////
  switch ( NumParams )                                          {
    case 0                                                      :
      Result = ((ForthFunction)CF)()                            ;
    break                                                       ;
    case 1                                                      :
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc1)CF)(P1)                                 ;
    break                                                       ;
    case 2                                                      :
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc2)CF)(P1,P2)                              ;
    break                                                       ;
    case 3                                                      :
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc3)CF)(P1,P2,P3)                           ;
    break                                                       ;
    case 4                                                      :
      P4     = POP_DATA_STACK                                   ;
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc4)CF)(P1,P2,P3,P4)                        ;
    break                                                       ;
    case 5                                                      :
      P5     = POP_DATA_STACK                                   ;
      P4     = POP_DATA_STACK                                   ;
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc5)CF)(P1,P2,P3,P4,P5)                     ;
    break                                                       ;
    case 6                                                      :
      P6     = POP_DATA_STACK                                   ;
      P5     = POP_DATA_STACK                                   ;
      P4     = POP_DATA_STACK                                   ;
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc6)CF)(P1,P2,P3,P4,P5,P6)                  ;
    break                                                       ;
    case 7                                                      :
      P7     = POP_DATA_STACK                                   ;
      P6     = POP_DATA_STACK                                   ;
      P5     = POP_DATA_STACK                                   ;
      P4     = POP_DATA_STACK                                   ;
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc7)CF)(P1,P2,P3,P4,P5,P6,P7)               ;
    break                                                       ;
    case 8                                                      :
      P8     = POP_DATA_STACK                                   ;
      P7     = POP_DATA_STACK                                   ;
      P6     = POP_DATA_STACK                                   ;
      P5     = POP_DATA_STACK                                   ;
      P4     = POP_DATA_STACK                                   ;
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc8)CF)(P1,P2,P3,P4,P5,P6,P7,P8)            ;
    break                                                       ;
    case 9                                                      :
      P9     = POP_DATA_STACK                                   ;
      P8     = POP_DATA_STACK                                   ;
      P7     = POP_DATA_STACK                                   ;
      P6     = POP_DATA_STACK                                   ;
      P5     = POP_DATA_STACK                                   ;
      P4     = POP_DATA_STACK                                   ;
      P3     = POP_DATA_STACK                                   ;
      P2     = POP_DATA_STACK                                   ;
      P1     = POP_DATA_STACK                                   ;
      Result = ((CFunc9)CF)(P1,P2,P3,P4,P5,P6,P7,P8,P9)         ;
    break                                                       ;
    default                                                     :
      Error ( "N::Forth::Call" , PF_ERR_NUM_PARAMS )              ;
      Exit  ( 1                                  )              ;
    break                                                       ;
  }                                                             ;
  /* Push result on Forth stack if requested.                  */
  if (ReturnMode == C_RETURNS_VALUE) PUSH_DATA_STACK (Result)   ;
  return Result                                                 ;
}

int N::Forth::Glue(const char * cName,unsigned int Index,int ReturnMode,int NumParams)
{
  unsigned int Packed                                                ;
  QByteArray FName = toForth(QString(cName))                         ;
  Packed = (Index & 0xFFFF) | (NumParams << 24) | (ReturnMode << 31) ;
  SecondaryHeader ( FName.data() )                                   ;
    CODE_COMMA    ( ID_CALL_C    )                                   ;
    CODE_COMMA    ( Packed       )                                   ;
  FinishSecondary (              )                                   ;
  return 0                                                           ;
}

/***************************************************************
** Convert absolute namefield address to previous absolute name
** field address or NULL.
*/
const char * N::Forth::toPrevious(const char * NFA)
{
  const cfNameLinks * cfnl                                              ;
  int                 RelNamePtr                                        ;
  ///////////////////////////////////////////////////////////////////////
  cfnl = (const cfNameLinks *)(((const char *)NFA)-sizeof(cfNameLinks)) ;
  RelNamePtr = READ_CELL_DIC((const int *)(&cfnl->cfnl_PreviousName  )) ;
  if ( RelNamePtr == 0 ) return NULL                                    ;
  return ((const char *)NAMEREL_TO_ABS(RelNamePtr))                     ;
}

/* Convert NFA to ExecToken. */
unsigned int N::Forth::toToken(const char * NFA)
{
  const cfNameLinks * cfnl                                               ;
  /* Convert absolute namefield address to absolute link field address. */
  cfnl = (const cfNameLinks *)(((const char *) NFA)-sizeof(cfNameLinks)) ;
  return READ_CELL_DIC((const int *)(&cfnl->cfnl_ExecToken))             ;
}

/*
** ( xt -- nfa 1 , x 0 , find NFA in dictionary from XT )
** 1 for IMMEDIATE values
*/

int N::Forth::toName(unsigned int XT,const char ** NfaPtr)
{
  const char * NameField                   ;
  int          Searching = TRUE            ;
  int          Result    = 0               ;
  unsigned int TempXT                      ;
  //////////////////////////////////////////
  NameField = (char *)VarContext           ;
  do                                       {
    TempXT = toToken ( NameField )         ;
    if ( TempXT == XT )                    {
      *NfaPtr   = NameField                ;
      Result    = 1                        ;
      Searching = FALSE                    ;
    } else                                 {
      NameField = toPrevious ( NameField ) ;
      if ( NameField == NULL )             {
        *NfaPtr   = 0                      ;
        Searching = FALSE                  ;
      }                                    ;
    }                                      ;
  } while ( Searching )                    ;
  return Result                            ;
}

/*
** ( $name -- $addr 0 | nfa -1 | nfa 1 , find NFA in dictionary )
** 1 for IMMEDIATE values
*/

int N::Forth::FindNfa(const char * WordName,const char ** NfaPtr)
{
  const char  * WordChar                                                    ;
  const char  * NameField                                                   ;
  const char  * NameChar                                                    ;
  unsigned char WordLen                                                     ;
  char          NameLen                                                     ;
  int           Searching = TRUE                                            ;
  int           Result    = 0                                               ;
  ///////////////////////////////////////////////////////////////////////////
  WordLen   = (unsigned char)((unsigned int)(*WordName) & 0x1F)             ;
  WordChar  = WordName+1                                                    ;
  ///////////////////////////////////////////////////////////////////////////
  NameField = (char *)VarContext                                            ;
  do                                                                        {
    NameLen  = (unsigned char)((unsigned int)(*NameField) & MASK_NAME_SIZE) ;
    NameChar = NameField+1                                                  ;
    if ( (((*NameField) & FLAG_SMUDGE) == 0) && ( NameLen == WordLen )     &&
          icompare ( NameChar , WordChar , WordLen )                      ) {
       *NfaPtr   =    NameField                                             ;
       Result    = ((*NameField) & FLAG_IMMEDIATE) ? 1 : -1                 ;
       Searching = FALSE                                                    ;
    } else                                                                  {
      NameField = toPrevious ( NameField )                                  ;
      if ( NameField == NULL )                                              {
        *NfaPtr   = WordName                                                ;
        Searching = FALSE                                                   ;
      }                                                                     ;
    }                                                                       ;
  } while ( Searching )                                                     ;
  return Result                                                             ;
}

/***************************************************************
** ( $name -- $name 0 | xt -1 | xt 1 )
** 1 for IMMEDIATE values
*/
int N::Forth::Find(const char * WordName,unsigned int * pXT)
{
  const char * NFA                            ;
  int          Result                         ;
  Result = FindNfa ( WordName , &NFA )        ;
  if ( Result ) *pXT = toToken ( NFA )        ;
           else *pXT = (unsigned int)WordName ;
  return Result                               ;
}

/* Find name when passed 'C' string. */
int N::Forth::FindC(const char * WordName,unsigned int * pXT)
{
  QByteArray FName = toForth ( WordName ) ;
  strcpy      ( Scratch , FName.data()  ) ;
  return Find ( Scratch , pXT           ) ;
}

/* Convert name then create deferred dictionary entry. */
void N::Forth::DeferredC(unsigned int DefaultXT,const char * cName)
{
  QByteArray FName = toForth(cName) ;
  StringDefer ( FName , DefaultXT ) ;
}

/* Find XTs needed by compiler. */
int N::Forth::SpecialXTs(void)
{
  bool success = true                                         ;
  if ( FindC ( "(QUIT)"  , &QuitPXT  ) == 0 ) success = false ;
  if ( FindC ( "NUMBER?" , &NumberQXT) == 0 ) success = false ;
  if ( FindC ( "ACCEPT"  , &AcceptPXT) == 0 ) success = false ;
  if ( success ) return 0                                     ;
  Report ( QObject::tr("FindSpecialXTs failed!\n") )          ;
  return -1                                                   ;
}

/* Build a dictionary from scratch. */
void * N::Forth::BuildDictionary(int HeaderSize,int CodeSize)
{
  pfDictionary_t * dic                                          ;
  dic = (pfDictionary_t *)CreateDictionary(HeaderSize,CodeSize) ;
  if (IsNull(dic))                                              {
    Report(QObject::tr("No memory to create dictionary"))       ;
    return NULL                                                 ;
  }                                                             ;
  CurrentDictionary = (void *)dic                               ;
  NumPrimitives     = NUM_PRIMITIVES                            ;
  ///////////////////////////////////////////////////////////////
  CreateEntryC ( ID_EXIT                  , "EXIT", 0 );
  CreateEntryC ( ID_1MINUS                , "1-", 0 );
  CreateEntryC ( ID_1PLUS                 , "1+", 0 );
  CreateEntryC ( ID_2_R_FETCH             , "2R@", 0 );
  CreateEntryC ( ID_2_R_FROM              , "2R>", 0 );
  CreateEntryC ( ID_2_TO_R                , "2>R", 0 );
  CreateEntryC ( ID_2DUP                  , "2DUP", 0 );
  CreateEntryC ( ID_2LITERAL              , "2LITERAL", FLAG_IMMEDIATE );
  CreateEntryC ( ID_2LITERAL_P            , "(2LITERAL)", 0 );
  CreateEntryC ( ID_2MINUS                , "2-", 0 );
  CreateEntryC ( ID_2PLUS                 , "2+", 0 );
  CreateEntryC ( ID_2OVER                 , "2OVER", 0 );
  CreateEntryC ( ID_2SWAP                 , "2SWAP", 0 );
  CreateEntryC ( ID_ACCEPT_P              , "(ACCEPT)", 0 );
  DeferredC    ( ID_ACCEPT_P              , "ACCEPT" );
  CreateEntryC ( ID_ALITERAL              , "ALITERAL", FLAG_IMMEDIATE );
  CreateEntryC ( ID_ALITERAL_P            , "(ALITERAL)", 0 );
  CreateEntryC ( ID_ALLOCATE              , "ALLOCATE", 0 );
  CreateEntryC ( ID_ARSHIFT               , "ARSHIFT", 0 );
  CreateEntryC ( ID_AND                   , "AND", 0 );
  CreateEntryC ( ID_BAIL                  , "BAIL", 0 );
  CreateEntryC ( ID_BRANCH                , "BRANCH", 0 );
  CreateEntryC ( ID_BODY_OFFSET           , "BODY_OFFSET", 0 );
  CreateEntryC ( ID_BYE                   , "BYE", 0 );
  CreateEntryC ( ID_CATCH                 , "CATCH", 0 );
  CreateEntryC ( ID_CELL                  , "CELL", 0 );
  CreateEntryC ( ID_CELLS                 , "CELLS", 0 );
  CreateEntryC ( ID_CFETCH                , "C@", 0 );
  CreateEntryC ( ID_CLEAR                 , "CLEAR", 0 );
  CreateEntryC ( ID_CMOVE                 , "CMOVE", 0 );
  CreateEntryC ( ID_CMOVE_UP              , "CMOVE>", 0 );
  CreateEntryC ( ID_COLON                 , ":", 0 );
  CreateEntryC ( ID_COLON_P               , "(:)", 0 );
  CreateEntryC ( ID_COMPARE               , "COMPARE", 0 );
  CreateEntryC ( ID_COMP_EQUAL            , "=", 0 );
  CreateEntryC ( ID_COMP_NOT_EQUAL        , "<>", 0 );
  CreateEntryC ( ID_COMP_GREATERTHAN      , ">", 0 );
  CreateEntryC ( ID_COMP_U_GREATERTHAN    , "U>", 0 );
  CreateEntryC ( ID_COMP_LESSTHAN         , "<", 0 );
  CreateEntryC ( ID_COMP_U_LESSTHAN       , "U<", 0 );
  CreateEntryC ( ID_COMP_ZERO_EQUAL       , "0=", 0 );
  CreateEntryC ( ID_COMP_ZERO_NOT_EQUAL   , "0<>", 0 );
  CreateEntryC ( ID_COMP_ZERO_GREATERTHAN , "0>", 0 );
  CreateEntryC ( ID_COMP_ZERO_LESSTHAN    , "0<", 0 );
  CreateEntryC ( ID_CR                    , "CR", 0 );
  CreateEntryC ( ID_CREATE                , "CREATE", 0 );
  CreateEntryC ( ID_CREATE_P              , "(CREATE)", 0 );
  CreateEntryC ( ID_D_PLUS                , "D+", 0 );
  CreateEntryC ( ID_D_MINUS               , "D-", 0 );
  CreateEntryC ( ID_D_UMSMOD              , "UM/MOD", 0 );
  CreateEntryC ( ID_D_MUSMOD              , "MU/MOD", 0 );
  CreateEntryC ( ID_D_MTIMES              , "M*", 0 );
  CreateEntryC ( ID_D_UMTIMES             , "UM*", 0 );
  CreateEntryC ( ID_DEFER                 , "DEFER", 0 );
  CreateEntryC ( ID_DEFER_P               , "(DEFER)", 0 );
  CreateEntryC ( ID_CSTORE                , "C!", 0 );
  CreateEntryC ( ID_DEPTH                 , "DEPTH",  0 );
  CreateEntryC ( ID_DIVIDE                , "/", 0 );
  CreateEntryC ( ID_DOT                   , ".",  0 );
  CreateEntryC ( ID_DOTS                  , ".S",  0 );
  CreateEntryC ( ID_DO_P                  , "(DO)", 0 );
  CreateEntryC ( ID_DROP                  , "DROP", 0 );
  CreateEntryC ( ID_DUMP                  , "DUMP", 0 );
  CreateEntryC ( ID_DUP                   , "DUP",  0 );
  CreateEntryC ( ID_EMIT_P                , "(EMIT)",  0 );
  DeferredC    ( ID_EMIT_P                , "EMIT");
  CreateEntryC ( ID_EOL                   , "EOL",  0 );
  CreateEntryC ( ID_ERRORQ_P              , "(?ERROR)",  0 );
  CreateEntryC ( ID_ERRORQ_P              , "?ERROR",  0 );
  CreateEntryC ( ID_EXECUTE               , "EXECUTE",  0 );
  CreateEntryC ( ID_FETCH                 , "@",  0 );
  CreateEntryC ( ID_FILL                  , "FILL", 0 );
  CreateEntryC ( ID_FIND                  , "FIND",  0 );
  CreateEntryC ( ID_FILE_CREATE           , "CREATE-FILE",  0 );
  CreateEntryC ( ID_FILE_DELETE           , "DELETE-FILE",  0 );
  CreateEntryC ( ID_FILE_OPEN             , "OPEN-FILE",  0 );
  CreateEntryC ( ID_FILE_CLOSE            , "CLOSE-FILE",  0 );
  CreateEntryC ( ID_FILE_READ             , "READ-FILE",  0 );
  CreateEntryC ( ID_FILE_SIZE             , "FILE-SIZE",  0 );
  CreateEntryC ( ID_FILE_WRITE            , "WRITE-FILE",  0 );
  CreateEntryC ( ID_FILE_POSITION         , "FILE-POSITION",  0 );
  CreateEntryC ( ID_FILE_REPOSITION       , "REPOSITION-FILE",  0 );
  CreateEntryC ( ID_FILE_RO               , "R/O",  0 );
  CreateEntryC ( ID_FILE_RW               , "R/W",  0 );
  CreateEntryC ( ID_FILE_WO               , "W/O",  0 );
  CreateEntryC ( ID_FILE_BIN              , "BIN",  0 );
  CreateEntryC ( ID_FINDNFA               , "FINDNFA",  0 );
  CreateEntryC ( ID_FLUSHEMIT             , "FLUSHEMIT",  0 );
  CreateEntryC ( ID_FREE                  , "FREE",  0 );
  CreateEntryC ( ID_FP_D_TO_F             , "D>F", 0 );
  CreateEntryC ( ID_FP_FSTORE             , "F!", 0 );
  CreateEntryC ( ID_FP_FTIMES             , "F*", 0 );
  CreateEntryC ( ID_FP_FPLUS              , "F+", 0 );
  CreateEntryC ( ID_FP_FMINUS             , "F-", 0 );
  CreateEntryC ( ID_FP_FSLASH             , "F/", 0 );
  CreateEntryC ( ID_FP_F_ZERO_LESS_THAN   , "F0<", 0 );
  CreateEntryC ( ID_FP_F_ZERO_EQUALS      , "F0=", 0 );
  CreateEntryC ( ID_FP_F_LESS_THAN        , "F<", 0 );
  CreateEntryC ( ID_FP_F_TO_D             , "F>D", 0 );
  CreateEntryC ( ID_FP_FFETCH             , "F@", 0 );
  CreateEntryC ( ID_FP_FDEPTH             , "FDEPTH", 0 );
  CreateEntryC ( ID_FP_FDROP              , "FDROP", 0 );
  CreateEntryC ( ID_FP_FDUP               , "FDUP", 0 );
  CreateEntryC ( ID_FP_FLITERAL           , "FLITERAL", FLAG_IMMEDIATE );
  CreateEntryC ( ID_FP_FLITERAL_P         , "(FLITERAL)", 0 );
  CreateEntryC ( ID_FP_FLOAT_PLUS         , "FLOAT+", 0 );
  CreateEntryC ( ID_FP_FLOATS             , "FLOATS", 0 );
  CreateEntryC ( ID_FP_FLOOR              , "FLOOR", 0 );
  CreateEntryC ( ID_FP_FMAX               , "FMAX", 0 );
  CreateEntryC ( ID_FP_FMIN               , "FMIN", 0 );
  CreateEntryC ( ID_FP_FNEGATE            , "FNEGATE", 0 );
  CreateEntryC ( ID_FP_FOVER              , "FOVER", 0 );
  CreateEntryC ( ID_FP_FROT               , "FROT", 0 );
  CreateEntryC ( ID_FP_FROUND             , "FROUND", 0 );
  CreateEntryC ( ID_FP_FSWAP              , "FSWAP", 0 );
  CreateEntryC ( ID_FP_FSTAR_STAR         , "F**", 0 );
  CreateEntryC ( ID_FP_FABS               , "FABS", 0 );
  CreateEntryC ( ID_FP_FACOS              , "FACOS", 0 );
  CreateEntryC ( ID_FP_FACOSH             , "FACOSH", 0 );
  CreateEntryC ( ID_FP_FALOG              , "FALOG", 0 );
  CreateEntryC ( ID_FP_FASIN              , "FASIN", 0 );
  CreateEntryC ( ID_FP_FASINH             , "FASINH", 0 );
  CreateEntryC ( ID_FP_FATAN              , "FATAN", 0 );
  CreateEntryC ( ID_FP_FATAN2             , "FATAN2", 0 );
  CreateEntryC ( ID_FP_FATANH             , "FATANH", 0 );
  CreateEntryC ( ID_FP_FCOS               , "FCOS", 0 );
  CreateEntryC ( ID_FP_FCOSH              , "FCOSH", 0 );
  CreateEntryC ( ID_FP_FLN                , "FLN", 0 );
  CreateEntryC ( ID_FP_FLNP1              , "FLNP1", 0 );
  CreateEntryC ( ID_FP_FLOG               , "FLOG", 0 );
  CreateEntryC ( ID_FP_FSIN               , "FSIN", 0 );
  CreateEntryC ( ID_FP_FSINCOS            , "FSINCOS", 0 );
  CreateEntryC ( ID_FP_FSINH              , "FSINH", 0 );
  CreateEntryC ( ID_FP_FSQRT              , "FSQRT", 0 );
  CreateEntryC ( ID_FP_FTAN               , "FTAN", 0 );
  CreateEntryC ( ID_FP_FTANH              , "FTANH", 0 );
  CreateEntryC ( ID_FP_FPICK              , "FPICK", 0 );
  CreateEntryC ( ID_HERE                  , "HERE",  0 );
  CreateEntryC ( ID_NUMBERQ_P             , "(SNUMBER?)",  0 );
  CreateEntryC ( ID_I                     , "I",  0 );
  CreateEntryC ( ID_INTERPRET             , "INTERPRET", 0 );
  CreateEntryC ( ID_J                     , "J",  0 );
  CreateEntryC ( ID_INCLUDE_FILE          , "INCLUDE-FILE",  0 );
  CreateEntryC ( ID_KEY                   , "KEY",  0 );
  CreateEntryC ( ID_LEAVE_P               , "(LEAVE)", 0 );
  CreateEntryC ( ID_LITERAL               , "LITERAL", FLAG_IMMEDIATE );
  CreateEntryC ( ID_LITERAL_P             , "(LITERAL)", 0 );
  CreateEntryC ( ID_LOADSYS               , "LOADSYS", 0 );
  CreateEntryC ( ID_LOCAL_COMPILER        , "LOCAL-COMPILER", 0 );
  CreateEntryC ( ID_LOCAL_ENTRY           , "(LOCAL.ENTRY)", 0 );
  CreateEntryC ( ID_LOCAL_EXIT            , "(LOCAL.EXIT)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH           , "(LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_1         , "(1_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_2         , "(2_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_3         , "(3_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_4         , "(4_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_5         , "(5_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_6         , "(6_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_7         , "(7_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_FETCH_8         , "(8_LOCAL@)", 0 );
  CreateEntryC ( ID_LOCAL_STORE           , "(LOCAL!)"  , 0 );
  CreateEntryC ( ID_LOCAL_STORE_1         , "(1_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_2         , "(2_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_3         , "(3_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_4         , "(4_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_5         , "(5_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_6         , "(6_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_7         , "(7_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_STORE_8         , "(8_LOCAL!)", 0 );
  CreateEntryC ( ID_LOCAL_PLUSSTORE       , "(LOCAL+!)", 0 );
  CreateEntryC ( ID_LOOP_P                , "(LOOP)", 0 );
  CreateEntryC ( ID_LSHIFT                , "LSHIFT", 0 );
  CreateEntryC ( ID_MAX                   , "MAX", 0 );
  CreateEntryC ( ID_MIN                   , "MIN", 0 );
  CreateEntryC ( ID_MINUS                 , "-", 0 );
  CreateEntryC ( ID_NAME_TO_TOKEN         , "NAME>", 0 );
  CreateEntryC ( ID_NAME_TO_PREVIOUS      , "PREVNAME", 0 );
  CreateEntryC ( ID_NOOP                  , "NOOP", 0 );
  DeferredC    ( ID_NUMBERQ_P             , "NUMBER?" );
  CreateEntryC ( ID_OR                    , "OR", 0 );
  CreateEntryC ( ID_OVER                  , "OVER", 0 );
  CreateEntryC ( ID_PICK                  , "PICK",  0 );
  CreateEntryC ( ID_PLUS                  , "+",  0 );
  CreateEntryC ( ID_PLUSLOOP_P            , "(+LOOP)", 0 );
  CreateEntryC ( ID_PLUS_STORE            , "+!",  0 );
  CreateEntryC ( ID_QUIT_P                , "(QUIT)",  0 );
  DeferredC    ( ID_QUIT_P                , "QUIT" );
  CreateEntryC ( ID_QDO_P                 , "(?DO)", 0 );
  CreateEntryC ( ID_QDUP                  , "?DUP",  0 );
  CreateEntryC ( ID_QTERMINAL             , "?TERMINAL",  0 );
  CreateEntryC ( ID_QTERMINAL             , "KEY?",  0 );
  CreateEntryC ( ID_REFILL                , "REFILL",  0 );
  CreateEntryC ( ID_RESIZE                , "RESIZE",  0 );
  CreateEntryC ( ID_ROLL                  , "ROLL",  0 );
  CreateEntryC ( ID_ROT                   , "ROT",  0 );
  CreateEntryC ( ID_RSHIFT                , "RSHIFT",  0 );
  CreateEntryC ( ID_R_DROP                , "RDROP",  0 );
  CreateEntryC ( ID_R_FETCH               , "R@",  0 );
  CreateEntryC ( ID_R_FROM                , "R>",  0 );
  CreateEntryC ( ID_RP_FETCH              , "RP@",  0 );
  CreateEntryC ( ID_RP_STORE              , "RP!",  0 );
  CreateEntryC ( ID_SEMICOLON             , ";",  FLAG_IMMEDIATE );
  CreateEntryC ( ID_SP_FETCH              , "SP@",  0 );
  CreateEntryC ( ID_SP_STORE              , "SP!",  0 );
  CreateEntryC ( ID_STORE                 , "!",  0 );
  CreateEntryC ( ID_SAVE_FORTH_P          , "(SAVE-FORTH)",  0 );
  CreateEntryC ( ID_SCAN                  , "SCAN",  0 );
  CreateEntryC ( ID_SKIP                  , "SKIP",  0 );
  CreateEntryC ( ID_SOURCE                , "SOURCE",  0 );
  CreateEntryC ( ID_SOURCE_SET            , "SET-SOURCE",  0 );
  CreateEntryC ( ID_SOURCE_ID             , "SOURCE-ID",  0 );
  CreateEntryC ( ID_SOURCE_ID_PUSH        , "PUSH-SOURCE-ID",  0 );
  CreateEntryC ( ID_SOURCE_ID_POP         , "POP-SOURCE-ID",  0 );
  CreateEntryC ( ID_SWAP                  , "SWAP",  0 );
  CreateEntryC ( ID_TICK                  , "'", 0 );
  CreateEntryC ( ID_TIMES                 , "*", 0 );
  CreateEntryC ( ID_THROW                 , "THROW", 0 );
  CreateEntryC ( ID_TO_R                  , ">R", 0 );
  CreateEntryC ( ID_TYPE                  , "TYPE", 0 );
  CreateEntryC ( ID_VAR_BASE              , "BASE", 0 );
  CreateEntryC ( ID_VAR_CODE_BASE         , "CODE-BASE", 0 );
  CreateEntryC ( ID_VAR_CODE_LIMIT        , "CODE-LIMIT", 0 );
  CreateEntryC ( ID_VAR_CONTEXT           , "CONTEXT", 0 );
  CreateEntryC ( ID_VAR_DP                , "DP", 0 );
  CreateEntryC ( ID_VAR_ECHO              , "ECHO", 0 );
  CreateEntryC ( ID_VAR_HEADERS_PTR       , "HEADERS-PTR", 0 );
  CreateEntryC ( ID_VAR_HEADERS_BASE      , "HEADERS-BASE", 0 );
  CreateEntryC ( ID_VAR_HEADERS_LIMIT     , "HEADERS-LIMIT", 0 );
  CreateEntryC ( ID_VAR_NUM_TIB           , "#TIB", 0 );
  CreateEntryC ( ID_VAR_RETURN_CODE       , "RETURN-CODE", 0 );
  CreateEntryC ( ID_VAR_TRACE_FLAGS       , "TRACE-FLAGS", 0 );
  CreateEntryC ( ID_VAR_TRACE_LEVEL       , "TRACE-LEVEL", 0 );
  CreateEntryC ( ID_VAR_TRACE_STACK       , "TRACE-STACK", 0 );
  CreateEntryC ( ID_VAR_OUT               , "OUT", 0 );
  CreateEntryC ( ID_VAR_STATE             , "STATE", 0 );
  CreateEntryC ( ID_VAR_TO_IN             , ">IN", 0 );
  CreateEntryC ( ID_WORD                  , "WORD", 0 );
  CreateEntryC ( ID_WORD_FETCH            , "W@", 0 );
  CreateEntryC ( ID_WORD_STORE            , "W!", 0 );
  CreateEntryC ( ID_XOR                   , "XOR", 0 );
  CreateEntryC ( ID_ZERO_BRANCH           , "0BRANCH", 0 );

  if ( SpecialXTs() < 0 )                         {
    DeleteDictionary((void *)dic)                 ;
    return NULL                                   ;
  }                                               ;
   /* Call custom 'C' call builder.              */
  if (!GlueFunctions())                           {
    Report(QObject::tr("Glue functions failure")) ;
    DeleteDictionary((void *)dic)                 ;
    return NULL                                   ;
  }                                               ;
  return (void *) dic                             ;
}

/* Check for dictionary overflow. */
int N::Forth::CheckRoom(void)
{
  int RoomLeft = (char *)Dictionary->dic_HeaderLimit   -
                 (char *)Dictionary->dic_HeaderPtr     ;
  if ( RoomLeft < DIC_SAFETY_MARGIN )                  {
    Error ( "N::Forth::CheckRoom" , PF_ERR_HEADER_ROOM ) ;
    return PF_ERR_HEADER_ROOM                          ;
  }                                                    ;
  RoomLeft = (char *)Dictionary->dic_CodeLimit         -
             (char *)Dictionary->dic_CodePtr.Byte      ;
  if ( RoomLeft < DIC_SAFETY_MARGIN )                  {
    Error ( "N::Forth::CheckRoom" , PF_ERR_CODE_ROOM )   ;
    return PF_ERR_CODE_ROOM                            ;
  }                                                    ;
  return 0                                             ;
}

/* Check to see if name is already in dictionary. */
int N::Forth::CheckRedefinition(const char * FName)
{
  unsigned int XT                                 ;
  int          flag = Find(FName,&XT)             ;
  if ( flag && !VarQuiet )                        {
    QString s = toString(FName)                   ;
    Report(QObject::tr("%1 redefined.\n").arg(s)) ;
  }                                               ;
  return flag                                     ;
}

/* Create a dictionary entry given a string name. */
void N::Forth::SecondaryHeader( const char * FName)
{ /* Check for dictionary overflow. */
  nDropOut          ( CheckRoom() !=0 )                                     ;
  CheckRedefinition ( FName           )                                     ;
  /* Align CODE_HERE                                                       */
  CODE_HERE = (int *)((((unsigned int)CODE_HERE)+UINT32_MASK)&~UINT32_MASK) ;
  CreateEntry((unsigned int)ABS_TO_CODEREL(CODE_HERE),FName,FLAG_SMUDGE   ) ;
}

/* Finish the definition of a Forth word. */
void N::Forth::FinishSecondary(void)
{
  CODE_COMMA ( ID_EXIT ) ;
  UnSmudge   (         ) ;
}

/* Begin compiling a secondary word. */
void N::Forth::StringColon(const char * FName)
{
  SecondaryHeader ( FName ) ;
  VarState = 1              ;
}

/* Read the next ExecToken from the Source and create a word. */
void N::Forth::Colon(void)
{
  char * FName                    ;
  DepthAtColon = DATA_STACK_DEPTH ;
  FName = Word ( BLANK )          ;
  if ( *FName > 0 )               {
    StringColon ( FName )         ;
  }                               ;
}

void N::Forth::StringCreate(char * FName)
{
  SecondaryHeader ( FName       ) ;
    CODE_COMMA    ( ID_CREATE_P ) ;
    CODE_COMMA    ( ID_EXIT     ) ;
  FinishSecondary (             ) ;
}

void N::Forth::StringDefer(const char * FName,unsigned int DefaultXT)
{
  SecondaryHeader ( FName      ) ;
    CODE_COMMA    ( ID_DEFER_P ) ;
    CODE_COMMA    ( DefaultXT  ) ;
  FinishSecondary (            ) ;
}

/* Read the next ExecToken from the Source and create a word. */
void N::Forth::Create(void)
{
  char * FName = Word ( BLANK ) ;
  if ( (*FName) > 0 )           {
    StringCreate ( FName )      ;
  }                             ;
}

/* Read the next token from the Source and create a word. */
void N::Forth::Defer(void)
{
  char * FName = Word ( BLANK )      ;
  if ( (*FName) > 0 )                {
    StringDefer ( FName, ID_QUIT_P ) ;
  }                                  ;
}

/* Unsmudge the word to make it visible. */
void N::Forth::UnSmudge(void)
{
  (*(char*)VarContext) &= ~FLAG_SMUDGE ;
}

/* Implement ; */
int N::Forth::SemiColon(void)
{
  int exception = 0                                ;
  VarState      = 0                                ;
  //////////////////////////////////////////////////
  if ( (DepthAtColon != DATA_STACK_DEPTH        ) &&
       (DepthAtColon != DEPTH_AT_COLON_INVALID) )  { /* Ignore if no ':' */
    exception = THROW_SEMICOLON                    ;
  } else FinishSecondary ( )                       ;
  DepthAtColon = DEPTH_AT_COLON_INVALID            ;
  return exception                                 ;
}

/**************************************************************/
/* Used to pull a number from the dictionary to the stack */
void N::Forth::Literal2(int dHi,int dLo)
{
  CODE_COMMA ( ID_2LITERAL_P ) ;
  CODE_COMMA ( dHi           ) ;
  CODE_COMMA ( dLo           ) ;
}

void N::Forth::LiteralA(int Num)
{
  CODE_COMMA ( ID_ALITERAL_P ) ;
  CODE_COMMA ( Num           ) ;
}

void N::Forth::Literal(int Num)
{
  CODE_COMMA ( ID_LITERAL_P ) ;
  CODE_COMMA ( Num          ) ;
}

void N::Forth::LiteralFP(double fnum)
{ /* Hack for Metrowerks complier which won't compile the original expression. */
  double * temp                                                ;
  int    * dicPtr                                              ;
  /* Make sure that literal float data is float aligned.      */
  dicPtr = CODE_HERE + 1                                       ;
  while( (((unsigned int)dicPtr++) & (sizeof(double)-1)) != 0) {
     CODE_COMMA ( ID_NOOP )                                    ;
  }                                                            ;
  CODE_COMMA ( ID_FP_FLITERAL_P )                              ;
  temp = (double *)CODE_HERE                                   ;
  WRITE_FLOAT_DIC(temp,fnum)                                   ; /* Write to dictionary. */
  temp++                                                       ;
  CODE_HERE = (int *)temp                                      ;
}

int N::Forth::FindAndCompile(const char * theWord)
{
  int          Num                                         ;
  unsigned int XT                                          ;
  int          Flag      = Find ( theWord , &XT )          ;
  unsigned int exception = 0                               ;
  if ( Flag == -1 )                                        { /* Is it a normal word ? */
    if ( VarState !=0 )                                    { /* compiling? */
      CODE_COMMA ( XT )                                    ;
    } else                                                 {
      exception = Catch ( XT )                             ;
    }                                                      ;
  } else
  if ( Flag == 1 )                                         { /* or is it IMMEDIATE ? */
    exception = Catch ( XT )                               ;
  } else                                                   { /* try to interpret it as a number. */
    /* Call deferred NUMBER?                              */
    int NumResult                                          ;
    PUSH_DATA_STACK ( theWord )                            ; /* Push text of number */
    exception = Catch ( NumberQXT )                        ;
    if ( exception !=0 ) return exception                  ;
    NumResult = POP_DATA_STACK                             ; /* Success? */
    switch ( NumResult )                                   {
      case NUM_TYPE_SINGLE                                 :
        if ( VarState != 0 )                               { /* compiling? */
          Num = POP_DATA_STACK                             ;
          Literal ( Num )                                  ;
        }                                                  ;
      break                                                ;
      case NUM_TYPE_DOUBLE                                 :
        if ( VarState != 0 )                               { /* compiling? */
          Num = POP_DATA_STACK                             ; /* get hi portion */
          Literal2 ( Num, POP_DATA_STACK )                 ;
        }                                                  ;
      break                                                ;
      case NUM_TYPE_FLOAT                                  :
        if ( VarState !=0 )                                { /* compiling? */
          LiteralFP ( *(TaskNow->td_FloatStackPtr++) )     ;
        }                                                  ;
      break                                                ;
      case NUM_TYPE_BAD                                    :
      default                                              :
        if ( (*theWord) > 0 )                              {
          QString s = toString(theWord)                    ;
          Report(QObject::tr("%1  ? - unrecognized word!\n")
                 .arg(s)                                 ) ;
        }                                                  ;
        exception = THROW_UNDEFINED_WORD                   ;
      break                                                ;
    }                                                      ;
  }                                                        ;
  return exception                                         ;
}

/*******************************************************
 * Forth outer interpreter.  Parses words from Source. *
 * Executes them or compiles them based on STATE.      *
 *******************************************************/

int N::Forth::Interpret(void)
{
  int    flag                                      ;
  char * theWord                                   ;
  int    exception = 0                             ;
  /* Is there any text left in Source ?           */
  while ( TaskNow->td_IN < TaskNow->td_SourceNum ) {
    theWord = Word ( BLANK )                       ;
    if ( (*theWord) > 0 )                          {
      flag = 0                                     ;
      if ( LocalCompilerXT )                       {
        PUSH_DATA_STACK ( theWord )                ; /* Push word. */
        exception = Catch ( LocalCompilerXT )      ;
        if ( exception !=0 ) return exception      ;
        flag = POP_DATA_STACK                      ; /* Compiled local? */
      }                                            ;
      if ( flag == 0 )                             {
        exception = FindAndCompile ( theWord )     ;
        if ( exception != 0 ) return exception     ;
      }                                            ;
    }                                              ;
  }                                                ;
  return exception                                 ;
}

int N::Forth::OK(void)
{
  int exception = 0                                                 ;
  /* Check for stack underflow.   %Q what about overflows?         */
  if ( (TaskNow->td_StackBase - TaskNow->td_StackPtr) < 0 )         {
    exception = THROW_STACK_UNDERFLOW                               ;
  } else
  if ((TaskNow->td_FloatStackBase - TaskNow->td_FloatStackPtr) < 0) {
    exception = THROW_FLOAT_STACK_UNDERFLOW                         ;
  } else
  if ( TaskNow->td_InputStream == NULL)                             {
    if ( !VarState )                                                {  /* executing? */
      if ( !VarQuiet )                                              {
        Report ( QObject::tr("   ok\n") )                           ;
        if ( VarTraceStack ) DotS ( )                               ;
      } else Report(QString("\n"))                                  ;
    }                                                               ;
  }                                                                 ;
  return exception                                                  ;
}

/* Cleanup Include stack by popping and closing files. */
void N::Forth::HandleIncludeError(void)
{
  File * cur = NULL                   ;
  while ( (cur = PopInput()) != NULL) {
    cur->close()                      ;
  }                                   ;
}

/* Interpret input in a loop. */
int N::Forth::OuterInterpreterLoop(void)
{
  int exception = 0             ;
  do                            {
    exception = Refill ( )      ;
    if ( exception <= 0 ) break ;
    exception = Interpret ( )   ;
    if ( exception == 0 )       {
      exception = OK ( )        ;
    }                           ;
  } while( exception == 0 )     ;
  return exception              ;
}

bool N::Forth::PushInput(File * file)
{
  IncludeFrame * inf = NULL                                            ;
  /* Push current input state onto special include stack.             */
  if (IncludeIndex < MAX_INCLUDE_DEPTH)                                {
    inf                 = &IncludeStk[IncludeIndex++]                  ;
    inf->inf_File       = TaskNow->td_InputStream                      ;
    inf->inf_IN         = TaskNow->td_IN                               ;
    inf->inf_LineNumber = TaskNow->td_LineNumber                       ;
    inf->inf_SourceNum  = TaskNow->td_SourceNum                        ;
    if ((inf->inf_SourceNum>0) && (inf->inf_SourceNum<(TIB_SIZE-1)))   {
      memcpy(inf->inf_SaveTIB,TaskNow->td_TIB,inf->inf_SourceNum+1)    ;
    }                                                                  ;
    /* Set new current input.                                         */
    if (file!=NULL)                                                    {
      Report(QObject::tr("PushInput: %1\n\n").arg(file->fileName()))   ;
    } else                                                             {
      Report(QObject::tr("Switch input stream to standard input\n\n")) ;
    }                                                                  ;
    TaskNow->td_InputStream = file                                     ;
    TaskNow->td_LineNumber  = 0                                        ;
  } else                                                               {
    Report(QObject::tr("PushInput: max depth exceeded.\n"))            ;
    return false                                                       ;
  }                                                                    ;
  return true                                                          ;
}

/*******************************************************
** Go back to reading previous stream.                 *
** Just return TaskNow->td_InputStream upon underflow. *
********************************************************/

N::File * N::Forth::PopInput(void)
{
  IncludeFrame * inf    = NULL                                      ;
  File         * Result = TaskNow->td_InputStream                   ;
  /* Restore input state.                                          */
  if ( IncludeIndex > 0 )                                           {
    inf = &IncludeStk[--IncludeIndex]                               ;
    TaskNow->td_InputStream = inf->inf_File                         ;
    TaskNow->td_IN          = inf->inf_IN                           ;
    TaskNow->td_LineNumber  = inf->inf_LineNumber                   ;
    TaskNow->td_SourceNum   = inf->inf_SourceNum                    ;
    /* Copy TIB plus any NUL terminator into saved area.           */
    if ((inf->inf_SourceNum>0)&&(inf->inf_SourceNum<(TIB_SIZE-1)))  {
      memcpy(TaskNow->td_TIB,inf->inf_SaveTIB,inf->inf_SourceNum+1) ;
    }                                                               ;
  }                                                                 ;
  return Result                                                     ;
}

/***************************************************************
** Convert file pointer to value consistent with SOURCE-ID.
***************************************************************/

unsigned int N::Forth::toSourceId(File * file)
{ // questionable code
  unsigned int Result = 0       ;
  if (NotNull(file))            {
    Result = (unsigned int)file ;
  }                             ;
  return Result                 ;
}

/***************************************************************
** Convert file pointer to value consistent with SOURCE-ID.
***************************************************************/

N::File * N::Forth::toStream(unsigned int id)
{ // questionable code
  File * stream = (File *)id ;
  return stream              ;
}

/* Include then close a file */
int N::Forth::IncludeFile(File * file)
{
  int exception                                                 ;
  /* Push file stream.                                         */
  exception = PushInput( file )                                 ;
  if (exception<0) return exception                             ;
  /* Run outer interpreter for stream.                         */
  exception = OuterInterpreterLoop()                            ;
  if ( exception )                                              {
    int i                                                       ;
    QString m                                                   ;
    /* Report line number and nesting level.                   */
    Report                                                      (
      QObject::tr("INCLUDE error on line #%1, level = %2\n"     )
      .arg( TaskNow->td_LineNumber , 0 , Radix                  )
      .arg( IncludeIndex           , 0 , Radix                  )
    )                                                           ;
    /* Dump line of error and show offset in line for >IN      */
    for (i=0;i<TaskNow->td_SourceNum;i++)                       {
      char c = TaskNow->td_SourcePtr[i]                         ;
      if ( c == '\t' ) c = ' '                                  ;
      m.append(QChar(c))                                        ;
    }                                                           ;
    m += "\n"                                                   ;
    for (i=0;i<(TaskNow->td_IN-1);i++) m.append('^')            ;
    m += "\n"                                                   ;
    Report ( m )                                                ;
  }                                                             ;
  /* Pop file stream.                                          */
  PopInput ( )                                                  ;
  /* ANSI spec specifies that this should also close the file. */
  if (NotNull(file)) file->close()                              ;
  return exception                                              ;
}

int N::Forth::Feed(char * buffer,int maxChars)
{
  return 0 ;
}

/***********************************
 * Receive line from input stream. *
 * Return length, or -1 for EOF.   *
 ***********************************/

int N::Forth::readLine(char * buffer,int maxChars,File * stream)
{
  if (IsNull(stream)) return Feed(buffer,maxChars) ;
  if ( stream->atEnd      ()) return -1            ;
  if (!stream->canReadLine()) return  0            ;
  QByteArray B = stream->readLine(maxChars)        ;
  B.replace("\r","")                               ;
  B.replace("\n","")                               ;
  memcpy(buffer,B.data(),B.size())                 ;
  return B.size()                                  ;
}

/***************************************************************
** Include file based on 'C' name.
***************************************************************/

int N::Forth::Include(const char * FileName)
{
  QString actualFile = ""                                              ;
  QString fn = QString::fromUtf8(FileName)                             ;
  for (int i=0;actualFile.length()<=0 && i<Includes.count();i++)       {
    QString ef = Includes[i].absoluteFilePath(fn)                      ;
    QFileInfo FX(ef)                                                   ;
    if (FX.exists()) actualFile = ef                                   ;
  }                                                                    ;
  if (actualFile.length()<=0) return -1                                ;
  QFileInfo F(actualFile)                                              ;
  if (!F.exists()) return -1                                           ;
  //////////////////////////////////////////////////////////////////////
  File * fid = new File()                                              ;
  int    Result                                                        ;
  char   buffer[32]                                                    ;
  int    numChars                                                      ;
  int    len                                                           ;
  if (!fid->open(QIODevice::ReadOnly))                                 {
      Report(QObject::tr("Include %1 could not open").arg(actualFile)) ;
    delete fid                                                         ;
    return -1                                                          ;
  }                                                                    ;
  //////////////////////////////////////////////////////////////////////
  memset(buffer,0,32)                                                  ;
  strcpy(buffer,"::::")                                                ;
  /* Create a dictionary word named ::::FileName for FILE?            */
  len = (int)strlen(FileName)                                          ;
  numChars = ( len > (32-4-1) ) ? (32-4-1) : len                       ;
  memcpy(&buffer[4],&FileName[len-numChars], numChars+1)               ;
  CreateEntryC ( ID_NOOP , buffer , 0 )                                ;
  Result = IncludeFile( fid )                                          ;
  /* Create a dictionary word named ;;;; for FILE?                    */
  CreateEntryC ( ID_NOOP , ";;;;" , 0 )                                ;
  return Result                                                        ;
}

/**************************************************************
** ( -- , fill Source from current stream )
** Return 1 if successful, 0 for EOF, or a negative error.
*/

int N::Forth::Refill(void)
{
  int Num                                                                   ;
  int Result = 1                                                            ;
  /* reset >IN for parser                                                  */
  TaskNow->td_IN = 0                                                        ;
  /* get line from current stream                                          */
  if ( TaskNow->td_InputStream == NULL )                                    {
    /* ACCEPT is deferred so we call it through the dictionary.            */
    PUSH_DATA_STACK ( TaskNow->td_SourcePtr )                               ;
    PUSH_DATA_STACK ( TIB_SIZE              )                               ;
    Catch           ( AcceptPXT             )                               ;
    Num = POP_DATA_STACK                                                    ;
    if ( Num < 0 )                                                          {
      Result = Num                                                          ;
      return -1                                                             ;
    }                                                                       ;
  } else                                                                    {
    Num = readLine( TaskNow->td_SourcePtr,TIB_SIZE,TaskNow->td_InputStream) ;
    if ( Num == -1 )                                                        {
      Result = 0                                                            ;
      Num    = 0                                                            ;
    }                                                                       ;
  }                                                                         ;
  /* Bump for include.                                                     */
  TaskNow->td_SourceNum = Num                                               ;
  TaskNow->td_LineNumber++                                                  ;
  /* echo input if requested                                               */
  if ( VarEcho && ( Num > 0))                                               {
    char * dd = new char [ TaskNow->td_SourceNum + 2 ]                      ;
    memset ( dd , 0                     , TaskNow->td_SourceNum + 1 )       ;
    memcpy ( dd , TaskNow->td_SourcePtr , TaskNow->td_SourceNum + 1 )       ;
    strcpy ( dd , "\n"                                              )       ;
    Report ( QString(dd)                                            )       ;
  }                                                                         ;
  return Result                                                             ;
}

/* Display name of executing routine. */
void N::Forth::TraceNames(unsigned int Token,int Level)
{
  char * DebugName = NULL                                              ;
  if ( toName( Token, (const char **)&DebugName ) )                    {
    int NumSpaces                                                      ;
    QString m                                                          ;
    if ( TaskNow->td_OUT > 0 ) Report(QString("\n"))                   ;
    m  = ">"                                                           ;
    nFullLoop ( i , Level )                                            {
      m += "  "                                                        ;
    }                                                                  ;
    m += toString ( DebugName )                                        ;
    /* Space out to column N then .S                                  */
    NumSpaces = 30 - TaskNow->td_OUT                                   ;
    nFullLoop ( i , NumSpaces )                                        {
      m += " "                                                         ;
    }                                                                  ;
    Report ( m )                                                       ;
    DotS ( )                                                           ;
    /* No longer needed? TaskNow->td_OUT = 0;                         */
    /* !!! Hack for DotS()                                            */
  } else                                                               {
    Report(QObject::tr("Couldn't find Name for %1\n").arg(Token,0,16)) ;
  }                                                                    ;
}

/*
Dictionary File Format based on IFF standard.
The chunk IDs, sizes, and data values are all Big Endian in conformance with the IFF standard.
The dictionaries may be big or little endian.
    'FORM'
    size
    'P4TH'  -  Form Identifier

Chunks
    'P4DI'
    size
    struct DictionaryInfoChunk

    'P4NM'
    size
    Name and Header portion of dictionary. (Big or Little Endian) (Optional)

    'P4CD'
    size
    Code portion of dictionary. (Big or Little Endian)
*/

int N::Forth::Catch(unsigned int XT)
{
  register int    TopOfStack                                                ;
  register int  * DataStackPtr                                              ;
  register int  * ReturnStackPtr                                            ;
  register int  * InsPtr = NULL                                             ;
  register int    Token                                                     ;
  int             scratch                                                   ;
  double          fpTopOfStack                                              ;
  double        * FloatStackPtr                                             ;
  double          fpScratch                                                 ;
  double          fpTemp                                                    ;
  double        * InitialFloatStack                                         ;
  int             Level     = 0                                             ;
  int           * LocalsPtr = NULL                                          ;
  int             Temp                                                      ;
  int           * InitialReturnStack                                        ;
  int           * InitialDataStack                                          ;
  int             FakeSecondary [ 2 ]                                       ;
  char          * CharPtr                                                   ;
  int           * CellPtr                                                   ;
  File          * file = NULL                                               ;
  unsigned char * CodeBase            = (unsigned char *) CODE_BASE         ;
  int             ExceptionReturnCode = 0                                   ;
  /* Initialize FakeSecondary this way to avoid having stuff in the data
   * section, which is not supported for some embedded system loaders.     */
  FakeSecondary[0]   = 0                                                    ;
  FakeSecondary[1]   = ID_EXIT                                              ; /* For EXECUTE */
  /* Move data from task structure to registers for speed.                 */
  LOAD_REGISTERS                                                            ;
  /* Save initial stack depths for THROW                                   */
  InitialReturnStack = TORPTR                                               ;
  InitialDataStack   = STKPTR                                               ;
  InitialFloatStack  = FP_STKPTR                                            ;
  Token              = XT                                                   ;
  ///////////////////////////////////////////////////////////////////////////
  do                                                                        {
    /* If secondary, thread down code tree until we hit a primitive.       */
    while ( !IsTokenPrimitive( Token ) )                                    {
      if ((VarTraceFlags & TRACE_INNER) )                                   {
        Report(QObject::tr("Catch: Secondary Token = 0x%1, InsPtr = 0x%2\n" )
               .arg(Token               ,0,16                               )
               .arg((unsigned int)InsPtr,0,16                           ) ) ;
      }                                                                     ;
      TRACENAMES                                                            ;
      /* Save IP on return stack like a JSR.                               */
      M_R_PUSH ( InsPtr )                                                   ;
      /* Convert execution token to absolute address.                      */
      InsPtr = (int *)( LOCAL_CODEREL_TO_ABS(Token) )                       ;
      /* Fetch token at IP.                                                */
      Token = READ_CELL_DIC(InsPtr++)                                       ;
      /* Bump level for trace display                                      */
      Level++                                                               ;
    }                                                                       ;
    TRACENAMES                                                              ;
    /* Execute primitive Token.                                            */
    switch ( Token )                                                        {
      /* Pop up a level in Forth inner interpreter.
       * Used to implement semicolon.
       * Put first in switch because ID_EXIT==0                            */
        case ID_EXIT                                                        :
          InsPtr =  (int *) M_R_POP                                         ;
          Level  --                                                         ;
        break                                                               ;
        case ID_1MINUS:  TOS--; break                                       ;
        case ID_1PLUS:   TOS++; break                                       ;
        case ID_2LITERAL                                                    :
          Literal2 ( TOS, M_POP )                                           ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_2LITERAL_P                                                  :
          /* hi part stored first, put on top of stack                     */
          PUSH_TOS                                                          ;
          TOS  = READ_CELL_DIC(InsPtr++)                                    ;
          M_PUSH(READ_CELL_DIC(InsPtr++))                                   ;
        break                                                               ;
        case ID_2MINUS : TOS -= 2; break                                    ;
        case ID_2PLUS  : TOS += 2; break                                    ;
        case ID_2OVER  : /* ( a b c d -- a b c d a b )                     */
          PUSH_TOS                                                          ;
          scratch = M_STACK(3)                                              ;
          M_PUSH ( scratch )                                                ;
          TOS = M_STACK(3)                                                  ;
        break                                                               ;
        case ID_2SWAP  : /* ( a b c d -- c d a b )                         */
          scratch    = M_STACK(0)                                           ; /* c */
          M_STACK(0) = M_STACK(2)                                           ; /* a */
          M_STACK(2) = scratch                                              ; /* c */
          scratch    = TOS                                                  ; /* d */
          TOS = M_STACK(1)                                                  ; /* b */
          M_STACK(1) = scratch                                              ; /* d */
        break                                                               ;
        case ID_2DUP                                                        :
          /* ( a b -- a b a b )                                            */
          PUSH_TOS                                                          ;
          scratch = M_STACK(1)                                              ;
          M_PUSH(scratch)                                                   ;
        break                                                               ;
        case ID_2_R_FETCH                                                   :
          PUSH_TOS                                                          ;
          M_PUSH ( (*(TORPTR+1)) )                                          ;
          TOS =  (  *(TORPTR  )  )                                          ;
        break                                                               ;
        case ID_2_R_FROM                                                    :
          PUSH_TOS                                                          ;
          TOS  =   M_R_POP                                                  ;
          M_PUSH ( M_R_POP )                                                ;
        break                                                               ;
        case ID_2_TO_R                                                      :
          M_R_PUSH ( M_POP )                                                ;
          M_R_PUSH ( TOS   )                                                ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_ACCEPT_P : /* ( c-addr +n1 -- +n2 )                        */
          CharPtr = (char *) M_POP                                          ;
          TOS = Feed ( CharPtr, TOS )                                       ;
        break                                                               ;
        case ID_ALITERAL                                                    :
          LiteralA ( ABS_TO_CODEREL(TOS) )                                  ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_ALITERAL_P                                                  :
          PUSH_TOS                                                          ;
          TOS = (int)LOCAL_CODEREL_TO_ABS(READ_CELL_DIC(InsPtr++))          ;
        break                                                               ;
        /* Allocate some extra and put validation identifier at base       */
        #define PF_MEMORY_VALIDATOR  (0xA81B4D69)
        case ID_ALLOCATE                                                    :
        /*Allocate at least one cell's worth because we clobber first cell.*/
          if ( TOS < sizeof(int) )                                          {
            Temp = sizeof(int)                                              ;
          } else                                                            {
            Temp = TOS                                                      ;
          }                                                                 ;
          /* Allocate extra cells worth because we store validation info.  */
          CellPtr = (int *)new char [ Temp + sizeof(int) ]                  ;
          if ( NotNull(CellPtr) )                                           {
          /* This was broken into two steps because different
           * compilers incremented CellPtr before or after the XOR step.   */
            Temp       = (int)CellPtr ^ PF_MEMORY_VALIDATOR                 ;
            *CellPtr++ = Temp                                               ;
            M_PUSH( (int) CellPtr )                                         ;
            TOS = 0                                                         ;
          } else                                                            {
            M_PUSH ( 0 )                                                    ;
            TOS = -1                                                        ;
          }                                                                 ;
        break                                                               ;
        case ID_AND                                                         :
          BINARY_OP( & )                                                    ;
        break                                                               ;
        /* Arithmetic right shift                                          */
        case ID_ARSHIFT                                                     :
          BINARY_OP( >> )                                                   ;
        break                                                               ;
        case ID_BODY_OFFSET                                                 :
          PUSH_TOS                                                          ;
          TOS = CREATE_BODY_OFFSET                                          ;
        break                                                               ;
        /* Branch is followed by an offset relative to address of offset.  */
        case ID_BRANCH                                                      :
          M_BRANCH                                                          ;
        break                                                               ;
        case ID_BYE                                                         :
          M_THROW ( THROW_BYE )                                             ;
        break                                                               ;
        case ID_BAIL                                                        :
          Report ( QObject::tr("Emergency exit.\n") )                       ;
          Exit   ( 1                                )                       ;
        break                                                               ;
        case ID_CATCH                                                       :
          scratch = TOS                                                     ;
          TOS     = M_POP                                                   ;
          SAVE_REGISTERS                                                    ;
          scratch = Catch( scratch )                                        ;
          LOAD_REGISTERS                                                    ;
          M_PUSH( TOS )                                                     ;
          TOS = scratch                                                     ;
        break                                                               ;
        case ID_CALL_C                                                      :
          SAVE_REGISTERS                                                    ;
          scratch = READ_CELL_DIC(InsPtr++)                                 ;
          Call( scratch & 0xFFFF                                            ,
               (scratch >> 31) & 1                                          ,
               (scratch >> 24) & 0x7F                                     ) ;
          LOAD_REGISTERS                                                    ;
        break                                                               ;
        /* Support 32/64 bit operation.                                    */
        case ID_CELL                                                        :
           M_PUSH ( TOS )                                                   ;
           TOS = sizeof(int)                                                ;
        break                                                               ;
        case ID_CELLS  : TOS = TOS * sizeof(int) ; break                    ;
        case ID_CFETCH : TOS = *((unsigned char *) TOS)                     ;
        break                                                               ;
        case ID_CMOVE                                                     : { /* ( src dst n -- ) */
          register char *DstPtr = (char *) M_POP                            ; /* dst */
          CharPtr = (char *) M_POP                                          ; /* src */
          for (scratch=0                                                    ;
               (unsigned int)scratch<(unsigned int)TOS                      ;
               scratch++                                                  ) {
            *DstPtr++ = *CharPtr++                                          ;
          }                                                                 ;
          M_DROP                                                            ;
        } break                                                             ;
        case ID_CMOVE_UP                                                  : { /* ( src dst n -- ) */
          register char *DstPtr = ((char *) M_POP) + TOS                    ; /* dst */
          CharPtr = ((char *) M_POP) + TOS                                  ; /* src */
          for (scratch=0                                                    ;
               (unsigned int)scratch<(unsigned int)TOS                      ;
               scratch++                                                  ) {
            *(--DstPtr) = *(--CharPtr)                                      ;
          }                                                                 ;
          M_DROP                                                            ;
        } break                                                             ;
        case ID_CLEAR                                                       :
          SAVE_REGISTERS                                                    ;
            Clear ( )                                                       ;
          LOAD_REGISTERS                                                    ;
        break                                                               ;
        case ID_COLON                                                       :
          SAVE_REGISTERS                                                    ;
            Colon ( )                                                       ;
          LOAD_REGISTERS                                                    ;
        break                                                               ;
        case ID_COLON_P                                                     : /* ( $name xt -- ) */
          CreateEntry ( TOS, (char *) M_POP, 0 )                            ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_COMPARE                                                   : {
          const char *s1                                                    ;
          const char *s2 = (const char *) M_POP                             ;
          int len1       = M_POP                                            ;
          s1   = (const char *) M_POP                                       ;
          TOS  = compare ( s1, len1, s2, TOS )                              ;
        } break                                                             ;
        case ID_COMP_EQUAL                                                  : /* ( a b -- flag , Comparisons ) */
          TOS = ( TOS == M_POP ) ? FTRUE : FFALSE                           ;
        break                                                               ;
        case ID_COMP_NOT_EQUAL                                              :
          TOS = ( TOS != M_POP ) ? FTRUE : FFALSE                           ;
        break                                                               ;
        case ID_COMP_GREATERTHAN                                            :
          TOS = ( M_POP > TOS  ) ? FTRUE : FFALSE                           ;
        break                                                               ;
        case ID_COMP_LESSTHAN                                               :
          TOS = (  M_POP < TOS ) ? FTRUE : FFALSE                           ;
        break                                                               ;
        case ID_COMP_U_GREATERTHAN                                          :
          TOS = ( ((unsigned int)M_POP) > ((unsigned int)TOS) )             ?
                FTRUE : FFALSE                                              ;
        break                                                               ;
        case ID_COMP_U_LESSTHAN                                             :
          TOS = ( ((unsigned int)M_POP) < ((unsigned int)TOS) )             ?
                FTRUE : FFALSE                                              ;
        break                                                               ;
        case ID_COMP_ZERO_EQUAL                                             :
          TOS = ( TOS == 0 ) ? FTRUE : FFALSE                               ;
        break                                                               ;
        case ID_COMP_ZERO_NOT_EQUAL                                         :
          TOS = ( TOS != 0 ) ? FTRUE : FALSE                                ;
        break                                                               ;
        case ID_COMP_ZERO_GREATERTHAN                                       :
          TOS = ( TOS > 0 ) ? FTRUE : FFALSE                                ;
        break                                                               ;
        case ID_COMP_ZERO_LESSTHAN                                          :
          TOS = ( TOS < 0 ) ? FTRUE : FFALSE                                ;
        break                                                               ;
        case ID_CR                                                          :
          Report ( QString("\n") )                                          ;
        break                                                               ;
        case ID_CREATE                                                      :
          SAVE_REGISTERS                                                    ;
            Create ( )                                                      ;
          LOAD_REGISTERS                                                    ;
        break                                                               ;
        /* Put address of body on stack.  Insptr points after code start.  */
        case ID_CREATE_P                                                    :
          PUSH_TOS                                                          ;
          TOS = (int) ( (char *)InsPtr - sizeof(int) + CREATE_BODY_OFFSET ) ;
        break                                                               ;
        case ID_CSTORE                                                      : /* ( c caddr -- ) */
          *((unsigned char *) TOS) = (unsigned char) M_POP                  ;
          M_DROP                                                            ;
        break                                                               ;
        /* Double precision add.                                           */
        case ID_D_PLUS                                                    : { /* D+ ( al ah bl bh -- sl sh ) */
          register unsigned int ah,al,bl,sh,sl                              ;
          bl  = M_POP                                                       ;
          ah  = M_POP                                                       ;
          al  = M_POP                                                       ;
          sh  = 0                                                           ;
          sl  = al + bl                                                     ;
          if ( sl < bl ) sh = 1                                             ; /* Carry */
          sh += ah + TOS                                                    ;
          M_PUSH( sl )                                                      ;
          TOS = sh                                                          ;
        } break                                                             ;
        /* Double precision subtract.                                      */
        case ID_D_MINUS                                                     : { /* D- ( al ah bl bh -- sl sh ) */
          register unsigned int ah,al,bl,sh,sl                              ;
          bl  = M_POP                                                       ;
          ah  = M_POP                                                       ;
          al  = M_POP                                                       ;
          sh  = 0                                                           ;
          sl  = al - bl                                                     ;
          if ( al < bl ) sh = 1                                             ; /* Borrow */
          sh  = ah - TOS - sh                                               ;
          M_PUSH ( sl )                                                     ;
          TOS = sh                                                          ;
        } break                                                             ;
        /* Perform cell*cell bit multiply for a 2 cell result, by factoring
         * into half cell quantities.
         * Using an improved algorithm suggested by Steve Green.
         * Converted to 64-bit by Aleksej Saushev.                         */
        case ID_D_UMTIMES                                                 : { /* UM* ( a b -- lo hi ) */
          unsigned int ahi , alo , bhi , blo                                ; /* input parts */
          unsigned int lo  , hi  , temp                                     ; /* Get values from stack. */
          ahi  = M_POP                                                      ;
          bhi  = TOS                                                        ; /* Break into hi and lo 16 bit parts. */
          alo  = LOWER_HALF(ahi)                                            ;
          ahi  = ahi >> HNBITS                                              ;
          blo  = LOWER_HALF(bhi)                                            ;
          bhi  = bhi >> HNBITS                                              ;
          lo   = 0                                                          ;
          hi   = 0                                                          ; /* higher part: ahi * bhi */
          hi  += ahi * bhi                                                  ; /* middle (overlapping) part: ahi * blo */
          temp = ahi * blo                                                  ;
          lo  += LOWER_HALF(temp)                                           ;
          hi  += temp >> HNBITS                                             ; /* middle (overlapping) part: alo * bhi  */
          temp = alo * bhi                                                  ;
          lo  += LOWER_HALF(temp)                                           ;
          hi  += temp >> HNBITS                                             ; /* lower part: alo * blo */
          temp = alo * blo                                                  ;
          /* its higher half overlaps with middle's lower half:            */
          lo  += temp >> HNBITS                                             ; /* process carry: */
          hi  += lo >> HNBITS                                               ;
          lo   = LOWER_HALF(lo)                                             ; /* combine lower part of result: */
          lo   = (lo << HNBITS) + LOWER_HALF(temp)                          ;
          M_PUSH ( lo )                                                     ;
          TOS  = hi                                                         ;
        } break                                                             ;
        /* Perform cell*cell bit multiply for 2 cell result, using shift and add. */
        case ID_D_MTIMES                                                  : { /* M* ( a b -- pl ph ) */
          unsigned int ahi, alo, bhi, blo                                   ; /* input parts */
          unsigned int lo, hi, temp                                         ;
          int          sg                                                   ;
          /* Get values from stack.                                        */
          ahi = M_POP                                                       ;
          bhi = TOS                                                         ;
          /* Calculate product sign:                                       */
          sg  = ((int)(ahi ^ bhi) < 0)                                      ;
          /* Take absolute values and reduce to um*                        */
          if ((int)ahi < 0) ahi = (unsigned int)(-ahi)                      ;
          if ((int)bhi < 0) bhi = (unsigned int)(-bhi)                      ;
          /* Break into hi and lo 16 bit parts.                            */
          alo  = LOWER_HALF(ahi)                                            ;
          ahi  = ahi >> HNBITS                                              ;
          blo  = LOWER_HALF(bhi)                                            ;
          bhi  = bhi >> HNBITS                                              ;
          lo   = 0                                                          ;
          hi   = 0                                                          ;
          /* higher part: ahi * bhi                                        */
          hi  += ahi * bhi                                                  ;
          /* middle (overlapping) part: ahi * blo                          */
          temp = ahi * blo                                                  ;
          lo  += LOWER_HALF(temp)                                           ;
          hi  += temp >> HNBITS                                             ;
          /* middle (overlapping) part: alo * bhi                          */
          temp = alo * bhi                                                  ;
          lo  += LOWER_HALF(temp)                                           ;
          hi  += temp >> HNBITS                                             ;
          /* lower part: alo * blo                                         */
          temp = alo * blo                                                  ;
          /* its higher half overlaps with middle's lower half:            */
          lo  += temp >> HNBITS                                             ;
          /* process carry:                                                */
          hi  += lo >> HNBITS                                               ;
          lo   = LOWER_HALF(lo)                                             ;
          /* combine lower part of result:                                 */
          lo   = (lo << HNBITS) + LOWER_HALF(temp)                          ;
          /* Negate product if one operand negative.                       */
          if ( sg )                                                         {
            /* lo = (ucell_t)(- lo);                                       */
            lo = ~lo + 1                                                    ;
            hi = ~hi + ((lo == 0) ? 1 : 0)                                  ;
          }                                                                 ;
          M_PUSH ( lo )                                                     ;
          TOS = hi                                                          ;
        } break                                                             ;
        #define DULT(du1l,du1h,du2l,du2h)                                   \
        ( (du2h<du1h) ? FALSE : ( (du2h==du1h) ? (du1l<du2l) : TRUE)        )
        /* Perform 2 cell by 1 cell divide for 1 cell result and remainder,
         * using shift and subtract.                                       */
        case ID_D_UMSMOD                                                  : { /* UM/MOD ( al ah bdiv -- rem q ) */
          unsigned int ah,al,q,di,bl,bh,sl,sh                               ;
          ah = M_POP                                                        ;
          al = M_POP                                                        ;
          bh = TOS                                                          ;
          bl = 0                                                            ;
          q  = 0                                                            ;
          for ( di=0; di<NBITS; di++ )                                      {
            if ( !DULT(al,ah,bl,bh) )                                       {
              sh = 0                                                        ;
              sl = al - bl                                                  ;
              if( al < bl ) sh = 1                                          ; /* Borrow */
              sh = ah - bh - sh                                             ;
              ah = sh                                                       ;
              al = sl                                                       ;
              q |= 1                                                        ;
            }                                                               ;
            q  =  q  << 1                                                   ;
            bl = (bl >> 1) | (bh << (NBITS-1))                              ;
            bh =  bh >> 1                                                   ;
          }                                                                 ;
          if ( !DULT(al,ah,bl,bh) )                                         {
            al  = al - bl                                                   ;
            q  |= 1                                                         ;
          }                                                                 ;
          M_PUSH ( al )                                                     ; /* rem */
          TOS = q                                                           ;
        } break                                                             ;
        /* Perform 2 cell by 1 cell divide for 2 cell result and remainder, using shift and subtract. */
        case ID_D_MUSMOD                                                  : { /* MU/MOD ( al am bdiv -- rem ql qh ) */
          register unsigned int ah,am,al,ql,qh,di                           ;
          #define bdiv ((int)TOS)
          ah = 0                                                            ;
          am = M_POP                                                        ;
          al = M_POP                                                        ;
          qh = ql = 0                                                       ;
          for ( di=0; di < 2*NBITS ; di++ )                                 {
            if ( bdiv <= ah )                                               {
              ah  = ah - bdiv                                               ;
              ql |= 1                                                       ;
            }                                                               ;
            qh = (qh << 1) | (ql >> (NBITS-1))                              ;
            ql =  ql << 1                                                   ;
            ah = (ah << 1) | (am >> (NBITS-1))                              ;
            am = (am << 1) | (al >> (NBITS-1))                              ;
            al =  al << 1                                                   ;
          }                                                                 ;
          if ( bdiv <= ah )                                                 {
            ah  = ah - bdiv                                                 ;
            ql |= 1                                                         ;
          }                                                                 ;
          M_PUSH ( ah )                                                     ; /* rem */
          M_PUSH ( ql )                                                     ;
          TOS = qh                                                          ;
          #undef bdiv
        } break                                                             ;
        case ID_DEFER: Defer ( ) ; break                                    ;
        case ID_DEFER_P: break                                              ;
        case ID_DEPTH                                                       :
          PUSH_TOS                                                          ;
          TOS = TaskNow->td_StackBase - STKPTR                              ;
        break                                                               ;
        case ID_DIVIDE                                                      :
          BINARY_OP ( / )                                                   ;
        break                                                               ;
        case ID_DOT                                                         :
          Dot ( TOS )                                                       ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_DOTS                                                        :
          M_DOTS                                                            ;
        break                                                               ;
        case ID_DROP                                                        :
          M_DROP                                                            ;
        break                                                               ;
        case ID_DUMP                                                        :
          scratch = M_POP                                                   ;
          Dump ( (char *) scratch, TOS )                                    ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_DUP                                                         :
          M_DUP                                                             ;
        break                                                               ;
        case ID_DO_P                                                        :
          /* ( limit start -- ) ( R: -- start limit )                      */
          M_R_PUSH ( TOS   )                                                ;
          M_R_PUSH ( M_POP )                                                ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_EOL                                                         : /* ( -- end_of_line_char ) */
          PUSH_TOS                                                          ;
          TOS = (unsigned int) '\n'                                         ;
        break                                                               ;
        case ID_ERRORQ_P                                                    :
          /* ( flag num -- , quit if flag true )                           */
          scratch = TOS                                                     ;
          M_DROP                                                            ;
          if (TOS)                                                          {
            M_THROW(scratch)                                                ;
          } else                                                            {
            M_DROP                                                          ;
          }                                                                 ;
        break                                                               ;
        case ID_EMIT_P                                                      :
          Report(QString((char)TOS))                                        ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_EXECUTE                                                     :
          /* Save IP on return stack like a JSR.                           */
          M_R_PUSH ( InsPtr )                                               ;
          /* Bump level for trace.                                         */
          Level++                                                           ;
          if ( IsTokenPrimitive( TOS ) )                                    {
            WRITE_CELL_DIC( (int *) &FakeSecondary[0], TOS)                 ; /* Build a fake secondary and execute it. */
            InsPtr = &FakeSecondary[0]                                      ;
          } else                                                            {
            InsPtr = (int *) LOCAL_CODEREL_TO_ABS(TOS)                      ;
          }                                                                 ;
          M_DROP                                                            ;
        break                                                               ;
        case ID_FETCH                                                       :
          TOS = *((int *)TOS)                                               ;
        break                                                               ;
        case ID_FILE_CREATE                                                 :
          /* ( c-addr u fam -- fid ior )                                   */
          /* Build NUL terminated name string.                             */
          scratch = M_POP                                                   ; /* u */
          Temp    = M_POP                                                   ; /* caddr */
          if ( scratch < TIB_SIZE-2 )                                       {
            QIODevice::OpenMode om = CreateMode(TOS)                        ;
            memcpy( Scratch, (char *) Temp, (unsigned int) scratch )        ;
            Scratch[scratch] = '\0'                                         ;
            file = new File()                                               ;
            file->setFileName(QString(Scratch))                             ;
            if (!file->open(om))                                            {
              delete file                                                   ;
              file = NULL                                                   ;
            }                                                               ;
            TOS = ( file == NULL ) ? -1 : 0                                 ;
            M_PUSH( (int) file )                                            ;
          } else                                                            {
            Report(QObject::tr("Filename too large for name buffer.\n"))    ;
            M_PUSH( 0 )                                                     ;
            TOS = -2                                                        ;
          }                                                                 ;
        break                                                               ;
        case ID_FILE_DELETE                                                 :
          /* ( c-addr u -- ior )                                           */
          /* Build NUL terminated name string.                             */
          Temp = M_POP                                                      ; /* caddr */
          if ( TOS < TIB_SIZE-2 )                                           {
            memcpy( Scratch, (char *) Temp, (unsigned int) TOS )            ;
            Scratch[TOS] = '\0'                                             ;
            QFile::remove(QString(Scratch))                                 ;
            TOS = 0                                                         ;
            Report(QString("Delete file %1").arg(Scratch)                 ) ;
          } else                                                            {
            Report(QObject::tr("Filename too large for name buffer.\n"))    ;
            TOS = -2                                                        ;
          }                                                                 ;
        break                                                               ;
        case ID_FILE_OPEN                                                   :
          /* ( c-addr u fam -- fid ior )                                   */
          /* Build NUL terminated name string.                             */
          scratch = M_POP                                                   ; /* u */
          Temp    = M_POP                                                   ; /* caddr */
          if ( scratch < TIB_SIZE-2 )                                       {
            QIODevice::OpenMode om = OpenMode(TOS)                          ;
            memcpy( Scratch,(char *) Temp,(unsigned int)scratch )           ;
            Scratch[scratch] = '\0'                                         ;
            file = new File()                                              ;
            file->setFileName(QString(Scratch))                             ;
            if (!file->open(om))                                            {
              delete file                                                   ;
              file = NULL                                                   ;
            }                                                               ;
            TOS = ( file == NULL ) ? -1 : 0                                 ;
            M_PUSH( (int) file )                                            ;
            Report(QString("Debug: request to open file"))                  ;
          } else                                                            {
            Report(QObject::tr("Filename too large for name buffer.\n"))    ;
            M_PUSH( 0 )                                                     ;
            TOS = -2                                                        ;
          }                                                                 ;
        break                                                               ;
        case ID_FILE_CLOSE                                                  : /* ( fid -- ior ) */
          file = (File *)TOS                                               ;
          if (NotNull(file)) file->close()                                  ;
          file = NULL                                                       ;
          TOS  = 0                                                          ;
        break                                                               ;
        case ID_FILE_READ                                                   :
          /* ( addr len fid -- u2 ior )                                    */
          file    = (File *) TOS                                            ;
          scratch = M_POP                                                   ;
          CharPtr = (char *) M_POP                                          ;
          Temp = file->read( CharPtr , scratch )                            ;
          M_PUSH ( Temp )                                                   ;
          TOS = 0                                                           ;
        break                                                               ;
        /* ( fid -- ud ior )                                               */
        /* Determine file size by seeking to end and returning position.   */
        case ID_FILE_SIZE                                                 : {
//          FileID = (FileStream *) TOS;
//          off_t endposition, offsetHi;
//          off_t original = sdTellFile( FileID );
//          sdSeekFile( FileID, 0, PF_SEEK_END );
//          endposition = sdTellFile( FileID );
//          M_PUSH(endposition);
          /* Just use a 0 if they are the same size. */
//          offsetHi = (sizeof(off_t) > sizeof(cell_t)) ? (endposition >> (8*sizeof(cell_t))) : 0 ;
//          M_PUSH(offsetHi);
//          sdSeekFile( FileID, original, PF_SEEK_SET );
//          TOS = (original < 0) ? -4 : 0 ; /* !!! err num */
        } break                                                     ;
        case ID_FILE_WRITE                                          :
          /* ( addr len fid -- ior )                               */
//          FileID = (FileStream *) TOS                               ;
//          scratch = M_POP                                           ;
//          CharPtr = (char *) M_POP                                  ;
//          Temp = sdWriteFile( CharPtr, 1, Scratch, FileID )         ;
//          TOS = (Temp != Scratch) ? -3 : 0                          ;
        break                                                       ;
        case ID_FILE_REPOSITION                                   : { /* ( ud fid -- ior ) */
//          off_t offset;
//          FileID = (FileStream *) TOS;
//          offset = M_POP;
          /* Avoid compiler warnings on Mac. */
//          offset = (sizeof(off_t) > sizeof(cell_t)) ? (offset << 8*sizeof(cell_t)) : 0 ;
//          offset += M_POP;
//          TOS = sdSeekFile( FileID, offset, PF_SEEK_SET );
        } break                                                     ;
        case ID_FILE_POSITION                                     : { /* ( fid -- ud ior ) */
          off_t position = -1                                       ;
          off_t offsetHi                                            ;
          file = (File *) TOS                                       ;
          if (NotNull(file)) position = file->pos()                 ;
          M_PUSH(position)                                          ;
          /* Just use a 0 if they are the same size.               */
          offsetHi = (sizeof(off_t) > sizeof(int)) ? (position >> (8*sizeof(int))) : 0 ;
          M_PUSH(offsetHi)                                          ;
          TOS = (position < 0) ? -4 : 0                             ; /* !!! err num */
        } break                                                     ;
        case ID_FILE_RO                                             : /* (  -- fam ) */
          PUSH_TOS                                                  ;
          TOS = PF_FAM_READ_ONLY                                    ;
        break                                                       ;
        case ID_FILE_RW                                             : /* ( -- fam ) */
          PUSH_TOS                                                  ;
          TOS = PF_FAM_READ_WRITE                                   ;
        break                                                       ;
        case ID_FILE_WO                                             : /* ( -- fam ) */
          PUSH_TOS                                                  ;
          TOS = PF_FAM_WRITE_ONLY                                   ;
        break                                                       ;
        case ID_FILE_BIN                                            : /* ( -- fam ) */
          TOS = TOS | PF_FAM_BINARY_FLAG                            ;
        break                                                       ;
        case ID_FILL                                              : { /* ( caddr num charval -- ) */
          register char *DstPtr                                     ;
          Temp = M_POP                                              ; /* num */
          DstPtr = (char *) M_POP                                   ; /* dst */
          for ( scratch=0                                           ;
                (unsigned int) scratch < (unsigned int) Temp        ;
                scratch++                                         ) {
            *DstPtr++ = (char) TOS                                  ;
          }                                                         ;
          M_DROP                                                    ;
        } break                                                     ;
        case ID_FIND                                                :
          /* ( $addr -- $addr 0 | xt +-1 )                         */
          TOS = Find ( (char *) TOS, (unsigned int *) &Temp )       ;
          M_PUSH     ( Temp                                 )       ;
        break                                                       ;
        case ID_FINDNFA                                             :
          TOS = FindNfa((const char *)TOS,(const char **) &Temp )   ;
          M_PUSH( (unsigned int) Temp )                             ;
        break                                                       ;
        case ID_FLUSHEMIT                                           :
//          sdTerminalFlush()                                         ;
        break                                                       ;
        /* Validate memory before freeing. Clobber validator and first word. */
        case ID_FREE                                                : /* ( addr -- result ) */
          if ( TOS == 0 )                                           {
            Report(QObject::tr("FREE passed NULL!\n"))              ;
            TOS = -2                                                ; /* FIXME error code */
          } else                                                    {
            CellPtr  = (int *) TOS                                  ;
            CellPtr --                                              ;
            if ( ((unsigned int)*CellPtr)                          !=
                 ((unsigned int) CellPtr ^ PF_MEMORY_VALIDATOR))    {
              TOS = -2                                              ; /* FIXME error code */
            } else                                                  {
              CellPtr[0] = 0xDeadBeef                               ;
              free((void *)CellPtr)                                 ;
              TOS = 0                                               ;
            }                                                       ;
          }                                                         ;
        break                                                       ;
        case ID_FP_D_TO_F                                           :
          /* ( dlo dhi -- ) ( F: -- r )                            */
          PUSH_FP_TOS                                               ;
          scratch = M_POP                                           ; /* dlo */
          if ( ((TOS ==  0) && (scratch >= 0))                     ||
               ((TOS == -1) && (scratch <  0))                    ) {
            /* <=  32 bit precision.                               */
            FP_TOS = ((double) scratch)                             ;
            /* Convert dlo and push on FP stack.                   */
          } else                                                    { /* > 32 bit precision. */
            fpTemp    = ((double) TOS)                              ; /* dhi */
            fpTemp   *= FP_DHI1                                     ;
            fpScratch = ( (double) ((unsigned int)scratch) )        ; /* Convert TOS and push on FP stack. */
            FP_TOS = fpTemp + fpScratch                             ;
          }                                                         ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_FP_FSTORE                                           : /* ( addr -- ) ( F: r -- ) */
          *((double *) TOS) = FP_TOS                                ;
          M_FP_DROP                                                 ; /* drop FP value */
          M_DROP                                                    ; /* drop addr */
        break                                                       ;
        case ID_FP_FTIMES                                           : /* ( F: r1 r2 -- r1*r2 ) */
          FP_TOS = M_FP_POP * FP_TOS                                ;
        break                                                       ;
        case ID_FP_FPLUS                                            : /* ( F: r1 r2 -- r1+r2 ) */
          FP_TOS = M_FP_POP + FP_TOS                                ;
        break                                                       ;
        case ID_FP_FMINUS                                           : /* ( F: r1 r2 -- r1-r2 ) */
          FP_TOS = M_FP_POP - FP_TOS                                ;
        break                                                       ;
        case ID_FP_FSLASH                                           : /* ( F: r1 r2 -- r1/r2 ) */
          FP_TOS = M_FP_POP / FP_TOS                                ;
        break                                                       ;
        case ID_FP_F_ZERO_LESS_THAN                                 : /* ( -- flag )  ( F: r --  ) */
          PUSH_TOS                                                  ;
          TOS = (FP_TOS < 0.0) ? FTRUE : FFALSE                     ;
          M_FP_DROP                                                 ;
        break                                                       ;
        case ID_FP_F_ZERO_EQUALS                                    : /* ( -- flag )  ( F: r --  ) */
          PUSH_TOS                                                  ;
          TOS = (FP_TOS == 0.0) ? FTRUE : FFALSE                    ;
          M_FP_DROP                                                 ;
        break                                                       ;
        case ID_FP_F_LESS_THAN                                      : /* ( -- flag )  ( F: r1 r2 -- ) */
          PUSH_TOS                                                  ;
          TOS = (M_FP_POP < FP_TOS) ? FTRUE : FFALSE                ;
          M_FP_DROP                                                 ;
        break                                                       ;
        case ID_FP_F_TO_D                                         : {
          /* ( -- dlo dhi) ( F: r -- )                             */
          unsigned int dlo                                          ;
          int          dhi                                          ;
          int          ifNeg                                        ;
          /* Convert absolute value, then negate D if negative.    */
          PUSH_TOS                                                  ; /* Save old TOS */
          fpTemp = FP_TOS                                           ;
          M_FP_DROP                                                 ;
          ifNeg = ( fpTemp < 0.0 )                                  ;
          if ( ifNeg ) fpTemp = 0.0 - fpTemp                        ;
          fpScratch = fpTemp / FP_DHI1                              ;
          dhi       = (int) fpScratch                               ;  /* dhi */
          fpScratch = ((double) dhi) * FP_DHI1                      ;
          fpTemp = fpTemp - fpScratch                               ; /* Remainder */
          dlo    = (unsigned int) fpTemp                            ;
          if ( ifNeg )                                              {
            dlo = 0 - dlo                                           ;
            dhi = 0 - dhi - 1                                       ;
          }                                                         ;
          /* Push onto stack.                                      */
          TOS = dlo                                                 ;
          PUSH_TOS                                                  ;
          TOS = dhi                                                 ;
        } break                                                     ;
        case ID_FP_FFETCH                                           : /* ( addr -- ) ( F: -- r ) */
          PUSH_FP_TOS                                               ;
          FP_TOS = *((double *) TOS)                                ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_FP_FDEPTH                                           : /* ( -- n ) ( F: -- ) */
          PUSH_TOS                                                  ;
          /* Add 1 to account for FP_TOS in cached in register.    */
           TOS = (( M_FP_SPZERO - FP_STKPTR) + 1)                   ;
        break                                                       ;
        case ID_FP_FDROP                                            : /* ( -- ) ( F: r -- ) */
          M_FP_DROP                                                 ;
        break                                                       ;
        case ID_FP_FDUP                                             : /* ( -- ) ( F: r -- r r ) */
          PUSH_FP_TOS                                               ;
        break                                                       ;
        case ID_FP_FLOAT_PLUS                                       : /* ( addr1 -- addr2 ) ( F: -- ) */
          TOS = TOS + sizeof(double)                                ;
        break                                                       ;
        case ID_FP_FLOATS                                           : /* ( n -- size ) ( F: -- ) */
          TOS = TOS * sizeof(double)                                ;
        break                                                       ;
        case ID_FP_FLOOR                                            : /* ( -- ) ( F: r1 -- r2 ) */
          FP_TOS = (double) fp_floor( FP_TOS )                      ;
        break                                                       ;
        case ID_FP_FMAX                                             : /* ( -- ) ( F: r1 r2 -- r3 ) */
          fpScratch = M_FP_POP                                      ;
          FP_TOS = ( FP_TOS > fpScratch ) ? FP_TOS : fpScratch      ;
        break                                                       ;
        case ID_FP_FMIN                                             : /* ( -- ) ( F: r1 r2 -- r3 ) */
          fpScratch = M_FP_POP                                      ;
          FP_TOS = ( FP_TOS < fpScratch ) ? FP_TOS : fpScratch      ;
        break                                                       ;
        case ID_FP_FNEGATE                                          :
          FP_TOS = -FP_TOS                                          ;
        break                                                       ;
        case ID_FP_FOVER                                            : /* ( -- ) ( F: r1 r2 -- r1 r2 r1 ) */
          PUSH_FP_TOS                                               ;
          FP_TOS = M_FP_STACK(1)                                    ;
        break                                                       ;
        case ID_FP_FROT                                             : /* ( -- ) ( F: r1 r2 r3 -- r2 r3 r1 ) */
          fpScratch = M_FP_POP                                      ; /* r2 */
          fpTemp    = M_FP_POP                                      ; /* r1 */
          M_FP_PUSH ( fpScratch )                                   ; /* r2 */
          PUSH_FP_TOS                                               ; /* r3 */
          FP_TOS    = fpTemp                                        ; /* r1 */
        break                                                       ;
        case ID_FP_FROUND                                           :
          Report(QObject::tr("\nID_FP_FROUND - Not Yet!! FIXME\n")) ;
        break                                                       ;
        case ID_FP_FSWAP                                            : /* ( -- ) ( F: r1 r2 -- r2 r1 ) */
          fpScratch  = FP_TOS                                       ;
          FP_TOS     = *FP_STKPTR                                   ;
          *FP_STKPTR = fpScratch                                    ;
        break                                                       ;
        case ID_FP_FSTAR_STAR                                       : /* ( -- ) ( F: r1 r2 -- r1^r2 ) */
          fpScratch = M_FP_POP                                      ;
          FP_TOS = (double) fp_pow(fpScratch, FP_TOS)               ;
        break                                                       ;
        case ID_FP_FABS                                             : /* ( -- ) ( F: r1 -- r2 ) */
          FP_TOS = (double) fp_fabs( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FACOS                                            : /* ( -- ) ( F: r1 -- r2 ) */
          FP_TOS = (double) fp_acos( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FACOSH                                           : /* ( -- ) ( F: r1 -- r2 ) */
          /* fp_acosh(x) = fp_log(y + sqrt(y^2 - 1)                */
          FP_TOS = (double) fp_log(FP_TOS + (fp_sqrt((FP_TOS * FP_TOS) - 1)));
        break                                                       ;
        case ID_FP_FALOG                                            : /* ( -- ) ( F: r1 -- r2 ) */
          FP_TOS = (double) fp_pow(10.0,FP_TOS)                     ;
        break                                                       ;
        case ID_FP_FASIN                                            : /* ( -- ) ( F: r1 -- r2 ) */
          FP_TOS = (double) fp_asin( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FASINH                                           : /* ( -- ) ( F: r1 -- r2 ) */
          /* asinh(x) = fp_log(y + fp_sqrt(y^2 + 1)                */
          FP_TOS = (double) fp_log(FP_TOS + (fp_sqrt((FP_TOS * FP_TOS) + 1)));
        break                                                       ;
        case ID_FP_FATAN                                            : /* ( -- ) ( F: r1 -- r2 ) */
          FP_TOS = (double) fp_atan( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FATAN2                                           :
          /* ( -- ) ( F: r1 r2 -- atan(r1/r2) )                    */
          fpTemp = M_FP_POP                                         ;
          FP_TOS = (double) fp_atan2( fpTemp, FP_TOS )              ;
        break                                                       ;
        case ID_FP_FATANH                                           :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double)(0.5*fp_log((1 + FP_TOS)/(1 - FP_TOS)))  ;
        break                                                       ;
        case ID_FP_FCOS                                             :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_cos( FP_TOS )                        ;
        break                                                       ;
        case ID_FP_FCOSH                                            :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_cosh( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FLITERAL                                         :
          LiteralFP ( FP_TOS )                                      ;
          M_FP_DROP                                                 ;
        break                                                       ;
        case ID_FP_FLITERAL_P                                     : {
          PUSH_FP_TOS                                               ;
          double * fptr                                             ;
          fptr = (double *)InsPtr                                   ;
          FP_TOS = READ_FLOAT_DIC( fptr++ )                         ;
          InsPtr = (int *) fptr                                     ;
        } break                                                     ;
        case ID_FP_FLN                                              :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_log(FP_TOS)                          ;
        break                                                       ;
        case ID_FP_FLNP1                                            :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) (fp_log(FP_TOS) + 1.0)                  ;
        break                                                       ;
        case ID_FP_FLOG                                             :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_log10( FP_TOS )                      ;
        break                                                       ;
        case ID_FP_FSIN                                             :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_sin( FP_TOS )                        ;
        break                                                       ;
        case ID_FP_FSINCOS                                          :
          /* ( -- ) ( F: r1 -- r2 r3 )                             */
          M_FP_PUSH((double) fp_sin(FP_TOS))                        ;
          FP_TOS  = (double) fp_cos(FP_TOS)                         ;
        break                                                       ;
        case ID_FP_FSINH                                            :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_sinh( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FSQRT                                            :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_sqrt( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FTAN                                             :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_tan( FP_TOS )                        ;
        break                                                       ;
        case ID_FP_FTANH                                            :
          /* ( -- ) ( F: r1 -- r2 )                                */
          FP_TOS = (double) fp_tanh( FP_TOS )                       ;
        break                                                       ;
        case ID_FP_FPICK                                            :
          /* ( n -- ) ( F: -- f[n] )                               */
          PUSH_FP_TOS                                               ; /* push cached floats into RAM */
          FP_TOS = FP_STKPTR[TOS]                                   ; /* 0 FPICK gets top of FP stack */
          M_DROP                                                    ;
        break                                                       ;
        case ID_HERE                                                :
          PUSH_TOS                                                  ;
          TOS = (int)CODE_HERE                                      ;
        break                                                       ;
        case ID_NUMBERQ_P                                           :
          /* ( addr -- 0 | n 1 )                                   */
          /* Convert using number converter in 'C'.
           * Only supports single precision for bootstrap.         */
          TOS = (int) NumberQ( (char *) TOS, &Temp )                ;
          if ( TOS == NUM_TYPE_SINGLE)                              {
            /* Push single number                                  */
            M_PUSH( Temp )                                          ;
          }                                                         ;
        break                                                       ;
        case ID_I                                                   :
          /* ( -- i , DO LOOP index )                              */
          PUSH_TOS                                                  ;
          TOS = M_R_PICK(1)                                         ;
        break                                                       ;
        case ID_INCLUDE_FILE                                        :
          file = (File *) TOS                                       ;
          M_DROP                                                    ; /* Drop now so that INCLUDE has a clean stack. */
          SAVE_REGISTERS                                            ;
          scratch = IncludeFile( file )                             ;
          LOAD_REGISTERS                                            ;
          if ( scratch ) M_THROW(scratch)                           ;
        break                                                       ;
        case ID_INTERPRET                                           :
          SAVE_REGISTERS                                            ;
          scratch = Interpret ( )                                   ;
          LOAD_REGISTERS                                            ;
          if ( scratch ) M_THROW(scratch)                           ;
        break                                                       ;
        case ID_J                                                   :
          /* ( -- j , second DO LOOP index )                       */
          PUSH_TOS                                                  ;
          TOS = M_R_PICK(3)                                         ;
        break                                                       ;
        case ID_KEY                                                 :
//          PUSH_TOS                                                  ;
//          TOS = ioKey()                                             ;
        break                                                       ;
        case ID_LITERAL                                             :
          Literal ( TOS )                                           ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_LITERAL_P                                           :
          PUSH_TOS                                                  ;
          TOS = READ_CELL_DIC(InsPtr++)                             ;
        break                                                       ;
        case ID_LOCAL_COMPILER: DO_VAR(LocalCompilerXT) ; break     ;
        case ID_LOCAL_FETCH                                         :
          /* ( i <local> -- n , fetch from local )                 */
          TOS = *(LocalsPtr - TOS)                                  ;
        break                                                       ;
        /* ( <local> -- n , fetch from local )                     */
        #define LOCAL_FETCH_N(num)                                  \
        case ID_LOCAL_FETCH_##num:                                  \
          PUSH_TOS;                                                 \
          TOS = *(LocalsPtr -(num));                                \
        break                                                       ;
        LOCAL_FETCH_N(1)                                            ;
        LOCAL_FETCH_N(2)                                            ;
        LOCAL_FETCH_N(3)                                            ;
        LOCAL_FETCH_N(4)                                            ;
        LOCAL_FETCH_N(5)                                            ;
        LOCAL_FETCH_N(6)                                            ;
        LOCAL_FETCH_N(7)                                            ;
        LOCAL_FETCH_N(8)                                            ;
        case ID_LOCAL_STORE                                         :
          /* ( n i <local> -- , store n in local )                 */
          *(LocalsPtr - TOS) = M_POP                                ;
          M_DROP                                                    ;
        break                                                       ;
        /* ( n <local> -- , store n in local )                     */
        #define LOCAL_STORE_N(num)                                  \
        case ID_LOCAL_STORE_##num:                                  \
          *(LocalsPtr - (num)) = TOS;                               \
          M_DROP;                                                   \
        break                                                       ;
        LOCAL_STORE_N(1)                                            ;
        LOCAL_STORE_N(2)                                            ;
        LOCAL_STORE_N(3)                                            ;
        LOCAL_STORE_N(4)                                            ;
        LOCAL_STORE_N(5)                                            ;
        LOCAL_STORE_N(6)                                            ;
        LOCAL_STORE_N(7)                                            ;
        LOCAL_STORE_N(8)                                            ;
        case ID_LOCAL_PLUSSTORE                                     :
          /* ( n i <local> -- , add n to local )                   */
          *(LocalsPtr - TOS) += M_POP                               ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_LOCAL_ENTRY                                         :
          /* ( x0 x1 ... xn n -- )                                 */
          /* create local stack frame                              */
          {
            int   i = TOS                                           ;
            int * lp                                                ;
            /* End of locals. Create stack frame                   */
            M_R_PUSH(LocalsPtr)                                     ;
            LocalsPtr = TORPTR                                      ;
            TORPTR   -= TOS                                         ;
            lp        = TORPTR                                      ;
            while (i-- > 0)                                         {
              /* Load local vars from stack                        */
              *lp++ = M_POP                                         ;
            }                                                       ;
            M_DROP                                                  ;
          }                                                         ;
        break                                                       ;
        case ID_LOCAL_EXIT                                          :
          /* cleanup up local stack frame                          */
          TORPTR = LocalsPtr                                        ;
          LocalsPtr = (int *) M_R_POP                               ;
        break                                                       ;
        case ID_LOADSYS                                             :
          Report(QObject::tr("Load %1\n").arg(SYSTEM_LOAD_FILE))    ;
          file = new File()                                         ;
          file->setFileName(QString(SYSTEM_LOAD_FILE))              ;
          if (file->open(QIODevice::ReadOnly))                      {
            SAVE_REGISTERS                                          ;
            scratch = IncludeFile( file )                           ; /* Also closes the file. */
            LOAD_REGISTERS                                          ;
            if ( scratch ) M_THROW(scratch)                         ;
          } else                                                    {
            Report(QObject::tr("%1 could not be opened!\n"          )
                   .arg(SYSTEM_LOAD_FILE)                         ) ;
          }                                                         ;
        break                                                       ;
        case ID_LEAVE_P                                             : /* ( R: index limit --  ) */
          M_R_DROP                                                  ;
          M_R_DROP                                                  ;
          M_BRANCH                                                  ;
        break                                                       ;
        case ID_LOOP_P                                              :
          /* ( R: index limit -- | index limit )                   */
          Temp    = M_R_POP                                         ; /* limit */
          scratch = M_R_POP + 1                                     ; /* index */
          if ( scratch == Temp )                                    {
            /* skip branch offset, exit loop                       */
            InsPtr++                                                ;
          } else                                                    {
            /* Push index and limit back to R                      */
            M_R_PUSH ( scratch )                                    ;
            M_R_PUSH ( Temp    )                                    ;
            /* Branch back to just after (DO)                      */
            M_BRANCH                                                ;
          }                                                         ;
        break                                                       ;
        case ID_LSHIFT                                              :
          BINARY_OP ( << )                                          ;
        break                                                       ;
        case ID_MAX                                                 :
          scratch = M_POP                                           ;
          TOS = ( TOS > scratch ) ? TOS : scratch                   ;
        break                                                       ;
        case ID_MIN                                                 :
          scratch = M_POP                                           ;
          TOS = ( TOS < scratch ) ? TOS : scratch                   ;
        break                                                       ;
        case ID_MINUS                                               :
          BINARY_OP ( - )                                           ;
        break                                                       ;
        case ID_NAME_TO_TOKEN                                       :
          TOS = (int) toToken((char *)TOS)                          ;
        break                                                       ;
        case ID_NAME_TO_PREVIOUS                                    :
          TOS = (int) toPrevious((char *)TOS)                       ;
        break                                                       ;
        case ID_NOOP                                                :
        break                                                       ;
        case ID_OR                                                  :
          BINARY_OP ( | )                                           ;
        break                                                       ;
        case ID_OVER                                                :
          PUSH_TOS                                                  ;
          TOS = M_STACK(1)                                          ;
        break                                                       ;
        case ID_PICK                                                :
          /* ( ... n -- sp(n) )                                    */
          TOS = M_STACK(TOS)                                        ;
        break                                                       ;
        case ID_PLUS                                                :
          BINARY_OP ( + )                                           ;
        break                                                       ;
        case ID_PLUS_STORE                                          :
          /* ( n addr -- , add n to *addr )                        */
          *((int *)TOS) += M_POP                                    ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_PLUSLOOP_P                                        : {
          /* ( delta -- ) ( R: index limit -- | index limit )      */
          unsigned int OldIndex, NewIndex, Limit                    ;
          Limit    = M_R_POP                                        ;
          OldIndex = M_R_POP                                        ;
          NewIndex = OldIndex + TOS                                 ;
          /* add TOS to index, not 1                               */
          /* Do indices cross boundary between LIMIT-1 and LIMIT ? */
          if (((OldIndex-Limit)&((Limit-1)-NewIndex)&0x80000000 )  ||
              ((NewIndex-Limit)&((Limit-1)-OldIndex)&0x80000000 ) ) {
            InsPtr++                                                ;
            /* skip branch offset, exit loop                       */
          } else                                                    {
            /* Push index and limit back to R                      */
            M_R_PUSH ( NewIndex )                                   ;
            M_R_PUSH ( Limit    )                                   ;
            /* Branch back to just after (DO)                      */
            M_BRANCH                                                ;
          }                                                         ;
          M_DROP                                                    ;
        } break                                                     ;
        case ID_QDO_P                                               :
          /* (?DO) ( limit start -- ) ( R: -- start limit )        */
          scratch = M_POP                                           ;
          if ( scratch == TOS )                                     {
            /* Branch to just after (LOOP)                         */
            M_BRANCH                                                ;
          } else                                                    {
            M_R_PUSH ( TOS     )                                    ;
            M_R_PUSH ( Scratch )                                    ;
            InsPtr++                                                ;
            /* skip branch offset, enter loop                      */
          }                                                         ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_QDUP                                                :
          if ( TOS ) M_DUP                                          ;
        break                                                       ;
        case ID_QTERMINAL                                           :
          /* WARNING: Typically not fully implemented!             */
//          PUSH_TOS                                                  ;
//          TOS = sdQueryTerminal()                                   ;
         break                                                      ;
        case ID_QUIT_P                                              :
          /* Stop inner interpreter, go back to user.              */
          Level = 0                                                 ;
          M_THROW(THROW_QUIT)                                       ;
        break                                                       ;
        case ID_R_DROP: M_R_DROP; break                             ;
        case ID_R_FETCH                                             :
          PUSH_TOS                                                  ;
          TOS = (*(TORPTR))                                         ;
        break                                                       ;
        case ID_R_FROM                                              :
          PUSH_TOS                                                  ;
          TOS = M_R_POP                                             ;
        break                                                       ;
        case ID_REFILL                                              :
          PUSH_TOS                                                  ;
          TOS = (Refill() > 0) ? FTRUE : FFALSE                     ;
        break                                                       ;
        /* Resize memory allocated by ALLOCATE.                    */
        case ID_RESIZE                                            : {
          /* ( addr1 u -- addr2 result )                           */
          int * Addr1   = (int *) M_POP                             ;
          /* Point to validator below users address.               */
          int * FreePtr = Addr1 - 1                                 ;
          if ( ((unsigned int)*FreePtr)                            !=
               ((unsigned int)FreePtr ^ PF_MEMORY_VALIDATOR)      ) {
            /* 090218 - Fixed bug, was returning zero.             */
            M_PUSH ( Addr1 )                                        ;
            TOS = -3                                                ;
          } else                                                    {
            /* Try to allocate.                                    */
            CellPtr = (int *)new int[ ( TOS / 4 ) + 1 ]             ;
            if ( CellPtr )                                          {
              /* Copy memory including validation.                 */
              memcpy(CellPtr,FreePtr,TOS + sizeof(int))             ;
              *CellPtr = (int)(((unsigned int)CellPtr) ^ (unsigned int)PF_MEMORY_VALIDATOR);
              /* 090218 - Fixed bug that was incrementing the address twice. Thanks Reinhold Straub. */
              /* Increment past validator to user address.         */
              M_PUSH( (int) (CellPtr + 1) )                         ;
              TOS = 0                                               ; /* Result code. */
              /* Mark old cell as dead so we can't free it twice.   */
              FreePtr[0] = 0xDeadBeef                               ;
              free(FreePtr)                                         ;
            } else                                                  {
              /* 090218 - Fixed bug, was returning zero.           */
              M_PUSH ( Addr1 )                                      ;
              TOS = -4                                              ;
              /* FIXME Fix error code.                             */
            }                                                       ;
          }                                                         ;
        } break                                                     ;
        /*
         * RP@ and RP! are called secondaries so we must
         * account for the return address pushed before calling.   */
        case ID_RP_FETCH                                            :
          /* ( -- rp , address of top of return stack )            */
          PUSH_TOS                                                  ;
          TOS = (int)TORPTR                                         ;
          /* value before calling RP@                              */
        break                                                       ;
        case ID_RP_STORE                                            :
          /* ( rp -- , address of top of return stack )            */
          TORPTR = (int *) TOS                                      ;
          M_DROP                                                    ;
        break                                                       ;
        /* ( xu xu-1 xu-1 ... x0 u -- xu-1 xu-1 ... x0 xu )        */
        case ID_ROLL                                              : {
          int   ri                                                  ;
          int * srcPtr                                              ;
          int * dstPtr                                              ;
          scratch =  M_STACK(TOS  )                                 ;
          srcPtr  = &M_STACK(TOS-1)                                 ;
          dstPtr  = &M_STACK(TOS  )                                 ;
          for ( ri=0; ri<TOS; ri++ )                                {
            *dstPtr-- = *srcPtr--                                   ;
          }                                                         ;
          TOS = scratch                                             ;
          STKPTR++                                                  ;
        } break                                                     ;
        case ID_ROT                                                 :
          /* ( a b c -- b c a )                                    */
          scratch = M_POP                                           ; /* b */
          Temp    = M_POP                                           ; /* a */
          M_PUSH ( scratch )                                        ; /* b */
          PUSH_TOS                                                  ; /* c */
          TOS = Temp                                                ; /* a */
        break                                                       ;
        /* Logical right shift                                     */
        case ID_RSHIFT: TOS = ((unsigned int)M_POP) >> TOS; break   ;
        case ID_SAVE_FORTH_P                                      : {
          /* ( $name Entry NameSize CodeSize -- err )              */
          int     CodeSize   = TOS                                  ;
          int     NameSize   = M_POP                                ;
          int     EntryPoint = M_POP                                ;
          QString SaveTo     = toString((char *) M_POP)             ; // ForthStringToC( gScratch, (char *) M_POP, sizeof(gScratch) );
          TOS = SaveForth( SaveTo,EntryPoint,NameSize,CodeSize )    ;
        } break                                                     ;
        /* Source     Stack
         * EVALUATE    >IN  SourceID=(-1)  1111
         * keyboard    >IN  SourceID=(0)   2222
         * file        >IN  lineNumber filePos  SourceID=(fileID)  */
        case ID_SAVE_INPUT: break                                   ;
        case ID_SP_FETCH                                            :
          /* ( -- sp , address of top of stack, sorta )            */
          PUSH_TOS                                                  ;
          TOS = (int)STKPTR                                         ;
        break                                                       ;
        case ID_SP_STORE                                            :
          /* ( sp -- , address of top of stack, sorta )            */
          STKPTR = (int *) TOS                                      ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_STORE                                               :
          /* ( n addr -- , write n to addr )                       */
          *((int *)TOS) = M_POP                                     ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_SCAN                                                :
          /* ( addr cnt char -- addr' cnt' )                       */
          scratch = M_POP                                           ; /* cnt */
          Temp    = M_POP                                           ; /* addr */
          TOS     = Scan((char *)Temp,scratch,(char)TOS,&CharPtr)   ;
          M_PUSH((int)CharPtr)                                      ;
        break                                                       ;
        case ID_SEMICOLON                                           :
          SAVE_REGISTERS                                            ;
          scratch = SemiColon ( )                                   ;
          LOAD_REGISTERS                                            ;
          if ( scratch ) M_THROW( scratch )                         ;
        break                                                       ;
        case ID_SKIP                                                :
          /* ( addr cnt char -- addr' cnt' )                       */
          scratch = M_POP                                           ; /* cnt */
          Temp    = M_POP                                           ; /* addr */
          TOS     = Skip((char *)Temp,scratch,(char)TOS,&CharPtr)   ;
          M_PUSH((int)CharPtr)                                      ;
        break                                                       ;
        case ID_SOURCE                                              :  /* ( -- c-addr num ) */
          PUSH_TOS                                                  ;
          M_PUSH( (int) TaskNow->td_SourcePtr )                     ;
          TOS   = (int) TaskNow->td_SourceNum                       ;
        break                                                       ;
        case ID_SOURCE_SET                                          : /* ( c-addr num -- ) */
          TaskNow->td_SourcePtr = (char *) M_POP                    ;
          TaskNow->td_SourceNum = TOS                               ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_SOURCE_ID                                           :
          PUSH_TOS                                                  ;
          TOS = toSourceId ( TaskNow->td_InputStream )              ;
        break                                                       ;
        case ID_SOURCE_ID_POP                                       :
          PUSH_TOS                                                  ;
          TOS = toSourceId( PopInput() )                            ;
        break                                                       ;
        case ID_SOURCE_ID_PUSH                                      :
          /* ( source-id -- )                                      */
          TOS = (int)toStream( TOS )                                ;
          scratch = PushInput( (File *) TOS )                       ;
          if ( scratch )                                            {
            M_THROW(scratch)                                        ;
          } else M_DROP                                             ;
        break                                                       ;
        case ID_SWAP                                                :
          scratch = TOS                                             ;
          TOS     = *STKPTR                                         ;
          *STKPTR = scratch                                         ;
        break                                                       ;
        case ID_THROW                                               :
          /* ( k*x err -- k*x | i*x err ,
           * jump to where CATCH was called )                      */
          if (TOS)                                                  {
            M_THROW(TOS)                                            ;
          } else M_DROP                                             ;
        break                                                       ;
        case ID_TICK                                                :
          PUSH_TOS                                                  ;
          CharPtr = (char *) Word( (char) ' ' )                     ;
          TOS = Find( CharPtr, (unsigned int *) &Temp )             ;
          if ( TOS == 0 )                                           {
            Report(QObject::tr("Can not find %1\n"                  )
                   .arg(toString(CharPtr)                       ) ) ;
            M_THROW(-13)                                            ;
          } else                                                    {
            TOS = Temp                                              ;
          }                                                         ;
        break                                                       ;
        case ID_TIMES                                               :
          BINARY_OP( * )                                            ;
        break                                                       ;
        case ID_TYPE                                                :
          scratch = M_POP                                           ; /* addr */
          if (scratch!=0)                                           {
            char * m = new char[TOS+1]                              ;
            memset ( m , 0 , TOS+1             )                    ;
            memcpy ( m , (char *)scratch , TOS )                    ;
            Report ( QString(m)                )                    ;
            delete [] m                                             ;
          }                                                         ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_TO_R                                                :
          M_R_PUSH ( TOS )                                          ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_VAR_CODE_BASE                                       :
          DO_VAR ( Dictionary->dic_CodeBase )                       ;
        break                                                       ;
        case ID_VAR_CODE_LIMIT                                      :
          DO_VAR ( Dictionary->dic_CodeLimit )                      ;
        break                                                       ;
        case ID_VAR_DP                                              :
          DO_VAR ( Dictionary->dic_CodePtr.Cell )                   ;
        break                                                       ;
        case ID_VAR_HEADERS_BASE                                    :
          DO_VAR ( Dictionary->dic_HeaderBase )                     ;
        break                                                       ;
        case ID_VAR_HEADERS_LIMIT                                   :
          DO_VAR ( Dictionary->dic_HeaderLimit )                    ;
        break                                                       ;
        case ID_VAR_HEADERS_PTR                                     :
          DO_VAR ( Dictionary->dic_HeaderPtr )                      ;
        break                                                       ;
        case ID_VAR_NUM_TIB                                         :
          DO_VAR ( TaskNow->td_SourceNum )                          ;
        break                                                       ;
        case ID_VAR_OUT                                             :
          DO_VAR ( TaskNow->td_OUT )                                ;
        break                                                       ;
        case ID_VAR_TO_IN        : DO_VAR(TaskNow->td_IN ) ; break  ;
        case ID_VAR_BASE         : DO_VAR(Radix          ) ; break  ;
        case ID_VAR_CONTEXT      : DO_VAR(VarContext     ) ; break  ;
        case ID_VAR_ECHO         : DO_VAR(VarEcho        ) ; break  ;
        case ID_VAR_STATE        : DO_VAR(VarState       ) ; break  ;
        case ID_VAR_TRACE_FLAGS  : DO_VAR(VarTraceFlags  ) ; break  ;
        case ID_VAR_TRACE_LEVEL  : DO_VAR(VarTraceLevel  ) ; break  ;
        case ID_VAR_TRACE_STACK  : DO_VAR(VarTraceStack  ) ; break  ;
        case ID_VAR_RETURN_CODE  : DO_VAR(VarReturnCode  ) ; break  ;
        case ID_WORD                                                :
          TOS = (int)Word ( (char) TOS )       ;
        break                                                       ;
        case ID_WORD_FETCH                                          : /* ( waddr -- w ) */
          TOS = *((unsigned short *)TOS)                            ;
        break                                                       ;
        case ID_WORD_STORE                                          : /* ( w waddr -- ) */
          *((unsigned short *)TOS) = (unsigned short) M_POP         ;
          M_DROP                                                    ;
        break                                                       ;
        case ID_XOR                                                 :
          BINARY_OP ( ^ )                                           ;
        break                                                       ;
        /* Branch is followed by an offset relative to address of
         * offset.                                                 */
        case ID_ZERO_BRANCH                                         :
          if ( TOS == 0 )                                           {
            M_BRANCH                                                ;
          } else                                                    {
            /* skip over offset                                    */
            InsPtr++                                                ;
          }                                                         ;
          M_DROP                                                    ;
        break                                                       ;
        default                                                     :
          Report                                                    (
            QObject::tr("Catch: Unrecognised token = 0x%1 at 0x%2\n")
            .arg(Token,0,16).arg((AddressPointer)InsPtr,0,16)     ) ;
          InsPtr = 0                                                ;
        break                                                       ;
    }                                                               ;
    /* Traverse to next token in secondary.                        */
    if (InsPtr) Token = READ_CELL_DIC(InsPtr++)                     ;
  } while ( ( InitialReturnStack - TORPTR ) > 0 )                   ;
  SAVE_REGISTERS                                                    ;
  return ExceptionReturnCode                                        ;
}

/* Main entry point for Forth. */
int N::Forth::Do(const char * DicFileName,const char * SourceName,bool IfInit)
{
  pfTaskData_t   * cftd       = NULL                            ;
  pfDictionary_t * dic        = NULL                            ;
  int              Result     = 0                               ;
  unsigned int     EntryPoint = 0                               ;
  /* Allocate Task structure.                                  */
  cftd = (pfTaskData_t *)CreateTask                             (
                     DEFAULT_USER_DEPTH, DEFAULT_RETURN_DEPTH ) ;
  if ( NotNull( cftd ) )                                        {
    setCurrentTask ( (void *)cftd )                             ;
    if ( !VarQuiet )                                            {
      if( IsHostLittleEndian() ) Report(QString("-LE"))         ;
                            else Report(QString("-BE"))         ;
      if (sizeof(int) == 8) Report(QString("/64"))              ; else
      if (sizeof(int) == 4) Report(QString("/32"))              ;
      Report(QString("\n"))                                     ;
    }                                                           ;
    if ( IfInit )                                               {
      dic = (pfDictionary_t *) BuildDictionary( DEFAULT_HEADER_SIZE, DEFAULT_CODE_SIZE ) ;
    } else                                                      {
      if ( NotNull(DicFileName) )                               {
        dic = (pfDictionary_t *) LoadDictionary ( DicFileName,&EntryPoint ) ;
      } else                                                    {
        Report(" (static)\n")                                   ;
        dic = (pfDictionary_t *) LoadStaticDictionary ( )       ;
      }                                                         ;
    }                                                           ;
    /////////////////////////////////////////////////////////////
    if ( dic == NULL )                                          {
      Report     ( QObject::tr("DoForth: Error occured.\n") )   ;
      DeleteTask ( cftd                                     )   ;
      return -1                                                 ;
    }                                                           ;
    if ( !VarQuiet ) Report("\n")                               ;
    Result = ExecIfDefined("AUTO.INIT")                         ;
    if ( Result != 0 )                                          {
      Report(QObject::tr("Error in AUTO.INIT"))                 ;
      Report     ( QObject::tr("DoForth: Error occured.\n") )   ;
      DeleteTask ( cftd                                     )   ;
      return -1                                                 ;
    }                                                           ;
    if ( EntryPoint != 0 )                                      {
      Result = Catch ( EntryPoint )                             ;
    } else                                                      {
      if ( IsNull(SourceName) )                                 {
        Result = Quit ( )                                       ;
      } else                                                    {
        if ( !VarQuiet )                                        {
          Report(QObject::tr("Including:%1\n").arg(SourceName)) ;
        }                                                       ;
        Result = Include ( SourceName )                         ;
      }                                                         ;
    }                                                           ;
    /* Clean up after running Forth. */
    ExecIfDefined    ( "AUTO.TERM" )                            ;
    DeleteDictionary ( dic         )                            ;
    DeleteTask       ( cftd        )                            ;
  }                                                             ;
  return Result                                                 ;
}

int N::Forth::Write(File & file,unsigned int value)
{
  int           numw                     ;
  unsigned char pad[4]                   ;
  Write32BE(pad,value)                   ;
  numw = file.write((const char *)pad,4) ;
  if (numw!=4) return -1                 ;
  return 0                               ;
}

/***************************************************************/
unsigned int N::Forth::Read(File & file,unsigned int * value)
{
  int           numr               ;
  unsigned char pad[4]             ;
  numr = file.read ((char *)pad,4) ;
  if (numr!=4) return 0            ; // ridious
  *value = Read32BE( pad )         ;
  return 0                         ;
}

/* Use local copy of CODE_BASE for speed. */

QIODevice::OpenMode N::Forth::OpenMode(int mode)
{
  switch ( mode )    {
    case (PF_FAM_READ_ONLY + PF_FAM_BINARY_FLAG)  :
    return QIODevice::ReadOnly                    ;
    case (PF_FAM_WRITE_ONLY + PF_FAM_BINARY_FLAG) :
    return QIODevice::WriteOnly                   ;
    case (PF_FAM_READ_WRITE + PF_FAM_BINARY_FLAG) :
    return QIODevice::ReadWrite                   ;
    case PF_FAM_READ_ONLY                         :
    return QIODevice::ReadOnly                    ;
    case PF_FAM_WRITE_ONLY                        :
    return QIODevice::WriteOnly                   ;
    case PF_FAM_READ_WRITE                        :
    default                                       :
    return QIODevice::ReadWrite                   ;
  }                                               ;
  return QIODevice::NotOpen                       ;
}

QIODevice::OpenMode N::Forth::CreateMode(int mode)
{
  switch( mode )                                      {
    case (PF_FAM_WRITE_ONLY + PF_FAM_BINARY_FLAG)     :
    return QIODevice::Truncate | QIODevice::WriteOnly ;
    case (PF_FAM_READ_WRITE + PF_FAM_BINARY_FLAG)     :
    return QIODevice::Truncate | QIODevice::ReadWrite ;
    case PF_FAM_WRITE_ONLY                            :
    return QIODevice::Truncate | QIODevice::WriteOnly ;
    case PF_FAM_READ_WRITE                            :
    return QIODevice::Truncate | QIODevice::ReadWrite ;
  }                                                   ;
  return QIODevice::NotOpen                           ;
}

int N::Forth::toFile(File & fid,int ID,char * Data,int NumBytes)
{
  int  numw                                                      ;
  int  EvenNumW = EVENUP(NumBytes)                               ;
  bool correct  = true                                           ;
  ////////////////////////////////////////////////////////////////
  if ( correct && Write ( fid , ID       ) < 0 ) correct = false ;
  if ( correct && Write ( fid , EvenNumW ) < 0 ) correct = false ;
  ////////////////////////////////////////////////////////////////
  if (correct)                                                   {
    numw = fid . write ( Data , EvenNumW )                       ;
    if ( numw != EvenNumW )                                      {
      Error ("N::Forth::toFile", PF_ERR_WRITE_FILE)                ;
      return -1                                                  ;
    }                                                            ;
    return 0                                                     ;
  }                                                              ;
  Error ( "N::Forth::toFile" , PF_ERR_WRITE_FILE )                 ;
  return -1                                                      ;
}

/* Convert dictionary info chunk between native and on-disk (big-endian). */

void N::Forth::WriteDictionary(void * dictionary)
{
  DictionaryInfoChunk * sd = (DictionaryInfoChunk *)dictionary ;
  unsigned int        * p  = (unsigned int        *)sd         ;
  int                   i                                      ;
  for (i=0; i<((int)(sizeof(*sd)/sizeof(unsigned int))); i++)  {
    Write32BE ( (unsigned char *)&p[i], p[i] )                 ;
  }                                                            ;
}

/* Convert all fields in structure from BigEndian to Native. */

void N::Forth::ReadDictionary(void * dictionary)
{
  DictionaryInfoChunk * sd = (DictionaryInfoChunk *)dictionary ;
  unsigned int        * p  = (unsigned int        *)sd         ;
  int                   i                                      ;
  for (i=0; i<((int)(sizeof(*sd)/sizeof(unsigned int))); i++)  {
    p[i] = Read32BE ( (unsigned char *)&p[i] )                 ;
  }                                                            ;
}

/*************************************************************************
 * Save Dictionary in File.                                              *
 * If EntryPoint is NULL, save as development environment.               *
 * If EntryPoint is non-NULL, save as turnKey environment with no names. *
 *************************************************************************/

int N::Forth::SaveForth(QString fileName,int EntryPoint,int NameSize,int CodeSize)
{
  File               * fid = NULL                                              ;
  DictionaryInfoChunk  SD                                                      ;
  unsigned int         FormSize                                                ;
  unsigned int         NameChunkSize = 0                                       ;
  unsigned int         CodeChunkSize                                           ;
  unsigned int         relativeCodePtr                                         ;
  int                  eflag                                                   ;
  //////////////////////////////////////////////////////////////////////////////
  fid  = new File    (          )                                              ;
  fid -> setFileName ( fileName )                                              ;
  if (!fid->open(QIODevice::WriteOnly))                                        {
    Error( "N::Forth::SaveDictionary" , PF_ERR_OPEN_FILE )                     ;
    return -1                                                                  ;
  }                                                                            ;
  //////////////////////////////////////////////////////////////////////////////
  /* Save in uninitialized form.                                              */
  ExecIfDefined ( "AUTO.TERM" )                                                ;
  /* Write FORM Header ----------------------------                           */
  if ( Write ( *fid , ID_FORM ) < 0 ) goto SaveError                           ;
  if ( Write ( *fid , 0       ) < 0 ) goto SaveError                           ;
  if ( Write ( *fid , ID_P4TH ) < 0 ) goto SaveError                           ;
  /* Write P4DI Dictionary Info  ------------------                           */
  SD.sd_Version         = (int)(EXCITON_VERSION * 100)                         ;
  relativeCodePtr       = ABS_TO_CODEREL(Dictionary->dic_CodePtr.Byte)         ;
  SD.sd_RelCodePtr      = relativeCodePtr                                                  ;
  SD.sd_UserStackSize   = sizeof(int) * (TaskNow->td_StackBase  - TaskNow->td_StackLimit ) ;
  SD.sd_ReturnStackSize = sizeof(int) * (TaskNow->td_ReturnBase - TaskNow->td_ReturnLimit) ;
  SD.sd_NumPrimitives   = NumPrimitives  ; /* Must match compiled dictionary. */
  SD.sd_FloatSize       = sizeof(double) ; /* Must match compiled dictionary. */
  SD.sd_CellSize        = sizeof(int)                                          ;
  /* Set bit that specifies whether dictionary is BIG or LITTLE Endian.       */
  eflag                 = IsHostLittleEndian() ? 0 : SD_F_BIG_ENDIAN_DIC       ;
  SD.sd_Flags           = eflag                                                ;
  if ( EntryPoint ) SD.sd_EntryPoint = EntryPoint                              ; /* Turnkey! */
               else SD.sd_EntryPoint = 0                                       ;
  /* Do we save names?                                                        */
  if ( NameSize == 0 )                                                         {
    SD.sd_RelContext   = 0                                                     ;
    SD.sd_RelHeaderPtr = 0                                                     ;
    SD.sd_NameSize     = 0                                                     ;
  } else                                                                       {
    unsigned int relativeHeaderPtr                                             ;
    /* Development mode.                                                      */
    SD.sd_RelContext   = ABS_TO_NAMEREL ( VarContext                )          ;
    relativeHeaderPtr  = ABS_TO_NAMEREL ( Dictionary->dic_HeaderPtr )          ;
    SD.sd_RelHeaderPtr = relativeHeaderPtr                                     ;
    /* How much real name space is there?                                     */
    NameChunkSize      = QUADUP         ( relativeHeaderPtr                )   ; /* Align */
    /* NameSize must be 0 or greater than NameChunkSize + 1K                  */
    NameSize           = QUADUP         ( NameSize                         )   ; /* Align */
    if ( NameSize > 0 )                                                        {
      NameSize = max ( NameSize, (NameChunkSize + 1024) )                      ;
    }                                                                          ;
    SD.sd_NameSize = NameSize                                                  ;
  }                                                                            ;
  /* How much real code is there?                                             */
  CodeChunkSize  = QUADUP ( relativeCodePtr                   )                ;
  CodeSize       = QUADUP ( CodeSize                          )                ; /* Align */
  CodeSize       = max    ( CodeSize , (CodeChunkSize + 2048) )                ;
  SD.sd_CodeSize = CodeSize                                                    ;
  WriteDictionary (&SD)                                                        ;
  if (toFile(*fid,ID_P4DI,(char *)&SD,sizeof(DictionaryInfoChunk))<0) goto SaveError ;
  /* Write Name Fields if NameSize non-zero -------                           */
  if ( NameSize > 0 )                                                          {
    if ( toFile(*fid,ID_P4NM,(char *)NAME_BASE,NameChunkSize)<0) goto SaveError ;
  }                                                                            ;
  /* Write Code Fields ----------------------------                           */
  if (toFile(*fid,ID_P4CD,(char *)CODE_BASE,CodeChunkSize)<0) goto SaveError   ;
  FormSize = fid->pos() - 8                                                    ;
  fid->seek(4)                                                                 ;
  if ( Write( *fid, FormSize ) < 0 ) goto SaveError                            ;
  fid->close()                                                                 ;
  /* Restore initialization.                                                  */
  ExecIfDefined ( "AUTO.INIT" )                                                ;
  return 0                                                                     ;

SaveError:

  fid->seek(0)                                                                 ;
  Write ( *fid , ID_BADF )                                                     ;
  fid->close()                                                                 ;
  /* Restore initialization.                                                  */
  ExecIfDefined ( "AUTO.INIT" )                                                ;
  return -1                                                                    ;
}

void * N::Forth::LoadDictionary(const char * FileName,unsigned int * EntryPoint)
{
  pfDictionary_t      * dic = NULL                                             ;
  File                * fid                                                    ;
  DictionaryInfoChunk * sd                                                     ;
  unsigned int          ChunkID                                                ;
  unsigned int          ChunkSize                                              ;
  unsigned int          FormSize                                               ;
  unsigned int          BytesLeft                                              ;
  unsigned int          numr                                                   ;
  int                   isDicBigEndian                                         ;
  /* Open file.                                                               */
  fid = new File()                                                             ;
  fid->setFileName(QString(FileName))                                          ;
  if (!fid->open(QIODevice::ReadOnly))                                         {
    Error ( "N::Forth::LoadDictionary" , PF_ERR_OPEN_FILE )                      ;
    goto xt_error                                                              ;
  }                                                                            ;
  /* Read FORM, Size, ID                                                      */
  if ( Read ( *fid , &ChunkID  ) < 0 ) goto read_error                         ;
  if ( ChunkID != ID_FORM )                                                    {
    Error ( "N::Forth::LoadDictionary" , PF_ERR_WRONG_FILE )                   ;
    goto LoadError                                                             ;
  }                                                                            ;
  if ( Read ( *fid , &FormSize ) < 0) goto read_error                          ;
  BytesLeft = FormSize                                                         ;
  if ( Read ( *fid , &ChunkID  ) < 0) goto read_error                          ;
  BytesLeft -= 4                                                               ;
  if ( ChunkID != ID_P4TH )                                                    {
    Error ( "N::Forth::LoadDictionary" , PF_ERR_BAD_FILE )                     ;
    goto LoadError                                                             ;
  }                                                                            ;
  /* Scan and parse all chunks in file.                                       */
  while ( BytesLeft > 0 )                                                      {
    if ( Read ( *fid, &ChunkID   ) < 0) goto read_error                        ;
    if ( Read ( *fid, &ChunkSize ) < 0) goto read_error                        ;
    BytesLeft -= 8                                                             ;
    switch ( ChunkID )                                                         {
      case ID_P4DI                                                             :
        sd = (DictionaryInfoChunk *)new char [ ChunkSize ]                     ;
        if ( sd == NULL ) goto nomem_error                                     ;
        numr = fid->read( (char *)sd , ChunkSize )                             ;
        if ( numr != ChunkSize ) goto read_error                               ;
        BytesLeft -= ChunkSize                                                 ;
        ReadDictionary ( sd )                                                  ;
        isDicBigEndian = sd->sd_Flags & SD_F_BIG_ENDIAN_DIC                    ;
        ////////////////////////////////////////////////////////////////////////
        if ( !VarQuiet )                                                       {
          QString m                                                            ;
          QString d                                                            ;
          QString c = ""                                                       ;
          d  = isDicBigEndian ? QObject::tr("Big Endian Dictionary"            )
                              : QObject::tr("Little  Endian Dictionary")       ;
          if ( isDicBigEndian == IsHostLittleEndian() ) c = " !!!!"            ;
          m  = QObject::tr("Forth loading dictionary from file %1\n").arg(QString(FileName)) ;
          m += QObject::tr("     File format version is %1\n").arg(sd->sd_Version) ;
          m += QObject::tr("     Name space size = %1\n").arg(sd->sd_NameSize) ;
          m += QObject::tr("     Code space size = %1\n").arg(sd->sd_CodeSize) ;
          m += QObject::tr("     Entry Point     = %1\n").arg(sd->sd_EntryPoint) ;
          m += QObject::tr("     Cell Size       = %1\n").arg(sd->sd_CellSize) ;
          m += QObject::tr("     %1%2\n"                ).arg(d).arg(c)        ;
          Report(m)                                                            ;
        }                                                                      ;
        ////////////////////////////////////////////////////////////////////////
        if ( sd->sd_Version > (EXCITON_VERSION * 100) )                        {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_VERSION_FUTURE )         ;
          goto LoadError                                                       ;
        }                                                                      ;
        if ( sd->sd_CellSize != sizeof(int) )                                  {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_CELL_SIZE_CONFLICT )     ;
          goto LoadError                                                       ;
        }                                                                      ;
        if ( sd->sd_NumPrimitives > NUM_PRIMITIVES )                           {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_NOT_SUPPORTED )          ;
          goto LoadError                                                       ;
        }                                                                      ;
  /* Check to make sure that EndianNess of dictionary matches mode of pForth. */
        if ( isDicBigEndian == IsHostLittleEndian() )                          {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_ENDIAN_CONFLICT )        ;
          goto LoadError                                                       ;
        }                                                                      ;
        /* Check for compatible float size.                                   */
        if ( sd->sd_FloatSize != sizeof(double) )                              {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_FLOAT_CONFLICT )         ;
          goto LoadError                                                       ;
        }                                                                      ;
        dic = (pfDictionary_t *)CreateDictionary(sd->sd_NameSize,sd->sd_CodeSize) ;
        if ( dic == NULL ) goto nomem_error                                    ;
        CurrentDictionary = (void *)dic                                        ;
        if ( sd->sd_NameSize > 0 )                                             {
          VarContext = NAMEREL_TO_ABS(sd->sd_RelContext)                       ; /* Restore context. */
          Dictionary->dic_HeaderPtr = (AddressPointer)(unsigned char *)
                  NAMEREL_TO_ABS(sd->sd_RelHeaderPtr)                          ;
        } else                                                                 {
          VarContext = 0                                                       ;
          Dictionary->dic_HeaderPtr = (unsigned int)NULL                       ;
        }                                                                      ;
        Dictionary->dic_CodePtr.Byte = (unsigned char *) CODEREL_TO_ABS(sd->sd_RelCodePtr) ;
        NumPrimitives = sd->sd_NumPrimitives                                   ; /* Must match compiled dictionary. */
        /* Pass EntryPoint back to caller.                                    */
        if ( EntryPoint != NULL ) *EntryPoint = sd->sd_EntryPoint              ;
        free(sd)                                                               ;
      break                                                                    ;
      case ID_P4NM                                                             :
        if ( NAME_BASE == 0 )                                                  {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_NO_NAMES )               ;
          goto LoadError                                                       ;
        }                                                                      ;
        if ( Dictionary == NULL )                                              {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_BAD_FILE )               ;
          goto LoadError                                                       ;
        }                                                                      ;
        if ( ChunkSize > NAME_SIZE )                                           {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_TOO_BIG  )               ;
          goto LoadError                                                       ;
        }                                                                      ;
        numr = fid->read( (char *) NAME_BASE , ChunkSize )                     ;
        if ( numr != ChunkSize ) goto read_error                               ;
        BytesLeft -= ChunkSize                                                 ;
      break                                                                    ;
      case ID_P4CD                                                             :
        if ( Dictionary == NULL )                                              {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_BAD_FILE )               ;
          goto LoadError                                                       ;
        }                                                                      ;
        if ( ChunkSize > CODE_SIZE )                                           {
          Error ( "N::Forth::LoadDictionary" , PF_ERR_TOO_BIG )                ;
          goto LoadError                                                       ;
        }                                                                      ;
        numr = fid->read( (char *) CODE_BASE , ChunkSize )                     ;
        if ( numr != ChunkSize ) goto read_error                               ;
        BytesLeft -= ChunkSize                                                 ;
      break                                                                    ;
      default                                                                  :
        Error ( "N::Forth::LoadDictionary" , PF_ERR_BAD_FILE )                 ;
        if ( NotNull(fid) )                                                    {
          qint64 ps = fid->pos() + ChunkSize                                   ;
          fid->seek(ps)                                                        ;
        }                                                                      ;
      break                                                                    ;
    }                                                                          ;
  }                                                                            ;
  //////////////////////////////////////////////////////////////////////////////
  fid->close()                                                                 ;
  if ( NAME_BASE != 0)                                                         {
    int Result                                                                 ;
    /* Find special words in dictionary for global XTs.                       */
    if ( (Result = SpecialXTs()) < 0 )                                         {
      Report(QObject::tr("N::Forth::LoadDictionary: SpecialXTs = %1\n").arg(Result)) ;
      goto LoadError                                                           ;
    }                                                                          ;
  }                                                                            ;
  //////////////////////////////////////////////////////////////////////////////
  return (void *) dic                                                          ;

nomem_error:

  Error ( "N::Forth::LoadDictionary" , PF_ERR_NO_MEM ) ;
  fid->close() ;

  return NULL;

read_error:

  Error ( "N::Forth::LoadDictionary" , PF_ERR_READ_FILE ) ;

LoadError:

  fid->close() ;

xt_error:

  return NULL ;
}

#define STATIC_NAME_SIZE 0x3D68
#define STATIC_CODE_SIZE 0x93EC

void * N::Forth::LoadStaticDictionary(void)
{
  pfDictionary_t * dic = NULL                                       ;
  int              NewNameSize                                      ;
  int              NewCodeSize                                      ;
  int              Result                                           ;
  ///////////////////////////////////////////////////////////////////
  if ( IF_LITTLE_ENDIAN != IsHostLittleEndian() )                   {
    QString m                                                       ;
    QString d                                                       ;
    QString c                                                       ;
    d = IF_LITTLE_ENDIAN ? QObject::tr("Little Endian Dictionary"   )
                         : QObject::tr("Big Endian Dictionary"    ) ;
    c = IsHostLittleEndian() ? QObject::tr("Little Endian CPU"      )
                             : QObject::tr("Big Endian CPU"       ) ;
    m  = QObject::tr("%1 on %2\n").arg(d).arg(c)                    ;
    Report ( m )                                                    ;
  }                                                                 ;
  ///////////////////////////////////////////////////////////////////
  /* Check to make sure that EndianNess of dictionary matches mode
   * of Forth.                                                     */
  if ( IF_LITTLE_ENDIAN != IsHostLittleEndian() )                   {
    Error("N::Forth::LoadStaticDictionary",PF_ERR_ENDIAN_CONFLICT)    ;
    return NULL                                                     ;
  }                                                                 ;
  #ifndef PF_EXTRA_HEADERS
    #define PF_EXTRA_HEADERS  (20000)
  #endif
  #ifndef PF_EXTRA_CODE
    #define PF_EXTRA_CODE  (40000)
  #endif
  /* Copy static const data to allocated dictionaries.             */
  NewNameSize = STATIC_NAME_SIZE + PF_EXTRA_HEADERS              ;
  NewCodeSize = STATIC_CODE_SIZE + PF_EXTRA_CODE                 ;
  dic = (pfDictionary_t *)CreateDictionary(NewNameSize,NewCodeSize) ;
  CurrentDictionary = dic                                           ;
  if ( !dic )                                                       {
    Error("N::Forth::LoadStaticDictionary",PF_ERR_NO_MEM)             ;
    return NULL                                                     ;
  }                                                                 ;
  ///////////////////////////////////////////////////////////////////
  memcpy((unsigned char *)dic->dic_HeaderBase,MinDicNames,STATIC_NAME_SIZE) ;
  memcpy((unsigned char *)dic->dic_CodeBase  ,MinDicCode ,STATIC_CODE_SIZE) ;
  ///////////////////////////////////////////////////////////////////
  dic->dic_CodePtr.Byte = (unsigned char *) CODEREL_TO_ABS(CODEPTR) ;
  NumPrimitives = NUM_PRIMITIVES                                    ;
  if ( NAME_BASE != 0)                                              {
    /* Setup name space.                                           */
    dic->dic_HeaderPtr = (AddressPointer)(unsigned char *) NAMEREL_TO_ABS(HEADERPTR) ;
    VarContext         = NAMEREL_TO_ABS(RELCONTEXT)                 ; /* Restore context. */
    /* Find special words in dictionary for global XTs.            */
    if ( (Result = SpecialXTs()) < 0 )                              {
      Report(QObject::tr("N::Forth::LoadStaticDictionary: SpecialXTs = %1\n").arg(Result)) ;
      return NULL                                                   ;
    }                                                               ;
  }                                                                 ;
  return (void *)dic                                                ;
}
