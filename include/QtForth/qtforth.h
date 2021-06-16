/****************************************************************************
 *                                                                          *
 * Copyright (C) 2015 Neutrino International Inc.                           *
 *                                                                          *
 * Author : Brian Lin <lin.foxman@gmail.com>, Skype: wolfram_lin            *
 *                                                                          *
 ****************************************************************************/

#ifndef QT_FORTH_H
#define QT_FORTH_H

#include <QtCore>
#include <QtNetwork>
#include <QtSql>
#include <QtScript>
#include <Essentials>
#include <Mathematics>

QT_BEGIN_NAMESPACE

#ifndef QT_STATIC
#    if defined(QT_BUILD_QTFORTH_LIB)
#      define Q_FORTH_EXPORT Q_DECL_EXPORT
#    else
#      define Q_FORTH_EXPORT Q_DECL_IMPORT
#    endif
#else
#    define Q_FORTH_EXPORT
#endif

#define ForthFunction AnythingFunction

namespace N {

class Q_FORTH_EXPORT Forth ;

class Q_FORTH_EXPORT Forth
{
  public:

    enum ForthIDs              {
      ID_EXIT            =   0 ,
      ID_1MINUS                ,
      ID_1PLUS                 ,
      ID_2DUP                  ,
      ID_2LITERAL              ,
      ID_2LITERAL_P            ,
      ID_2MINUS                ,
      ID_2OVER                 ,
      ID_2PLUS                 ,
      ID_2SWAP                 ,
      ID_2_R_FETCH             ,
      ID_2_R_FROM              ,
      ID_2_TO_R                ,
      ID_ACCEPT_P              ,
      ID_ALITERAL              ,
      ID_ALITERAL_P            ,
      ID_ALLOCATE              ,
      ID_AND                   ,
      ID_ARSHIFT               ,
      ID_BAIL                  ,
      ID_BODY_OFFSET           ,
      ID_BRANCH                ,
      ID_BYE                   ,
      ID_CALL_C                ,
      ID_CFETCH                ,
      ID_CLEAR                 ,
      ID_CMOVE                 ,
      ID_CMOVE_UP              ,
      ID_COLON                 ,
      ID_COLON_P               ,
      ID_COMPARE               ,
      ID_COMP_EQUAL            ,
      ID_COMP_GREATERTHAN      ,
      ID_COMP_LESSTHAN         ,
      ID_COMP_NOT_EQUAL        ,
      ID_COMP_U_GREATERTHAN    ,
      ID_COMP_U_LESSTHAN       ,
      ID_COMP_ZERO_EQUAL       ,
      ID_COMP_ZERO_GREATERTHAN ,
      ID_COMP_ZERO_LESSTHAN    ,
      ID_COMP_ZERO_NOT_EQUAL   ,
      ID_CR                    ,
      ID_CREATE                ,
      ID_CREATE_P              ,
      ID_CSTORE                ,
      ID_DEFER                 ,
      ID_DEFER_P               ,
      ID_DEPTH                 ,
      ID_DIVIDE                ,
      ID_DOT                   ,
      ID_DOTS                  ,
      ID_DO_P                  ,
      ID_DROP                  ,
      ID_DUMP                  ,
      ID_DUP                   ,
      ID_D_MINUS               ,
      ID_D_MTIMES              ,
      ID_D_MUSMOD              ,
      ID_D_PLUS                ,
      ID_D_UMSMOD              ,
      ID_D_UMTIMES             ,
      ID_EMIT                  ,
      ID_EMIT_P                ,
      ID_EOL                   ,
      ID_ERRORQ_P              ,
      ID_EXECUTE               ,
      ID_FETCH                 ,
      ID_FILE_CLOSE            ,
      ID_FILE_CREATE           ,
      ID_FILE_OPEN             ,
      ID_FILE_POSITION         ,
      ID_FILE_READ             ,
      ID_FILE_REPOSITION       ,
      ID_FILE_RO               ,
      ID_FILE_RW               ,
      ID_FILE_SIZE             ,
      ID_FILE_WRITE            ,
      ID_FILL                  ,
      ID_FIND                  ,
      ID_FINDNFA               ,
      ID_FLUSHEMIT             ,
      ID_FREE                  ,
      ID_HERE                  ,
      ID_NUMBERQ_P             ,
      ID_I                     ,
      ID_INCLUDE_FILE          ,
      ID_J                     ,
      ID_KEY                   ,
      ID_LEAVE_P               ,
      ID_LITERAL               ,
      ID_LITERAL_P             ,
      ID_LOADSYS               ,
      ID_LOCAL_COMPILER        ,
      ID_LOCAL_ENTRY           ,
      ID_LOCAL_EXIT            ,
      ID_LOCAL_FETCH           ,
      ID_LOCAL_FETCH_1         ,
      ID_LOCAL_FETCH_2         ,
      ID_LOCAL_FETCH_3         ,
      ID_LOCAL_FETCH_4         ,
      ID_LOCAL_FETCH_5         ,
      ID_LOCAL_FETCH_6         ,
      ID_LOCAL_FETCH_7         ,
      ID_LOCAL_FETCH_8         ,
      ID_LOCAL_PLUSSTORE       ,
      ID_LOCAL_STORE           ,
      ID_LOCAL_STORE_1         ,
      ID_LOCAL_STORE_2         ,
      ID_LOCAL_STORE_3         ,
      ID_LOCAL_STORE_4         ,
      ID_LOCAL_STORE_5         ,
      ID_LOCAL_STORE_6         ,
      ID_LOCAL_STORE_7         ,
      ID_LOCAL_STORE_8         ,
      ID_LOOP_P                ,
      ID_LSHIFT                ,
      ID_MAX                   ,
      ID_MIN                   ,
      ID_MINUS                 ,
      ID_NAME_TO_PREVIOUS      ,
      ID_NAME_TO_TOKEN         ,
      ID_NOOP                  ,
      ID_NUMBERQ               ,
      ID_OR                    ,
      ID_OVER                  ,
      ID_PICK                  ,
      ID_PLUS                  ,
      ID_PLUSLOOP_P            ,
      ID_PLUS_STORE            ,
      ID_QDO_P                 ,
      ID_QDUP                  ,
      ID_QTERMINAL             ,
      ID_QUIT_P                ,
      ID_REFILL                ,
      ID_RESIZE                ,
      ID_RESTORE_INPUT         ,
      ID_ROLL                  ,
      ID_ROT                   ,
      ID_RP_FETCH              ,
      ID_RP_STORE              ,
      ID_RSHIFT                ,
      ID_R_DROP                ,
      ID_R_FETCH               ,
      ID_R_FROM                ,
      ID_SAVE_FORTH_P          ,
      ID_SAVE_INPUT            ,
      ID_SCAN                  ,
      ID_SEMICOLON             ,
      ID_SKIP                  ,
      ID_SOURCE                ,
      ID_SOURCE_ID             ,
      ID_SOURCE_ID_POP         ,
      ID_SOURCE_ID_PUSH        ,
      ID_SOURCE_SET            ,
      ID_SP_FETCH              ,
      ID_SP_STORE              ,
      ID_STORE                 ,
      ID_SWAP                  ,
      ID_TICK                  ,
      ID_TIMES                 ,
      ID_TO_R                  ,
      ID_TYPE                  ,
      ID_TYPE_P                ,
      ID_VAR_BASE              ,
      ID_VAR_CODE_BASE         ,
      ID_VAR_CODE_LIMIT        ,
      ID_VAR_CONTEXT           ,
      ID_VAR_DP                ,
      ID_VAR_ECHO              ,
      ID_VAR_HEADERS_BASE      ,
      ID_VAR_HEADERS_LIMIT     ,
      ID_VAR_HEADERS_PTR       ,
      ID_VAR_NUM_TIB           ,
      ID_VAR_OUT               ,
      ID_VAR_RETURN_CODE       ,
      ID_VAR_SOURCE_ID         ,
      ID_VAR_STATE             ,
      ID_VAR_TO_IN             ,
      ID_VAR_TRACE_FLAGS       ,
      ID_VAR_TRACE_LEVEL       ,
      ID_VAR_TRACE_STACK       ,
      ID_VLIST                 ,
      ID_WORD                  ,
      ID_WORD_FETCH            ,
      ID_WORD_STORE            ,
      ID_XOR                   ,
      ID_ZERO_BRANCH           ,
      ID_CATCH                 ,
      ID_THROW                 ,
      ID_INTERPRET             ,
      ID_FILE_WO               ,
      ID_FILE_BIN              ,
      ID_CELL                  ,
      ID_CELLS                 ,
      ID_FILE_DELETE           ,
      ID_RESERVED01            ,
      ID_RESERVED02            ,
      ID_RESERVED03            ,
      ID_RESERVED04            ,
      ID_RESERVED05            ,
      ID_RESERVED06            ,
      ID_RESERVED07            ,
      ID_RESERVED08            ,
      ID_RESERVED09            ,
      ID_RESERVED10            ,
      ID_RESERVED11            ,
      ID_RESERVED12            ,
      ID_RESERVED13            ,
      ID_FP_D_TO_F             ,
      ID_FP_FSTORE             ,
      ID_FP_FTIMES             ,
      ID_FP_FPLUS              ,
      ID_FP_FMINUS             ,
      ID_FP_FSLASH             ,
      ID_FP_F_ZERO_LESS_THAN   ,
      ID_FP_F_ZERO_EQUALS      ,
      ID_FP_F_LESS_THAN        ,
      ID_FP_F_TO_D             ,
      ID_FP_FFETCH             ,
      ID_FP_FDEPTH             ,
      ID_FP_FDROP              ,
      ID_FP_FDUP               ,
      ID_FP_FLITERAL           ,
      ID_FP_FLITERAL_P         ,
      ID_FP_FLOAT_PLUS         ,
      ID_FP_FLOATS             ,
      ID_FP_FLOOR              ,
      ID_FP_FMAX               ,
      ID_FP_FMIN               ,
      ID_FP_FNEGATE            ,
      ID_FP_FOVER              ,
      ID_FP_FROT               ,
      ID_FP_FROUND             ,
      ID_FP_FSWAP              ,
      ID_FP_FSTAR_STAR         ,
      ID_FP_FABS               ,
      ID_FP_FACOS              ,
      ID_FP_FACOSH             ,
      ID_FP_FALOG              ,
      ID_FP_FASIN              ,
      ID_FP_FASINH             ,
      ID_FP_FATAN              ,
      ID_FP_FATAN2             ,
      ID_FP_FATANH             ,
      ID_FP_FCOS               ,
      ID_FP_FCOSH              ,
      ID_FP_FLN                ,
      ID_FP_FLNP1              ,
      ID_FP_FLOG               ,
      ID_FP_FSIN               ,
      ID_FP_FSINCOS            ,
      ID_FP_FSINH              ,
      ID_FP_FSQRT              ,
      ID_FP_FTAN               ,
      ID_FP_FTANH              ,
      ID_FP_FPICK              ,
      NUM_PRIMITIVES
    }                          ;

    SUID            uuid            ;
    QString         Name            ;
    QList<QDir>     Includes        ;
    bool            Traditional     ; // Traditional Forth string, default false
    int             Radix           ; // default base-10
    int             NumPrimitives   ;
    int             DepthAtColon    ; /* Depth of data stack when colon called. */
    int             IncludeIndex    ; /* data for INCLUDE that allows multiple nested files. */

    unsigned int    LocalCompilerXT ; /* custom compiler for local variables */
    unsigned int    NumberQXT       ; /* XT of NUMBER? */
    unsigned int    QuitPXT         ; /* XT of (QUIT) */
    unsigned int    AcceptPXT       ; /* XT of ACCEPT */

    /* Global Forth variables. */
    int             VarContext      ; /* Points to last name field. */
    int             VarState        ; /* 1 if compiling. */
    int             VarEcho         ; /* Echo input. */
    int             VarTraceLevel   ; /* Trace Level for Inner Interpreter. */
    int             VarTraceStack   ; /* Dump Stack each time if true. */
    int             VarTraceFlags   ; /* Enable various internal debug messages. */
    int             VarQuiet        ; /* Suppress unnecessary messages, OK, etc. */
    int             VarReturnCode   ; /* Returned to caller of Forth, eg. UNIX shell. */

    explicit     Forth             (void) ;
    virtual     ~Forth             (void) ;

    char         toUpper           (char c) ;
    char         toLower           (char c) ;
    int          toNumber          (char c) ;

    int          compare           (const char * s1,int len1,const char * s2,int len2) ;
    int          compare           (const char * s1,const char *s2,int length) ;
    int          icompare          (const char * s1,const char *s2,int length) ;

    QString      toString          (const char * forthString) ;
    QByteArray   toForth           (QString text) ;
    char *       toString          (int Num,int Base,bool IfSigned,int MinChars) ;
    const char * toPrevious        (const char * NFA) ;
    unsigned int toToken           (const char * NFA) ;
    int          toName            (unsigned int XT,const char ** NfaPtr) ;

    virtual bool InLoop            (void) ;
    virtual bool GlueFunctions     (void) ;
    virtual void Clear             (void) ;
    virtual void Dump              (void * addr,int count) ;
    virtual void Report            (QString message) ;
    virtual void Report            (QByteArray & message) ;
    virtual void Report            (const char * forthString) ;
    virtual void Error             (const char * functionName,int errorCode) ;
    virtual void Throw             (int code) ;
    virtual void Exit              (int code) ;
    virtual int  Feed              (char * buffer,int maxChars) ;
    virtual int  readLine          (char * buffer,int maxChars,File * stream) ;

    void   Dot                     (int n) ;
    void   DotHex                  (int n) ;
    void   DotS                    (void) ;
    char * Word                    (char c) ;
    int    NumberQ                 (const char * FWord,int * Num) ;
    int    Skip                    (char * AddrIn,int count,char c,char ** AddrOut) ;
    int    Scan                    (char * AddrIn,int count,char c,char ** AddrOut) ;
    int    FindNfa                 (const char * WordName,const char ** NfaPtr) ;
    int    Find                    (const char * WordName,unsigned int * pXT) ;
    int    FindC                   (const char * WordName,unsigned int * pXT) ;
    void   DeferredC               (unsigned int DefaultXT,const char * cName) ;
    int    CheckRoom               (void) ;
    int    CheckRedefinition       (const char * FName) ;
    void   SecondaryHeader         (const char * FName) ;
    void   FinishSecondary         (void) ;
    void   StringColon             (const char * FName) ;
    void   StringCreate            (char * FName) ;
    void   StringDefer             (const char * FName,unsigned int DefaultXT) ;
    void   Colon                   (void) ;
    void   Create                  (void) ;
    void   Defer                   (void) ;
    void   UnSmudge                (void) ;
    int    SemiColon               (void) ;
    void   Literal                 (int Num) ;
    void   LiteralA                (int Num) ;
    void   Literal2                (int dHi,int dLo) ;
    void   LiteralFP               (double fnum) ;
    int    FindAndCompile          (const char * theWord) ;
    int    Interpret               (void) ;
    int    Refill                  (void) ;
    int    OK                      (void) ;
    int    Catch                   (unsigned int XT);

    void   setQuiet                (int IfQuiet) ;
    int    isQuiet                 (void) ;

    int    SpecialXTs              (void) ;
    int    NotCompiled             (const char * functionName) ;
    int    ExecIfDefined           (const char * cString) ;

    int    PopInt                  (void) ;

    virtual int Call               (int Index,int ReturnMode,int NumParams) ;
    virtual int Glue               (const char * cName,unsigned int Index,int ReturnMode,int NumParams) ;

    int    Quit                    (void) ;

    void   CreateEntry             (unsigned int XT,const char * fName,unsigned int Flags) ;
    void   CreateEntryC            (unsigned int XT,const char * cName,unsigned int Flags) ;

    void   HandleIncludeError      (void) ;
    int    OuterInterpreterLoop    (void) ;

    unsigned int   ReadUIntBE      (const unsigned char * addr) ;
    unsigned int   ReadUIntLE      (const unsigned char * addr) ;
    unsigned int   Read32BE        (const unsigned char * addr) ;
    unsigned int   Read32LE        (const unsigned char * addr) ;
    unsigned short Read16BE        (const unsigned char * addr) ;
    unsigned short Read16LE        (const unsigned char * addr) ;

    void           WriteUIntBE     (unsigned char * addr,unsigned int   data) ;
    void           WriteUIntLE     (unsigned char * addr,unsigned int   data) ;
    void           Write32BE       (unsigned char * addr,unsigned int   data) ;
    void           Write32LE       (unsigned char * addr,unsigned int   data) ;
    void           Write16BE       (unsigned char * addr,unsigned short data) ;
    void           Write16LE       (unsigned char * addr,unsigned short data) ;

    void           ReverseFloat    (const double * src,double * dst) ;
    void           WriteFloatBE    (double * addr,double data) ;
    void           WriteFloatLE    (double * addr,double data) ;
    double         ReadFloatBE     (const double * addr) ;
    double         ReadFloatLE     (const double * addr) ;

    int IsHostLittleEndian         (void) ;

    void TraceNames                (unsigned int Token,int Level) ;

    int  Do                        (const char * DicFileName,const char * SourceName,bool IfInit) ;

    int  SaveForth                 (QString fileName,int EntryPoint,int NameSize,int CodeSize) ;

  protected:

    int             CurrentFunctions  ;
    int             MaxFunctions      ;
    char            Scratch [ 4096 ]  ;
    void          * CurrentTask       ;
    void          * CurrentDictionary ;
    void          * IncludeStack      ;
    ForthFunction * Functions         ;

    void * CreateTask              (int UserStackDepth,int ReturnStackDepth) ;
    void   DeleteTask              (void * task) ;
    void   setCurrentTask          (void * task) ;
    void   ResetTask               (void) ;

    void * BuildDictionary         (int    HeaderSize,int CodeSize) ;
    void * CreateDictionary        (int    HeaderSize,int CodeSize) ;
    void   DeleteDictionary        (void * dictionary) ;
    void   WriteDictionary         (void * dictionary) ;
    void   ReadDictionary          (void * dictionary) ;

    int    Include                 (const char * FileName) ;
    int    IncludeFile             (File * file) ;
    bool   PushInput               (File * file) ;
    File * PopInput                (void) ;

    unsigned int toSourceId        (File * file) ;
    File *       toStream          (unsigned int id) ;
    int          Write             (File & file,unsigned int   value) ;
    unsigned int Read              (File & file,unsigned int * value) ;
    int          toFile            (File & fid,int ID,char * Data,int NumBytes) ;

    QIODevice::OpenMode OpenMode   (int mode) ;
    QIODevice::OpenMode CreateMode (int mode) ;

  private:

    void * LoadDictionary          (const char * FileName,unsigned int * EntryPoint) ;
    void * LoadStaticDictionary    (void) ;

};

}

QT_END_NAMESPACE

#endif
