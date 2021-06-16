#include <qtforth.h>

bool N::Forth::InLoop(void)
{
  return true ;
}

void N::Forth::Clear(void)
{
}

bool N::Forth::GlueFunctions(void)
{
  return true ;
}

void N::Forth::Report(QString message)
{
}

void N::Forth::Report(QByteArray & message)
{
  Report ( (const char *)message.data() ) ;
}

void N::Forth::Report(const char * forthString)
{
  QString S = toString(forthString) ;
  if (S.length()<=0) return         ;
  Report ( S )                      ;
}

char N::Forth::toUpper(char c)
{
  return (char) ( ((c>='a') && (c<='z')) ? (c - ('a' - 'A')) : c ) ;
}

char N::Forth::toLower(char c)
{
  return (char) ( ((c>='A') && (c<='Z')) ? (c + ('a' - 'A')) : c ) ;
}

int N::Forth::toNumber(char c)
{
  if ( (c >= '0') && (c <= '9') ) return ( c - '0'        ) ; else
  if ( (c >= 'A') && (c <= 'F') ) return ( c - 'A' + 0x0A ) ;
  return -1                                                 ;
}

QString N::Forth::toString(const char * forthString)
{
  int        L = (int)*forthString ;
  QByteArray B(forthString+1,L)    ;
  return     QString::fromUtf8(B)  ;
}

QByteArray N::Forth::toForth(QString text)
{
  QByteArray T = text.toUtf8() ;
  QByteArray L                 ;
  L . resize ( T.size() + 1 )  ;
  //////////////////////////////
  char * t = (char *)T.data()  ;
  char * l = (char *)L.data()  ;
  l[0] = T.size()              ;
  memcpy( l+1 , t , T.size() ) ;
  return L                     ;
}

int N::Forth::compare(const char * s1,const char *s2,int length)
{
  for (int i=0;i<length;i++) {
    if ( *s1++ != *s2++ )    {
      return 0               ;
    }                        ;
  }                          ;
  return 1                   ;
}

int N::Forth::icompare(const char * s1,const char *s2,int length)
{
  char c1,c2                 ;
  for (int i=0;i<length;i++) {
    c1 = toLower ( *s1++ )   ;
    c2 = toLower ( *s2++ )   ;
    if ( c1 != c2 )          {
      return 0               ;
    }                        ;
  }                          ;
  return 1                   ;
}

void N::Forth::Dot(int n)
{
  char  * p = toString(n,Radix,true,1) ;
  QString P(p)                         ;
  Report (QString("%1 ").arg(P))       ;
  delete [] p                          ;
}

void N::Forth::DotHex(int n)
{
  char  * p = toString(n,16,false,1) ;
  QString P(p)                       ;
  Report (QString("%1 ").arg(P))     ;
  delete [] p                        ;
}

void N::Forth::setQuiet(int IfQuiet)
{
  VarQuiet = IfQuiet ;
}

int N::Forth::isQuiet(void)
{
  return VarQuiet ;
}

int N::Forth::NotCompiled(const char * functionName)
{
  QString P(functionName) ;
  Report (QObject::tr("Function %1 not compiled in this version of Forth.\n").arg(P)) ;
  return -1               ;
}

void N::Forth::Exit(int code)
{
}

int N::Forth::Skip(char * AddrIn,int count,char c,char ** AddrOut)
{
  char * s = AddrIn                                            ;
  if ( c == ' ' )                                              {
    while ( ( count > 0 ) && (( *s == ' ') || ( *s == '\t')) ) {
      s     ++                                                 ;
      count --                                                 ;
    }                                                          ;
  } else                                                       {
    while (( count > 0 ) && ( *s == c ))                       {
      s     ++                                                 ;
      count --                                                 ;
    }                                                          ;
  }                                                            ;
  *AddrOut = s                                                 ;
  return count                                                 ;
}

int N::Forth::Scan(char * AddrIn,int count,char c,char ** AddrOut)
{
  char * s = AddrIn        ;
  if ( c == ' ' )          {
    while (( count > 0  ) &&
           ( *s != ' '  ) &&
           ( *s != '\r' ) &&
           ( *s != '\n' ) &&
           ( *s != '\t' )) {
      s     ++             ;
      count --             ;
    }                      ;
  } else                   {
    while(( count > 0 )   &&
          ( *s != c   )  ) {
      s     ++             ;
      count --             ;
    }                      ;
  }                        ;
  *AddrOut = s             ;
  return count             ;
}

/**************************************************************
** Compare two strings, case sensitive.
** Return zero if they match, -1 if s1<s2, +1 is s1>s2;
*/

int N::Forth::compare(const char * s1,int len1,const char * s2,int len2)
{
  int result = 0                         ;
  int n      = qMin(len1,len2)           ;
  int diff                               ;
  nFullLoop ( i , n )                    {
    if ( (diff = (*s2++ - *s1++)) != 0 ) {
      result = (diff > 0) ? -1 : 1       ;
      break                              ;
    }                                    ;
  }                                      ;
  /* Match up to MIN(len1,len2)         */
  if ( result == 0 )                     {
    if ( len1 < len2 ) result = -1;   else
    if ( len1 > len2 ) result =  1       ;
  }                                      ;
  return result                          ;
}

/***************************************************************
** Diagnostic routine that prints memory in table format.
*/

void N::Forth::Dump(void * addr,int count)
{
  int             nlines                  ;
  unsigned char * ptr  = NULL             ;
  unsigned char * cptr = NULL             ;
  unsigned char   c                       ;
  QString         S                       ;
  /////////////////////////////////////////
  nlines = ( count + 15 ) / 16            ;
  ptr    = (unsigned char *) addr         ;
  Report(QString("\n"))                   ;
  nFullLoop ( ln , nlines)                {
    S += toString((int)ptr,16,0,8)        ;
    S += ": "                             ;
    cptr = ptr                            ;
    nFullLoop ( cn , 16 )                 {
      S += toString((int)*cptr++,16,0,2)  ;
      S += " "                            ;
    }                                     ;
    S += " "                              ;
    nFullLoop ( cn , 16 )                 {
      c = *ptr++                          ;
      if ((c < ' ') || (c > '}')) c = '.' ;
      S += QString(c)                     ;
    }                                     ;
    S += QString("\n")                    ;
  }                                       ;
  if (S.length()>0) Report ( S )          ;
}

/***************************************************************/
/* Endian-ness tools. */
unsigned int N::Forth::ReadUIntBE(const unsigned char * addr)
{
  unsigned int temp =     (unsigned int)addr[0]  ;
  temp   = (temp << 8) | ((unsigned int)addr[1]) ;
  temp   = (temp << 8) | ((unsigned int)addr[2]) ;
  temp   = (temp << 8) | ((unsigned int)addr[3]) ;
  if ( sizeof(unsigned int) == 8 )               {
    temp = (temp << 8) | ((unsigned int)addr[4]) ;
    temp = (temp << 8) | ((unsigned int)addr[5]) ;
    temp = (temp << 8) | ((unsigned int)addr[6]) ;
    temp = (temp << 8) | ((unsigned int)addr[7]) ;
  }                                              ;
  return temp                                    ;
}

/* Endian-ness tools. */
unsigned int N::Forth::Read32BE(const unsigned char * addr)
{
  unsigned int temp =   (unsigned int)addr[0]  ;
  temp = (temp << 8) | ((unsigned int)addr[1]) ;
  temp = (temp << 8) | ((unsigned int)addr[2]) ;
  temp = (temp << 8) | ((unsigned int)addr[3]) ;
  return  temp                                 ;
}

unsigned short N::Forth::Read16BE(const unsigned char * addr)
{
  return (unsigned short)((addr[0]<<8) | addr[1]) ;
}

unsigned int N::Forth::ReadUIntLE(const unsigned char * addr)
{
  unsigned int temp = 0                          ;
  if ( sizeof(unsigned int) == 8 )               {
    temp = (temp << 8) | ((unsigned int)addr[7]) ;
    temp = (temp << 8) | ((unsigned int)addr[6]) ;
    temp = (temp << 8) | ((unsigned int)addr[5]) ;
    temp = (temp << 8) | ((unsigned int)addr[4]) ;
  }                                              ;
  temp   = (temp << 8) | ((unsigned int)addr[3]) ;
  temp   = (temp << 8) | ((unsigned int)addr[2]) ;
  temp   = (temp << 8) | ((unsigned int)addr[1]) ;
  temp   = (temp << 8) | ((unsigned int)addr[0]) ;
  return temp                                    ;
}

unsigned int N::Forth::Read32LE(const unsigned char * addr)
{
  unsigned int temp =   (unsigned int)addr[3]  ;
  temp = (temp << 8) | ((unsigned int)addr[2]) ;
  temp = (temp << 8) | ((unsigned int)addr[1]) ;
  temp = (temp << 8) | ((unsigned int)addr[0]) ;
  return temp                                  ;
}

unsigned short N::Forth::Read16LE(const unsigned char * addr)
{
  const unsigned char * bp = (const unsigned char *)addr ;
  return (unsigned short) ( ( bp[1] << 8 ) | bp[0] )     ;
}

void N::Forth::ReverseFloat(const double * src,double * dst)
{
  unsigned char       * d = (unsigned char       *) dst ;
  const unsigned char * s = (const unsigned char *) src ;
  for (int i=0;i<sizeof(double);i++)                    {
    d [ i ] = s [ sizeof(double) - 1 - i ]              ;
  }                                                     ;
}

void N::Forth::WriteFloatBE(double * addr,double data)
{
  if ( IsHostLittleEndian() )    {
    ReverseFloat ( &data, addr ) ;
  } else *addr = data            ;
}

double N::Forth::ReadFloatBE(const double * addr)
{
  if ( IsHostLittleEndian() )    {
    double data                  ;
    ReverseFloat ( addr, &data ) ;
    return data                  ;
  } else return *addr            ;
}

void N::Forth::WriteFloatLE(double * addr,double data)
{
  if ( IsHostLittleEndian() )    {
    *addr = data                 ;
  } else                         {
    ReverseFloat ( &data, addr ) ;
  }                              ;
}

double N::Forth::ReadFloatLE(const double * addr)
{
  double data                    ;
  if ( IsHostLittleEndian() )    {
    return *addr                 ;
  } else                         {
    ReverseFloat ( addr, &data ) ;
    return data                  ;
  }                              ;
}

void N::Forth::WriteUIntBE(unsigned char * addr,unsigned int data)
{
  if ( sizeof (unsigned int ) == 8 )     {
    *addr++ = (unsigned char) (data>>56) ;
    *addr++ = (unsigned char) (data>>48) ;
    *addr++ = (unsigned char) (data>>40) ;
    *addr++ = (unsigned char) (data>>32) ;
  }                                      ;
  *addr++   = (unsigned char) (data>>24) ;
  *addr++   = (unsigned char) (data>>16) ;
  *addr++   = (unsigned char) (data>> 8) ;
  *addr     = (unsigned char) (data    ) ;
}

void N::Forth::Write32BE(unsigned char * addr,unsigned int data)
{
  *addr++ = (unsigned char)(data>>24) ;
  *addr++ = (unsigned char)(data>>16) ;
  *addr++ = (unsigned char)(data>> 8) ;
  *addr   = (unsigned char)(data    ) ;
}

void N::Forth::Write16BE(unsigned char * addr,unsigned short data)
{
  *addr++ = (unsigned char) ( data >> 8 ) ;
  *addr   = (unsigned char) ( data      ) ;
}

void N::Forth::WriteUIntLE(unsigned char * addr,unsigned int data)
{
  if ( sizeof(unsigned int) == 8 )                   {
    *addr++ = (unsigned char) data; data = data >> 8 ;
    *addr++ = (unsigned char) data; data = data >> 8 ;
    *addr++ = (unsigned char) data; data = data >> 8 ;
    *addr++ = (unsigned char) data; data = data >> 8 ;
  }                                                  ;
  *addr++   = (unsigned char) data; data = data >> 8 ;
  *addr++   = (unsigned char) data; data = data >> 8 ;
  *addr++   = (unsigned char) data; data = data >> 8 ;
  *addr     = (unsigned char) data                   ;
}

void N::Forth::Write32LE(unsigned char * addr,unsigned int data)
{
  *addr++ = (unsigned char)data ; data = data >> 8 ;
  *addr++ = (unsigned char)data ; data = data >> 8 ;
  *addr++ = (unsigned char)data ; data = data >> 8 ;
  *addr   = (unsigned char)data ;
}

void N::Forth::Write16LE(unsigned char * addr,unsigned short data)
{
  *addr++ = (unsigned char)data ; data = data >> 8 ;
  *addr   = (unsigned char)data ;
}

/* Return 1 if host CPU is Little Endian */
int N::Forth::IsHostLittleEndian(void)
{
  union un              {
    unsigned int  us    ;
    unsigned char uc[4] ;
  }                     ;
  union un ux           ;
  ux.us = 1             ;
  return (int)ux.uc[0]  ;
}
