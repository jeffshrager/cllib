/* sunstuff.c - suntools interface routines for xlisp                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

# include <signal.h>
# include "xlisp.h"
# include "xlstat.h"
# include "version.h"
# include <sunwindow/notify.h>

/* externals */
extern FILEP tfp;

static time_t time_stamp;

LOCAL VOID intercatch _((int)), fpecatch _((int));
  
osinit(name)
  char *name;
{
  time_stamp = time((time_t *) NULL);
  
  if (signal(SIGINT, SIG_IGN) != SIG_IGN)
    signal(SIGINT, intercatch);
  signal(SIGFPE, fpecatch);
  printf("%s\n", name);
  printf("XLISP-STAT Release %d.%d.%d%s.\n",
	 XLS_MAJOR_RELEASE, XLS_MINOR_RELEASE, XLS_SUBMINOR_RELEASE,
	 XLS_RELEASE_STATUS);
  printf("Copyright (c) 1989, by Luke Tierney.\n\n");	 
}

osfinish() {}
osreset()
{
  StGWResetBuffer();
  DialogReset();
  MouseReset();
}

VOID xoserror(msg)
     char *msg;
{
    char line[100],*p;
    sprintf(line,"error: %s\n",msg);
    for (p = line; *p != '\0'; ++p)
	ostputc(*p);
}

FILE *osaopen(name,mode)
  char *name,*mode;
{
    return (fopen(name,mode));
}

FILE *osbopen(name,mode)
  char *name,*mode;
{
    char nmode[4];
    strcpy(nmode,mode); strcat(nmode,"b");
    return (fopen(name,nmode));
}

int osclose(fp)
  FILE *fp;
{
    return (fclose(fp));
}

int osagetc(fp)
  FILE *fp;
{
    return (getc(fp));
}

int osbgetc(fp)
  FILE *fp;
{
    return (getc(fp));
}

int osaputc(ch,fp)
  int ch; FILE *fp;
{
    return (putc(ch,fp));
}

int osbputc(ch,fp)
  int ch; FILE *fp;
{
    return (putc(ch,fp));
}

int jump_to_top = FALSE;

int ostgetc()
{
  char ch;

  if (jump_to_top) {
    notify_do_dispatch();
    jump_to_top = FALSE;
    xlsigint();
  }

  ch = getchar();
  if (tfp != CLOSED)
    OSPUTC(ch,tfp);
  return(ch);
}

int ostputc(ch)
  int ch;
{
    putchar(ch);
    if (tfp != CLOSED)
	OSPUTC(ch,tfp);
    return (1);
}

osflush()
{
}

oscheck()
{
  if (jump_to_top) {
    notify_do_dispatch();
    jump_to_top = FALSE;
    xlsigint();
  }
}

ossymbols()
{
  statsymbols();
}

osfinit()
{
  statfinit();
}

LOCAL VOID intercatch(arg)
     int arg;
{
  signal(SIGINT, intercatch);
  notify_no_dispatch();
  jump_to_top = TRUE;
}

LOCAL VOID fpecatch(arg)
     int arg;
{
  signal(SIGFPE, fpecatch);
  xlfail("floating point error");
}

max(x, y)
     int x, y;
{
  return((x > y) ? x : y);
}

min(x, y)
     int x, y;
{
  return((x < y) ? x : y);
}

set_gc_cursor() {}

SysBeep(n)
     int n;
{
  n = n % 10;
  do {
    printf("\007");
  } while (n-- > 0);
  fflush(stdout);
}

#ifdef SUN3X
int (*signal(sig, fn))()
     int sig, (*fn)();
{
  return((int (*)()) notify_set_signal_func(sig, fn, sig, NOTIFY_ASYNC));
}
#else
void (*signal(sig, fn))()
     int sig, (*fn)();
{
  return((void (*)()) notify_set_signal_func(sig, fn, sig, NOTIFY_ASYNC));
}
#endif /*SUN3X*/

#ifndef CLK_TCK
#ifndef HZ
#define HZ 60
#endif
#define CLK_TCK HZ
#endif /* CLK_TCK */

#ifdef NODIFFTIME
#ifndef difftime
#define difftime(x,y) (((unsigned long) (x)) - ((unsigned long) (y)))
#endif
#endif

unsigned long ticks_per_second() { return((unsigned long)(CLK_TCK)); }

unsigned long run_tick_count()
{
  struct tms tm;

  times(&tm);
  
  return((unsigned long) tm.tms_utime + (unsigned long) tm.tms_stime);
}

unsigned long real_tick_count()
{
  return((unsigned long) (CLK_TCK * difftime(time((time_t *) 0), time_stamp)));
}

unsigned long system_tick_count()
{
  return((unsigned long) time((time_t *) 0));
}

extern char *getenv();

VOID get_directory(s)
     char *s;
{
  char *libdir;
  int n;

  libdir = getenv("XLISPLIB");
  if (libdir == NULL) libdir = "";
  strcpy(s, libdir);
  n = strlen(s);
  if (n > 0 && s[n - 1] != '/')
    strcat(s, "/");
}

int renamebackup(name) char *name; { return(TRUE); }

/* xgetwd - builtin function GET-WORKING-DIRECTORY */
LVAL xgetwd()
{
  xllastarg();
  if (! getcwd(buf, FNAMEMAX))
    return NIL;
  else
    return cvstring(buf);
}

/* xsetwd - builtin function SET-WORKING-DIRECTORY */
LVAL xsetwd()
{
  char *dir = getstring(xlgastring());
  xllastarg();
  if (chdir(dir))
    return NIL;
  else
    return s_true;
}

#ifdef USEMATHERR
int matherr P1C(struct exception *, e)
{
  return 1;
}
#endif

#ifdef NOMEMMOVE
VOID memmove P3C(char *, s1, char *, s2, int, n)
{
  if (s1 < s2)
    while (n--)
      *s1++ = *s2++;
  else {
    s1 += (n-1);
    s2 += (n-1);
    while (n--)
      *s1-- = *s2--;
  }
}
#endif /* NOMEMMOVE */

/* internal version of directory function */
/***** probably needs to be protected by check that dirent is available */
/***** need to drop non-files, i.e. directories, using stat() call */
#include <dirent.h>

LVAL xdirectory()
{
  LVAL name, val;
  DIR *dir;

  name = xlgastring();
  xllastarg();

  xlsave1(val);
  val = NIL;
  if ((dir = opendir(getstring(name)))) {
    struct dirent *dentry;
    while ((dentry = readdir(dir)))
      val = cons(cvstring(dentry->d_name), val);
    /* protect this with an unwind-protect? */
    closedir(dir);
  }
  xlpop();

  return val;
}
