/* xlmath - xlisp built-in arithmetic functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"
#include <math.h>

#ifndef COMPLX
/* When COMPLX is defined, the math functions in xlmath2.c are used */

/* forward local declarations */
LOCAL LVAL unary _((int fcn));
LOCAL LVAL binary _((int fcn));
LOCAL LVAL predicate _((int fcn));
LOCAL LVAL compare _((int fcn));
LOCAL VOID checkizero _((FIXTYPE iarg));
LOCAL VOID checkfzero _((FLOTYPE farg));
LOCAL VOID checkfneg _((FLOTYPE farg));
LOCAL VOID badiop _((void));
LOCAL VOID badfop _((void));

/* binary functions */
/* TAA fix allowing (+) */
LVAL xadd()    { return (moreargs()?binary('+'):cvfixnum((FIXTYPE)0)); }
LVAL xsub()    { return (binary('-')); } /* - */
/* TAA fix allowing (*) */
LVAL xmul()    { return (moreargs()?binary('*'):cvfixnum((FIXTYPE)1)); }
LVAL xdiv()    { return (binary('/')); } /* / */
LVAL xrem()    { return (binary('%')); } /* rem */
LVAL xmin()    { return (binary('m')); } /* min */
LVAL xmax()    { return (binary('M')); } /* max */
LVAL xexpt()   { return (binary('E')); } /* expt */
/* TAA fix allowing (logand) */
LVAL xlogand() { return (moreargs()?binary('&'):cvfixnum((FIXTYPE)-1)); }
/* TAA fix allowing (logior) */
LVAL xlogior() { return (moreargs()?binary('|'):cvfixnum((FIXTYPE)0)); }
/* TAA fix allowing (logxor) */
LVAL xlogxor() { return (moreargs()?binary('^'):cvfixnum((FIXTYPE)0)); }

/* xgcd - greatest common divisor */
LVAL xgcd()
{
    FIXTYPE m,n,r;
    LVAL arg;

    if (!moreargs())                    /* check for identity case */
        return (cvfixnum((FIXTYPE)0));
    arg = xlgafixnum();
    n = getfixnum(arg);
    if (n < (FIXTYPE)0) n = -n;         /* absolute value */
    while (moreargs()) {
        arg = xlgafixnum();
        m = getfixnum(arg);
        if (m < (FIXTYPE)0) m = -m;     /* absolute value */
        for (;;) {                      /* euclid's algorithm */
            r = m % n;
            if (r == (FIXTYPE)0)
                break;
            m = n;
            n = r;
        }
    }
    return (cvfixnum(n));
}

/* binary - handle binary operations */
LOCAL LVAL binary(fcn)
  int fcn;
{
    FIXTYPE ival,iarg;
    FLOTYPE fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
        ival = getfixnum(arg);
        mode = 'I';
    }
    else if (floatp(arg)) {
        fval = getflonum(arg);
        mode = 'F';
    }
    else
        xlbadtype(arg);

    /* treat a single argument as a special case */
    if (!moreargs()) {
        switch (fcn) {
        case '-':
            switch (mode) {
            case 'I':
                ival = -ival;
                break;
            case 'F':
                fval = -fval;
                break;
            }
            break;
        case '/':
            switch (mode) {
            case 'I':
                checkizero(ival);
                ival = 1 / ival;
                break;
            case 'F':
                checkfzero(fval);
                fval = 1.0 / fval;
                break;
            }
        }
    }

    /* handle each remaining argument */
    while (moreargs()) {

        /* get the next argument */
        arg = xlgetarg();

        /* check its type */
        if (fixp(arg)) {
            switch (mode) {
            case 'I':
                iarg = getfixnum(arg);
                break;
            case 'F':
                farg = (FLOTYPE)getfixnum(arg);
                break;
            }
        }
        else if (floatp(arg)) {
            switch (mode) {
            case 'I':
                fval = (FLOTYPE)ival;
                farg = getflonum(arg);
                mode = 'F';
                break;
            case 'F':
                farg = getflonum(arg);
                break;
            }
        }
        else
            xlbadtype(arg);

        /* accumulate the result value */
        switch (mode) {
        case 'I':
            switch (fcn) {
            case '+':   ival += iarg; break;
            case '-':   ival -= iarg; break;
            case '*':   ival *= iarg; break;
            case '/':   checkizero(iarg); ival /= iarg; break;
            case '%':   checkizero(iarg); ival %= iarg; break;
            case 'M':   if (iarg > ival) ival = iarg; break;
            case 'm':   if (iarg < ival) ival = iarg; break;
            case '&':   ival &= iarg; break;
            case '|':   ival |= iarg; break;
            case '^':   ival ^= iarg; break;
            default:    badiop();
            }
            break;
        case 'F':
            switch (fcn) {
            case '+':   fval += farg; break;
            case '-':   fval -= farg; break;
            case '*':   fval *= farg; break;
            case '/':   checkfzero(farg); fval /= farg; break;
            case 'M':   if (farg > fval) fval = farg; break;
            case 'm':   if (farg < fval) fval = farg; break;
            case 'E':   fval = pow(fval,farg); break;
            default:    badfop();
            }
            break;
        }
    }

    /* return the result */
    if (mode=='I')
        return (cvfixnum(ival));
    else
        return (cvflonum(fval));
}

/* checkizero - check for integer division by zero */
LOCAL VOID checkizero(iarg)
  FIXTYPE iarg;
{
    if (iarg == 0)
	xlfail("division by zero");
}

/* checkfzero - check for floating point division by zero */
LOCAL VOID checkfzero(farg)
  FLOTYPE farg;
{
    if (farg == 0.0)
        xlfail("division by zero");
}

/* checkfneg - check for square root of a negative number */
LOCAL VOID checkfneg(farg)
  FLOTYPE farg;
{
    if (farg < 0.0)
        xlfail("square root of a negative number");
}

/* unary functions */
LVAL xlognot() { return (unary('~')); } /* lognot */
LVAL xabs()    { return (unary('A')); } /* abs */
LVAL xadd1()   { return (unary('+')); } /* 1+ */
LVAL xsub1()   { return (unary('-')); } /* 1- */
LVAL xsin()    { return (unary('S')); } /* sin */
LVAL xcos()    { return (unary('C')); } /* cos */
LVAL xtan()    { return (unary('T')); } /* tan */
LVAL xasin()   { return (unary('s')); } /* asin */
LVAL xacos()   { return (unary('c')); } /* acos */
LVAL xatan()   { return (unary('t')); } /* atan */
LVAL xexp()    { return (unary('E')); } /* exp */
LVAL xsqrt()   { return (unary('R')); } /* sqrt */
LVAL xfix()    { return (unary('I')); } /* truncate */
LVAL xfloat()  { return (unary('F')); } /* float */

/* unary - handle unary operations */
LOCAL LVAL unary(fcn)
  int fcn;
{
    FLOTYPE fval;
    FIXTYPE ival;
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check its type */
    if (fixp(arg)) {
        ival = getfixnum(arg);
        switch (fcn) {
        case '~':       ival = ~ival; break;
        case 'A':       ival = (ival < 0 ? -ival : ival); break;
        case '+':       ival++; break;
        case '-':       ival--; break;
        case 'I':       break;
        case 'F':       return (cvflonum((FLOTYPE)ival));
        default:        badiop();
        }
        return (cvfixnum(ival));
    }
    else if (floatp(arg)) {
        fval = getflonum(arg);
        switch (fcn) {
        case 'A':       fval = (fval < 0.0 ? -fval : fval); break;
        case '+':       fval += 1.0; break;
        case '-':       fval -= 1.0; break;
        case 'S':       fval = sin(fval); break;
        case 'C':       fval = cos(fval); break;
        case 'T':       fval = tan(fval); break;
        case 's':       fval = asin(fval); break;
        case 'c':       fval = acos(fval); break;
        case 't':       fval = atan(fval); break;
        case 'E':       fval = exp(fval); break;
        case 'R':       checkfneg(fval); fval = sqrt(fval); break;
        case 'I':       return (cvfixnum((FIXTYPE)fval));
        case 'F':       break;
        default:        badfop();
        }
        return (cvflonum(fval));
    }
    else {
        xlbadtype(arg);
        return (NIL);   /* fake out compiler warning */
    }
}

/* unary predicates */
LVAL xminusp() { return (predicate('-')); } /* minusp */
LVAL xzerop()  { return (predicate('Z')); } /* zerop */
LVAL xplusp()  { return (predicate('+')); } /* plusp */
LVAL xevenp()  { return (predicate('E')); } /* evenp */
LVAL xoddp()   { return (predicate('O')); } /* oddp */

/* predicate - handle a predicate function */
LOCAL LVAL predicate(fcn)
  int fcn;
{
    FLOTYPE fval;
    FIXTYPE ival;
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check the argument type */
    if (fixp(arg)) {
        ival = getfixnum(arg);
        switch (fcn) {
        case '-':       ival = (ival < 0); break;
        case 'Z':       ival = (ival == 0); break;
        case '+':       ival = (ival > 0); break;
        case 'E':       ival = ((ival & 1) == 0); break;
        case 'O':       ival = ((ival & 1) != 0); break;
        default:        badiop();
        }
    }
    else if (floatp(arg)) {
        fval = getflonum(arg);
        switch (fcn) {
        case '-':       ival = (fval < 0); break;
        case 'Z':       ival = (fval == 0); break;
        case '+':       ival = (fval > 0); break;
        default:        badfop();
        }
    }
    else
        xlbadtype(arg);

    /* return the result value */
    return (ival ? s_true : NIL);
}

/* comparison functions */
LVAL xlss() { return (compare('<')); } /* < */
LVAL xleq() { return (compare('L')); } /* <= */
LVAL xequ() { return (compare('=')); } /* = */
LVAL xneq() { return (compare('#')); } /* /= */
LVAL xgeq() { return (compare('G')); } /* >= */
LVAL xgtr() { return (compare('>')); } /* > */

/* compare - common compare function */
LOCAL LVAL compare(fcn)
  int fcn;
{
    FIXTYPE icmp,ival,iarg;
    FLOTYPE fcmp,fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
        ival = getfixnum(arg);
        mode = 'I';
    }
    else if (floatp(arg)) {
        fval = getflonum(arg);
        mode = 'F';
    }
    else
        xlbadtype(arg);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ival = iarg, fval = farg) {

        /* get the next argument */
        arg = xlgetarg();

        /* check its type */
        if (fixp(arg)) {
            switch (mode) {
            case 'I':
                iarg = getfixnum(arg);
                break;
            case 'F':
                farg = (FLOTYPE)getfixnum(arg);
                break;
            }
        }
        else if (floatp(arg)) {
            switch (mode) {
            case 'I':
                fval = (FLOTYPE)ival;
                farg = getflonum(arg);
                mode = 'F';
                break;
            case 'F':
                farg = getflonum(arg);
                break;
            }
        }
        else
            xlbadtype(arg);

        /* compute result of the compare */
        switch (mode) {
        case 'I':
            icmp = ival - iarg;
            switch (fcn) {
            case '<':   icmp = (icmp < 0); break;
            case 'L':   icmp = (icmp <= 0); break;
            case '=':   icmp = (icmp == 0); break;
            case '#':   icmp = (icmp != 0); break;
            case 'G':   icmp = (icmp >= 0); break;
            case '>':   icmp = (icmp > 0); break;
            }
            break;
        case 'F':
            fcmp = fval - farg;
            switch (fcn) {
            case '<':   icmp = (fcmp < 0.0); break;
            case 'L':   icmp = (fcmp <= 0.0); break;
            case '=':   icmp = (fcmp == 0.0); break;
            case '#':   icmp = (fcmp != 0.0); break;
            case 'G':   icmp = (fcmp >= 0.0); break;
            case '>':   icmp = (fcmp > 0.0); break;
            }
            break;
        }
    }

    /* return the result */
    return (icmp ? s_true : NIL);
}

/* badiop - bad integer operation */
LOCAL VOID badiop()
{
    xlfail("bad integer operation");
}

/* badfop - bad floating point operation */
LOCAL VOID badfop()
{
    xlfail("bad floating point operation");
}
#endif
