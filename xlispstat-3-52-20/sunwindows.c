/* windows - SunView window functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

/* external variables */
extern LVAL s_true, sk_update, sk_close, sk_activate, s_title, s_size,
 s_location;
extern char buf[];

/* external functions */
extern LVAL list3(), xmsend(), integer_list_2(), slot_value(), list2();

/**************************************************************************/
/**                                                                      **/
/**                           Utility Functions                          **/
/**                                                                      **/
/**************************************************************************/

errset_applysubr(subr, args)
     int (*subr)();
     LVAL args;
{
  CONTEXT cntxt;
  int error;

  /* establish an execution context */
  xlbegin(&cntxt,CF_ERROR,s_true);

  /* check for error */
  if (XL_SETJMP(cntxt.c_jmpbuf)) error = TRUE;
  else {

    /* call the function */
    xsapplysubr(subr, args);
    error = FALSE;
  }
  xlend(&cntxt);
  return(error);
}

errset_send(object, selector)
     LVAL object, selector;
{
  LVAL args;

  xlsave1(args);
  args = list2(object, selector);
  errset_applysubr(xmsend, args);
  xlpop();
}

/**************************************************************************/
/**                                                                      **/
/**                        Window Data Functions                         **/
/**                                                                      **/
/**************************************************************************/

LVAL get_window_object(w)
	WindowPtr w;
{
  WindowData data;
  
  data = (WindowData) get_window_data(w);
  if (data == NULL || ! objectp((LVAL) data->object)) return(NIL);
  else return((LVAL) data->object);
}

set_window_object(w, object)
	WindowPtr w;
	LVAL object;
{
  WindowData data;
  
  data = (WindowData) get_window_data(w);
  if (data == NULL) return;
  else data->object = (char *) object;
}

sun_close_action(w)
     IVIEW_WINDOW w;
{
  LVAL object = get_window_object(w);
  
  if (! objectp(object)) return;
  errset_send(object, sk_close);
}

/***********************************************************************/
/**                                                                   **/
/**                  General Window Methods Functions                 **/
/**                                                                   **/
/***********************************************************************/

StShowWindow(w)
     IVIEW_WINDOW w; 
{
  window_set(w, WIN_SHOW, TRUE, 0);
}

StHideWindow(w)
     IVIEW_WINDOW w; 
{
  window_set(w, WIN_SHOW, FALSE, 0);
}

StWSetTitle(w, title)
     IVIEW_WINDOW w;
     char *title;
{
  window_set(w, FRAME_LABEL, title, 0);
}

/**************************************************************************/
/**                                                                      **/
/**                         Screen Info Functions                        **/
/**                                                                      **/
/**************************************************************************/

StScreenHasColor() { return(FALSE); }

/**************************************************************************/
/**                                                                      **/
/**                    Protected Callback Functions                      **/
/**                                                                      **/
/**************************************************************************/

errset_window_call(subr, w)
     int (*subr)();
     IVIEW_WINDOW w;
{
  CONTEXT cntxt;
  int error;

  /* establish an execution context */
  xlbegin(&cntxt,CF_ERROR,s_true);

  /* check for error */
  if (XL_SETJMP(cntxt.c_jmpbuf)) error = TRUE;
  else {

    /* call the function */
    (*subr)(w);
    error = FALSE;
  }
  xlend(&cntxt);
  return(error);
}

SunStGWObDoMouse(object, x, y, type, mods)
     LVAL object;
     int x, y;
     MouseEventType type;
     MouseClickModifier mods;
{
  CONTEXT cntxt;

  /* establish an execution context */
  xlbegin(&cntxt,CF_ERROR,s_true);

  /* check for error */
  if (! XL_SETJMP(cntxt.c_jmpbuf)) {

    /* call the function */
    StGWObDoMouse(object, x, y, type, mods);
  }
  xlend(&cntxt);
}  

sun_button_down_action(action, w, x, y)
     int (*action)(), x, y;
     IVIEW_WINDOW w;
{
  CONTEXT cntxt;
  int error;

  /* establish an execution context */
  xlbegin(&cntxt,CF_ERROR,s_true);

  /* check for error */
  if (XL_SETJMP(cntxt.c_jmpbuf)) error = TRUE;
  else {

    /* call the function */
    if (action != NULL) (*action)(w, x, y);
    error = FALSE;
  }
  xlend(&cntxt);
  return(error);
}
