/* sungraphwin - SunView interface for XLISP-STAT                      */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

#include <fcntl.h>

extern Notify_error notify_dispatch();
extern int sun_close_action();

extern int StGWObResize(), StGWObRedraw(), StGWObDoIdle();
extern char *StGWObWinInfo();
extern char *realloc();

/**************************************************************************/
/**                                                                      **/
/**                        General Definitions                           **/
/**                                                                      **/
/**************************************************************************/

# define MemWidth 1200   /* Should be computed from the screen size */
# define MemHeight 1000

typedef struct {
  short h, v;
} Point;

/**************************************************************************/
/**                                                                      **/
/**                            Global Variables                          **/
/**                                                                      **/
/**************************************************************************/

static struct pixrect *WorkPort;
Frame BaseFrame;

static int buffering = FALSE;
static int bufflevel = 0;

static Cursor arrow_curs, finger_curs, hand_curs, brush_curs;

static short arrow_data[] = {
#include "arrow.curs"
};
mpr_static(arrow_pix, 16, 16, 1, arrow_data);

static short finger_data[] = {
#include "finger.curs"
};
mpr_static(finger_pix, 16, 16, 1, finger_data);

static short hand_data[] = {
#include "hand.curs"
};
mpr_static(hand_pix, 16, 16, 1, hand_data);

static short brush_data[] = {
#include "brush.curs"
};
mpr_static(brush_pix, 16, 16, 1, brush_data);

char *IViewWindowWinInfo(w)
     IVIEW_WINDOW w;
{
  return((! IVIEW_WINDOW_NULL(w)) ? (char *) GetWRefCon(w) : NULL);
}
/**************************************************************************/
/**                                                                      **/
/**                       Initialization Functions                       **/
/**                                                                      **/
/**************************************************************************/

StInitGraphics()
{
  if (BaseFrame == NULL) { /* should check for errors here */
    BaseFrame = window_create(NULL, FRAME, 0);
    if (BaseFrame == NULL) return;
    (void) notify_do_dispatch();
  }
  WorkPort = mem_create(MemWidth, MemHeight, 1);
  if (WorkPort == NULL) {
    printf("Buffer allocation failed");
    exit(1);
  }
  MakeSymbols();
  MakeCursors();
}

StHasWindows() { return(BaseFrame != NULL); }

/**************************************************************************/
/**                                                                      **/
/**                       Window Creation Functions                      **/
/**                                                                      **/
/**************************************************************************/

static StGWWinInfo *WindowIViewWindow(w)
	IVIEW_WINDOW w;
{
  return((StGWWinInfo *) GetWRefCon(w));
}

static close_action(w)
     IVIEW_WINDOW w;
{
  StGWRemove(IViewWindowWinInfo(w));
}

static resize_content(wind, width, height)
     Canvas wind;
     int width, height;
{
  StGWWinInfo *gwinfo;

  gwinfo = (StGWWinInfo *) GetWRefCon(window_get(wind, WIN_OWNER));
  if (gwinfo != NULL) {
    gwinfo->initialized = TRUE;
    gwinfo->canvasWidth = width;
    gwinfo->canvasHeight = height;
    errset_window_call(StGWObResize, gwinfo->Object);
  }
}

Event *mouse_event = NULL;
IVIEW_WINDOW mouse_window = NULL;

MouseReset()
{
  mouse_event = NULL;
  mouse_window = NULL;
  StSunReleaseButton();
}

static mouse_action(wind, event, arg)
          IVIEW_WINDOW wind;
          Event *event;
          caddr_t arg;
{
  MouseEventType type;
  MouseClickModifier mods;
  StGWWinInfo *gwinfo;

  gwinfo = (StGWWinInfo *) GetWRefCon(window_get(wind, WIN_OWNER));
 
  mouse_event = event;
  mouse_window = wind;

  if (event_is_ascii(event)) {
    StGWObDoKey(gwinfo->Object, event_id(event), 
		event_shift_is_down(event), event_ctrl_is_down(event));
  }
  else if (event_id(event) == MS_RIGHT) {
    canvas_window_event(gwinfo->content_canvas, event);
    sun_show_menu(gwinfo->window, event);
  }
  else {
    if (event_is_down(event) && event_id(event) == MS_LEFT) {
      type = MouseClick;
      mods = NoModifiers;
    }
    else if (event_is_down(event) && event_id(event) == MS_MIDDLE) {
      type = MouseClick;
      mods = ExtendModifier;
    }
    else {
      type = MouseMove;
      mods = NoModifiers;
    }

    if (gwinfo != NULL) {
      if (type == MouseClick) StSunPressButton();
      SunStGWObDoMouse(gwinfo->Object, event_x(event), event_y(event), 
		       type, mods);
      if (type == MouseClick) StSunReleaseButton();
    }
  }
  MouseReset();
}

static repaint_content(wind, pw, repaint_area)
          Canvas wind;
          Pixwin *pw;
          Rectlist repaint_area;
{
  StGWWinInfo *gwinfo;

  gwinfo= (StGWWinInfo *) GetWRefCon(window_get(wind, WIN_OWNER));
  if (gwinfo != NULL) errset_window_call(StGWObRedraw, gwinfo->Object);
}

static idle_proc(wind, which)
     Window wind;
     int which;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(wind);

  if (gwinfo != NULL) errset_window_call(StGWObDoIdle, gwinfo->Object);
}

StGWWinInfoSize() { return(sizeof(StGWWinInfo)); }
     
StGWInitWinInfo(object)
     char *object;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo(object);

  gwinfo->Object = (long) object;
  gwinfo->canvasWidth = 0;
  gwinfo->canvasHeight = 0;
  gwinfo->hasHscroll = FALSE;
  gwinfo->hasVscroll = FALSE;
  gwinfo->view_h = 0;
  gwinfo->view_v = 0;
  gwinfo->h_scroll_inc[0] = 1; gwinfo->h_scroll_inc[1] = 50;
  gwinfo->v_scroll_inc[0] = 1; gwinfo->v_scroll_inc[1] = 50;
  gwinfo->lineType = 0;
  gwinfo->drawMode = 0;
  gwinfo->backColor = 0;
  gwinfo->drawColor = 1;
  gwinfo->lineWidth = 1;
  gwinfo->window = NULL;
  gwinfo->idleOn = FALSE;
  gwinfo->use_color = FALSE;
  gwinfo->cursor = 0;
  gwinfo->RefCon = NULL;
  gwinfo->initialized = FALSE;
}

IVIEW_WINDOW IViewWindowNew(object, is_GW)
     char *object;
     int is_GW;
{
  char *title;
  int left, top, width, height, goAway;
  StGWWinInfo *gwinfo;
  Frame wind;
  Cursor cursor;
	
  StGWGetAllocInfo(object, &title, &left, &top, &width, &height, &goAway);
  if (title == NULL || strlen(title) <= 0) title = "IView";
	
  wind = window_create(BaseFrame, FRAME, WIN_SHOW, FALSE, 0);
  if (wind == NULL) xlfail("allocation Failed");

  window_set(wind, FRAME_NO_CONFIRM, TRUE,
                   FRAME_LABEL, title,
                   FRAME_SHOW_LABEL, TRUE,
                   0);
  gwinfo = (StGWWinInfo *) StGWObWinInfo(object);

  gwinfo->window = wind;
  gwinfo->Object = (long) object;
  if (! gwinfo->hasHscroll) gwinfo->canvasWidth = width;
  if (! gwinfo->hasVscroll) gwinfo->canvasHeight = height;
  gwinfo->initialized = FALSE;

  SetWRefCon(wind, gwinfo);
  window_set(wind, FRAME_DONE_PROC, sun_close_action, 0);
  
  gwinfo->content_canvas = 
      window_create(wind, CANVAS, 
		          CANVAS_RETAINED, FALSE,
		          CANVAS_AUTO_CLEAR, FALSE,
                          WIN_HEIGHT, height,
                          WIN_WIDTH, width,
                          0);
  if (gwinfo->content_canvas == NULL) xlfail("allocation Failed");

  cursor = window_get(gwinfo->content_canvas, WIN_CURSOR);
  cursor_set(cursor, CURSOR_OP, PIX_SRC ^ PIX_DST, 0);
  window_set(gwinfo->content_canvas, WIN_CURSOR, cursor, 0);
  		 
  window_set(gwinfo->content_canvas, CANVAS_RESIZE_PROC, resize_content, 0);
  window_set(gwinfo->content_canvas, CANVAS_REPAINT_PROC, repaint_content, 0);
  window_set(gwinfo->content_canvas, WIN_EVENT_PROC, mouse_action, 0);
  window_set(gwinfo->content_canvas, 
	     WIN_CONSUME_KBD_EVENT, WIN_ASCII_EVENTS, 0);
  window_fit(wind);

  StGWEraseRect(gwinfo, 0, 0, width, height);

  if (is_GW) set_iview_window_address(wind, object);
  else set_iview_address(wind, object);

  return(wind);
}

StGWSetFreeMem(gwinfo, FreeMem)
     StGWWinInfo *gwinfo;
     int (*FreeMem)();
{
  if (gwinfo == NULL) return;
  else gwinfo->FreeMem = FreeMem;
}

StGWIdleOn(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(FALSE);
  else return(gwinfo->idleOn);
}

#define ITIMER_NULL ((struct itimerval *) 0)

StGWSetIdleOn(gwinfo, on)
     StGWWinInfo *gwinfo;
     int on;
{
  Window w;

  if (gwinfo == NULL || (w = gwinfo->window) == NULL) return;
  gwinfo->idleOn = on;
  if (on) 
    notify_set_itimer_func(w, idle_proc, ITIMER_REAL,
			   &NOTIFY_POLLING_ITIMER, ITIMER_NULL);
  else
    notify_set_itimer_func(w, NULL, ITIMER_REAL,
			   &NOTIFY_POLLING_ITIMER, ITIMER_NULL);
}

/**************************************************************************/
/**                                                                      **/
/**                      Window Management Functions                     **/
/**                                                                      **/
/**************************************************************************/

StGWShowWindow(gwinfo)
     StGWWinInfo *gwinfo;
{
  Window w;

  if (gwinfo == NULL || (w = gwinfo->window) == NULL) return;
  window_set(w, WIN_SHOW, TRUE, 0);
  if (! gwinfo->initialized) StGWInitialDraw(gwinfo);
}

StGWRemove(gwinfo)
     StGWWinInfo *gwinfo;
{
  Window w;
  
  if (gwinfo == NULL || (w = gwinfo->window) == NULL) return;
  if (IViewInternalIsLinked(w)) IViewUnlinkWindow(w);
  StGWObDoClobber(gwinfo->Object);
  if (gwinfo->FreeMem != NULL) (*gwinfo->FreeMem)(w);

  gwinfo->window = NULL;
  gwinfo->content_canvas = NULL;
  window_destroy(w);
}

static int button_down;

static button_check(w, event, arg)
          IVIEW_WINDOW w;
          Event *event;
          caddr_t arg;
{
  StGWWinInfo *gwinfo;

  gwinfo = (StGWWinInfo *) GetWRefCon(w);
  if (gwinfo == NULL) return;

  if (event_is_up(event)) button_down = FALSE;
  gwinfo->mouse_x = event_x(event); 
  gwinfo->mouse_y = event_y(event);
}

static int pointer_button_down = FALSE;
StSunPressButton()   { pointer_button_down = TRUE; }
StSunReleaseButton() { pointer_button_down = FALSE; }
StSunButtonIsDown()  { return(pointer_button_down); }

StGWWhileButtonDown(gwinfo, action, motionOnly)
     StGWWinInfo *gwinfo;
     int (*action)(), motionOnly;
{
  Event myEvent, *event = &myEvent;
  Window w;
  int error;

  if (gwinfo == NULL || (w = gwinfo->window) == NULL || ! StSunButtonIsDown())
    return;

  if (motionOnly) {
    window_read_event(gwinfo->content_canvas, event);
    error = FALSE;
    while (! error && ! event_is_up(event)) {
      gwinfo->mouse_x = event_x(event); 
      gwinfo->mouse_y = event_y(event);
      error = sun_button_down_action(action, w, 
				     event_x(event), event_y(event));
      window_read_event(gwinfo->content_canvas, event);
    }
  }
  else {
    int fd = (int) window_get(gwinfo->content_canvas, WIN_FD);
    int oldflags = fcntl(fd, F_GETFL, FNDELAY);

    fcntl(fd, F_SETFL, oldflags | FNDELAY);
    error = FALSE;
    while (! error && input_readevent(fd, event))
      error = sun_button_down_action(action, w, 
				     gwinfo->mouse_x, gwinfo->mouse_y);
    error = FALSE;
    while (! error && ! event_is_up(event)) {
      gwinfo->mouse_x = event_x(event); 
      gwinfo->mouse_y = event_y(event);
      error = sun_button_down_action(action, w, 
				     gwinfo->mouse_x, gwinfo->mouse_y);
      while (! error && input_readevent(fd, event))
	error = sun_button_down_action(action, w, 
				       gwinfo->mouse_x, gwinfo->mouse_y);
    }
    fcntl(fd, F_SETFL, oldflags);
  }
}

StWSetLocation(w, left, top, frame)
     Window w;
     int left, top, frame;
{
  Rect r, *old_r_p;

  if (IVIEW_WINDOW_NULL(w)) return;
  old_r_p = (Rect *) window_get(w, WIN_RECT);
  r = *old_r_p;
  r.r_left = left; r.r_top = top;
  window_set(w, WIN_RECT, &r, 0);
}

StWGetLocation(w, left, top, frame)
     Window w;
     int *left, *top, frame;
{
  Rect *r_p;

  if (! IVIEW_WINDOW_NULL(w)) {
    r_p = (Rect *) window_get(w, WIN_RECT);
    if (left != NULL) *left = r_p->r_left;
    if (top != NULL) *top = r_p->r_top;
  }
  else {
    if (left != NULL) *left = 0;
    if (top != NULL) *top = 0;
  }
}

StWSetSize(w, width, height, frame)
     Window w;
     int width, height;
{
  Rect r, *old_r_p;
  
  if (IVIEW_WINDOW_NULL(w)) return;
  old_r_p = (Rect *) window_get(w, WIN_RECT);
  r = *old_r_p;
  r.r_width = width; r.r_height = height;
  window_set(w, WIN_RECT, &r, 0);
}

StWGetSize(w, width, height, frame)
     Window w;
     int *width, *height;
{
  Rect *r_p;

  if (! IVIEW_WINDOW_NULL(w)) {
    r_p = (Rect *) window_get(w, WIN_RECT);
    if (width != NULL) *width = r_p->r_width;
    if (height != NULL) *height = r_p->r_height;
  }
  else {
    if (width != NULL) *width = 1;
    if (height != NULL) *height = 1;
  }    
}

StGWSetSize(gwinfo, width, height, frame)
	StGWWinInfo *gwinfo;
	int width, height;
{
  Window w;
  if (gwinfo == NULL || (w = gwinfo->window) == NULL) return;
  else StWSetSize(w, width, height, frame);
}

Canvas StGWCanvas(w)
     Window w;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);
  return (gwinfo->content_canvas);
}

/**************************************************************************/
/**                                                                      **/
/**             Window State Access and Mutation Functions               **/
/**                                                                      **/
/**************************************************************************/

static get_state(gwinfo, which)
     StGWWinInfo *gwinfo;
     int which;
{
  if (gwinfo == NULL) return(0);
  switch (which) {
  case 'W': return(gwinfo->canvasWidth);
  case 'H': return(gwinfo->canvasHeight);
  case 'L': return(gwinfo->lineType);
  case 'M': return(gwinfo->drawMode);
  case 'D': return(gwinfo->drawColor);
  case 'B': return(gwinfo->backColor);
  case 'C': return(gwinfo->use_color);
  }
}

StGWCanvasWidth(gwinfo)
     StGWWinInfo *gwinfo;
{
  return (get_state(gwinfo, 'W'));
}

StGWCanvasHeight(gwinfo)
     StGWWinInfo *gwinfo;
{
  return (get_state(gwinfo, 'H'));
}

StGWLineType(gwinfo)
     StGWWinInfo *gwinfo;
{
  return (get_state(gwinfo, 'L'));
}

StGWDrawMode(gwinfo)
     StGWWinInfo *gwinfo;
{
  return (get_state(gwinfo, 'M'));
}

ColorCode StGWDrawColor(gwinfo)
     StGWWinInfo *gwinfo;
{
  return ((ColorCode) get_state(gwinfo, 'D'));
}

ColorCode StGWBackColor(gwinfo)
     StGWWinInfo *gwinfo;
{
  return ((ColorCode) get_state(gwinfo, 'B'));
}

StGWUseColor(gwinfo)
     StGWWinInfo *gwinfo;
{
  return (get_state(gwinfo, 'C'));
}

StGWGetLineWidth(gwinfo, width)
     StGWWinInfo *gwinfo;
     int *width;
{
  if (gwinfo == NULL) return;
  if (width != NULL) *width = gwinfo->lineWidth;
}

static set_state(gwinfo, which, value)
     StGWWinInfo *gwinfo;
     int which, value;
{
  int changed;
  
  if (gwinfo == NULL) return;
  switch (which) {
  case 'L': 
    if ((changed = (value != gwinfo->lineType))) gwinfo->lineType = value;
    break;
  case 'M': 
    if ((changed = (value != gwinfo->drawMode))) gwinfo->drawMode = value;
    break;
  case 'D':
    if ((changed = (value != gwinfo->drawColor))) gwinfo->drawColor = value;
    break;
  case 'B':
    if ((changed = (value != gwinfo->backColor))) gwinfo->backColor = value;
    break;
  case 'C':
    if ((changed = (value != gwinfo->use_color))) 
      gwinfo->use_color = (StScreenHasColor()) ? value : FALSE;
    break;
  }
}

StGWSetLineType(gwinfo, type)
     StGWWinInfo *gwinfo;
     int type;
{
  set_state(gwinfo, 'L', type);
}

StGWSetDrawMode(gwinfo, mode)
     StGWWinInfo *gwinfo;
     int mode;
{
  set_state(gwinfo, 'M', mode);
}

StGWSetDrawColor(gwinfo, color)
     StGWWinInfo *gwinfo;
     ColorCode color;
{
  set_state(gwinfo, 'D', (ColorCode) color);
}

StGWSetBackColor(gwinfo, color)
     StGWWinInfo *gwinfo;
     ColorCode color;
{
  set_state(gwinfo, 'B', (ColorCode) color);
}

StGWSetUseColor(gwinfo, use)
     StGWWinInfo *gwinfo;
     int use;
{
  set_state(gwinfo, 'C', use);
}

StGWSetLineWidth(gwinfo, width)
     StGWWinInfo *gwinfo;
     int width;
{
  int changed;
  
  if (gwinfo == NULL) return;
  changed = (width != gwinfo->lineWidth);
  if (changed) {
    gwinfo->lineWidth = width;
  }
}

StGWReverseColors(gwinfo)
     StGWWinInfo *gwinfo;
{
  ColorCode backColor, drawColor;
  
  if (gwinfo == NULL) return;
  backColor = StGWBackColor(gwinfo);
  drawColor = StGWDrawColor(gwinfo);
  if (backColor != drawColor) {
    StGWSetBackColor(gwinfo, drawColor);
    StGWSetDrawColor(gwinfo, backColor);
    StGWObRedraw(gwinfo->Object);
  }
}

StGWGetViewRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int *left, *top, *width, *height;
{
  /*+++++++++++++*/
  Pixwin *pw = canvas_pixwin(gwinfo->content_canvas);

  if (gwinfo != NULL) {
    if (left != NULL) *left = 0; 
    if (top != NULL) *top = 0;
    if (width != NULL) *width = gwinfo->canvasWidth;
    if (height != NULL) *height = gwinfo->canvasHeight;
  }
}

/**************************************************************************/
/**                                                                      **/
/**                           Drawing Functions                          **/
/**                                                                      **/
/**************************************************************************/

StGWDrawLine(gwinfo, x1, y1, x2, y2)
     StGWWinInfo *gwinfo;
     int x1, y1, x2, y2;
{
  static struct pr_texture tex, *texp;
  static struct pr_brush brush, *brushp;
  int op;

  if (gwinfo == NULL || gwinfo->window == NULL) return;

  op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC;

  if (gwinfo->drawMode != 0) {
    if (gwinfo->backColor == 0)
      op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC ^ PIX_DST;
    else
      op = PIX_COLOR(gwinfo->drawColor) | PIX_NOT(PIX_SRC ^ PIX_DST);
  }

  tex.pattern = pr_tex_dashed;
  texp = (gwinfo->lineType != 0) ? &tex : NULL;
  brush.width = gwinfo->lineWidth;
  brushp = &brush;

  if (buffering)
    pr_line(WorkPort, x1, y1, x2, y2, brushp, texp, op);
  else
    pw_line(canvas_pixwin(gwinfo->content_canvas), x1, y1, x2, y2,
	    brushp, texp, op);
}

StGWEraseRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  if (gwinfo == NULL || gwinfo->window == NULL) return;

  if (buffering)
    pr_rop(WorkPort, left, top, width, height,
	   PIX_COLOR(gwinfo->backColor) | PIX_SRC, 0, 0, 0);
  else
    pw_rop(canvas_pixwin(gwinfo->content_canvas), left, top, width, height, 
	   PIX_COLOR(gwinfo->backColor) | PIX_SRC, 0, 0, 0);  
}

StGWFrameRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  /*++++++++++*/
  int right = left + width - 1, bottom = top + height - 1;

  StGWDrawLine(gwinfo, left, top, right, top);
  StGWDrawLine(gwinfo, right, top, right, bottom);
  StGWDrawLine(gwinfo, right, bottom, left, bottom);
  StGWDrawLine(gwinfo, left, bottom, left, top);
} 

StGWPaintRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  if (gwinfo == NULL || gwinfo->window == NULL) return;

  if (buffering)
    pr_rop(WorkPort, left, top, width, height,
	   PIX_COLOR(gwinfo->drawColor) | PIX_SRC, 0, 0, 0);
  else
    pw_rop(canvas_pixwin(gwinfo->content_canvas), left, top, width, height, 
	   PIX_COLOR(gwinfo->drawColor) | PIX_SRC, 0, 0, 0);  
}

StGWEraseOval(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  /*+++++++++*/
}

StGWFrameOval(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  /*++++++++++*/
}

StGWPaintOval(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  /*++++++++++*/
}

StGWPaintArc(gwinfo, left, top, width, height, angle1, angle2)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     double angle1, angle2;
{
  /*++++++++++*/
}

StGWEraseArc(gwinfo, left, top, width, height, angle1, angle2)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     double angle1, angle2;
{
  /*++++++++++*/
}

StGWFrameArc(gwinfo, left, top, width, height, angle1, angle2)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     double angle1, angle2;
{
  /*++++++++++*/
}

StGWErasePoly(gwinfo, n, p, from_origin)
     StGWWinInfo *gwinfo;
     int n, from_origin;
     short *p;
{
  /*++++++++++*/
}

StGWFramePoly(gwinfo, n, p, from_origin)
     StGWWinInfo *gwinfo;
     int n, from_origin;
     short *p;
{
  /*++++++++++*/
}

StGWPaintPoly(gwinfo, n, p, from_origin)
     StGWWinInfo *gwinfo;
     int n, from_origin;
     short *p;
{
  /*++++++++++*/
}

StGWDrawPoint(gwinfo, x, y)
     StGWWinInfo *gwinfo;
     int x, y;
{
  if (gwinfo == NULL || gwinfo->window == NULL) return;
  if (buffering) pr_put(WorkPort, x, y, gwinfo->drawColor);
  else pw_put(canvas_pixwin(gwinfo->content_canvas),
	      x, y, gwinfo->drawColor);
}

StGWDrawBitmap(gwinfo, left, top, width, height, image)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     char *image;
{
  Pixrect *pr;
  int op, i, j;

  if (gwinfo == NULL || gwinfo->window == NULL) return;

  if (width <= 0 || height <= 0) return;
  pr = mem_create(width, height, 1);
  if (pr == NULL) return;

  for (i = 0; i < width; i++)
    for (j = 0; j < height; j++)
      if (image[i  + width * j] != 0) pr_put(pr, i, j, 1);

  if (gwinfo->drawMode != 0) {
    if (gwinfo->backColor == 0)
      op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC ^ PIX_DST;
    else
      op = PIX_COLOR(gwinfo->drawColor) | PIX_NOT(PIX_SRC ^ PIX_DST);
  }
  else {
    if (gwinfo->backColor == 0)
      op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC;
    else
      op = PIX_COLOR(gwinfo->drawColor) | PIX_NOT(PIX_SRC);
  }

  if (buffering)
    pr_rop(WorkPort, left, top, width, height, op, pr, 0, 0);
  else
    pw_rop(canvas_pixwin(gwinfo->content_canvas),
	   left, top, width, height, op, pr, 0, 0);
  
  mem_destroy(pr);
}

/**************************************************************************/
/**                                                                      **/
/**                            Text Functions                            **/
/**                                                                      **/
/**************************************************************************/

static short upchardat[64];
static mpr_static(MyUpCharPixRect, 32, 32, 1, (short *) upchardat);

StGWTextAscent(gwinfo)
     StGWWinInfo *gwinfo;
{
  struct pixfont *font = pf_default();

  /* a guess on the ascent */
  return(font->pf_defaultsize.y - font->pf_defaultsize.y / 6);
}

StGWTextDescent(gwinfo)
     StGWWinInfo *gwinfo;
{
  struct pixfont *font = pf_default();
  
  /* a guess on the descent */
  return(font->pf_defaultsize.y / 6 + 1);
}

StGWTextWidth(gwinfo, text)
     StGWWinInfo *gwinfo;
     char *text;
{
  struct pixfont *font;
  struct pr_size string_size;
  struct pr_prpos where;

  font = pf_default();
  if (text != NULL) {
    string_size = pf_textwidth(strlen(text), font, text);
    return(string_size.x);
  }
  else return (0);
}

StGWDrawString(gwinfo, s, x, y)
     StGWWinInfo *gwinfo;
     char *s;
     int x, y;
{
  Pixwin *pw;
  struct pr_prpos where;
  int op;

  if (s == NULL || gwinfo == NULL || gwinfo->window == NULL) return;

  pw = canvas_pixwin(gwinfo->content_canvas);
  if (gwinfo->drawMode != 0) {
    op = PIX_SRC ^ PIX_DST;
  }
  else {
    if (gwinfo->backColor == 0)
      op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC;
    else
      op = PIX_COLOR(gwinfo->drawColor) | PIX_NOT(PIX_SRC);
  }

  if (buffering) {
    where.pr = WorkPort;
    where.pos.x = x;
    where.pos.y = y;
    pf_text(where, op, pf_default(), s);
  }
  else
    pw_text(pw, x, y, op, NULL, s);
}

StGWDrawText(gwinfo, text, x, y, h, v)
     StGWWinInfo *gwinfo;
     char *text;
     int x, y, h, v;
{
  int FontAscent, string_width;

  FontAscent = StGWTextAscent(gwinfo);
  string_width = StGWTextWidth(gwinfo, text);

  if (v == 1) y += FontAscent;
  if (h == 1) x -= string_width / 2;
  if (h == 2) x -= string_width;

  StGWDrawString(gwinfo, text, x, y);
}

static draw_char_up(gwinfo, myChar, x, y)
     StGWWinInfo *gwinfo;
     char myChar;
     int x, y;
{
  struct pr_prpos where;
  char str[2];
  register int i, j;

  if (gwinfo == NULL || gwinfo->window == NULL) return;

  str[0] = myChar;
  str[1] = '\0';

  pr_rop(&MyUpCharPixRect, 0, 0, 32, 32, PIX_COLOR(0) | PIX_SRC, 0, 0, 0);
  where.pr = &MyUpCharPixRect;
  where.pos.x = 5;
  where.pos.y = 20;
  pf_text(where, PIX_SRC ^ PIX_DST, pf_default(), str);

  for (i = 0; i < 32; i++)
    for (j = 0; j < 32; j ++)
      if (pr_get(&MyUpCharPixRect, i, j)) {
	if (buffering) 
	  pr_put(WorkPort, x - 10 + j, y + 5 -i, gwinfo->drawColor);
	else 
	  pw_put(canvas_pixwin(gwinfo->content_canvas), 
		 x - 10 + j, y + 5 -i, gwinfo->drawColor);
      }
}

StGWDrawStringUp(gwinfo, s, x, y)
     StGWWinInfo *gwinfo;
     char *s;
     int x, y;
{
  char str[2];
  int n;
  
  str[1] = '\0';
  
  if (s == NULL || gwinfo == NULL) return;

  for (n = strlen(s); n > 0; n--, s++) {
  	draw_char_up(gwinfo, *s, x, y);
  	str[0] = *s;
  	y -= StGWTextWidth(gwinfo, str);
  }
}
 
StGWDrawTextUp(gwinfo, text, x, y, h, v)
     StGWWinInfo *gwinfo;
     char *text;
     int x, y, h, v;
{
  int FontAscent, string_width;

  FontAscent = StGWTextAscent(gwinfo);
  string_width = StGWTextWidth(gwinfo, text);

  if (v == 1) x -= FontAscent;
  if (h == 1) y += string_width / 2;
  if (h == 2) y += string_width;

  StGWDrawStringUp(gwinfo, text, x, y);
}

/**************************************************************************/
/**                                                                      **/
/**                           Symbol Functions                           **/
/**                                                                      **/
/**************************************************************************/
#define NUMSYMBOLS 18
#define SYMROWS 5

typedef struct {
  unsigned bit0 :1;
  unsigned bit1 :1;
  unsigned bit2 :1;
  unsigned bit3 :1;
  unsigned bit4 :1;
} SymBits;

typedef struct {
  int left, top, width, height;
  short *image;
  struct pixrect *pr;
  long refcon;
} Symbol;

Symbol Symbols[NUMSYMBOLS];

StGWSetSymRefCon(index, rc)
	unsigned int index;
	long rc;
{
  if (index < NUMSYMBOLS) Symbols[index].refcon = rc;
}

long StGWGetSymRefCon(index)
	unsigned int index;
{	
  if (index < NUMSYMBOLS) return(Symbols[index].refcon);
  else return(NULL);
}

static InitSymbol(sym, left, top, width, height)
     int sym, left, top, width, height;
{
  Symbols[sym].image = (short *) StCalloc(sizeof(short), SYMROWS);
  Symbols[sym].pr = mem_point(SYMROWS, SYMROWS, 1, Symbols[sym].image);
  Symbols[sym].left = left;
  Symbols[sym].top = top;
  Symbols[sym].width = width;
  Symbols[sym].height = height;
}

StGWGetSymbolSize(sym, width, height)
	int sym, *width, *height;
{
  *width = Symbols[sym].width;
  *height = Symbols[sym].height;
}
  
#ifdef DODO
static SetSymbolData(sym, row, bit0, bit1, bit2, bit3, bit4)
     int sym, row;
     int bit0, bit1, bit2, bit3, bit4;
{
  SymBits *d = (SymBits *) &(Symbols[sym].image[row]);
  d->bit0 = bit0;
  d->bit1 = bit1;
  d->bit2 = bit2;
  d->bit3 = bit3;
  d->bit4 = bit4;
}
#endif DODO
static SetSymbolData(sym, row, bit0, bit1, bit2, bit3, bit4)
     int sym, row;
     int bit0, bit1, bit2, bit3, bit4;
{
  pr_put(Symbols[sym].pr, 0, row, bit0);
  pr_put(Symbols[sym].pr, 1, row, bit1);
  pr_put(Symbols[sym].pr, 2, row, bit2);
  pr_put(Symbols[sym].pr, 3, row, bit3);
  pr_put(Symbols[sym].pr, 4, row, bit4);
}

static MakeSymbols()
{
  InitSymbol(0, 0, 0, 1, 1);
  SetSymbolData(0, 0, 1, 0, 0, 0, 0);
  SetSymbolData(0, 1, 0, 0, 0, 0, 0);
  SetSymbolData(0, 2, 0, 0, 0, 0, 0);
  SetSymbolData(0, 3, 0, 0, 0, 0, 0);
  SetSymbolData(0, 4, 0, 0, 0, 0, 0);

  InitSymbol(1, 0, 0, 2, 1);
  SetSymbolData(1, 0, 1, 1, 0, 0, 0);
  SetSymbolData(1, 1, 0, 0, 0, 0, 0);
  SetSymbolData(1, 2, 0, 0, 0, 0, 0);
  SetSymbolData(1, 3, 0, 0, 0, 0, 0);
  SetSymbolData(1, 4, 0, 0, 0, 0, 0);

  InitSymbol(2, 1, 1, 2, 2);
  SetSymbolData(2, 0, 1, 1, 0, 0, 0);
  SetSymbolData(2, 1, 1, 0, 0, 0, 0);
  SetSymbolData(2, 2, 0, 0, 0, 0, 0);
  SetSymbolData(2, 3, 0, 0, 0, 0, 0);
  SetSymbolData(2, 4, 0, 0, 0, 0, 0);

  InitSymbol(3, 1, 1, 2, 2);
  SetSymbolData(3, 0, 1, 1, 0, 0, 0);
  SetSymbolData(3, 1, 1, 1, 0, 0, 0);
  SetSymbolData(3, 2, 0, 0, 0, 0, 0);
  SetSymbolData(3, 3, 0, 0, 0, 0, 0);
  SetSymbolData(3, 4, 0, 0, 0, 0, 0);

  InitSymbol(4, 2, 2, 4, 4);
  SetSymbolData(4, 0, 0, 1, 1, 0, 0);
  SetSymbolData(4, 1, 1, 0, 0, 1, 0);
  SetSymbolData(4, 2, 1, 0, 0, 1, 0);
  SetSymbolData(4, 3, 0, 1, 1, 0, 0);
  SetSymbolData(4, 4, 0, 0, 0, 0, 0);

  InitSymbol(5, 2, 2, 4, 4);
  SetSymbolData(5, 0, 0, 1, 1, 0, 0);
  SetSymbolData(5, 1, 1, 1, 1, 1, 0);
  SetSymbolData(5, 2, 1, 1, 1, 1, 0);
  SetSymbolData(5, 3, 0, 1, 1, 0, 0);
  SetSymbolData(5, 4, 0, 0, 0, 0, 0);

  InitSymbol(6, 3, 3, 5, 5);
  SetSymbolData(6, 0, 0, 0, 1, 0, 0);
  SetSymbolData(6, 1, 0, 1, 0, 1, 0);
  SetSymbolData(6, 2, 1, 0, 0, 0, 1);
  SetSymbolData(6, 3, 0, 1, 0, 1, 0);
  SetSymbolData(6, 4, 0, 0, 1, 0, 0);

  InitSymbol(7, 3, 3, 5, 5);
  SetSymbolData(7, 0, 0, 0, 1, 0, 0);
  SetSymbolData(7, 1, 0, 1, 1, 1, 0);
  SetSymbolData(7, 2, 1, 1, 1, 1, 1);
  SetSymbolData(7, 3, 0, 1, 1, 1, 0);
  SetSymbolData(7, 4, 0, 0, 1, 0, 0);

  InitSymbol(8, 3, 3, 5, 5);
  SetSymbolData(8, 0, 0, 0, 1, 0, 0);
  SetSymbolData(8, 1, 0, 0, 1, 0, 0);
  SetSymbolData(8, 2, 1, 1, 1, 1, 1);
  SetSymbolData(8, 3, 0, 0, 1, 0, 0);
  SetSymbolData(8, 4, 0, 0, 1, 0, 0);

  InitSymbol(9, 3, 3, 5, 5);
  SetSymbolData(9, 0, 0, 1, 0, 1, 0);
  SetSymbolData(9, 1, 1, 1, 0, 1, 1);
  SetSymbolData(9, 2, 0, 0, 0, 0, 0);
  SetSymbolData(9, 3, 1, 1, 0, 1, 1);
  SetSymbolData(9, 4, 0, 1, 0, 1, 0);

  InitSymbol(10, 2, 2, 4, 4);
  SetSymbolData(10, 0, 1, 1, 1, 1, 0);
  SetSymbolData(10, 1, 1, 0, 0, 1, 0);
  SetSymbolData(10, 2, 1, 0, 0, 1, 0);
  SetSymbolData(10, 3, 1, 1, 1, 1, 0);
  SetSymbolData(10, 4, 0, 0, 0, 0, 0);

  InitSymbol(11, 2, 2, 4, 4);
  SetSymbolData(11, 0, 1, 1, 1, 1, 0);
  SetSymbolData(11, 1, 1, 1, 1, 1, 0);
  SetSymbolData(11, 2, 1, 1, 1, 1, 0);
  SetSymbolData(11, 3, 1, 1, 1, 1, 0);
  SetSymbolData(11, 4, 0, 0, 0, 0, 0);

  InitSymbol(12, 3, 3, 5, 5);
  SetSymbolData(12, 0, 0, 1, 1, 1, 0);
  SetSymbolData(12, 1, 1, 0, 0, 0, 1);
  SetSymbolData(12, 2, 1, 0, 0, 0, 1);
  SetSymbolData(12, 3, 0, 1, 0, 1, 0);
  SetSymbolData(12, 4, 0, 0, 1, 0, 0);

  InitSymbol(13, 3, 3, 5, 5);
  SetSymbolData(13, 0, 0, 1, 1, 1, 0);
  SetSymbolData(13, 1, 1, 1, 1, 1, 1);
  SetSymbolData(13, 2, 1, 1, 1, 1, 1);
  SetSymbolData(13, 3, 0, 1, 1, 1, 0);
  SetSymbolData(13, 4, 0, 0, 1, 0, 0);

  InitSymbol(14, 3, 3, 5, 5);
  SetSymbolData(14, 0, 0, 0, 1, 0, 0);
  SetSymbolData(14, 1, 0, 1, 0, 1, 0);
  SetSymbolData(14, 2, 1, 0, 0, 0, 1);
  SetSymbolData(14, 3, 1, 0, 0, 0, 1);
  SetSymbolData(14, 4, 0, 1, 1, 1, 0);

  InitSymbol(15, 3, 3, 5, 5);
  SetSymbolData(15, 0, 0, 0, 1, 0, 0);
  SetSymbolData(15, 1, 0, 1, 1, 1, 0);
  SetSymbolData(15, 2, 1, 1, 1, 1, 1);
  SetSymbolData(15, 3, 1, 1, 1, 1, 1);
  SetSymbolData(15, 4, 0, 1, 1, 1, 0);

  InitSymbol(16, 3, 3, 5, 5);
  SetSymbolData(16, 0, 1, 0, 0, 0, 1);
  SetSymbolData(16, 1, 0, 1, 0, 1, 0);
  SetSymbolData(16, 2, 0, 0, 1, 0, 0);
  SetSymbolData(16, 3, 0, 1, 0, 1, 0);
  SetSymbolData(16, 4, 1, 0, 0, 0, 1);

  InitSymbol(17, 3, 3, 5, 5);
  SetSymbolData(17, 0, 1, 1, 0, 1, 1);
  SetSymbolData(17, 1, 1, 1, 0, 1, 1);
  SetSymbolData(17, 2, 0, 0, 1, 0, 0);
  SetSymbolData(17, 3, 1, 1, 0, 1, 1);
  SetSymbolData(17, 4, 1, 1, 0, 1, 1);
}

StGWDrawSymbol(gwinfo, sym, x, y)
	StGWWinInfo *gwinfo;
	int sym, x, y;
{
  int op;

  if (gwinfo == NULL || gwinfo->window == NULL) return;

  if (gwinfo->drawMode != 0) {
    if (gwinfo->backColor == 0)
      op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC ^ PIX_DST;
    else
      op = PIX_COLOR(gwinfo->drawColor) | PIX_NOT(PIX_SRC ^ PIX_DST);
  }
  else {
    if (gwinfo->backColor == 0)
      op = PIX_COLOR(gwinfo->drawColor) | PIX_SRC;
    else
      op = PIX_COLOR(gwinfo->drawColor) | PIX_NOT(PIX_SRC);
  }

  if (buffering)
    pr_rop(WorkPort, 
	   x - Symbols[sym].left, y - Symbols[sym].top, 
	   Symbols[sym].width, Symbols[sym].height,
	   op, Symbols[sym].pr, 0, 0);
  else
    pw_rop(canvas_pixwin(gwinfo->content_canvas),
	   x - Symbols[sym].left, y - Symbols[sym].top,
	   Symbols[sym].width, Symbols[sym].height,
	   op, Symbols[sym].pr, 0, 0);
}

StGWReplaceSymbol(gwinfo, oldsym, newsym, x, y)
     StGWWinInfo *gwinfo;
     unsigned oldsym, newsym;
     int x, y;
{
  int oldwidth, oldheight, newwidth, newheight;
  
  if (oldsym >= NUMSYMBOLS || newsym >= NUMSYMBOLS) return;
  
  StGWGetSymbolSize(oldsym, &oldwidth, &oldheight);
  StGWGetSymbolSize(newsym, &newwidth, &newheight);
  if (oldwidth > newwidth || oldheight > newheight)
    StGWEraseRect(gwinfo, x - Symbols[oldsym].left, y - Symbols[oldsym].top,
		  oldwidth, oldheight);
  StGWDrawSymbol(gwinfo, newsym, x, y);
}

/**************************************************************************/
/**                                                                      **/
/**                         Buffering Functions                          **/
/**                                                                      **/
/**************************************************************************/

StGWStartBuffering(gwinfo)
     StGWWinInfo *gwinfo;
{
  buffering = TRUE;
  bufflevel++;
}

StGWBufferToScreen(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  IVIEW_WINDOW w;

  if (gwinfo == NULL || (w = gwinfo->window) == NULL) return;

  if (bufflevel > 0) bufflevel--;
  if (bufflevel > 0) return;
  if (! buffering) return;

  buffering = FALSE;
  pw_write(canvas_pixwin(gwinfo->content_canvas), left, top, width, height,
	   PIX_SRC, WorkPort, left, top);
}

StGWResetBuffer()
{
  bufflevel = 0;
  buffering = FALSE;  
}

/**************************************************************************/
/**                                                                      **/
/**                       Miscellaneous Functions                        **/
/**                                                                      **/
/**************************************************************************/

StGWSetRefCon(gwinfo, x)
     StGWWinInfo *gwinfo;
     long x;
{
  if (gwinfo == NULL) return;
  else gwinfo->RefCon = x;
}

long StGWGetRefCon(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(NULL);
  else return(gwinfo->RefCon);
}

GetWRefCon(w)
     Window w;
{
  return((int) window_get(w, WIN_CLIENT_DATA));
}
SetWRefCon(w, c)
     Window w;
     int c;
{
  window_set(w, WIN_CLIENT_DATA, c, 0);
}

StGWSetObject(gwinfo, x)
     StGWWinInfo *gwinfo;
     long x;
{
  if (gwinfo == NULL) return;
  else gwinfo->Object = x;
}

LVAL IViewWindowGetObject(w)
     IVIEW_WINDOW w;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);
  if (gwinfo == NULL) return(NIL);
  else return(gwinfo->Object);
}

#define NumBasicCursors 9
static int NumCursors;

typedef struct {
  Cursor curs;
  struct pixrect *pr;
  long refcon;
} cursor_entry;

static cursor_entry *curstab;
       
StGWSetCursRefCon(index, rc)
	unsigned int index;
	long rc;
{
  if (index < NumCursors) curstab[index].refcon = rc;
}

long StGWGetCursRefCon(index)
	unsigned int index;
{	
  if (index < NumCursors) return(curstab[index].refcon);
  else return(NULL);
}

StGWSetCursor(gwinfo, cursor)
     StGWWinInfo *gwinfo;
     unsigned int cursor;
{
  Cursor curs;
  
  if (gwinfo == NULL) return;
  curs = (cursor < NumCursors) ? curstab[cursor].curs : NULL;
  if (curs == NULL) curs = arrow_curs;
  if (gwinfo->window != NULL)
    window_set(gwinfo->content_canvas, WIN_CURSOR, curs, 0);
  gwinfo->cursor = cursor;
}

StGWCursor(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(FALSE);
  else return(gwinfo->cursor);
}

static MakeCursors()
{
  NumCursors = NumBasicCursors;
  curstab = (cursor_entry *) StCalloc(NumCursors, sizeof(cursor_entry));

  arrow_curs = cursor_create(CURSOR_IMAGE, &arrow_pix, 0);
  cursor_set(arrow_curs, CURSOR_OP, PIX_SRC ^ PIX_DST, 0);
  curstab[ARROW_CURSOR].curs = arrow_curs;

  finger_curs = cursor_create(CURSOR_IMAGE, &finger_pix, 0);
  cursor_set(finger_curs, CURSOR_OP, PIX_SRC ^ PIX_DST, 
	                  CURSOR_XHOT, 7, 0);
  curstab[FINGER_CURSOR].curs = finger_curs;

  hand_curs = cursor_create(CURSOR_IMAGE, &hand_pix, 0);
  cursor_set(hand_curs, CURSOR_OP, PIX_SRC ^ PIX_DST, 
	                CURSOR_XHOT, 7, 0);
  curstab[HAND_CURSOR].curs = hand_curs;

  brush_curs = cursor_create(CURSOR_IMAGE, &brush_pix, 0);
  cursor_set(brush_curs, CURSOR_OP, PIX_SRC ^ PIX_DST, 
	                 CURSOR_XHOT, 12, 0);
  curstab[BRUSH_CURSOR].curs = brush_curs;
}

StGWMakeCursor(n, image, mask, h, v, refcon)
	int n, h, v;
	char *image, *mask;
	long refcon;
{ 
  int index, i, j;
  char *temp;

  if (n != 16 && image == NULL) return(-1);
  for (index = 0;
       index < NumCursors && StGWGetCursRefCon(index) != NULL;
       index++);
  if (index >= NumCursors) {
    temp = realloc(curstab, (NumCursors + 1) * sizeof(cursor_entry));
    if (temp == NULL) return(-1);
    curstab = (cursor_entry *) temp;
    NumCursors++;
    curstab[index].curs = NULL;
    curstab[index].refcon = NULL;
    curstab[index].pr = NULL;
  }
  
  if (curstab[index].pr == NULL)
    curstab[index].pr = mem_create(16, 16, 1);
  if (curstab[index].pr == NULL) return(-1);
  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      if (image[i + j * 16] != 0) pr_put(curstab[index].pr, i, j, 1);

  if (curstab[index].curs != NULL) cursor_destroy(curstab[index].curs);
  curstab[index].curs = cursor_create(CURSOR_IMAGE, curstab[index].pr,
				      CURSOR_XHOT, h,
				      CURSOR_YHOT, v,
				      CURSOR_OP, PIX_SRC ^ PIX_DST,
				      0);
  if (curstab[index].curs == NULL) return(-1);
  curstab[index].refcon = refcon;
  return(index);
}

StGWMakeResCursor() { xlfail("not supported"); }

StGWFreeCursor(index)
     unsigned int index;
{
  if (index < NumCursors && index >= NumBasicCursors) {
    if (curstab[index].curs != NULL)
      cursor_destroy(curstab[index].curs);
    if (curstab[index].pr != NULL)
      mem_destroy(curstab[index].pr);
    curstab[index].curs = NULL;
    curstab[index].pr =NULL;
    curstab[index].refcon = NULL;
  }
  else xlfail("can't free standerd cursor");
}

StGWInitialDraw(gwinfo)
     StGWWinInfo *gwinfo;
{
  StGWObResize(gwinfo->Object);
  StGWObRedraw(gwinfo->Object);
  gwinfo->initialized = TRUE;
}

StGetScreenSize(width, height) 
     int *width, *height;
{
  Rect *r;

  if (BaseFrame == NULL) return;

  r = (Rect *) window_get(BaseFrame, WIN_SCREEN_RECT);
  if (width != NULL) *width = r->r_width;
  if (height != NULL) *height = r->r_height;
}

StFlushGraphics(){}

/**************************************************************************/
/**                                                                      **/
/**                         Scrolling Functions                          **/
/**                                                                      **/
/**************************************************************************/

StGWSetHasHscroll() {}
StGWSetHasVscroll() {}
StGWGetScroll() {}
StGWSetScroll() {}
StGWHasHscroll() { return(FALSE); }
StGWHasVscroll() { return(FALSE); }
StGWGetVscrollIncs() {}
StGWGetHscrollIncs() {}
StGWSetVscrollIncs() {}
StGWSetHscrollIncs() {}

/**************************************************************************/
/**                                                                      **/
/**                         Clipping Functions                           **/
/**                                                                      **/
/**************************************************************************/

StGWGetClipRect(gwinfo, a, b, c, d) 
     StGWWinInfo *gwinfo;
     int *a, *b,*c, *d;
{
  StGWGetViewRect(gwinfo, a, b, c, d);
}

StGWSetClipRect() {}

#include <stdio.h>

StGWDumpImage(gwinfo, file, scale)
     StGWWinInfo *gwinfo;
     FILE *file;
{
  int left, top, width, height;
  int x, y, padright;
  
  if (gwinfo == NULL || gwinfo->window == NULL) return;

  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  StGWStartBuffering(gwinfo);
  StGWObRedraw(gwinfo->Object);
  StGWResetBuffer();

  if (scale <= 0) scale = 1.0;

  /* Compute padding to round cols up to the nearest multiple of 8. */
  padright = ((width + 7) / 8) * 8 - width;

  /* write out the image */
  psputinit(file, width, height, scale);
  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++)
      psputbit(pr_get(WorkPort, x, y));
    for (x = 0; x < padright; x++)
      psputbit( 0 );
  }
  psputrest();
}

/**************************************************************************/
/**                                                                      **/
/**                    Graph Window Color Functions                      **/
/**                                                                      **/
/**************************************************************************/

#define NumColors 8

static struct {
  long refcon;
} ctable[NumColors];

StGWMakeColor(red, green, blue, refcon)
	double red, green, blue;
	long refcon;
{
  return(-1);
}

StGWFreeColor(index)
	unsigned int index;
{
  xlfail("not supported");
}

StGWSetColRefCon(index, rc)
	unsigned int index;
	long rc;
{
  if (index < NumColors) ctable[index].refcon = rc;
}

long StGWGetColRefCon(index)
	unsigned int index;
{	
  if (index < NumColors) return(ctable[index].refcon);
  else return(NULL);
}

