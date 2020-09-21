/* sunresizebr - resize brush dialog for SunView                       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

extern Canvas StGWCanvas();

typedef struct {
  int h, v;
} Point;

static SetRect(r, a, b, c, d)
     Rect *r;
     int a, b, c, d;
{
  r->r_left = a;
  r->r_top = b;
  r->r_width = c - a;
  r->r_height = d - b;
}

static Rect TrackIViewDrag(w, pt)
     IVIEW_WINDOW w;
     Point pt;
{
  int down;
  Rect r;
  Event myEvent, *event;
  int x, y;

  event = &myEvent;
  
  SetRect(&r, pt.h, pt.v, pt.h, pt.v);
  XorRect(w, r);
  do {
    window_read_event(StGWCanvas(w), event);
    canvas_event(StGWCanvas(w), event);
    down = ! event_is_up(event);
    XorRect(w, r);
    x = event_x(event);
    y = event_y(event);
    SetRect(&r, pt.h, pt.v, x, y);
    XorRect(w, r);
  } while (down);
  XorRect(w, r);
  if (r.r_height < 0) {
    r.r_height = -r.r_height;
    r.r_top = r.r_top - r.r_height;
  }
  if (r.r_width < 0) {
    r.r_width = -r.r_width;
    r.r_left = r.r_left - r.r_width;
  }
  return(r);
}

static int resizing_brush;

static resize_proc(win, event, arg)
     Window win;
     Event *event;
     caddr_t arg;
{
  static int down = FALSE;

  if (event_is_down(event)) {
    if (event_id(event) == MS_MIDDLE) resizing_brush = TRUE;
    window_return(0);
  }
}

static Rect GetNewIViewBrush(w)
     IVIEW_WINDOW w;
{
  Frame message_frame;
  Panel panel;
  Panel_item message_item;
  int left, top, width, height;
  Rect *mr, r;
  Point mousePt;
  Event myEvent, *event;

  event = &myEvent;

  StSunReleaseButton();
  message_frame = window_create(NULL, FRAME, FRAME_SHOW_LABEL, FALSE, 0);
  panel = window_create(message_frame, PANEL, WIN_EVENT_PROC, resize_proc, 0);
  message_item = panel_create_item(panel, PANEL_MESSAGE, PANEL_LABEL_STRING,
				   "Click middle button and drag", 0);
  window_fit(panel);
  window_fit(message_frame);

  /* center the message_frame frame on the screen */
  mr = (Rect *) window_get(message_frame, WIN_SCREEN_RECT);
  width = (int) window_get(message_frame, WIN_WIDTH);
  height = (int) window_get(message_frame, WIN_HEIGHT);
  left = (mr->r_width - width) / 2;
  top = (mr->r_height - height) / 2;
  if (left < 0) left = 0;
  if (top < 0) top = 0;
  window_set(message_frame, WIN_X, left, WIN_Y, top, 0);

  window_loop(message_frame);

  window_set(message_frame, FRAME_NO_CONFIRM, TRUE, 0);
  window_destroy(message_frame);

  if (resizing_brush) {
    window_read_event(StGWCanvas(w), event);
    canvas_event(StGWCanvas(w), event);
    mousePt.h = event_x(event);
    mousePt.v = event_y(event);
    return(TrackIViewDrag(w, mousePt));
  }
  else {
    IViewGetBrush(w, &r.r_left, &r.r_top, &r.r_width, &r.r_height);
    return (r);
  }
}

static XorRect(w, r)
     IVIEW_WINDOW w;
     Rect r;
{
  static struct pr_texture tex, *texp;
  Pixwin *pw = canvas_pixwin(StGWCanvas(w));
  int op;

  tex.pattern = pr_tex_dashed;
  texp = &tex;

  op = PIX_COLOR(1) | (PIX_SRC ^ PIX_DST);

  pw_line(pw, r.r_left, r.r_top, r.r_left + r.r_width, r.r_top,
	  NULL, texp, op);
  pw_line(pw, r.r_left + r.r_width, r.r_top, r.r_left + r.r_width,
	  r.r_top + r.r_height,
	  NULL, texp, op);
  pw_line(pw, r.r_left + r.r_width, r.r_top + r.r_height,
	  r.r_left, r.r_top + r.r_height,
	  NULL, texp, op);
  pw_line(pw, r.r_left, r.r_top + r.r_height, r.r_left, r.r_top,
	  NULL, texp, op);
}

IViewGetNewBrushSize(w, new_width, new_height)
	IVIEW_WINDOW w;
	int *new_width, *new_height;
{
  Rect r;

  r = GetNewIViewBrush(w);

  if (new_width != NULL) *new_width = r.r_width;
  if (new_height != NULL) *new_height = r.r_height;
  return(TRUE);
}
