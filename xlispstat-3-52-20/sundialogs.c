/* sundialogs - dialogs support for SunView                            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "dialogs.h"
#include xlstat.h"
#include "xlgraph.h"
#include <fcntl.h>

extern Frame BaseFrame;

extern LVAL s_text_length;

typedef struct {
  LVAL object;
  int idleOn, frontOnly;
  int mouse_x, mouse_y;
  Panel panel;
} DialogData;

int inModal = FALSE;

/* layout definitions */
# define BUTTON_HEIGHT_PAD 20
# define BUTTON_WIDTH_PAD 15

# define TOGGLE_HEIGHT_PAD 5
# define TOGGLE_WIDTH_PAD 30

# define CHOICE_WIDTH_PAD 20
# define CHOICE_HEIGHT 30

# define TEXT_WIDTH_PAD 20
# define TEXT_HEIGHT_PAD 5

# define SCROLL_WIDTH 250
# define SCROLL_HEIGHT 20

/***********************************************************************/
/**                                                                   **/
/**                         Utility Functions                         **/
/**                                                                   **/
/***********************************************************************/

static Point StringSize(text)
     char *text;
{
  struct pixfont *font = pf_default();
  struct pr_size string_size;
  Point size;

  size.v = font->pf_defaultsize.y;
  string_size = pf_textwidth(strlen(text), font, text);
  size.h = string_size.x;
  return(size);
}

static FindItemType(item)
	LVAL item;
{
  if (consp(item)) return(ITEM_LIST);
  else if (button_item_p(item)) return(BUTTON_ITEM);
  else if (toggle_item_p(item)) return(TOGGLE_ITEM);
  else if (text_item_p(item)) return(TEXT_ITEM);
  else if (choice_item_p(item)) return(CHOICE_ITEM);
  else if (scroll_item_p(item)) return(SCROLL_ITEM);
  else if (list_item_p(item)) return(LIST_ITEM);
  else xlfail("item of unknown type");
}

static Panel_item FindPanelItem(item)
     LVAL item;
{
  Frame frame;
  LVAL dialog;
  DialogData *ddata;
  Panel panel;
  Panel_item panel_item, current_item;

  dialog = slot_value(item, s_dialog);
  if (! objectp(dialog)) xlerror("not a dialog object", dialog);
  frame = (Frame) GETDIALOGADDRESS(dialog);

  if (frame == NULL) return(NULL);
  else {
    ddata = (DialogData *) GetWRefCon(frame);
    panel = ddata->panel;
    
    panel_item = NULL;
    panel_each_item(panel, current_item)
      if ((LVAL) panel_get(current_item, PANEL_CLIENT_DATA) == item)
	panel_item = current_item;
    panel_end_each;
    
    if (panel_item == NULL) xlfail("can't find internal item");
    return(panel_item);
  }
}

/***********************************************************************/
/**                                                                   **/
/**                      General Dialog Functions                     **/
/**                                                                   **/
/***********************************************************************/
static done_proc(frame)
     Frame frame;
{
  DialogData *ddata = (DialogData *) GetWRefCon(frame);
  
  if (ddata != NULL) send_message(ddata->object, sk_close);
}

DialogAllocate(dialog)
  LVAL dialog;
{
  Frame dialog_frame;
  Panel dialog_panel;
  Point loc, size;
  char *title;
  int is_modeless;
  DialogData *ddata;

  if (check_dialog_address(dialog)) DialogRemove(dialog);
    
  if (! stringp(slot_value(dialog, s_title))) 
    xlerror("not a string", slot_value(dialog, s_title));
  title = (char *) getstring(slot_value(dialog, s_title));
  
  loc = ListToPoint(slot_value(dialog, s_location));
  size = ListToPoint(slot_value(dialog, s_size));
  
  is_modeless = (slot_value(dialog, s_type) == s_modeless) ? TRUE : FALSE;
  
  dialog_frame = window_create(BaseFrame, FRAME, 
			       FRAME_NO_CONFIRM, TRUE,
			       FRAME_SHOW_LABEL, is_modeless,
			       FRAME_LABEL, title, 0);
  set_dialog_address(dialog_frame, dialog);
  dialog_panel = window_create(dialog_frame, PANEL, 0);
  if (is_modeless) window_set(dialog_frame, WIN_SHOW, TRUE, 0);
  window_set(dialog_frame, FRAME_DONE_PROC, done_proc, 0);

  ddata = (DialogData *) StCalloc(sizeof(DialogData), 1);
  SetWRefCon(dialog_frame, ddata);
  ddata->object = dialog;
  ddata->panel = dialog_panel;

  InstallDialogItems(dialog);
  window_fit(dialog_panel);
  window_fit(dialog_frame);
}

DialogRemove(dialog)
	LVAL dialog;
{
  DialogData *ddata;
  Frame frame;

  if (check_dialog_address(dialog) 
      && (frame = (Frame) GETDIALOGADDRESS(dialog)) != NULL) {
    ddata = (DialogData *) GetWRefCon(frame);
    StFree(ddata);
    window_destroy(frame);
  }
  if (objectp(dialog)) standard_hardware_clobber(dialog);
}

DialogSetDefaultButton(dialog, item)
	LVAL dialog, item;
{
}

DialogReset()
{
  inModal = FALSE;
}

LVAL DialogGetModalItem(dialog) 
     LVAL dialog;
{
  Frame frame;
  int oldInModal = inModal;
  LVAL item = NIL;

  /*++++++ deal with interrupts++++++*/
  /*+++++++++ does not work on dialogs created as modeless ++++*/

  StSunReleaseButton();
  inModal = TRUE;
  frame = (Frame) GETDIALOGADDRESS(dialog);
  if (frame != NULL) {
    /* make sure window is NOT showing before using window_loop */
    if (window_get(frame, WIN_SHOW)) window_set(frame, WIN_SHOW, FALSE, 0);
    item = (LVAL) window_loop(frame);
  }
  inModal = oldInModal;
  return(item);
}

/***********************************************************************/
/**                                                                   **/
/**                    Item Installation Functions                    **/
/**                                                                   **/
/***********************************************************************/

static button_proc(item, event)
     Panel_item item;
     Event *event;
{
  LVAL lisp_item = (LVAL) panel_get(item, PANEL_CLIENT_DATA);

  if (inModal) window_return(lisp_item);
  else errset_send(lisp_item, sk_do_action);
}

static choice_proc(item, event)
     Panel_item item;
     Event *event;
{
  LVAL lisp_item = (LVAL) panel_get(item, PANEL_CLIENT_DATA);

  errset_send(lisp_item, sk_do_action);
}

static scroll_proc(item, value, event)
     Panel_item item;
     int value;
     Event *event;
{
  LVAL lisp_item = (LVAL) panel_get(item, PANEL_CLIENT_DATA);
  LVAL dialog;
  Frame frame;
  Panel panel;
  DialogData *ddata;
  int oldval, increment;
  int fd, oldflags, error;

  dialog = slot_value(lisp_item, s_dialog);
  frame = (Frame) GETDIALOGADDRESS(dialog);
  if (frame == NULL) xlfail("dialog not allocated");

  ddata = (DialogData *) GetWRefCon(frame);
  panel = ddata->panel;

  oldval = (int) panel_get(item, PANEL_VALUE);
  if (oldval < value) increment = 1;
  else if (oldval > value) increment = -1;
  else increment = 0;

  fd = (int) window_get(panel, WIN_FD);
  oldflags = fcntl(fd, F_GETFL, FNDELAY);
  fcntl(fd, F_SETFL, oldflags | FNDELAY);
  value = oldval;
  error = FALSE;
  while (! error && input_readevent(fd, event)) {
    value += increment;
    panel_set(item, PANEL_VALUE, value, 0);
    error = errset_send(lisp_item, sk_do_action);
  }
  error = FALSE;
  while (! error && ! event_is_up(event)) {
    value += increment;
    panel_set(item, PANEL_VALUE, value, 0);
    error = errset_send(lisp_item, sk_do_action);
    while (! error && input_readevent(fd, event)) {
      value += increment;
      panel_set(item, PANEL_VALUE, value, 0);
      error = errset_send(lisp_item, sk_do_action);
    }
  }
  fcntl(fd, F_SETFL, oldflags);

  errset_send(lisp_item, sk_do_action);
}
  
static InstallDialogItems(dialog)
     LVAL dialog;
{
  Frame frame;
  Panel panel;
  DialogData *ddata;
  LVAL items;

  frame = (Frame) GETDIALOGADDRESS(dialog);
  if (frame != NULL) {
    ddata = (DialogData *) GetWRefCon(frame);
    items = slot_value(dialog, s_items);
  
    if (ddata != NULL) {
      panel = ddata->panel;
      InstallItemList(panel, items);
    }
  }
}

static InstallItemList(panel, items)
	Panel panel;
	LVAL items;
{
  for (; consp(items); items = cdr(items))
    if (consp(car(items))) InstallItemList(panel, car(items));
    else InstallItem(panel, car(items));
}
  
static InstallItem(panel, item)
     Panel panel;
     LVAL item;
{
  int type;
  
  if (! dialog_item_p(item)) xlerror("not a dialog item", item);
  
  type = FindItemType(item);
  
  switch (type) {
  case BUTTON_ITEM: InstallButtonItem(panel, item); break;
  case TOGGLE_ITEM: InstallToggleItem(panel, item); break;
  case CHOICE_ITEM: InstallChoiceItem(panel, item); break;
  case TEXT_ITEM:   InstallTextItem(panel, item); break;
  case SCROLL_ITEM: InstallScrollItem(panel, item); break;
  default: xlfail("unkown item type");
  }
}

static InstallButtonItem(panel, item)
     Panel panel;
     LVAL item;
{
  char *text;
  Point loc;
  Panel_item button_item;

  if (! stringp(slot_value(item, s_text))) 
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));
    
  loc = ListToPoint(slot_value(item, s_location));

  button_item = panel_create_item(panel, PANEL_BUTTON, 
				   PANEL_LABEL_IMAGE, 
				   panel_button_image(panel, text, 0, 0),
				   PANEL_ITEM_X, loc.h,
				   PANEL_ITEM_Y, loc.v,
				   PANEL_CLIENT_DATA, item,
				   PANEL_NOTIFY_PROC, button_proc,
				   0);
}

static InstallToggleItem(panel, item)
     Panel panel;
     LVAL item;
{
  char *text;
  Point loc;
  Panel_item toggle_item;
  int value;

  if (! stringp(slot_value(item, s_text))) 
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));
    
  loc = ListToPoint(slot_value(item, s_location));
  value = (slot_value(item, s_value) != NIL) ? TRUE : FALSE;

  toggle_item = panel_create_item(panel, PANEL_TOGGLE, 
				  PANEL_CHOICE_STRINGS, text, 0,
				  PANEL_ITEM_X, loc.h,
				  PANEL_ITEM_Y, loc.v,
				  PANEL_CLIENT_DATA, item,
				  PANEL_NOTIFY_PROC, choice_proc,
				  0);
  DialogToggleItemValue(item, TRUE, value);
}

static InstallChoiceItem(panel, item)
     Panel panel;
     LVAL item;
{
  LVAL text;
  Point loc;
  Panel_item choice_item;
  char *str;
  int i;
  LVAL value;

  loc = ListToPoint(slot_value(item, s_location));

  choice_item = panel_create_item(panel, PANEL_CHOICE, 
				  PANEL_LAYOUT, PANEL_VERTICAL,
				  PANEL_ITEM_X, loc.h,
				  PANEL_ITEM_Y, loc.v,
				  PANEL_CLIENT_DATA, item,
				  PANEL_NOTIFY_PROC, choice_proc,
				  0);
  
  for (text = slot_value(item, s_text), i = 0;
       consp(text); 
       text = cdr(text), i++) {
    str = (char *) getstring(car(text));
    panel_set(choice_item, PANEL_CHOICE_STRING, i, str, 0);
  }
  value = slot_value(item, s_value);
  if (fixp(value)) DialogChoiceItemValue(item, TRUE, getfixnum(value));
}

static InstallTextItem(panel, item)
     Panel panel;
     LVAL item;
{
  char *text;
  Point loc;
  Panel_item panel_item;
  int editable, len;
  LVAL text_length;

  if (! stringp(slot_value(item, s_text))) 
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));
    
  loc = ListToPoint(slot_value(item, s_location));
  editable = (slot_value(item, s_editable) != NIL) ? TRUE : FALSE;

  if (editable) {
    text_length = slot_value(item, s_text_length);
    len = (fixp(text_length)) ? getfixnum(text_length) : 0;
    len = max(len, strlen(text));
    panel_item = panel_create_item(panel, PANEL_TEXT,
				   PANEL_VALUE, text,
				   PANEL_VALUE_DISPLAY_LENGTH, len,
				   PANEL_ITEM_X, loc.h,
				   PANEL_ITEM_Y, loc.v,
				   PANEL_CLIENT_DATA, item,
				   0);
  }
  else
    panel_item = panel_create_item(panel, PANEL_MESSAGE, 
				   PANEL_LABEL_STRING, text,
				   PANEL_ITEM_X, loc.h,
				   PANEL_ITEM_Y, loc.v,
				   PANEL_CLIENT_DATA, item,
				   0);
}

static InstallScrollItem(panel, item)
     Panel panel;
     LVAL item;
{
  Point loc, size;
  Panel_item scroll_item;
  int low, high, value;
  LVAL temp;

  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));

  temp = slot_value(item, s_min_value);
  low = fixp(temp) ? getfixnum(temp) : 0;
  temp = slot_value(item, s_max_value);
  high = fixp(temp) ? getfixnum(temp) : 100;
  temp = slot_value(item, s_value);
  value = (fixp(temp)) ? getfixnum(temp) : low;

  scroll_item = panel_create_item(panel, PANEL_SLIDER, 
				  PANEL_ITEM_X, loc.h,
				  PANEL_ITEM_Y, loc.v,
				  PANEL_SLIDER_WIDTH, size.h,
				  PANEL_MIN_VALUE, low,
				  PANEL_MAX_VALUE, high,
				  PANEL_VALUE, value,
				  PANEL_CLIENT_DATA, item,
				  PANEL_NOTIFY_LEVEL, PANEL_ALL,
				  PANEL_SHOW_RANGE, FALSE,
				  PANEL_SHOW_VALUE, FALSE,
				  PANEL_NOTIFY_PROC, scroll_proc,
				  0);
}

/***********************************************************************/
/**                                                                   **/
/**                       Dialog Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

DialogButtonGetDefaultSize(item, width, height)
	LVAL item;
	int *width, *height;
{
  LVAL text = slot_value(item, s_text);
  Point sz;
  
  if (! stringp(text)) xlerror("not a string", text);
  sz = StringSize(getstring(text));
  
  if (width != NULL) *width = sz.h + BUTTON_WIDTH_PAD;
  if (height != NULL) *height = sz.v + BUTTON_HEIGHT_PAD;
}

DialogToggleGetDefaultSize(item, width, height)
	LVAL item;
	int *width, *height;
{
  LVAL text = slot_value(item, s_text);
  Point sz;
  
  if (! stringp(text)) xlerror("not a string", text);
  sz = StringSize(getstring(text));
  
  if (width != NULL) *width = sz.h + TOGGLE_WIDTH_PAD;
  if (height != NULL) *height = sz.v + TOGGLE_HEIGHT_PAD;
}

LVAL DialogToggleItemValue(item, set, value) 
     LVAL item;
     int set, value;
{
  Panel_item panel_item = FindPanelItem(item);

  if (set) set_slot_value(item, s_value, (value) ? s_true : NIL);
  if (panel_item != NULL) {
    if (set) panel_set(panel_item, PANEL_TOGGLE_VALUE, 0, value, 0);
    value = (int) panel_get(panel_item, PANEL_TOGGLE_VALUE, 0);
    set_slot_value(item, s_value, (value) ? s_true : NIL);
  }
  return(slot_value(item, s_value));  
}

DialogChoiceGetDefaultSize(item, width, height)
	LVAL item;
	int *width, *height;
{
  Point sz, pt;
  LVAL text = slot_value(item, s_text);
  
  for (sz.h = 0, sz.v = 0; consp(text); text = cdr(text)) {
    pt = StringSize(getstring(car(text)));
    sz.h = max(sz.h, pt.h);
    sz.v += CHOICE_HEIGHT;
  }
  if (width != NULL) *width = sz.h + CHOICE_WIDTH_PAD;
  if (height != NULL) *height = sz.v;
}

LVAL DialogChoiceItemValue(item, set, value)
     LVAL item;
     int set, value;
{
  Panel_item panel_item = FindPanelItem(item);

  if (set) set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
  if (panel_item != NULL) {
    if (set) panel_set(panel_item, PANEL_VALUE, value, 0);
    value = (int) panel_get(panel_item, PANEL_VALUE);
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
  }
  return(slot_value(item, s_value));
}

DialogTextGetDefaultSize(item, width, height)
	LVAL item;
	int *width, *height;
{
  Point sz;
  LVAL text_length = slot_value(item, s_text_length);
  int len;

  sz = StringSize(getstring(slot_value(item, s_text)));
  len = sz.h;
  if (fixp(text_length)) {
    sz = StringSize("M");
    len = max(len, getfixnum(text_length) * sz.h);
  }
  if (width != NULL) *width = len + TEXT_WIDTH_PAD;
  if (height != NULL) *height = sz.v + TEXT_HEIGHT_PAD;
}

LVAL DialogTextItemText(item, set, text)
     LVAL item;
     int set;
     char *text;
{
  Panel_item panel_item = FindPanelItem(item);
  int editable = (slot_value(item, s_editable) != NIL) ? TRUE : FALSE;

  if (set) set_slot_value(item, s_text, cvstring(text));
  if (panel_item != NULL) {
    if (editable) {
      if (set) panel_set(panel_item, PANEL_VALUE, text, 0);
      text = panel_get(panel_item, PANEL_VALUE);
    }
    else {
      if (set) panel_set(panel_item, PANEL_LABEL_STRING, text, 0);
      text = panel_get(panel_item, PANEL_LABEL_STRING);
    }
    set_slot_value(item, s_text, cvstring(text));
  }
  return(slot_value(item, s_text));
}

DialogScrollGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  Point sz;

  if (width != NULL) *width = SCROLL_WIDTH;
  if (height != NULL) *height = SCROLL_HEIGHT;
}

LVAL DialogScrollItemValue(item, set, value)
     LVAL item;
     int set, value;
{
  Panel_item panel_item = FindPanelItem(item);
  
  if (set) set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
  if (panel_item != NULL) {
    if (set) panel_set(panel_item, PANEL_VALUE, value, 0);
    value = (int) panel_get(panel_item, PANEL_VALUE);
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
  }
  return(slot_value(item, s_value));
}

LVAL DialogScrollItemMax(item, set, value)
     LVAL item;
     int set, value;
{
  Panel_item panel_item = FindPanelItem(item);

  if (set) set_slot_value(item, s_min_value, cvfixnum((FIXTYPE) value));
  if (panel_item != NULL) {
    if (set) panel_set(panel_item, PANEL_MIN_VALUE, value, 0);
    value = (int) panel_get(panel_item, PANEL_MIN_VALUE);
    set_slot_value(item, s_min_value, cvfixnum((FIXTYPE) value));
  }
  return(slot_value(item, s_min_value));
}

LVAL DialogScrollItemMin(item, set, value)
     LVAL item;
     int set, value;
{
  Panel_item panel_item = FindPanelItem(item);

  if (set) set_slot_value(item, s_max_value, cvfixnum((FIXTYPE) value));
  if (panel_item != NULL) {
    if (set) panel_set(panel_item, PANEL_MAX_VALUE, value, 0);
    value = (int) panel_get(panel_item, PANEL_MAX_VALUE);
    set_slot_value(item, s_max_value, cvfixnum((FIXTYPE) value));
  }
  return(slot_value(item, s_max_value));
}

DialogListItemSetText() {}
LVAL DialogListItemSelection() {}
DialogListGetDefaultSize() {}

#ifdef COMMENT
multiple lines in static text items
#endif COMMENT
