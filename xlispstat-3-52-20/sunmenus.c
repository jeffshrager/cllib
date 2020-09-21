/* sunmenus - Low Level Menu Objects for SunView                       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/***********************************************************************/
/**                                                                   **/
/**                    General Includes and Definitions               **/
/**                                                                   **/
/***********************************************************************/

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

/* external variables */
extern LVAL s_true, s_title, s_items, s_enabled, s_id, s_menu_list, s_key,
  s_mark, s_style, s_action, s_menu, s_menu_proto, s_apple_menu_proto,
  s_menu_item_proto, sk_select, sk_update, sk_do_action, s_bold, s_italic,
  s_underline, s_outline, s_shadow, s_condense, s_extend, sk_enabled,
  s_hardware_address, sk_allocate, sk_dispose;

extern char buf[];

/* external functions */
extern LVAL slot_value();
extern IVIEW_MENU get_menu_address();

/***********************************************************************/
/**                                                                   **/
/**                       MENU-PROTO Definitions                      **/
/**                                                                   **/
/***********************************************************************/

StMObInstalled(m) LVAL m; { return(FALSE); }

/***********************************************************************/
/**                                                                   **/
/**                       MENU-PROTO Definitions                      **/
/**                                                                   **/
/***********************************************************************/

FORWARD char *get_item_string();

/***********************************************************************/
/**                                                                   **/
/**                        Support Function                           **/
/**                                                                   **/
/***********************************************************************/

LOCAL LVAL GetMenuList()
{
  return(slot_value(getvalue(s_menu_proto), s_menu_list));
}

/* find the position of the item in the menu */
static get_item_position(menu, item)
	LVAL menu, item;
{
  int i;
  LVAL items;
  
  for (items = slot_value(menu, s_items), i = 1;
       consp(items) && car(items) != item; i++, items = cdr(items))
    ;
  if (item != car(items)) xlfail("item not in the menu");
  return(i);
}

/***********************************************************************/
/**                                                                   **/
/**                            Menu Functions                         **/
/**                                                                   **/
/***********************************************************************/

/* remove item from a suntools menu */
StMObDeleteItem(menu, item)
	LVAL menu, item;
{
  if (StMObAllocated(menu)) 
    menu_set(get_menu_address(menu), MENU_REMOVE, get_item_position(menu, item),
                                    0);
}

StMObRemove() {}
StMObInstall() {}

/* dispose of a suntools menu */
StMObDisposeMach(menu)
	LVAL menu;
{
  if (StMObAllocated(menu)) menu_destroy(get_menu_address(menu));
}

StMObEnable() {}

/* allocate a suntools internal menu */
StMObAllocateMach(menu)
	LVAL menu;
{
  IVIEW_MENU theMenu;
   
  theMenu = menu_create(0);
  if (theMenu == NULL) xlfail("menu allocation failed");
  set_menu_address(theMenu, menu);
}

/* add items to a suntools internal menu */
StMObAppendItems(menu, items)
	LVAL menu, items;
{
  LVAL item;
  char *s;
  int i;
  IVIEW_MENU theMenu;
  Menu_item menu_item;

  if (StMObAllocated(menu)) {
    theMenu = get_menu_address(menu);
    i = llength(slot_value(menu, s_items)) - llength(items);
    if (i < 0) xlfail("append list should not exceed item list");
    
    for (; consp(items); items = cdr(items), i++) {
      item = car(items);
      s = get_item_string(item);
      menu_set(theMenu, MENU_STRING_ITEM, s, i + 1, 0);
      if (slot_value(item, s_enabled) == NIL) {
	menu_item = menu_get(theMenu, MENU_NTH_ITEM, i + 1);
	menu_set(menu_item, MENU_INACTIVE, TRUE, 0);
      }
    }
  }
}

static Menu popup_menu;
static int popup_item;
extern Event *mouse_event;
extern Window mouse_window;

static mouse_action(win, event, arg)
          Window win;
          Event *event;
          caddr_t arg;
{
  if (event_is_down(event) && event_id(event) == MS_RIGHT) {
    popup_item = (int) menu_show(popup_menu, win, event, 0);
    window_set(win, WIN_EVENT_PROC, NULL, 0);
    window_return(0);
  }
}

StMObPopup(menu,x, y, window)
	LVAL menu, window;
	int x, y;
{
  Frame message_frame;
  Panel panel;
  Panel_item message_item;
  int left, top, width, height;
  Rect *mr;

  StSunReleaseButton();

  send_message(menu, sk_update);
  send_message(menu, sk_allocate);
  popup_menu = get_menu_address(menu);
  if (mouse_event != NULL && mouse_window != NULL) {
    if (event_is_down(mouse_event)) {
      popup_item = (int) menu_show(popup_menu, mouse_window, mouse_event, 0);
    }
  }
  else {
    message_frame = window_create(NULL, FRAME, FRAME_SHOW_LABEL, FALSE, 0);
    panel = window_create(message_frame, PANEL, WIN_EVENT_PROC, 
			  mouse_action, 0);
    message_item = panel_create_item(panel, PANEL_MESSAGE, PANEL_LABEL_STRING,
				     "Click right button", 0);
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
  }

  send_message(menu, sk_dispose);

  return(popup_item);
}

/***********************************************************************/
/**                                                                   **/
/**                         Menu Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

/* get the item's title string */
static char *get_item_string(item)
	LVAL item;
{
  LVAL title;

  if (! menu_item_p(item)) xlerror("not a menu item", item);
  
  title = slot_value(item, s_title);
  if (! stringp(title)) xlerror("title is not a string", title);
  return((char *) getstring(title));
}

/* adjust internal implementation of allocated menu to new instance value */ 
StMObSetItemProp(item, which)
     LVAL item;
     int which;
{
  char *s, ch;
  IVIEW_MENU theMenu;
  LVAL menu;
  Menu_item menu_item;
  int i;
  
  menu = slot_value(item, s_menu);
  if (menu != NIL && StMObAllocated(menu)) {
    theMenu = get_menu_address(menu);
    i = get_item_position(menu, item);
    menu_item = menu_get(theMenu, MENU_NTH_ITEM, i);
    switch (which) {
    case 'T': {
                LVAL title = slot_value(item, s_title);
                if (! stringp(title))
                  xlerror("title is not a string", title);
                s = (char *) getstring(title); 
		menu_set(menu_item, MENU_STRING, s, 0);
                break;
              }
    case 'K': /* not implemented */ break; 
    case 'M': /* not implemented */ break;
    case 'S': /* not implemented */ break;
    case 'A': break;
    case 'E': if (slot_value(item, s_enabled) != NIL) 
                menu_set(menu_item, MENU_INACTIVE, FALSE, 0);
              else menu_set(menu_item, MENU_INACTIVE, TRUE, 0);
              break;
    default:  xlfail("unknown item instance variable");
    }
  }  
}

sun_show_menu(w, event)
	char *w;
	char *event;
{
  LVAL object, menu;
  int item;
  IVIEW_MENU m;
  
  object = IViewWindowGetObject(w);
  menu = slot_value(object, s_menu);
  if (menu_p(menu)) {
    item = StMObPopup(menu, event_x(event), event_y(event), NIL);
    if (item > 0) send_message1(menu, sk_select, item);
  }
}
