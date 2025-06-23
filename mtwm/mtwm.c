#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>

#define MOD XCB_MOD_MASK_4 // Super key

// Configurable gap between windows (in pixels)
#define GAP 8

// Configurable border width around windows (in pixels)
#define BORDER_WIDTH 2

// Border colors (RGB values)
#define FOCUSED_BORDER_COLOR 0x4A9EFF  // Blue
#define UNFOCUSED_BORDER_COLOR 0x2E3440  // Dark gray

typedef struct
{
    xcb_window_t win;
    float width_factor;
} Client;

#define MAX_CLIENTS 32
Client clients[MAX_CLIENTS];
int client_count = 0;
int focused = -1;

xcb_connection_t *conn;
xcb_screen_t *scr;

void set_border_color(xcb_window_t win, uint32_t color) {
    xcb_change_window_attributes(conn, win, XCB_CW_BORDER_PIXEL, (uint32_t[]){color});
}

void update_focus_borders() {
    for (int i = 0; i < client_count; ++i) {
        uint32_t color = (i == focused) ? FOCUSED_BORDER_COLOR : UNFOCUSED_BORDER_COLOR;
        set_border_color(clients[i].win, color);
    }
    xcb_flush(conn);
}

void tile_windows() {
    int x = GAP; // Start with gap from left edge
    int screen_width = scr->width_in_pixels;
    int screen_height = scr->height_in_pixels;
    float total_factor = 0;

    for (int i = 0; i < client_count; ++i)
        total_factor += clients[i].width_factor;

    // Calculate available width (screen width minus gaps)
    int available_width = screen_width - (GAP * 2) - (GAP * (client_count - 1));

    for (int i = 0; i < client_count; ++i) {
        int w = (int)(available_width * (clients[i].width_factor / total_factor));
        xcb_configure_window(conn, clients[i].win,
                             XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y | XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
                             (uint32_t[]){x, GAP, w, screen_height - (GAP * 2)});
        x += w + GAP; // Add gap after each window
    }

    xcb_flush(conn);
}

void add_window(xcb_window_t win) {
    if (client_count >= MAX_CLIENTS)
        return;

    clients[client_count].win = win;
    clients[client_count].width_factor = 1.0;
    client_count++;
    focused = client_count - 1;
    
    // Set border on the window
    xcb_configure_window(conn, win, XCB_CONFIG_WINDOW_BORDER_WIDTH, (uint32_t[]){BORDER_WIDTH});
    
    tile_windows();
    update_focus_borders();
    xcb_set_input_focus(conn, XCB_INPUT_FOCUS_POINTER_ROOT, clients[focused].win, XCB_CURRENT_TIME);
}

void remove_window(xcb_window_t win) {
    for (int i = 0; i < client_count; ++i) {
        if (clients[i].win == win) {
            memmove(&clients[i], &clients[i + 1], sizeof(Client) * (client_count - i - 1));
            client_count--;
            if (client_count == 0) {
                focused = -1;
            } else if (focused >= client_count) {
                focused = client_count - 1;
            }
            tile_windows();
            if (focused >= 0) {
                xcb_set_input_focus(conn, XCB_INPUT_FOCUS_POINTER_ROOT, clients[focused].win, XCB_CURRENT_TIME);
                update_focus_borders();
            }
            break;
        }
    }
}

void resize_focused(float delta) {
    if (client_count == 0 || focused < 0)
        return;
    clients[focused].width_factor += delta;
    tile_windows();
}

void focus_next() {
    if (client_count == 0) return;
    focused = (focused + 1) % client_count;
    xcb_set_input_focus(conn, XCB_INPUT_FOCUS_POINTER_ROOT, clients[focused].win, XCB_CURRENT_TIME);
    update_focus_borders();
}

void focus_prev() {
    if (client_count == 0) return;
    focused = (focused - 1 + client_count) % client_count;
    xcb_set_input_focus(conn, XCB_INPUT_FOCUS_POINTER_ROOT, clients[focused].win, XCB_CURRENT_TIME);
    update_focus_borders();
}

void move_focused_left() {
    if (client_count <= 1 || focused <= 0) return;
    Client tmp = clients[focused];
    clients[focused] = clients[focused - 1];
    clients[focused - 1] = tmp;
    focused--;
    tile_windows();
}

void move_focused_right() {
    if (client_count <= 1 || focused < 0 || focused >= client_count - 1) return;
    Client tmp = clients[focused];
    clients[focused] = clients[focused + 1];
    clients[focused + 1] = tmp;
    focused++;
    tile_windows();
}

void grab_keys() {
    xcb_key_symbols_t *syms = xcb_key_symbols_alloc(conn);
    xcb_keycode_t l_key = *xcb_key_symbols_get_keycode(syms, 'l');
    xcb_keycode_t h_key = *xcb_key_symbols_get_keycode(syms, 'h');
    xcb_keycode_t j_key = *xcb_key_symbols_get_keycode(syms, 'j');
    xcb_keycode_t k_key = *xcb_key_symbols_get_keycode(syms, 'k');
    xcb_keycode_t d_key = *xcb_key_symbols_get_keycode(syms, 'd');
    xcb_keycode_t q_key = *xcb_key_symbols_get_keycode(syms, 'q');
    xcb_keycode_t return_key = *xcb_key_symbols_get_keycode(syms, 0xFF0D); // XK_Return

    // Focus left/right
    xcb_grab_key(conn, 1, scr->root, MOD, h_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+h
    xcb_grab_key(conn, 1, scr->root, MOD, l_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+l
    // Move window left/right
    xcb_grab_key(conn, 1, scr->root, MOD | XCB_MOD_MASK_SHIFT, h_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+Shift+h
    xcb_grab_key(conn, 1, scr->root, MOD | XCB_MOD_MASK_SHIFT, l_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+Shift+l
    // Resize left/right
    xcb_grab_key(conn, 1, scr->root, MOD | XCB_MOD_MASK_CONTROL, h_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+Ctrl+h
    xcb_grab_key(conn, 1, scr->root, MOD | XCB_MOD_MASK_CONTROL, l_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+Ctrl+l
    // Open terminal
    xcb_grab_key(conn, 1, scr->root, MOD, return_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+Enter
    // Close focused window
    xcb_grab_key(conn, 1, scr->root, MOD, q_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+q
    // Launch rofi
    xcb_grab_key(conn, 1, scr->root, MOD, d_key, XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); // MOD+d

    xcb_key_symbols_free(syms);
}

int main() {
    conn = xcb_connect(NULL, NULL);
    scr = xcb_setup_roots_iterator(xcb_get_setup(conn)).data;

    uint32_t values[] = {XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT |
                         XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY |
                         XCB_EVENT_MASK_PROPERTY_CHANGE |
                         XCB_EVENT_MASK_KEY_PRESS};

    xcb_change_window_attributes(conn, scr->root, XCB_CW_EVENT_MASK, values);
    grab_keys();
    xcb_flush(conn);

    xcb_generic_event_t *ev;
    while ((ev = xcb_wait_for_event(conn))) {
        switch (ev->response_type & ~0x80) {
        case XCB_MAP_REQUEST: {
            xcb_map_request_event_t *e = (xcb_map_request_event_t *)ev;
            xcb_map_window(conn, e->window);
            add_window(e->window);
            break;
        }
        case XCB_DESTROY_NOTIFY: {
            xcb_destroy_notify_event_t *e = (xcb_destroy_notify_event_t *)ev;
            remove_window(e->window);
            break;
        }
        case XCB_KEY_PRESS: {
            xcb_key_press_event_t *e = (xcb_key_press_event_t *)ev;
            xcb_key_symbols_t *syms = xcb_key_symbols_alloc(conn);
            xcb_keysym_t keysym = xcb_key_symbols_get_keysym(syms, e->detail, 0);

            // Check if MOD is pressed
            if (e->state & MOD) {
                // Focus navigation (MOD only)
                if (keysym == 'h' && !(e->state & XCB_MOD_MASK_SHIFT) && !(e->state & XCB_MOD_MASK_CONTROL)) {
                    focus_prev();
                    xcb_flush(conn);
                }
                else if (keysym == 'l' && !(e->state & XCB_MOD_MASK_SHIFT) && !(e->state & XCB_MOD_MASK_CONTROL)) {
                    focus_next();
                    xcb_flush(conn);
                }
                // Move windows (MOD + Shift)
                else if (keysym == 'h' && (e->state & XCB_MOD_MASK_SHIFT)) {
                    move_focused_left();
                    xcb_flush(conn);
                }
                else if (keysym == 'l' && (e->state & XCB_MOD_MASK_SHIFT)) {
                    move_focused_right();
                    xcb_flush(conn);
                }
                // Resize windows (MOD + Ctrl)
                else if (keysym == 'h' && (e->state & XCB_MOD_MASK_CONTROL)) {
                    resize_focused(-0.1);
                    xcb_flush(conn);
                }
                else if (keysym == 'l' && (e->state & XCB_MOD_MASK_CONTROL)) {
                    resize_focused(0.1);
                    xcb_flush(conn);
                }
                // Other actions (MOD only)
                else if (keysym == 0xFF0D) { // Enter
                    if (fork() == 0) {
                        setsid();                 // Start new session
                        execlp("st", "st", NULL); // Launch st
                        _exit(1);                 // If execlp fails
                    }
                    xcb_flush(conn);
                }
                else if (keysym == 'q') {
                    if (focused >= 0 && focused < client_count) {
                        xcb_destroy_window(conn, clients[focused].win);
                        xcb_flush(conn);
                    }
                }
                else if (keysym == 'd') {
                    if (fork() == 0) {
                        setsid();                 // Start new session
                        execlp("rofi", "/home/akisarou/dotfiles/rofi/launch", "-show", "drun", NULL);
                        _exit(1);                 // If execlp fails
                    }
                    xcb_flush(conn);
                }
            }

            xcb_key_symbols_free(syms);
            break;
        }
        }
        free(ev);
    }

    xcb_disconnect(conn);
    return 0;
}
