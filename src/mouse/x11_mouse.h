#include <X11/Xlib.h>
int init_mouse();
void get_mouse_data(int fd, Display *display, int* x, int* y, char* left, char* middle, char* right);